#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; agent.rkt -- main REPL loop with ESC-to-interrupt via threads
;;; Racket port of py-coding-agent/agent.py

(require racket/string
         racket/port
         racket/system
         racket/list
         racket/file
         json)

(require "fireworks-ai.rkt"
         "ollama-ai.rkt"
         "search.rkt"
         "tools.rkt"
         "approval.rkt"
         "interrupt.rkt")

(provide run
         reset-conversation
         print-banner
         show-history
         show-context
         compact-context
         handle-slash-command
         classify-intent)

;; ---------------------------------------------------------------------------
;; Config

(define SYSTEM-PROMPT-TEMPLATE
  "You are an interactive coding assistant working in the directory {cwd}.\n\nRules:\n- Use read_file, list_dir, and grep to understand the code BEFORE proposing edits.\n- To EDIT an existing file: read_file it first, then pass its exact current contents\n  as `old` to propose_edit.\n- To CREATE a new file: call propose_edit with the empty string \"\" as `old` and\n  the full desired contents as `new`. Do not call read_file first for a file that\n  does not exist yet.\n- One file per propose_edit call. Keep diffs small and focused.\n- If the user rejects an edit or `make check` fails, ask for clarification instead\n  of retrying blindly.\n- run_shell only accepts whitelisted commands: make, ls, pwd, cat, uv.\n- When you are done, reply with a short natural-language summary of what changed.")

(define GENERAL-SYSTEM-PROMPT
  "You are a helpful assistant. Answer the user's question clearly and concisely using the web search results provided. Do not reference files, directories, or code editing tools unless the user explicitly asks about code.")

(define COMPACT-SYSTEM-PROMPT
  "You are a context compactor for a coding assistant. Summarize the conversation transcript into a compact brief that will replace it. Preserve: the user's goals and instructions, decisions made, files created or modified (with paths), important code and tool-output details, and outstanding tasks. Write dense bullets, no preamble.")

(define GENERAL-KEYWORDS
  (list "movie" "film" "cinema" "theater" "theatre" "showing" "playing" "showtime"
        "weather" "forecast" "rain" "snow" "temperature outside"
        "restaurant" "recipe" "menu" "where to eat"
        "news" "sports" "score" "standings"
        "near me" "nearby" "directions to"
        "hotel" "flight" "travel" "vacation"
        "population of" "history of" "capital of"
        "who is " "who was " "where is " "when is " "when does "
        "price of" "cost of" "how much does"))

(define CODING-KEYWORDS
  (list ".lisp" ".py" ".js" ".ts" ".java" ".cpp" ".go" ".rb" ".rs" ".c "
        "def " "class " "function " "refactor" "implement " "compile" "makefile"
        "stacktrace" "segfault" "git commit" "git push" "git pull"
        "unit test" "pull request" "fix the bug" "add a function" "write a function"))

(define INTERRUPT-WAIT-TIMEOUT 180)

;; ---------------------------------------------------------------------------
;; LLM provider: 'fireworks (cloud, needs FIREWORKS_API_KEY) or 'ollama
;; (local models via http://localhost:11434, no key needed). Default can be
;; set with the AGENT_PROVIDER environment variable; switch at runtime with
;; the /provider command.

(define PROVIDER
  (make-parameter
   (let ([p (getenv "AGENT_PROVIDER")])
     (if (and p (string=? (string-downcase p) "ollama")) 'ollama 'fireworks))))

(define (using-ollama?) (eq? (PROVIDER) 'ollama))

(define (current-model-id)
  (if (using-ollama?) (OLLAMA-MODEL) (FIREWORKS-MODEL)))

(define (set-current-model! m)
  (if (using-ollama?) (OLLAMA-MODEL m) (FIREWORKS-MODEL m)))

(define (llm-chat msgs
                  #:max-tokens [max-tokens MAX-TOKENS]
                  #:temperature [temperature 0.6])
  (if (using-ollama?)
      (ollama-chat msgs
                   #:model-id (OLLAMA-MODEL)
                   #:max-tokens max-tokens
                   #:temperature temperature)
      (chat msgs
            #:model-id (FIREWORKS-MODEL)
            #:max-tokens max-tokens
            #:temperature temperature)))

(define (llm-chat-with-tools msgs tools)
  (if (using-ollama?)
      (ollama-chat-with-tools msgs tools #:model-id (OLLAMA-MODEL))
      (chat-with-tools msgs tools #:model-id (FIREWORKS-MODEL))))

;; Mutable state
(define messages-box (box '()))
(define search-enabled-box (box #f))
(define search-engine-box (box "brave")) ; "brave" or "exa"

;; ---------------------------------------------------------------------------
;; Skills:  ~/.agents/skills/<name>/SKILL.md

(define SKILLS-DIR
  (build-path (find-system-path 'home-dir) ".agents" "skills"))

(define (list-skills)
  (cond
    [(directory-exists? SKILLS-DIR)
     (sort
      (for/list ([e (in-list (directory-list SKILLS-DIR))]
                 #:when (file-exists? (build-path SKILLS-DIR e "SKILL.md")))
        (path->string e))
      string<?)]
    [else '()]))

(define (skill-file name)
  (build-path SKILLS-DIR name "SKILL.md"))

(define (skill-exists? name)
  (file-exists? (skill-file name)))

(define (skill-description name)
  ;; Parse the YAML frontmatter for a `description:` field; return #f if not found.
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define lines (string-split (file->string (skill-file name)) "\n"))
    (cond
      [(and (not (null? lines)) (string=? (string-trim (first lines)) "---"))
       (let loop ([rest (rest lines)])
         (cond
           [(null? rest) #f]
           [(string=? (string-trim (first rest)) "---") #f]
           [(string-prefix? (first rest) "description:")
            (string-trim (substring (first rest) 12))]
           [else (loop (rest rest))]))]
      [else #f])))

(define (show-skills)
  (define skills (list-skills))
  (cond
    [(null? skills)
     (displayln (format "No skills found in ~a" SKILLS-DIR))]
    [else
     (displayln (format "Available skills (from ~a):" SKILLS-DIR))
     (for ([s (in-list skills)])
       (define desc (skill-description s))
       (if desc
           (displayln (format "  /~a — ~a" s desc))
           (displayln (format "  /~a" s))))
     (displayln "\nType /<skill-name> to load a skill into the conversation.")]))

(define (load-skill name)
  (cond
    [(not (skill-exists? name))
     (displayln (format "Unknown command or skill: /~a  (try /skills)" name))]
    [else
     (with-handlers
       ([exn:fail? (lambda (e)
                     (displayln (format "Error loading skill '~a': ~a" name (exn-message e))))])
       (define content (file->string (skill-file name)))
       (define system-msg
         (hash 'role "system"
               'content
               (string-append
                (format "The user has loaded the following skill: '~a'. "
                        name)
                "Use it as authoritative reference and guidance for subsequent responses "
                "in this conversation.\n\n"
                content)))
       (set-box! messages-box (append (unbox messages-box) (list system-msg)))
       (displayln (format "Loaded skill: ~a  (~a chars)" name (string-length content))))]))

;; ---------------------------------------------------------------------------
;; Helpers

(define (clean text)
  ;; Python's textwrap.dedent + strip -- we just trim
  (string-trim text))

(define (reset-conversation)
  (define cwd (path->string (current-directory)))
  (define prompt (string-replace SYSTEM-PROMPT-TEMPLATE "{cwd}" cwd))
  (set-box! messages-box (list (hash 'role "system" 'content prompt))))

(define (print-banner)
  (displayln "")
  (displayln "Coding Agent REPL.  /help for commands, /quit to exit.")
  (displayln (format "  cwd:      ~a" (current-directory)))
  (displayln (format "  provider: ~a" (PROVIDER)))
  (displayln (format "  model:    ~a" (current-model-id)))
  (displayln ""))

(define (show-history)
  (for ([msg (in-list (unbox messages-box))])
    (define role (hash-ref msg 'role "?"))
    (define content (or (hash-ref msg 'content #f) "(no content)"))
    (displayln (format "\n--- ~a ---\n~a" role content))))

(define (message-char-size msg)
  ;; Approximate size of a message's contribution to the model context.
  (define (slen v) (if (string? v) (string-length v) 0))
  (+ (slen (hash-ref msg 'content ""))
     (slen (hash-ref msg 'reasoning_content ""))
     (let ([tcs (hash-ref msg 'tool_calls #f)])
       (if (list? tcs)
           (for/sum ([tc (in-list tcs)])
             (define f (hash-ref tc 'function (hash)))
             (+ (slen (hash-ref f 'name ""))
                (slen (hash-ref f 'arguments ""))))
           0))))

(define (message-preview msg)
  ;; Full single-line preview text (whitespace collapsed); wrapping is done
  ;; by wrap-preview at display time.
  (define raw
    (cond
      [(equal? (hash-ref msg 'role "?") "tool")
       (format "[~a] ~a" (hash-ref msg 'name "?") (or (hash-ref msg 'content #f) ""))]
      [else
       (define content (hash-ref msg 'content ""))
       (cond
         [(and (string? content) (not (string=? (string-trim content) ""))) content]
         [(hash-ref msg 'tool_calls #f)
          => (lambda (tcs)
               (format "[tool calls: ~a]"
                       (string-join
                        (for/list ([tc (in-list tcs)])
                          (hash-ref (hash-ref tc 'function (hash)) 'name "?"))
                        ", ")))]
         [else "(no content)"])]))
  (string-join (string-split (format "~a" raw)) " "))

(define PREVIEW-WIDTH 60)
(define PREVIEW-MAX-LINES 3)

(define (wrap-preview s)
  ;; Wrap s at PREVIEW-WIDTH (breaking on the last space in the window when
  ;; possible) into at most PREVIEW-MAX-LINES lines; "…" marks text that
  ;; still does not fit.
  (define len (string-length s))
  (let loop ([start 0] [lines '()])
    (cond
      [(>= start len) (reverse lines)]
      [(<= (- len start) PREVIEW-WIDTH)
       (reverse (cons (substring s start) lines))]
      [(= (length lines) (sub1 PREVIEW-MAX-LINES))
       (reverse (cons (string-append (substring s start (sub1 (+ start PREVIEW-WIDTH))) "…")
                      lines))]
      [else
       (define window (substring s start (+ start PREVIEW-WIDTH)))
       (define break
         (for/fold ([bp #f]) ([i (in-range (string-length window))])
           (if (char=? (string-ref window i) #\space) (add1 i) bp)))
       (define use (or break PREVIEW-WIDTH))
       (loop (+ start use)
             (cons (string-trim (substring s start (+ start use))) lines))])))

(define (show-context)
  (define msgs (unbox messages-box))
  (define total (for/sum ([m (in-list msgs)]) (message-char-size m)))
  (displayln "")
  (displayln (format "Context: ~a message~a, ~a chars, ~a tokens (est.)"
                     (length msgs)
                     (if (= (length msgs) 1) "" "s")
                     total
                     (quotient total 4)))
  (displayln "")
  (displayln (format " ~a  ~a  ~a  ~a"
                     (~a "#" #:width 3 #:align 'right)
                     (~a "role" #:width 9)
                     (~a "chars" #:width 7 #:align 'right)
                     "preview"))
  (displayln (format " ~a  ~a  ~a  ~a"
                     (make-string 3 #\-)
                     (make-string 9 #\-)
                     (make-string 7 #\-)
                     (make-string 50 #\-)))
  (for ([m (in-list msgs)] [i (in-naturals 1)])
    (define lines (wrap-preview (message-preview m)))
    (displayln (format " ~a  ~a  ~a  ~a"
                       (~a i #:width 3 #:align 'right)
                       (~a (hash-ref m 'role "?") #:width 9)
                       (~a (message-char-size m) #:width 7 #:align 'right)
                       (first lines)))
    (for ([extra (in-list (rest lines))])
      (displayln (format " ~a  ~a  ~a  ~a"
                         (make-string 3 #\space)
                         (make-string 9 #\space)
                         (make-string 7 #\space)
                         extra))))
  (displayln ""))

(define (compact-context)
  (define msgs (unbox messages-box))
  (cond
    [(<= (length msgs) 2)
     (displayln "Nothing to compact — conversation is already short.")]
    [else
     (define before (for/sum ([m (in-list msgs)]) (message-char-size m)))
     (displayln (format "Compacting ~a messages (~a chars)…" (length msgs) before))
     (define transcript
       (string-join
        (for/list ([m (in-list msgs)])
          (format "### ~a~a\n~a"
                  (hash-ref m 'role "?")
                  (if (hash-ref m 'tool_calls #f)
                      (format " (tool calls: ~a)"
                              (string-join
                               (for/list ([tc (in-list (hash-ref m 'tool_calls))])
                                 (hash-ref (hash-ref tc 'function (hash)) 'name "?"))
                               ", "))
                      "")
                  (let ([c (hash-ref m 'content "")])
                    (if (string? c) c ""))))
        "\n\n"))
     (define summary
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (displayln (format "Compaction failed: ~a" (exn-message e)))
                          #f)])
         (llm-chat (list (hash 'role "system" 'content COMPACT-SYSTEM-PROMPT)
                         (hash 'role "user" 'content transcript)))))
     (when summary
       ;; Keep the original system prompt; replace the rest with the summary.
       (set-box! messages-box
                 (list (first msgs)
                       (hash 'role "user"
                             'content
                             (string-append
                              "[Earlier conversation compacted to this summary. "
                              "Continue from where it left off.]\n\n"
                              summary))))
       (define after (for/sum ([m (in-list (unbox messages-box))]) (message-char-size m)))
       (displayln (format "Compacted: ~a → ~a chars." before after))
       (show-context))]))

;; ---------------------------------------------------------------------------
;; Slash commands

(define (handle-slash-command line)
  ;; Returns 'quit, 'continue, or #f (not a command)
  (cond
    [(or (string=? line "") (string=? line "/quit")) 'quit]
    [(string=? line "/reset")
     (reset-conversation)
     (displayln "Conversation reset.")
     'continue]
    [(string=? line "/history")
     (show-history)
     'continue]
    [(string=? line "/context")
     (show-context)
     'continue]
    [(string=? line "/compact")
     (compact-context)
     'continue]
    [(string-prefix? line "/model ")
     (define new-model (string-trim (substring line 7)))
     (set-current-model! new-model)
     (displayln (format "Model set to ~a" new-model))
     'continue]
    [(string=? line "/provider")
     (displayln (format "Current provider: ~a (model: ~a)" (PROVIDER) (current-model-id)))
     'continue]
    [(string-prefix? line "/provider ")
     (define p (string-downcase (string-trim (substring line 10))))
     (cond
       [(member p '("fireworks" "ollama"))
        (PROVIDER (string->symbol p))
        (displayln (format "Provider set to ~a (model: ~a)" p (current-model-id)))]
       [else
        (displayln (format "Unknown provider '~a' -- use 'fireworks' or 'ollama'" p))])
     'continue]
    [(string=? line "/debug")
     (DEBUG-LOG (not (DEBUG-LOG)))
     (displayln (format "Debug logging ~a" (if (DEBUG-LOG) "ON" "OFF")))
     'continue]
    [(string=? line "/tokens")
     (if (using-ollama?)
         (ollama-print-session-stats)
         (print-session-stats))
     'continue]
    [(string=? line "/search")
     (set-box! search-enabled-box (not (unbox search-enabled-box)))
     (displayln (format "Web search ~a (engine: ~a)"
                        (if (unbox search-enabled-box) "ON" "OFF")
                        (unbox search-engine-box)))
     'continue]
    [(string-prefix? line "/search ")
     (define engine (string-downcase (string-trim (substring line 8))))
     (cond
       [(member engine '("brave" "exa"))
        (set-box! search-engine-box engine)
        (set-box! search-enabled-box #t)
        (displayln (format "Web search ON (engine: ~a)" engine))]
       [else
        (displayln (format "Unknown engine '~a' — use 'brave' or 'exa'" engine))])
     'continue]
    [(string=? line "/help")
     (displayln "
Commands:
  /reset            clear conversation
  /history          dump message log
  /context          show a formatted summary of the current context
  /compact          compact history into a summary, then show the new context
  /model <id>       switch model (for the current provider)
  /provider         show current LLM provider
  /provider <name>  switch provider: fireworks (cloud) or ollama (local)
  /debug            toggle raw request/response logging
  /search           toggle web search on/off
  /search brave     enable Brave search
  /search exa       enable Exa search
  /tokens           show session token usage and estimated cost
  /skills           list available skills in ~/.agents/skills
  /<skill-name>     load that skill into the conversation
  /quit             exit

  ESC               interrupt the running task (stops before the next tool)
")
     'continue]
    [(string=? line "/skills")
     (show-skills)
     'continue]
    [(string-prefix? line "/")
     ;; Any other /xxx  --  treat as a skill name lookup.
     (load-skill (substring line 1))
     'continue]
    [else #f]))

;; ---------------------------------------------------------------------------
;; Intent classification

(define (heuristic-classify lower)
  (cond
    [(for/or ([kw (in-list GENERAL-KEYWORDS)])
       (string-contains? lower kw))
     "general"]
    [(for/or ([kw (in-list CODING-KEYWORDS)])
       (string-contains? lower kw))
     "coding"]
    [else #f]))

(define (llm-classify user-line)
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (format "[Classifier LLM error: ~a — defaulting to coding]" (exn-message e)))
                               "coding")])
    (define msgs
      (list (hash 'role "system" 'content "You are a one-word query classifier. Reply with exactly one word and nothing else.")
            (hash 'role "user" 'content
                  (string-append
                   "Classify this query as exactly one word — GENERAL, CODING, or HYBRID:\n"
                   "GENERAL = factual or informational; nothing to do with writing, editing, or debugging code.\n"
                   "CODING  = writing, editing, refactoring, or debugging code or files.\n"
                   "HYBRID  = coding question that benefits from web docs or library references.\n"
                   (format "Query: ~a\n" user-line)
                   "One-word answer:"))))
    (define raw (llm-chat msgs #:max-tokens 10 #:temperature 0.0))
    (define up (string-upcase (string-trim raw)))
    (cond
      [(string-contains? up "GENERAL") "general"]
      [(string-contains? up "HYBRID") "hybrid"]
      [else "coding"])))

(define (classify-intent user-line)
  (or (heuristic-classify (string-downcase user-line))
      (llm-classify user-line)))

;; ---------------------------------------------------------------------------
;; Search integration

(define (run-search query)
  (if (string=? (unbox search-engine-box) "exa")
      (exa-search query)
      (brave-search query)))

(define (format-search-results query results)
  ;; results : (listof (list url title desc))
  (define lines
    (cons (format "[Web search results for: ~s]" query)
          (for/list ([r (in-list results)] [i (in-naturals 1)])
            (format "~a. ~a\n   ~a\n   ~a" i (second r) (first r) (or (third r) "")))))
  (string-append (string-join lines "\n") "\n---"))

(define (maybe-search user-line force?)
  (cond
    [(not (or force? (unbox search-enabled-box))) #f]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (displayln (format "[Search error (~a): ~a]" (unbox search-engine-box) (exn-message e)))
                                  #f)])
       (define results (run-search user-line))
       (if (and results (not (null? results)))
           (string-append (format-search-results user-line results) "\n" user-line)
           #f))]))

;; ---------------------------------------------------------------------------
;; Send to model (intent-routed)

(define (send-to-model user-line)
  (define intent (classify-intent user-line))
  (define label
    (hash-ref (hash "general" "web search, no coding tools"
                    "coding"  "coding tools, no search"
                    "hybrid"  "coding tools + web search if /search is on")
              intent))
  (displayln (format "[intent: ~a → ~a]" intent label))
  (cond
    [(string=? intent "general")
     (define content (or (maybe-search user-line #t) user-line))
     (define msgs
       (list (hash 'role "system" 'content GENERAL-SYSTEM-PROMPT)
             (hash 'role "user" 'content content)))
     (define reply (llm-chat msgs))
     (displayln (format "\n~a" (clean reply)))]
    [(string=? intent "coding")
     (define updated (append (unbox messages-box) (list (hash 'role "user" 'content user-line))))
     (define-values (reply new-messages)
       (llm-chat-with-tools updated ENABLED-TOOLS))
     (set-box! messages-box new-messages)
     (displayln (format "\n~a" (clean reply)))]
    [else ; hybrid
     (define content (or (maybe-search user-line #f) user-line))
     (define updated (append (unbox messages-box) (list (hash 'role "user" 'content content))))
     (define-values (reply new-messages)
       (llm-chat-with-tools updated ENABLED-TOOLS))
     (set-box! messages-box new-messages)
     (displayln (format "\n~a" (clean reply)))]))

;; ---------------------------------------------------------------------------
;; ESC-to-interrupt via threads and stty raw mode

(define (escape-pressed?)
  ;; Non-blocking stdin check for bare ESC. Drains ANSI sequences.
  (cond
    [(not (char-ready? (current-input-port))) #f]
    [else
     (define ch (read-char (current-input-port)))
     (cond
       [(eof-object? ch) #f]
       [(not (char=? ch #\u001b)) #f]
       [else
        ;; Wait briefly for possible ANSI continuation
        (sleep 0.05)
        (if (char-ready? (current-input-port))
            ;; ANSI sequence -- drain
            (begin
              (let drain ()
                (when (char-ready? (current-input-port))
                  (read-char (current-input-port))
                  (sleep 0.02)
                  (drain)))
              #f)
            #t)])]))

(define (run-model-with-escape thunk)
  ;; Run thunk in background thread; main thread polls for ESC in non-canonical
  ;; mode (echo off, opost still on so newlines stay tidy).  Sets
  ;; task-interrupted on ESC.  Returns #t if ESC was pressed.
  (task-interrupted-clear!)
  (define worker (thread thunk))
  (define has-stty?
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (and (terminal-port? (current-input-port))
           (system "stty -g >/dev/null 2>&1"))))
  (define saved-stty (and has-stty? (save-stty-state)))
  (define escaped? #f)
  (when has-stty? (enter-cbreak-mode))
  (dynamic-wind
    void
    (lambda ()
      (let loop ()
        (when (thread-running? worker)
          (when (and (not escaped?) (escape-pressed?))
            (set! escaped? #t)
            (task-interrupted-set!)
            (display "\n[ESC — stopping after the current step…]\n")
            (flush-output))
          (sleep 0.05)
          (loop))))
    (lambda ()
      (when has-stty?
        (if saved-stty
            (restore-stty-state saved-stty)
            (system "stty sane 2>/dev/null")))))
  ;; Wait up to INTERRUPT-WAIT-TIMEOUT for worker to finish
  (define done? (sync/timeout INTERRUPT-WAIT-TIMEOUT worker))
  (unless done?
    (displayln (format "[Task did not stop within ~as; worker may still be running in background]"
                       INTERRUPT-WAIT-TIMEOUT)))
  escaped?)

;; ---------------------------------------------------------------------------
;; Main REPL

(define (run)
  (register-all)
  (reset-conversation)
  (print-banner)
  (let loop ()
    (display "\n> ")
    (flush-output)
    (define line
      (with-handlers ([exn:fail? (lambda (_) eof)])
        (read-line (current-input-port) 'any)))
    (cond
      [(eof-object? line)
       (displayln "")
       (void)]
      [else
       (define trimmed (string-trim line))
       (cond
         [(string=? trimmed "") (loop)]
         [else
          (define cmd (handle-slash-command trimmed))
          (cond
            [(eq? cmd 'quit) (void)]
            [(eq? cmd 'continue) (loop)]
            [else
             (define (task)
               (with-handlers ([exn:fail? (lambda (e)
                                            (unless (task-interrupted?)
                                              (displayln (format "\nError talking to model: ~a" (exn-message e)))
                                              (flush-output)))])
                 (send-to-model trimmed)))
             (define interrupted? (run-model-with-escape task))
             (when interrupted?
               (displayln "[Interrupted. Type your next request or /reset.]")
               (flush-output))
             (loop)])])])))

(module+ main
  (run))
