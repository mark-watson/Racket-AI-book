#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; agent.rkt -- main REPL loop with ESC-to-interrupt via threads
;;; Racket port of py-coding-agent/agent.py

(require racket/string
         racket/port
         racket/system
         racket/list
         json)

(require "fireworks-ai.rkt"
         "search.rkt"
         "tools.rkt")

(provide run
         reset-conversation
         print-banner
         show-history
         handle-slash-command
         classify-intent)

;; ---------------------------------------------------------------------------
;; Config

(define SYSTEM-PROMPT-TEMPLATE
  "You are an interactive coding assistant working in the directory {cwd}.\n\nRules:\n- Use read_file, list_dir, and grep to understand the code BEFORE proposing edits.\n- To EDIT an existing file: read_file it first, then pass its exact current contents\n  as `old` to propose_edit.\n- To CREATE a new file: call propose_edit with the empty string \"\" as `old` and\n  the full desired contents as `new`. Do not call read_file first for a file that\n  does not exist yet.\n- One file per propose_edit call. Keep diffs small and focused.\n- If the user rejects an edit or `make check` fails, ask for clarification instead\n  of retrying blindly.\n- run_shell only accepts whitelisted commands: make, ls, pwd, cat.\n- When you are done, reply with a short natural-language summary of what changed.")

(define GENERAL-SYSTEM-PROMPT
  "You are a helpful assistant. Answer the user's question clearly and concisely using the web search results provided. Do not reference files, directories, or code editing tools unless the user explicitly asks about code.")

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

;; Mutable state
(define messages-box (box '()))
(define search-enabled-box (box #f))
(define search-engine-box (box "brave")) ; "brave" or "exa"

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
  (displayln (format "  cwd:   ~a" (current-directory)))
  (displayln (format "  model: ~a" (FIREWORKS-MODEL)))
  (displayln ""))

(define (show-history)
  (for ([msg (in-list (unbox messages-box))])
    (define role (hash-ref msg 'role "?"))
    (define content (or (hash-ref msg 'content #f) "(no content)"))
    (displayln (format "\n--- ~a ---\n~a" role content))))

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
    [(string-prefix? line "/model ")
     (define new-model (string-trim (substring line 7)))
     (FIREWORKS-MODEL new-model)
     (displayln (format "Model set to ~a" new-model))
     'continue]
    [(string=? line "/debug")
     (DEBUG-LOG (not (DEBUG-LOG)))
     (displayln (format "Debug logging ~a" (if (DEBUG-LOG) "ON" "OFF")))
     'continue]
    [(string=? line "/tokens")
     (print-session-stats)
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
  /model <id>       switch model
  /debug            toggle raw request/response logging
  /search           toggle web search on/off
  /search brave     enable Brave search
  /search exa       enable Exa search
  /tokens           show session token usage and estimated cost
  /quit             exit

  ESC               interrupt the running task (stops before the next tool)
")
     'continue]
    [(string-prefix? line "/")
     (displayln (format "Unknown command: ~a" line))
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
    (define raw (chat msgs #:model-id (FIREWORKS-MODEL) #:max-tokens 10 #:temperature 0.0))
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
     (define reply (chat msgs #:model-id (FIREWORKS-MODEL)))
     (displayln (format "\n~a" (clean reply)))]
    [(string=? intent "coding")
     (define updated (append (unbox messages-box) (list (hash 'role "user" 'content user-line))))
     (define-values (reply new-messages)
       (chat-with-tools updated ENABLED-TOOLS #:model-id (FIREWORKS-MODEL)))
     (set-box! messages-box new-messages)
     (displayln (format "\n~a" (clean reply)))]
    [else ; hybrid
     (define content (or (maybe-search user-line #f) user-line))
     (define updated (append (unbox messages-box) (list (hash 'role "user" 'content content))))
     (define-values (reply new-messages)
       (chat-with-tools updated ENABLED-TOOLS #:model-id (FIREWORKS-MODEL)))
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
  ;; Run thunk in background thread; main thread polls for ESC in raw mode.
  ;; Sets task-interrupted on ESC. Returns #t if ESC was pressed.
  (task-interrupted-clear!)
  (define worker (thread thunk))
  (define has-stty?
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (and (terminal-port? (current-input-port))
           (system "stty -g >/dev/null 2>&1"))))
  (define escaped? #f)
  (when has-stty?
    (system "stty raw -echo 2>/dev/null"))
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
        (system "stty sane 2>/dev/null"))))
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
