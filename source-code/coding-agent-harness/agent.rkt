#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; agent.rkt -- main REPL loop
;;; Racket port of py-coding-agent/agent.py

(require racket/string
         racket/port
         racket/system
         racket/list
         racket/file
         racket/cmdline
         json)

(require "fireworks-ai.rkt"
         "mlx-serve.rkt"
         "harness-config.rkt"
         "search.rkt"
         "tools.rkt"
         "approval.rkt"
         "line-input.rkt")

(provide run
         run-repl
         run-one-shot
         reset-conversation
         print-banner
         show-history
         show-context
         compact-context
         handle-slash-command
         classify-intent
         VERSION
         cli-main
         build-prompt
         resolve-cwd
         load-config
         completion-candidates)

;; ---------------------------------------------------------------------------
;; Version

(define VERSION "0.2.0")

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

;; ---------------------------------------------------------------------------
;; LLM provider dispatch
;;
;; Providers are named profiles declared in ~/.coding_harness.json and/or
;; .local_coding_harness.json (see harness-config.rkt). Two wire types are
;; supported: "mlx" (local mlx_lm.server -- OpenAI-compatible /v1/chat/completions,
;; no API key) and "openai" (Fireworks.ai and compatible endpoints).
;;
;; When NO harness config exists, fall back to the legacy built-ins
;; 'fireworks / 'mlx (controlled by AGENT_PROVIDER / /provider) so old
;; setups keep working unchanged.

;; Session-level model override set by /model or --model; #f means "use the
;; model declared by the active provider profile".  All provider data
;; (endpoint, model, api_key_env, generation, pricing) lives in the harness
;; config, so nothing provider-specific is compiled in here.

(define model-override (box #f))

(define (config-loaded?)
  ;; Any harness config loaded at all?
  (not (hash-empty? (harness-config))))

(define (active-provider-hash)
  (and (config-loaded?) (config-active-provider)))

(define (require-provider who)
  ;; -> provider hash, or a clear error when nothing is configured.
  (or (active-provider-hash)
      (error who "~a"
             (string-append
              "no active provider profile; define \"providers\" in "
              "~/.coding_harness.json or .local_coding_harness.json"))))

(define (active-provider-type)
  ;; -> 'mlx | 'openai  (wire format of the active chat provider)
  (define p (active-provider-hash))
  (if p (provider-type p) 'openai))

(define (using-mlx?) (eq? (active-provider-type) 'mlx))

(define (current-provider-name-or-legacy)
  (or (config-active-provider-name) "?"))

(define (current-model-id)
  (or (unbox model-override)
      (let ([p (active-provider-hash)])
        (and p (provider-model p)))
      "?"))

(define (set-current-model! m)
  ;; /model <id> or --model: override the active profile's model this session.
  (set-box! model-override m))

(define (switch-provider! name)
  ;; Select a profile and drop any model override so the new profile's own
  ;; model takes effect (provider selection is always applied before --model).
  (define active (config-set-active-provider! name))
  (set-box! model-override #f)
  active)

;; Plain (no-tools) chat and agentic (tool-calling) chat both dispatch on the
;; active provider profile.  Explicit keyword args win; otherwise generation
;; parameters come from the profile, and a parameter the profile omits is left
;; out of the request rather than defaulted in code.
(define (chat-provider-hash)   (active-provider-hash))

(define (apply-mlx-provider! provider)
  ;; Point the mlx module at the given profile (or clear).
  (mlx-active-provider (or provider #f)))

(define (profile-gen provider key explicit)
  (or explicit (generation-ref (provider-generation provider) key #f)))

;; Plain (no-tools) chat
(define (llm-chat msgs
                  #:max-tokens [max-tokens #f]
                  #:temperature [temperature #f])
  (define p (require-provider 'llm-chat))
  (define mt (profile-gen p 'max_tokens max-tokens))
  (define tp (profile-gen p 'temperature temperature))
  (case (provider-type p)
    [(mlx)
     (parameterize ([mlx-active-provider p])
       (mlx-chat msgs #:model-id (current-model-id)
                 #:max-tokens mt #:temperature tp))]
    [else
     (chat msgs #:model-id (current-model-id)
           #:max-tokens mt #:temperature tp)]))

;; Agentic (tool-calling) chat -- uses the same active provider as plain chat.
(define (llm-chat-with-tools msgs tools)
  (define p (require-provider 'llm-chat-with-tools))
  (case (provider-type p)
    [(mlx)
     (parameterize ([mlx-active-provider p])
       (mlx-chat-with-tools msgs tools #:model-id (current-model-id)))]
    [else
     (chat-with-tools msgs tools #:model-id (current-model-id))]))

;; Mutable state
(define messages-box (box '()))
(define search-enabled-box (box #f))
(define search-engine-box (box "brave")) ; "brave" or "exa"

;; ---------------------------------------------------------------------------
;; CLI / config state

(define cli-quiet? (make-parameter #f))
(define cli-plain? (make-parameter #f))

;; Exit codes (section 5)
(define EXIT-OK 0)
(define EXIT-MODEL-ERROR 1)
(define EXIT-CHECK-FAILED 2)
(define EXIT-REJECTED 3)
(define EXIT-BAD-ARGS 5)

(define (exit-with-code code)
  ;; Flush before exit so piped output is not lost
  (flush-output)
  (flush-output (current-error-port))
  (exit code))

;; ---------------------------------------------------------------------------
;; Config file + env (section 9)
;; Precedence: CLI flags > env > config file > compiled defaults
;; Config file: $XDG_CONFIG_HOME/coding-agent/config.rktd  or
;;              ~/.config/coding-agent/config.rktd  (also .rkt)
;; Contents: a hash literal, e.g.  #hash((provider . "mlx") (model . "foo") (quiet . #t))
;;           or an alist  '((provider . "mlx") (model . "foo"))
;; Unknown keys are ignored.

(define (config-file-path)
  (define xdg (getenv "XDG_CONFIG_HOME"))
  (define base
    (cond
      [(and xdg (not (string=? xdg ""))) (string->path xdg)]
      [else (build-path (find-system-path 'home-dir) ".config")]))
  (define dir (build-path base "coding-agent"))
  (define rktd (build-path dir "config.rktd"))
  (define rkt (build-path dir "config.rkt"))
  (cond
    [(file-exists? rktd) rktd]
    [(file-exists? rkt) rkt]
    [else rktd]))

(define (alist->hash alist)
  (for/hash ([p (in-list alist)])
    (values (if (symbol? (car p)) (car p) (string->symbol (format "~a" (car p))))
            (cdr p))))

(define (load-config)
  (define p (config-file-path))
  (with-handlers ([exn:fail? (lambda (_) (hash))])
    (if (file-exists? p)
        (let ([v (with-input-from-file p read)])
          (cond
            [(hash? v) v]
            [(and (list? v) (andmap pair? v)) (alist->hash v)]
            [else (hash)]))
        (hash))))

(define (hash-ref* h k default)
  (cond
    [(hash-has-key? h k) (hash-ref h k)]
    [(hash-has-key? h (string->symbol (format "~a" k))) (hash-ref h (string->symbol (format "~a" k)))]
    [(hash-has-key? h (format "~a" k)) (hash-ref h (format "~a" k))]
    [else default]))

(define (apply-config-hash! cfg)
  ;; Legacy flat config (.rktd): provider/model/quiet/plain/debug/search.
  ;; Only applies fully when there is NO harness JSON config; a harness config
  ;; takes precedence for provider selection (legacy still sets flags like
  ;; quiet/plain/debug which do not conflict).
  (define legacy-provider?
    (not (config-loaded?)))
  ;; provider
  (define prov (or (hash-ref* cfg 'provider #f) (hash-ref* cfg 'AGENT_PROVIDER #f)))
  (when (and legacy-provider? prov (string? prov))
    (define low (string-downcase (string-trim prov)))
    ;; provider names are harness-config profile names; none are built in
    (when (config-provider low)
      (switch-provider! low)))
  ;; model  (provider-aware)
  (define mdl (or (hash-ref* cfg 'model #f) (hash-ref* cfg 'MODEL #f)))
  (when (and mdl (string? mdl) (not (string=? mdl "")))
    (set-current-model! mdl))
  ;; quiet / plain
  (when (hash-ref* cfg 'quiet #f)
    (cli-quiet? #t) (quiet-mode? #t))
  (when (hash-ref* cfg 'plain #f)
    (cli-plain? #t) (color-enabled? #f))
  ;; debug
  (when (hash-ref* cfg 'debug #f)
    (DEBUG-LOG #t))
  ;; search / search-engine
  (define se (hash-ref* cfg 'search-engine #f))
  (when (and se (string? se) (member (string-downcase se) '("brave" "exa")))
    (set-box! search-engine-box (string-downcase se)))
  (when (hash-has-key? cfg 'search)
    (set-box! search-enabled-box (if (hash-ref* cfg 'search #f) #t #f)))
  ;; cwd is not applied from config automatically (security); leave for CLI --cwd
  (void))

(define (apply-harness-flags!)
  ;; Apply top-level lifestyle flags from the harness JSON config
  ;; (quiet/plain/debug/search). Provider sections are consumed lazily by the
  ;; dispatch functions; here we just set the display/session flags.
  (define cfg (harness-config))
  (when (hash-ref cfg 'quiet #f)
    (cli-quiet? #t) (quiet-mode? #t))
  (when (hash-ref cfg 'plain #f)
    (cli-plain? #t) (color-enabled? #f))
  (when (hash-ref cfg 'debug #f)
    (DEBUG-LOG #t))
  (define s (hash-ref cfg 'search #f))
  (when (hash? s)
    (define se (hash-ref s 'engine #f))
    (when (and (string? se) (member (string-downcase se) '("brave" "exa")))
      (set-box! search-engine-box (string-downcase se)))
    (when (hash-has-key? s 'enabled)
      (set-box! search-enabled-box (if (hash-ref s 'enabled #f) #t #f))))
  (void))

(define (apply-env-overrides!)
  ;; Namespaced env aliases (section 9) — override config, still below CLI
  (define env-provider (or (getenv "CODING_AGENT_PROVIDER") (getenv "AGENT_PROVIDER")))
  (when (and env-provider (not (string=? (string-trim env-provider) "")))
    (define low (string-downcase (string-trim env-provider)))
    (cond
      [(config-provider low) (switch-provider! low)]
      [else
       (eprintf "warning: ignoring unknown provider '~a' from the environment~a\n"
                env-provider
                (if (pair? (config-provider-names))
                    (format " (available: ~a)" (string-join (config-provider-names) ", "))
                    " (no providers configured)"))]))
  (define env-model (or (getenv "CODING_AGENT_MODEL") #f))
  (when (and env-model (not (string=? env-model "")))
    (set-current-model! env-model))
  (when (getenv "CODING_AGENT_QUIET") (cli-quiet? #t) (quiet-mode? #t))
  (when (getenv "CODING_AGENT_PLAIN") (cli-plain? #t) (color-enabled? #f))
  (when (getenv "CODING_AGENT_DEBUG") (DEBUG-LOG #t))
  (void))

;; ---------------------------------------------------------------------------
;; CLI helpers (sections 2-4)

(define (read-all-stdin)
  (with-handlers ([exn:fail? (lambda (_) "")])
    (port->string (current-input-port) #:close? #f)))

(define (build-prompt positional prompt-parts stdin-text)
  ;; Join all sources with newlines; trim; return "" if nothing
  (define parts
    (filter (lambda (s) (and (string? s) (not (string=? (string-trim s) ""))))
            (append
             (if (and stdin-text (not (string=? (string-trim stdin-text) "")))
                 (list (string-trim stdin-text))
                 '())
             prompt-parts
             positional)))
  (string-trim (string-join parts "\n\n")))

(define (resolve-cwd dir-str)
  (when (and dir-str (not (string=? dir-str "")))
    (define p (string->path dir-str))
    (unless (directory-exists? p)
      (eprintf "error: --cwd directory does not exist: ~a\n" dir-str)
      (exit-with-code EXIT-BAD-ARGS))
    (current-directory p)))

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
       (let loop ([rest-lines (rest lines)])
         (cond
           [(null? rest-lines) #f]
           [(string=? (string-trim (first rest-lines)) "---") #f]
           [(string-prefix? (first rest-lines) "description:")
            (string-trim (substring (first rest-lines) 12))]
           [else (loop (rest rest-lines))]))]
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
                (format "The user has loaded the following skill: '~a'. " name)
                "Use it as authoritative reference and guidance for subsequent responses "
                "in this conversation.\n\n"
                content)))
       (set-box! messages-box (append (unbox messages-box) (list system-msg)))
       (define desc (skill-description name))
       (displayln (format "Loaded skill: ~a  (~a chars)" name (string-length content)))
       (when desc
         (displayln (format "  ~a" desc)))
       (displayln "\nAsking the model to load the skill into context...")
       (define reply
         (with-handlers
           ([exn:fail? (lambda (e)
                         (format "(model call failed: ~a)" (exn-message e)))])
           (llm-chat (unbox messages-box))))
       (displayln (format "\n~a" (clean reply))))]))

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
  (unless (cli-quiet?)
    (displayln "")
    (displayln "Coding Agent REPL.  /help for commands, /quit to exit.")
    (displayln (format "  cwd:      ~a" (current-directory)))
    (displayln (format "  provider: ~a" (current-provider-name-or-legacy)))
    (displayln (format "  model:    ~a" (current-model-id)))
    (displayln "")))

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
      (displayln (format "Current provider: ~a (model: ~a)"
                         (current-provider-name-or-legacy) (current-model-id)))
      (when (config-loaded?)
        (displayln (format "Available profiles: ~a"
                           (string-join (config-provider-names) ", "))))
      'continue]
     [(string-prefix? line "/provider ")
      (define p (string-downcase (string-trim (substring line 10))))
      (cond
        [(config-provider p)
         (switch-provider! p)
         (displayln (format "Provider set to profile '~a' (model: ~a)"
                            p (current-model-id)))]
        [else
         (displayln (format "Unknown provider '~a'~a"
                            p
                            (if (pair? (config-provider-names))
                                (format " -- use one of: ~a"
                                        (string-join (config-provider-names) ", "))
                                " -- no providers configured")))])
      'continue]
    [(string=? line "/debug")
     (DEBUG-LOG (not (DEBUG-LOG)))
     (displayln (format "Debug logging ~a" (if (DEBUG-LOG) "ON" "OFF")))
     'continue]
    [(string=? line "/tokens")
     (if (using-mlx?)
         (mlx-print-session-stats)
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
  /provider         show current LLM provider and available profiles
  /provider <name>  switch provider profile (or fireworks/mlx w/o config)
  /debug            toggle raw request/response logging
  /search           toggle web search on/off
  /search brave     enable Brave search
  /search exa       enable Exa search
  /tokens           show session token usage and estimated cost
  /skills           list available skills in ~/.agents/skills
  /<skill-name>     load that skill into the conversation
  /quit             exit
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
  (unless (cli-quiet?)
    (displayln (format "[intent: ~a → ~a]" intent label)))
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

;; One-shot helpers: infer exit code from tool results (section 5)
(define (tool-messages-contain? substr)
  (for/or ([m (in-list (unbox messages-box))])
    (and (equal? (hash-ref m 'role "") "tool")
         (string-contains? (string-downcase (format "~a" (hash-ref m 'content "")))
                           (string-downcase substr)))))

(define (infer-exit-code)
  (cond
    [(tool-messages-contain? "make check failed") EXIT-CHECK-FAILED]
    [(tool-messages-contain? "user rejected") EXIT-REJECTED]
    [(tool-messages-contain? "user skipped") EXIT-REJECTED]
    [else EXIT-OK]))

(define (run-one-shot prompt)
  ;; Prompt is already non-empty string. Run single task, print reply, exit.
  (register-all)
  (reset-conversation)
  ;; One-shot still respects quiet/plain but banner is suppressed anyway
  (define exit-code
    (with-handlers ([exn:fail? (lambda (e)
                                  (eprintf "Error talking to model: ~a\n" (exn-message e))
                                  EXIT-MODEL-ERROR)])
      (send-to-model prompt)
      (infer-exit-code)))
  (cond
    [(= exit-code EXIT-CHECK-FAILED)
     (unless (cli-quiet?) (displayln "\n[make check failed]"))
     (exit-with-code EXIT-CHECK-FAILED)]
    [(= exit-code EXIT-REJECTED)
     (unless (cli-quiet?) (displayln "\n[change rejected or skipped]"))
     (exit-with-code EXIT-REJECTED)]
    [(= exit-code EXIT-MODEL-ERROR)
     (exit-with-code EXIT-MODEL-ERROR)]
    [else (exit-with-code EXIT-OK)]))

;; ---------------------------------------------------------------------------
;; Interactive line editing (readline/Editline when available)

(define SLASH-COMMANDS
  '("/reset" "/history" "/context" "/compact" "/model" "/provider"
    "/debug" "/search" "/tokens" "/help" "/skills" "/quit"))

(define SEARCH-ENGINES '("brave" "exa"))

(define (configured-model-ids)
  (if (config-loaded?)
      (filter-map (lambda (name)
                    (define p (config-provider name))
                    (and p (provider-model p)))
                  (config-provider-names))
      '()))

(define (completion-candidates word)
  ;; Readline completes the word under the cursor rather than the whole line, so
  ;; the command set and the argument sets are merged and filtered by prefix.
  ;; Tab in the middle of prose only reacts to words that actually begin a known
  ;; name (command, provider profile, engine, or model id).
  (define (matching xs) (filter (lambda (x) (string-prefix? x word)) xs))
  (if (string-prefix? word "/")
      (matching SLASH-COMMANDS)
      (append (matching (config-provider-names))
              (matching SEARCH-ENGINES)
              (matching (configured-model-ids)))))

(define (setup-line-input!)
  ;; Enable readline editing, persisted history, and Tab completion when stdin
  ;; is a terminal and the readline backend is present.  Returns #t when active.
  (define available? (line-input-available?))
  (when available?
    (set-history-file! (default-history-file))
    (install-completion! completion-candidates)
    (plumber-add-flush! (current-plumber)
                        (lambda (_) (save-history!))))
  available?)

;; ---------------------------------------------------------------------------
;; Main REPL + CLI dispatch (sections 2-4)

(define (run-repl)
  (unless (config-loaded?) (load-harness-config))
  (register-all)
  (reset-conversation)
  (setup-line-input!)
  (print-banner)
  (let loop ()
    (define line
      (with-handlers ([exn:fail? (lambda (_) eof)])
        (read-input-line "\n> ")))
    (cond
      [(eof-object? line)
       (save-history!)
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
             (with-handlers ([exn:fail? (lambda (e)
                                          (displayln (format "\nError talking to model: ~a" (exn-message e)))
                                          (flush-output))])
               (send-to-model trimmed))
             (loop)])])])))

;; Back-compat alias
(define (run) (run-repl))

;; CLI entry (sections 2-4, 9)
(define (cli-main)
  ;; 1a) harness JSON config: ~/.coding_harness.json + .local_coding_harness.json
  (load-harness-config)
  (apply-harness-flags!)
  ;; 1b) legacy .rktd config + env (harness JSON takes precedence for provider)
  (define cfg (load-config))
  (apply-config-hash! cfg)
  (apply-env-overrides!)
  ;; 2) parse CLI; if a prompt is present, run one-shot, else REPL
  (define prompt-parts '())
  (define positional-parts '())
  (define stdin? #f)
  (define yes? #f)
  (define dry-run?flag #f)
  (define quiet?flag #f)
  (define plain?flag #f)
  (define cwd-str #f)
  (define provider-str #f)
  (define model-str #f)
  (define debug?flag #f)
  (define version?flag #f)
  ;; Use racket/cmdline — it handles -h/--help automatically and exits 0
  (command-line
   #:program "coding-agent"
   #:once-each
   [("--stdin") "Read prompt from stdin (pipe/heredoc)" (set! stdin? #t)]
   [("-y" "--yes") "Auto-approve all propose_edit diffs (still shows diff)" (set! yes? #t)]
   [("--dry-run") "Show diffs but do not write files" (set! dry-run?flag #t)]
   [("--provider") prov "LLM provider profile (from harness config) or fireworks/mlx/omlx/sushi" (set! provider-str prov)]
   [("--model") mid "Model id for current provider" (set! model-str mid)]
   [("--cwd") dir "Working directory before running" (set! cwd-str dir)]
   [("--debug") "Enable debug logging (same as /debug)" (set! debug?flag #t)]
   [("-q" "--quiet") "Quiet: no banner, no [intent] line, less tool chatter" (set! quiet?flag #t)]
   [("--plain" "--no-color") "Plain output: no ANSI colors in diffs" (set! plain?flag #t)]
   [("-v" "--version") "Show version and exit" (set! version?flag #t)]
   #:multi
   [("-p" "--prompt") p "Prompt text (repeatable, joined with newlines)" (set! prompt-parts (append prompt-parts (list p)))]
   #:args args
   (set! positional-parts args))
  ;; --version early exit
  (when version?flag
    (displayln (format "coding-agent ~a" VERSION))
    (exit-with-code EXIT-OK))
  ;; Provider data lives only in the harness config, so refuse to run with none
  ;; rather than falling back to compiled-in defaults.
  (unless (pair? (config-provider-names))
    (eprintf "~a"
             (string-append
              "error: no providers configured.\n"
              "  Add a \"providers\" section to ~/.coding_harness.json or\n"
              "  .local_coding_harness.json (see README.md for the format).\n"))
    (exit-with-code EXIT-BAD-ARGS))
  ;; Apply CLI overrides (highest precedence)
  (when debug?flag (DEBUG-LOG #t))
  (when quiet?flag (cli-quiet? #t) (quiet-mode? #t))
  (when plain?flag (cli-plain? #t) (color-enabled? #f))
  (when yes? (auto-approve? #t))
  (when dry-run?flag (dry-run? #t))
  (when provider-str
    (define low (string-downcase (string-trim provider-str)))
    (cond
      [(config-provider low) (switch-provider! low)]
      [else
       (eprintf "error: unknown provider '~a'~a\n" provider-str
                (if (pair? (config-provider-names))
                    (format " — available profiles: ~a"
                            (string-join (config-provider-names) ", "))
                    " — no providers configured"))
       (exit-with-code EXIT-BAD-ARGS)]))
  (when model-str
    (when (string=? (string-trim model-str) "")
      (eprintf "error: --model requires a non-empty value\n")
      (exit-with-code EXIT-BAD-ARGS))
    (set-current-model! (string-trim model-str)))
  (when cwd-str (resolve-cwd cwd-str))
  ;; Build prompt from all sources
  (define stdin-text (if stdin? (read-all-stdin) #f))
  (define prompt (build-prompt positional-parts prompt-parts stdin-text))
  (if (not (string=? prompt ""))
      (run-one-shot prompt)
      (run-repl)))

(module+ main
  (cli-main))
