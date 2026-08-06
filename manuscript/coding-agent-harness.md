# A Racket Coding Agent

The source code for this example is in [https://github.com/mark-watson/racket-coding-agent](https://github.com/mark-watson/racket-coding-agent).

## The Agentic Loop

Modern large language models are not limited to answering questions in a single turn. When given access to *tools* (i.e., callable functions that can read files, run commands, or search the web) an LLM can operate as an autonomous agent: it reasons about what it needs to know, calls a tool to gather information, receives the result, and continues reasoning until the task is complete. This pattern is called an **agentic loop**.

For a coding assistant, the loop typically looks like this:

1. The user describes a change or a bug to fix.
2. The LLM decides it needs to read a file and calls `read_file`.
3. After seeing the file contents, the LLM proposes an edit via `propose_edit`.
4. The user reviews the colored diff and approves or rejects it.
5. On approval, the agent writes the file and runs `make check`.
6. The LLM reads the check result and either continues or summarizes what changed.

The key architectural insight is that the LLM is stateless between API calls. It only knows what is in the message history. The agent accumulates tool results into that history turn by turn, giving the model the context it needs to decide what to do next.

This chapter builds a complete Racket implementation of such a coding agent, backed by the Fireworks AI API. The agent classifies each user request as a coding task, a general question, or a hybrid, and routes it accordingly. It supports live web search via Brave or Exa AI, renders colored unified diffs before any file is written, and lets the user interrupt a running task at any time with a single ESC keypress.

## Module Architecture

The project is organized into five source files, each with a single clear responsibility:

```
fireworks-ai.rkt   Fireworks API client, session stats, interrupt flag, chat helpers
tools.rkt          Tool registry, five coding tools, the propose_edit approval gate
approval.rkt       Colored diff printer, ESC-aware y/n/s prompt
search.rkt         Brave Search and Exa AI search backends
agent.rkt          REPL, intent classifier, ESC-interrupt thread, top-level run
```

The static dependency graph is nearly linear, but `chat-with-tools` in `fireworks-ai.rkt` needs to dispatch tool calls defined in `tools.rkt`, and both `tools.rkt` and `approval.rkt` need the interrupt flag from `fireworks-ai.rkt`. A direct `require` in either direction would create a circular dependency. Racket's `dynamic-require` resolves this by deferring the require to runtime, when all modules are already loaded and initialized.

## The Fireworks AI Client

### The API and Pricing

Fireworks AI is a hosted inference platform that serves many open-weight models through an OpenAI-compatible API. This agent defaults to DeepSeek v4 Flash, a fast and cost-effective model well-suited for coding tasks. The pricing at the time of writing is $0.22 per million prompt tokens and $0.88 per million completion tokens.

The estimated session cost accumulated over a conversation is:

```$
\text{cost} = p \times \frac{0.22}{10^6} + c \times \frac{0.88}{10^6}
```

where `p`$ is the total number of prompt tokens sent and `c`$ is the total number of completion tokens received. The agent tracks both and displays the running total on demand.

### Parameters and the Interrupt Flag

`fireworks-ai.rkt` starts by declaring a few Racket parameters and a thread-safe interrupt flag. Here is the complete file:

```racket
#lang racket

(require net/http-easy
         json
         racket/string
         racket/port)

(provide FIREWORKS-ENDPOINT
         FIREWORKS-MODEL
         MAX-TOKENS
         DEBUG-LOG
         CURL-MAX-TIME
         PRICE-PER-M-PROMPT
         PRICE-PER-M-COMPLETION
         task-interrupted?
         task-interrupted-set!
         task-interrupted-clear!
         reset-session-stats
         session-cost
         print-session-stats
         chat
         chat-with-tools)

(define FIREWORKS-ENDPOINT "https://api.fireworks.ai/inference/v1/chat/completions")
(define FIREWORKS-MODEL (make-parameter "accounts/fireworks/models/deepseek-v4-flash-0731"))
(define MAX-TOKENS 32768)
(define DEBUG-LOG (make-parameter #f))
(define CURL-MAX-TIME 300)

(define PRICE-PER-M-PROMPT 0.22)
(define PRICE-PER-M-COMPLETION 0.88)

(define task-interrupted-box (box #f))
(define task-interrupted-sema (make-semaphore 1))

(define (task-interrupted?)
  (call-with-semaphore task-interrupted-sema
    (lambda () (unbox task-interrupted-box))))

(define (task-interrupted-set!)
  (call-with-semaphore task-interrupted-sema
    (lambda () (set-box! task-interrupted-box #t))))

(define (task-interrupted-clear!)
  (call-with-semaphore task-interrupted-sema
    (lambda () (set-box! task-interrupted-box #f))))
```

`make-parameter` creates a dynamic variable. Its value can be read by calling it with no arguments -- `(FIREWORKS-MODEL)` -- and changed within a dynamic scope using `parameterize`. The `/model` slash command calls `(FIREWORKS-MODEL new-model)` which updates the parameter's value for the rest of the session.

The interrupt flag uses a plain `box` (a mutable cell) protected by a semaphore because the main thread writes to it while the worker thread reads from it. `call-with-semaphore` acquires the semaphore, runs the body, and releases the semaphore even if an exception is raised.

### Session Statistics

Every API response includes a `usage` field with token counts. The module accumulates these across all calls in the session:

```racket
(define stats-sema (make-semaphore 1))
(define session-prompt-tokens (box 0))
(define session-completion-tokens (box 0))
(define session-total-tokens (box 0))
(define session-cached-tokens (box 0))

(define (accumulate-usage data)
  (define usage (hash-ref data 'usage (hash)))
  (when (and (hash? usage) (not (hash-empty? usage)))
    (call-with-semaphore stats-sema
      (lambda ()
        (set-box! session-prompt-tokens
                  (+ (unbox session-prompt-tokens)
				     (hash-ref usage 'prompt_tokens 0)))
        (set-box! session-completion-tokens
                  (+ (unbox session-completion-tokens)
				     (hash-ref usage 'completion_tokens 0)))
        (set-box! session-total-tokens
                  (+ (unbox session-total-tokens)
				     (hash-ref usage 'total_tokens 0)))
        (define details (hash-ref usage 'prompt_tokens_details (hash)))
        (when (hash? details)
          (set-box! session-cached-tokens
                    (+ (unbox session-cached-tokens)
                       (hash-ref details 'cached_tokens 0))))))))
```

`print-session-stats` formats these for the `/tokens` command, showing a breakdown by token type and the estimated cost.

### The `chat` Function

The plain `chat` function sends a list of messages to the API and returns the text reply as a string:

```racket
(define (chat messages
              #:model-id [model-id (FIREWORKS-MODEL)]
              #:max-tokens [max-tokens MAX-TOKENS]
              #:temperature [temperature 0.6])
  (define payload
    (hash 'model model-id
          'max_tokens max-tokens
          'temperature temperature
          'messages messages))
  (define data (post-fireworks payload))
  (define content
    (hash-ref (hash-ref (first (hash-ref data 'choices)) 'message) 'content ""))
  (if (and (string? content) (not (string=? content "")))
      content
      "No response content"))
```

This path is used for the intent classifier (where a single-word response is all that is needed) and for general questions that do not require file access.

### The `chat-with-tools` Agentic Loop

`chat-with-tools` is the heart of the agent. It implements the multi-turn tool loop as a named recursive function:

```racket
(define (chat-with-tools messages tools
                         #:model-id [model-id (FIREWORKS-MODEL)]
                         #:max-tokens [max-tokens MAX-TOKENS]
                         #:temperature [temperature 0.6]
                         #:max-iterations [max-iterations 20])
  (define render-tools
    (dynamic-require "tools.rkt" 'render-tools))
  (define execute-tool-calls
    (dynamic-require "tools.rkt" 'execute-tool-calls))
  (define tools-rendered (render-tools tools))
  (define current-messages (box (map (lambda (m) m) messages)))

  (define (loop iter)
    (cond
      [(task-interrupted?)
       (values "(task interrupted by user)" (without-dangling
	   (unbox current-messages)))]
      [(>= iter max-iterations)
       ;; One final no-tools call to generate a summary
       (define payload
         (hash 'model model-id
               'max_tokens max-tokens
               'temperature temperature
               'messages (unbox current-messages)))
       (with-handlers ([exn:fail? (lambda (_) (values "(max tool iterations reached)"
                                                       (unbox current-messages)))])
         (define data (post-fireworks payload))
         (define msg (hash-ref (first (hash-ref data 'choices)) 'message))
         (define content (hash-ref msg 'content ""))
         (values (if (and (string? content) (not (string=? content "")))
                     content "(no summary from model)")
                 (append (unbox current-messages) (list msg))))]
      [else
       (define payload
         (let ([base (hash 'model model-id
                           'max_tokens max-tokens
                           'temperature temperature
                           'messages (unbox current-messages))])
           (if (null? tools-rendered)
               base
               (hash-set* base 'tools tools-rendered 'tool_choice "auto"))))
       (define data (post-fireworks payload))
       (define msg (hash-ref (first (hash-ref data 'choices)) 'message))
       (define tool-calls (hash-ref msg 'tool_calls #f))
       (define content (hash-ref msg 'content ""))
       (set-box! current-messages (append (unbox current-messages) (list msg)))
       (cond
         [(task-interrupted?)
          (values "(task interrupted by user)"
		  (without-dangling (unbox current-messages)))]
         [(not tool-calls)
          (values (or content "(empty response from model)") (unbox current-messages))]
         [else
          (define results (execute-tool-calls tool-calls))
          (for ([r (in-list results)])
            (define call-id (first r))
            (define name (second r))
            (define result-str (third r))
            (set-box! current-messages
                      (append (unbox current-messages)
                              (list (hash 'role "tool"
                                          'tool_call_id call-id
                                          'name name
                                          'content result-str)))))
          (loop (add1 iter))])]))
  (loop 0))
```

At each iteration the model either produces text (the loop ends) or requests tool calls. Tool results are appended as messages with `role "tool"` and the loop recurses. If the interrupt flag is set at any iteration boundary, the loop returns immediately with a graceful status message.

The `without-dangling` helper strips a trailing assistant message that has `tool_calls` but no corresponding `tool` results -- a history in that state would be rejected by the API.

## The Tool Registry

### Defining and Rendering Tools

`tools.rkt` maintains a central hash table of all registered tools. Here is the complete file:

```racket
#lang racket

(require racket/file
         racket/port
         racket/string
         racket/system
         racket/list
         json)

(provide define-tool
         render-tools
         execute-tool-calls
         register-all
         ENABLED-TOOLS)

(define registry (make-hash))

(define SHELL-WHITELIST (set "make" "ls" "pwd" "cat"))
(define MAX-CHECK-OUTPUT-CHARS 2000)

(define (define-tool name params description handler)
  (hash-set! registry name
             (hash 'name name
                   'description description
                   'parameters params
                   'handler handler)))

(define (render-tools names)
  (for/list ([name (in-list names)])
    (define tool (hash-ref registry name #f))
    (unless tool (error 'render-tools "Undefined tool: ~a" name))
    (define props (make-hash))
    (define required '())
    (for ([p (in-list (hash-ref tool 'parameters))])
      (define pname (first p))
      (define ptype (second p))
      (define pdesc (third p))
      (hash-set! props (string->symbol pname)
                 (hash 'type ptype 'description pdesc))
      (set! required (cons pname required)))
    (hash 'type "function"
          'function (hash 'name (hash-ref tool 'name)
                          'description (hash-ref tool 'description)
                          'parameters (hash 'type "object"
                                            'properties props
                                            'required (reverse required))))))
```

Each tool is stored as a hash with its name, description, parameter list, and handler function. `render-tools` converts the registry entries into OpenAI function-calling schema format: a list of hashes the API understands as callable functions. The model receives these alongside the conversation and decides which, if any, to invoke.

### Tool Dispatch

`execute-tool-calls` receives the list of tool call objects from the API response and dispatches each one:

```racket
(define (execute-tool-calls tool-calls)
  (define results '())
  (for ([call (in-list tool-calls)])
    #:break (task-interrupted?)
    (define call-id (hash-ref call 'id ""))
    (define func (hash-ref call 'function (hash)))
    (define name (hash-ref func 'name ""))
    (define args-json (hash-ref func 'arguments "{}"))
    (define short
      (if (<= (string-length args-json) 120)
          args-json
          (string-append (substring args-json 0 117) "...")))
    (displayln (format "* ~a ~a" name short))
    (define args
      (with-handlers ([exn:fail? (lambda (_) (hash))])
        (let ([j (string->jsexpr args-json)])
          (if (hash? j) j (hash)))))
    (define result (call-tool name args))
    (set! results (append results (list (list call-id name result)))))
  results)
```

The `#:break (task-interrupted?)` clause in the `for` loop is idiomatic Racket: it terminates the loop early if the interrupt flag is set between tool calls. Each tool call prints its name and (truncated) arguments so the user can see what the model is doing in real time.

### The Five Coding Tools

The agent registers five tools at startup:

| Tool | Purpose |
|---|---|
| `read_file` | Return the full text of a file |
| `list_dir` | List files and subdirectories in a directory |
| `grep` | Recursively search for an extended regex pattern |
| `run_shell` | Run a whitelisted shell command and return its output |
| `propose_edit` | Show a colored diff and ask the user to approve the change |

`run_shell` enforces a strict command whitelist to prevent the model from running arbitrary shell commands:

```racket
(define (tool-run-shell command)
  (define tokens (string-split (string-trim command)))
  (cond
    [(null? tokens) "empty command"]
    [else
     (define cmd (first tokens))
     (if (not (set-member? SHELL-WHITELIST cmd))
         (format "Command '~a' not whitelisted. Allowed: ~a"
                 cmd (string-join (sort (set->list SHELL-WHITELIST) string<?) ", "))
         (with-handlers ([exn:fail?
		                 (lambda (e) (format "Error: ~a" (exn-message e)))])
           (define args (rest tokens))
           (define-values (out code) (run-external cmd args))
           (string-append out (format "(exit ~a)" code))))]))
```

If the model attempts to run a disallowed command, it receives an error string describing what is allowed. It can then adapt its approach rather than causing the agent to crash.

### The `propose_edit` Approval Gate

`propose_edit` is the most critical tool. Before writing any file it checks for several error conditions, shows the user a diff, waits for approval, writes the file, and then runs `make check`:

```racket
(define (tool-propose-edit path old new)
  ...
  (define exists? (file-exists? path))
  (define current
    (if exists? (file->string path) ""))
  (cond
    [(and exists? (not (string=? current old)))
     "stale base: on-disk contents do not match 'old'. Read the file again and retry."]
    [(and exists? (string=? current new))
     "no changes (proposed content matches current file)"]
    [(and (not exists?) (string=? new ""))
     "refused: cannot create an empty file"]
    [else
     (define diff-text (unified-diff current new ...))
     (print-colored-diff diff-text)
     (define answer (prompt-yes-no-skip))
     (cond
       [(eq? answer 'yes)
        (call-with-output-file path #:exists 'truncate
          (lambda (out) (display new out)))
        (define-values (out status) (run-make-check))
        (if (= status 0)
            "applied; make check passed"
            (format "applied; make check FAILED (exit ~a):\n~a" status ...))]
       [(eq? answer 'no)  "user rejected the change"]
       [(eq? answer 'skip)
        (format "user skipped: ~a" (prompt-reason))]
       [(eq? answer 'interrupted) "change not applied (task interrupted by user)"])]))
```

The stale-base guard is worth understanding carefully. The model reads a file, then constructs a proposed edit based on that content. If the user edits the file externally in between, a naive tool would overwrite those changes silently. By requiring `old` to exactly match what is on disk, the tool forces the model to re-read the file before retrying -- the mismatch is reported as a tool result the model can read and respond to.

The `make check` gate closes another important feedback loop. If the edit compiles cleanly, `"applied; make check passed"` goes back into the conversation history and the model can proceed. If `make check` fails, the output goes back as well, giving the model the compiler errors it needs to self-correct on the next turn.

## The Approval and Diff System

### Generating a Unified Diff

`approval.rkt` generates diffs by writing the two file versions to temporary files and calling the system `diff -u` utility. Here is the full source file:

```racket
#lang racket

(require racket/file
         racket/port
         racket/string
         racket/system)

(provide unified-diff
         print-colored-diff
         prompt-yes-no-skip
         prompt-reason)

(define ANSI-RED   "\033[31m")
(define ANSI-GREEN "\033[32m")
(define ANSI-CYAN  "\033[36m")
(define ANSI-RESET "\033[0m")

(define (shell-quote s)
  (string-append "'"
                 (string-replace s "'" "'\\''")
                 "'"))

(define (unified-diff old-content new-content old-label new-label)
  (define old-path (make-temporary-file "rk-diff-old~a"))
  (define new-path (make-temporary-file "rk-diff-new~a"))
  (define out-path (make-temporary-file "rk-diff-out~a"))
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file old-path #:exists 'truncate
        (lambda (out) (display old-content out)))
      (call-with-output-file new-path #:exists 'truncate
        (lambda (out) (display new-content out)))
      (define cmd
        (format "diff -u -L ~a -L ~a ~a ~a > ~a 2>&1"
                (shell-quote old-label)
                (shell-quote new-label)
                (shell-quote (path->string old-path))
                (shell-quote (path->string new-path))
                (shell-quote (path->string out-path))))
      (system cmd)
      (with-handlers ([exn:fail? (lambda (_) "")])
        (file->string out-path)))
    (lambda ()
      (when (file-exists? old-path) (delete-file old-path))
      (when (file-exists? new-path) (delete-file new-path))
      (when (file-exists? out-path) (delete-file out-path)))))
```

`dynamic-wind` takes three thunks: a before-thunk (here `void`), a body-thunk, and an after-thunk. The after-thunk runs whether the body completes normally or raises an exception -- analogous to Python's `try/finally`. This guarantees the three temporary files are cleaned up regardless of what goes wrong.

### Colorizing the Diff

`print-colored-diff` walks each line of the unified diff output and applies ANSI terminal color codes:

```racket
(define (print-colored-diff diff-text)
  (for ([line (in-list (string-split diff-text "\n"))])
    (cond
      [(or (string-prefix? line "+++")
           (string-prefix? line "---")
           (string-prefix? line "@@"))
       (displayln (string-append ANSI-CYAN line ANSI-RESET))]
      [(string-prefix? line "+")
       (displayln (string-append ANSI-GREEN line ANSI-RESET))]
      [(string-prefix? line "-")
       (displayln (string-append ANSI-RED line ANSI-RESET))]
      [else (displayln line)])))
```

Lines beginning with `+` are added lines and appear green; lines beginning with `-` are removed and appear red; diff headers (`+++`, `---`, `@@`) appear cyan. This makes it straightforward to review a proposed change without reading both full file versions.

### ESC-Aware Prompts

The approval prompt puts the terminal in raw mode so that a bare ESC keypress can be detected immediately, without waiting for the user to press Enter. The trickiest part is distinguishing a bare ESC from the beginning of an ANSI escape sequence (which is how arrow keys and function keys are encoded):

```racket
(define (read-line-raw)
  (define esc? #f)
  (define chars '())
  (define done? #f)
  (let loop ()
    (unless done?
      (define ready? (char-ready? (current-input-port)))
      (if ready?
          (let ([ch (read-char (current-input-port))])
            (cond
              [(eof-object? ch) (set! done? #t)]
              [(char=? ch #)
               (sleep 0.05)
               (if (char-ready? (current-input-port))
                   (let drain ()
                     (when (char-ready? (current-input-port))
                       (read-char (current-input-port))
                       (sleep 0.02)
                       (drain)))
                   (begin (set! esc? #t) (set! done? #t)))
               (unless done? (loop))]
              [(or (char=? ch #\newline) (char=? ch #\return))
               (set! done? #t)]
              [else
               (set! chars (cons ch chars))
               (loop)]))
          (begin (sleep 0.02) (loop)))))
  (values (list->string (reverse chars)) esc?))
```

After reading an ESC byte (`#`) the code waits 50 ms. If the input port has more bytes ready within that window, they belong to an ANSI sequence and are drained. If no bytes arrive, it was a standalone ESC and `esc?` is set to `#t`. The function returns two values: the accumulated character string and the ESC flag.

`prompt-yes-no-skip` wraps `read-line-raw` in `dynamic-wind` to ensure `stty sane` always restores the terminal, then interprets the result:

```racket
(define (prompt-yes-no-skip)
  (define raw? (try-raw-mode))
  (dynamic-wind
    void
    (lambda ()
      (let loop ()
        (display "\nApply this change? [y]es / [n]o / [s]kip and tell the model why: ")
        (flush-output)
        (define-values (line esc?)
          (if raw?
              (read-line-raw)
              (let ([l (read-line (current-input-port))])
                (values (if (eof-object? l) "" (string-trim l)) #f))))
        (when raw? (displayln ""))
        (cond
          [esc?
           (task-interrupted-set!)
           (displayln "[interrupted]")
           'interrupted]
          [else
           (define norm (string-downcase (string-trim line)))
           (cond
             [(member norm '("y" "yes")) 'yes]
             [(member norm '("n" "no")) 'no]
             [(member norm '("s" "skip")) 'skip]
             [else
              (displayln "Please answer y, n, or s.")
              (loop)])])))
    (lambda () (when raw? (restore-mode)))))
```

When `stty` is not available (for example, when running in a non-TTY context), the code falls back to a standard `read-line` call and ESC detection is disabled.

## Web Search Integration

`search.rkt` provides two search backends with identical return shapes, making them interchangeable at the call site. Here is the complete file:

```racket
#lang racket

(require net/http-easy
         net/uri-codec
         json
         racket/string)

(provide brave-search
         exa-search)

(define EXA-ENDPOINT "https://api.exa.ai/search")

(define (brave-search query [num-results 5])
  (define api-key (getenv "BRAVE_SEARCH_API_KEY"))
  (unless (and api-key (not (string=? api-key "")))
    (error 'brave-search "BRAVE_SEARCH_API_KEY environment variable not set"))
  (define encoded (uri-encode query))
  (define url (format "https://api.search.brave.com/res/v1/web/search?q=~a&count=~a"
                      encoded num-results))
  (define headers
    (hash 'X-Subscription-Token api-key
          'content-type "application/json"
          'accept "application/json"))
  (define resp (get url #:headers headers))
  (define data (response-json resp))
  (define web (hash-ref data 'web (hash)))
  (define results (hash-ref web 'results '()))
  (for/list ([r (in-list results)])
    (list (hash-ref r 'url "")
          (hash-ref r 'title "")
          (hash-ref r 'description ""))))

(define (exa-search query [num-results 5])
  (define api-key (getenv "EXA_SEARCH_API_KEY"))
  (unless (and api-key (not (string=? api-key "")))
    (error 'exa-search "EXA_SEARCH_API_KEY environment variable not set"))
  (define payload
    (hash 'query query
          'type "auto"
          'numResults num-results
          'contents (hash 'highlights #t)))
  (define headers
    (hash 'content-type "application/json"
          'authorization (string-append "Bearer " api-key)))
  (define resp
    (post EXA-ENDPOINT
          #:headers headers
          #:json payload))
  (define data (response-json resp))
  (define results (hash-ref data 'results '()))
  (for/list ([r (in-list results)])
    (list (hash-ref r 'url "")
          (hash-ref r 'title "")
          (let ([hl (hash-ref r 'highlights '())])
            (if (and (list? hl) (not (null? hl))) (first hl) "")))))
```

Both functions return a list of `(url title description)` triples. Brave uses a GET request with an API key header and returns web search results with title and description snippets. Exa uses a POST with a JSON body and returns neural search results with highlighted excerpts.

The `net/http-easy` package, installable via `raco pkg install http-easy`, provides the `get`, `post`, and `response-json` procedures used here.

## The Main REPL

### Intent Classification

Before sending any message to the model, `agent.rkt` classifies the user's intent as one of three categories: `"general"`, `"coding"`, or `"hybrid"`. The classification uses a two-stage approach.

Stage one is a keyword heuristic -- free and instant:

```racket
(define GENERAL-KEYWORDS
  (list "movie" "film" "cinema" "theater" "weather" "forecast"
        "restaurant" "recipe" "news" "sports" "score"
        "near me" "nearby" "population of" "history of"
        "who is " "who was " "where is " "when is "
        "price of" "cost of" "how much does"))

(define CODING-KEYWORDS
  (list ".lisp" ".py" ".js" ".ts" ".java" ".cpp" ".go" ".rb" ".rs"
        "def " "class " "function " "refactor" "implement " "compile"
        "stacktrace" "segfault" "git commit" "unit test"
        "fix the bug" "add a function" "write a function"))

(define (heuristic-classify lower)
  (cond
    [(for/or ([kw (in-list GENERAL-KEYWORDS)])
       (string-contains? lower kw))
     "general"]
    [(for/or ([kw (in-list CODING-KEYWORDS)])
       (string-contains? lower kw))
     "coding"]
    [else #f]))
```

`for/or` is the Racket comprehension form that returns the first “truthy” value or `#f` if none is found.

If the heuristic returns `#f` (the query is ambiguous), stage two calls the LLM with a minimal two-message conversation and requests a single-word answer:

```racket
(define (llm-classify user-line)
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (format "[Classifier error: ~a]" (exn-message e)))
                               "coding")])
    (define msgs
      (list (hash 'role "system"
                  'content "You are a one-word query classifier. Reply with exactly one word.")
            (hash 'role "user"
                  'content
                  (string-append
                   "Classify as GENERAL, CODING, or HYBRID:\n"
                   "GENERAL = factual, nothing to do with code.\n"
                   "CODING  = writing, editing, or debugging code.\n"
                   "HYBRID  = coding question that benefits from web docs.\n"
                   (format "Query: ~a\nOne-word answer:" user-line)))))
    (define raw (chat msgs #:max-tokens 10 #:temperature 0.0))
    (define up (string-upcase (string-trim raw)))
    (cond
      [(string-contains? up "GENERAL") "general"]
      [(string-contains? up "HYBRID")  "hybrid"]
      [else "coding"])))

(define (classify-intent user-line)
  (or (heuristic-classify (string-downcase user-line))
      (llm-classify user-line)))
```

`max-tokens 10` and `temperature 0.0` keep the classifier call cheap and deterministic. If the classifier itself fails, the handler defaults to `"coding"` -- a conservative choice that enables the full tool set.

### Routing to the Model

`send-to-model` uses the classification to choose the right system prompt and call path:

```racket
(define (send-to-model user-line)
  (define intent (classify-intent user-line))
  (displayln (format "[intent: ~a]" intent))
  (cond
    [(string=? intent "general")
     (define content (or (maybe-search user-line #t) user-line))
     (define msgs
       (list (hash 'role "system" 'content GENERAL-SYSTEM-PROMPT)
             (hash 'role "user" 'content content)))
     (define reply (chat msgs #:model-id (FIREWORKS-MODEL)))
     (displayln (clean reply))]
    [(string=? intent "coding")
     (define updated
       (append (unbox messages-box) (list (hash 'role "user" 'content user-line))))
     (define-values (reply new-messages)
       (chat-with-tools updated ENABLED-TOOLS #:model-id (FIREWORKS-MODEL)))
     (set-box! messages-box new-messages)
     (displayln (clean reply))]
    [else ; hybrid
     (define content (or (maybe-search user-line #f) user-line))
     (define updated
       (append (unbox messages-box) (list (hash 'role "user" 'content content))))
     (define-values (reply new-messages)
       (chat-with-tools updated ENABLED-TOOLS #:model-id (FIREWORKS-MODEL)))
     (set-box! messages-box new-messages)
     (displayln (clean reply))]))
```

General questions use a lightweight one-shot call and a simple system prompt. Coding requests go through the full agentic tool loop using a system prompt that describes the five tools and the rules for using them. Hybrid requests get both the tool loop and web search results prepended to the message (if `/search` is enabled).

### The System Prompt

The coding system prompt is set once per session and injected as the first message with `role "system"`. It tells the model which tools are available and how to use them correctly:

```racket
(define SYSTEM-PROMPT-TEMPLATE
  "You are an interactive coding assistant working in the directory {cwd}.

Rules:
- Use read_file, list_dir, and grep to understand the code BEFORE proposing edits.
- To EDIT an existing file: read_file it first, then pass its exact current contents
  as `old` to propose_edit.
- To CREATE a new file: call propose_edit with the empty string \"\" as `old` and
  the full desired contents as `new`. Do not call read_file first for a file that
  does not exist yet.
- One file per propose_edit call. Keep diffs small and focused.
- If the user rejects an edit or `make check` fails, ask for clarification instead
  of retrying blindly.
- run_shell only accepts whitelisted commands: make, ls, pwd, cat.
- When you are done, reply with a short natural-language summary of what changed.")
```

The `{cwd}` placeholder is replaced with the actual working directory at session start. Telling the model the working directory helps it construct relative paths for `read_file` and `list_dir` calls.

### The ESC Interrupt

The most novel aspect of the REPL is the ESC-to-interrupt mechanism. The model call runs in a background thread while the main thread polls for ESC in raw terminal mode:

```racket
(define (run-model-with-escape thunk)
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
            (display "\n[ESC -- stopping after the current step...]\n")
            (flush-output))
          (sleep 0.05)
          (loop))))
    (lambda ()
      (when has-stty?
        (system "stty sane 2>/dev/null"))))
  (sync/timeout INTERRUPT-WAIT-TIMEOUT worker)
  escaped?)
```

`escape-pressed?` performs a non-blocking `char-ready?` check so the poll loop spends nearly all its time sleeping 50 ms between checks. When ESC is detected, `task-interrupted-set!` flips the shared flag. The worker thread's `chat-with-tools` loop checks that flag at each iteration boundary and returns early without being killed. This cooperative shutdown approach is cleaner than killing the thread forcefully because it allows the thread to restore any state it owns before exiting.

`sync/timeout` waits up to 180 seconds for the worker to finish; if it does not stop within that window the user sees a warning but the REPL continues normally.

### The REPL Loop

The main loop in `agent.rkt` is a straightforward tail-recursive function:

```racket
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
      [(eof-object? line) (void)]
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
                                              (displayln (format "\nError: ~a" (exn-message e)))))])
                 (send-to-model trimmed)))
             (run-model-with-escape task)
             (loop)])])])))

(module+ main
  (run))
```

`handle-slash-command` recognizes `/reset`, `/history`, `/model`, `/debug`, `/search`, `/tokens`, `/help`, and `/quit` before any model call is made. The `module+ main` form lets `agent.rkt` be both loaded as a library (for testing) and run directly from the command line.

## Running the Agent

### Installation

Install Racket 8.11 or later from racket-lang.org. Then install the `http-easy` HTTP client package:

```
raco pkg install http-easy
```

Export your API keys. Only `FIREWORKS_API_KEY` is required; the search keys are optional:

```
export FIREWORKS_API_KEY=fw_...
export BRAVE_SEARCH_API_KEY=BSA...   # optional
export EXA_SEARCH_API_KEY=...        # optional
```

### Starting the REPL

```
make run
```

or directly:

```
racket agent.rkt
```

The banner shows the working directory and the active model:

```
Coding Agent REPL.  /help for commands, /quit to exit.
  cwd:   /Users/mark/myproject
  model: accounts/fireworks/models/deepseek-v4-flash-0731

> 
```

### Sample Session

The following session asks the agent to add a helper function to an existing file. Lines beginning with `>` are user input; everything else is agent output.

```
> add a function called word-count that takes a string and returns the number of words

[intent: coding]
* read_file utils.rkt
* propose_edit utils.rkt

--- a/utils.rkt
+++ b/utils.rkt
@@ -14,3 +14,7 @@
 (define (trim-lines text)
   (string-join (map string-trim (string-split text "\n")) "\n"))
+
+(define (word-count str)
+  (length (string-split str)))
+
+(provide word-count)

Apply this change? [y]es / [n]o / [s]kip and tell the model why: y
applied; make check passed

Added `word-count` to utils.rkt. It splits the string on whitespace using
`string-split` (which treats consecutive spaces as one separator) and returns
the length of the resulting list.

> /tokens

Session token usage:
  Prompt tokens:     1842
  Completion tokens: 87
  Total tokens:      1929
  Estimated cost:    $0.000482  ($0.2200/M prompt, $0.8800/M completion)

> /quit
```

### Enabling Web Search

Toggle search on with `/search`. Switch between engines with `/search brave` or `/search exa`:

```
> /search brave
Web search ON (engine: brave)

> what is the current version of Racket?

[intent: general]
[Web search results for: what is the current version of Racket?]
1. Racket -- A programmable programming language
   https://racket-lang.org
   Racket 8.14 was released on ...
...

As of mid-2026, the current stable release of Racket is version 8.14.
```

### Interrupting a Long Task

Press ESC during any multi-step operation to stop the agent before the next tool call:

```
> refactor all error handling in the project to use a unified log-error helper

[intent: coding]
* list_dir .
* grep "error" .
* read_file src/main.rkt
^[
[ESC -- stopping after the current step...]
[Interrupted. Type your next request or /reset.]

>
```

The terminal is restored immediately and the REPL is ready for the next input.

## Interpreting the Output

When the agent prints `* tool-name arguments` it is showing a tool call in progress. The tool name and a truncated version of the arguments help you follow the model's reasoning. `read_file utils.rkt` means the model decided it needs to see the file before editing it -- a sign it is following the system prompt rules. `propose_edit` always appears after a `read_file` for the same path.

`make check passed` tells you both that the model's proposed syntax was valid Racket and that your project's own test suite or compile step accepted it. If you see `make check FAILED`, the failure output follows immediately and appears in the agent's next prompt -- giving the model a second chance to correct the error autonomously.

The `/tokens` output shows prompt tokens growing much faster than completion tokens. That is expected in an agentic loop: the conversation history (including tool results, which can be long) is re-sent to the model on every iteration, while the model's replies are comparatively short.

## Wrap Up

This chapter built a complete Racket coding agent in roughly 600 lines across four focused modules. The main ideas were:

**Separation of concerns.** The API client, tool registry, approval UI, and search backends each live in their own file. `dynamic-require` resolves the circular dependency between the API client and the tool dispatcher without coupling the modules statically.

**The stale-base guard in `propose_edit`.** Requiring the model to supply the exact current contents of a file before any edit is accepted prevents silent overwrites when the file changes between the read and edit steps. The mismatch is reported as a tool result the model can read and respond to.

**Two-stage intent classification.** A free keyword heuristic handles the common cases and falls back to a cheap LLM call only for ambiguous queries. Defaulting to `"coding"` on classifier failure keeps the full tool set available.

**The `make check` feedback loop.** Every accepted edit is immediately verified by the project's own build target. Failures go back into the conversation history, giving the model the information it needs to self-correct on the next iteration.

**Cooperative ESC interruption.** Racket's native threads make it straightforward to run the model call in the background while the main thread polls for user input. The interrupt flag coordinates graceful shutdown without forcefully killing the worker thread, so terminal state is always restored cleanly.

These patterns -- tool registries, approval gates with stale-base guards, intent routing, quality gates, and graceful interruption -- apply broadly across languages and LLM providers. The Racket implementation here serves as a concrete reference for how each piece fits together at the system level.

## Optional Practice Problems

**Problem 1: Add a `write_file` tool**

The agent currently has no way for the model to create a file without going through `propose_edit`. Add a `write_file` tool to `tools.rkt` that accepts a `path` and `content` parameter, writes the content directly (without a diff prompt), and returns a confirmation string. Register it in `register-all` and add it to `ENABLED-TOOLS`. Consider what safety constraints, if any, should prevent the model from overwriting files outside the working directory.

**Problem 2: Extend the shell whitelist dynamically**

`SHELL-WHITELIST` is currently a compile-time constant. Add a `/allow-cmd` slash command to `agent.rkt` that lets the user append a command to the whitelist at runtime -- for example, `/allow-cmd git` would let the model run `git status` and `git diff`. Update `handle-slash-command` to recognize the new command and update the set stored in `tools.rkt`. Think about where the mutable whitelist state should live and how `tools.rkt` should expose it.

**Problem 3: Persistent session history**

At present, `/reset` discards the conversation history and there is no way to resume a previous session. Add two slash commands: `/save <filename>` that writes the current `messages-box` contents to a JSON file using `jsexpr->string`, and `/load <filename>` that reads that file and restores the conversation. Use `string->jsexpr` for loading. Handle file-not-found and malformed JSON gracefully by printing an error and leaving the existing history unchanged.

**Problem 4: Token-budget guard**

`chat-with-tools` will keep iterating until the model stops calling tools or `max-iterations` is reached. Add a token-budget guard that checks `session-total-tokens` after each iteration and returns early with a warning message if the running total exceeds a configurable threshold. Expose the threshold as a `/budget <n>` slash command that sets it, and a `/budget` command with no argument that prints the current setting and remaining budget.

**Problem 5: Second search backend -- DuckDuckGo**

Add a `ddg-search` function to `search.rkt` using the DuckDuckGo Instant Answer API at `https://api.duckduckgo.com/?q=QUERY&format=json`. The response contains a `RelatedTopics` array of objects with `Text` and `FirstURL` fields. Return results in the same `(url title description)` triple format as `brave-search` and `exa-search`. Update `agent.rkt` to accept `/search ddg` as a valid engine selection.

**Problem 6: Colored intent label in the REPL prompt**

The line `[intent: coding]` is printed in plain text. Use ANSI codes (already defined in `approval.rkt`) to color the label: green for `"coding"`, cyan for `"general"`, and yellow (`"\033[33m"`) for `"hybrid"`. Update `send-to-model` in `agent.rkt` to apply the color. Since `approval.rkt` already defines the ANSI constants, think about whether to `require` them from there, redefine them locally, or move them to a shared `ansi.rkt` module.

**Problem 7: Retry on `make check` failure**

Currently, when `propose_edit` runs `make check` and it fails, the failure output is returned to the model as a tool result -- but the model must then propose a new edit from scratch. Modify `tool-propose-edit` so that on a `make check` failure it offers the user a `[r]etry` option at the approval prompt (in addition to the existing `y/n/s` choices). On retry, revert the file to its previous contents using `call-with-output-file`, print a confirmation, and return a result string that tells the model the file was reverted and includes the check output so it can try again with a corrected edit.

**Problem 8: Streaming output**

The Fireworks API (like most OpenAI-compatible APIs) supports server-sent event streaming via `"stream": true` in the request body. The response arrives as a sequence of `data: {...}` lines rather than a single JSON object. Modify `post-fireworks` in `fireworks-ai.rkt` to accept an optional `#:stream? [stream? #f]` keyword argument. When `stream?` is `#t`, read the response body line by line with `read-line`, parse each `data:` chunk with `string->jsexpr`, extract the `delta.content` field, and `display` each fragment as it arrives so the user sees the reply being written in real time. Fall back to the existing non-streaming path when `stream?` is `#f`.
