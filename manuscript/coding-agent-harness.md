# A Racket Coding Agent

The source code for this example is in the directory **coding-agent-harness**.

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

This chapter builds a complete Racket implementation of such a coding agent. The agent classifies each user request as a coding task, a general question, or a hybrid, and routes it accordingly. It supports live web search via Brave or Exa AI, renders colored unified diffs before any file is written, and gates every accepted edit on `make check`. Model access is configurable: named provider profiles in a JSON config file select the endpoint, the model, the generation parameters, and the pricing, so the same loop runs against either a cloud provider such as Fireworks AI or a local MLX server. The same program also works as a Unix-style command that runs a single prompt and exits, while the interactive REPL adds readline editing, persistent history, and Tab completion.

## Module Architecture

The project is organized into nine source files, each with a single clear responsibility:

```
agent.rkt          REPL, CLI, intent classifier, provider dispatch, context management
harness-config.rkt Hierarchical JSON config: providers, generation parameters, pricing
fireworks-ai.rkt   Fireworks and OpenAI-compatible client (SSE streaming), usage, cost
mlx-serve.rkt      Local MLX client (mlx_lm.server /v1/chat/completions), usage
chat-loop.rkt      Provider-agnostic agentic tool loop shared by both backends
tools.rkt          Tool registry, five coding tools, the propose_edit approval gate
approval.rkt       Colored diff printer, y/n/s prompt
search.rkt         Brave Search and Exa AI search backends
line-input.rkt     Readline-backed line editing, history, and Tab completion
```

The dependency graph is acyclic and close to linear. `harness-config.rkt` reads JSON and nothing else; `approval.rkt` only prints and prompts; `tools.rkt` requires `approval.rkt`; `chat-loop.rkt` requires `tools.rkt`; and the two provider clients require `chat-loop.rkt`, `tools.rkt`, and `harness-config.rkt`. `agent.rkt` requires everything and owns the session state.

The one place where a static `require` is not enough is `line-input.rkt`. Racket's `readline` collection raises at instantiation time when no Editline or GNU Readline shared library is installed, so an unconditional `require` would take the whole harness down on machines without one. The module loads `readline/readline` with `dynamic-require` inside an exception handler instead, so a missing library degrades to a plain `read-line` rather than a startup failure. That is also why the `Makefile` builds the standalone executable with `++lib readline/readline`: the flag embeds the module for `raco exe` while leaving it uninstantiated until the REPL actually needs it.

## The Provider-Agnostic Agentic Loop

The heart of the agent is a loop that is deliberately independent of any particular model vendor. It lives in `chat-loop.rkt` and is parameterized by a `post-fn` argument: a function that takes an OpenAI-style chat-completions payload and returns a normalized response hash. Both the Fireworks client and the MLX client supply their own `post-fn`, and the loop never talks to a network directly.

Here is the complete file:

```racket
#lang racket

(require racket/string
         "tools.rkt")

(provide chat*
         chat-with-tools*)

;; ---------------------------------------------------------------------------
;; Helpers

(define (response-message data)
  ;; Extract the assistant message hash from a normalized response, or #f if
  ;; the response has no usable choices/message.
  (define choices (hash-ref data 'choices #f))
  (and (pair? choices)
       (let ([choice (first choices)])
         (and (hash? choice) (hash-ref choice 'message #f)))))

(define (msg-content msg)
  ;; The assistant message's 'content, coerced to a string. Some OpenAI-compatible
  ;; servers (e.g. mlx_lm.server) emit "content": null on tool-only responses;
  ;; hash-ref with a default returns the literal 'null in that case, so coerce
  ;; any non-string value to "" here.
  (define c (and msg (hash-ref msg 'content "")))
  (if (string? c) c ""))

(define (without-dangling msgs)
  (if (and (not (null? msgs))
           (hash-has-key? (last msgs) 'tool_calls))
      (drop-right msgs 1)
      msgs))

;; ---------------------------------------------------------------------------
;; chat* : post-fn (listof hash) ... -> string

;; Build a request body, omitting generation parameters the active provider
;; profile did not declare.  No max_tokens/temperature default is compiled in
;; here; the provider config is the only source for them.
(define (request-payload model-id max-tokens temperature messages)
  (define base (hash 'model model-id 'messages messages))
  (define with-max (if max-tokens (hash-set base 'max_tokens max-tokens) base))
  (if temperature (hash-set with-max 'temperature temperature) with-max))

(define (chat* post-fn messages
               #:model-id model-id
               #:max-tokens max-tokens
               #:temperature temperature)
  (define payload
    (request-payload model-id max-tokens temperature messages))
  (define data (post-fn payload))
  (define msg (response-message data))
  (define content (msg-content msg))
  (if (and (string? content) (not (string=? content "")))
      content
      "No response content"))

;; ---------------------------------------------------------------------------
;; chat-with-tools* : post-fn (listof hash) (listof string) ... -> (values string (listof hash))
;; Multi-turn agentic loop. Returns two values: final-text and final-messages.

(define (chat-with-tools* post-fn messages tools
                          #:model-id model-id
                          #:max-tokens max-tokens
                          #:temperature temperature
                          #:max-iterations max-iterations)
  (define tools-rendered (render-tools tools))
  (define current-messages (box messages))
  ;; Repetition detection: track the last few tool-call signatures so a model
  ;; stuck issuing the identical failing call is stopped early rather than
  ;; burning all max-iterations. A signature is (name . args-json) per call,
  ;; sorted, so multi-call batches compare as a set.
  (define recent-signatures '())
  (define REPEAT-WINDOW 5)   ; remember the last N batches
  (define REPEAT-LIMIT 2)    ; >= 2 identical batches in the window => stuck

  (define (call-signature tool-calls)
    (sort
     (for/list ([tc (in-list tool-calls)])
       (define f (hash-ref tc 'function (hash)))
       (format "~a|~a" (hash-ref f 'name "") (hash-ref f 'arguments "")))
     string<?))

  ;; Returns #t when the same batch of calls appeared >= REPEAT-LIMIT times in
  ;; the recent window.
  (define (seen-too-often? sig)
    (>= (length (filter (lambda (s) (equal? s sig)) recent-signatures))
        (sub1 REPEAT-LIMIT)))

  ;; Take the rightmost (most recent) n items of a list.
  (define (take-right lst n)
    (if (<= (length lst) n) lst (drop lst (- (length lst) n))))

  (define (append-tool-results! results)
    ;; Append each (call-id name result-str) tuple as a tool-role message.
    (for ([r (in-list results)])
      (define call-id (first r))
      (define name (second r))
      (define result-str (third r))
      (set-box! current-messages
                (append (unbox current-messages)
                        (list (hash 'role "tool"
                                    'tool_call_id call-id
                                    'name name
                                    'content result-str))))))

  (define (loop iter)
    (cond
      [(>= iter max-iterations)
       ;; Max iterations -- one final no-tools call for summary
       (define payload
         (request-payload model-id max-tokens temperature (unbox current-messages)))
       (with-handlers ([exn:fail? (lambda (_) (values "(max tool iterations reached)" (unbox current-messages)))])
         (define data (post-fn payload))
         (define msg (response-message data))
         (define content (if msg (hash-ref msg 'content "") ""))
         (values (if (and (string? content) (not (string=? content "")))
                     content
                     "(no summary from model)")
                 (if msg
                     (append (unbox current-messages) (list msg))
                     (unbox current-messages))))]
      [else
       (define payload
         (let ([base (request-payload model-id max-tokens temperature
                                      (unbox current-messages))])
           (if (null? tools-rendered)
               base
               (hash-set* base 'tools tools-rendered 'tool_choice "auto"))))
       (define data (post-fn payload))
       (define msg (response-message data))
       (unless msg
         (error 'chat-with-tools* "response has no 'message'. Raw: ~a" data))
        (define tool-calls (hash-ref msg 'tool_calls #f))
        (define content (msg-content msg))
        ;; Append the assistant message
        (set-box! current-messages (append (unbox current-messages) (list msg)))
        (cond
          [(and tool-calls
                (list? tool-calls)
                (pair? tool-calls)
                (seen-too-often? (call-signature tool-calls)))
           ;; The model is stuck re-issuing the identical call(s) -- bail out
           ;; with an explanation instead of looping to max-iterations.
           (values (string-append
                    "(stopped: the model repeated the identical tool call(s) "
                    (number->string REPEAT-LIMIT)
                    " times without making progress; it may be too weak for this "
                    "task or its arguments are malformed)")
                   (unbox current-messages))]
          [(and content tool-calls (not (string=? (string-trim content) "")))
           (displayln "")
           (displayln (string-trim content))
           (set! recent-signatures
                 (take-right (cons (call-signature tool-calls) recent-signatures)
                             REPEAT-WINDOW))
           (append-tool-results! (execute-tool-calls tool-calls))
           (loop (add1 iter))]
          [(not tool-calls)
           (values (or content "(empty response from model)") (unbox current-messages))]
          [else
           (set! recent-signatures
                 (take-right (cons (call-signature tool-calls) recent-signatures)
                             REPEAT-WINDOW))
           (append-tool-results! (execute-tool-calls tool-calls))
           (loop (add1 iter))])]))

  (loop 0))
```

`chat*` is the single-shot path used for general questions and for the intent classifier: it builds a request, calls `post-fn` once, and returns the assistant's text. `chat-with-tools*` is the multi-turn loop. It renders the enabled tools into OpenAI function-calling schema, appends the assistant message and then one `tool`-role message per result, and recurses until the model answers without asking for a tool or the iteration cap is reached.

Two details in the loop are worth calling out. First, `request-payload` includes `max_tokens` and `temperature` only when they are actual values. A provider profile that omits a generation parameter sends no parameter at all, which lets the server apply its own default. Nothing is defaulted in Racket code.

Second, the loop defends itself against a model that gets stuck. `call-signature` reduces a batch of tool calls to a sorted list of `name|arguments` strings, and the loop remembers the signatures of the last `REPEAT-WINDOW` (five) batches. If the current batch already appears in that window, `seen-too-often?` fires and the loop returns an explanation instead of burning the remaining iterations on the identical call. This matters most with small local models, which tend to re-issue the same malformed call when a tool result does not change their mind.

There are two edge cases worth noting. The first is a model that emits *both* text and tool calls in the same turn: the text is printed to the user immediately (so it can narrate "I will read the file first"), then the tools run, then the loop continues. The second is the iteration cap: at `max-iterations` the loop makes one final call with no tools, asking the model to summarize what it did, rather than returning nothing.

The `without-dangling` helper trims a trailing assistant message whose `tool_calls` have no matching `tool` results, the state a history lands in if a run is cut short. The current loop never calls it, but it is the repair to reach for if you add a path that can abandon a turn mid-flight.

## The Fireworks AI Client

### The API and Pricing

Fireworks AI is a hosted inference platform that serves many open-weight models through an OpenAI-compatible API. The agent does not compile in an endpoint, a model, or a price. All of those come from the active provider profile in the harness config, which is covered later in this chapter. The example profile used in this chapter declares DeepSeek Flash (`accounts/fireworks/models/deepseek-v4p1-flash`) at $0.14 per million uncached input tokens, $0.028 per million cached input tokens (an 80 percent cache discount), and $0.28 per million output tokens.

The estimated session cost accumulated over a conversation is:

```$
\text{cost} = (p - k) \times \frac{0.14}{10^6} + k \times \frac{0.028}{10^6} + c \times \frac{0.28}{10^6}
```

where `p` is the total prompt tokens, `k` is the cached portion of those prompt tokens (billed at the discount), and `c` is the total completion tokens. The agent tracks all of these and displays the running total on demand. The rates themselves are read from the profile's `pricing` block, so a different profile can declare different numbers; a profile that declares no pricing at all reports the cost as unknown rather than pretending it is free.

### Streaming with Server-Sent Events

Unlike the plain one-shot `chat`, the Fireworks client streams its responses using **server-sent events (SSE)**. The API sends a sequence of `data:` lines, each carrying a small *delta* of the response, terminated by a `data: [DONE]` line. Streaming removes any wall-clock cap on generation time, at the cost of reassembling the response on the client side. The only timeouts are `CURL-MAX-TIME` (seconds to wait for response headers and the TCP connection) and `STREAM-IDLE-TIMEOUT` (seconds of *silence* from the server before giving up). As long as tokens keep flowing, a request may run for minutes.

Here is the complete file:

```racket
#lang racket

(require net/http-easy
         json
         racket/string
         racket/port
         "tools.rkt"
         "chat-loop.rkt"
         "harness-config.rkt")

(provide DEBUG-LOG
         CURL-MAX-TIME
         active-pricing
         prompt-cost
         completion-cost
         cached-cost
         accumulate-usage
         reset-session-stats
         session-cost
         print-session-stats
         chat
         chat-with-tools
         make-sse-line-reader
         parse-sse-response)

;; ---------------------------------------------------------------------------
;; Constants
;;
;; Endpoint, model, api_key_env, generation parameters, and pricing all come
;; from the active provider profile in the harness config; no provider-specific
;; value is compiled in here.

(define DEBUG-LOG (make-parameter #f))
;; Requests use SSE streaming ("stream": true), so there is NO total
;; wall-clock cap on generation: a long response that keeps producing
;; tokens simply keeps streaming. The only remaining timeouts are:
;;   CURL-MAX-TIME       -- seconds to wait for response headers (TTFT)
;;                          and for the TCP connection itself.
;;   STREAM-IDLE-TIMEOUT -- seconds of *silence* from the server before we
;;                          give up. Tokens arriving periodically never
;;                          trip this; only a genuinely stalled connection
;;                          does. Turn up / down as you like.
;; http-easy's default request timeout is only 30s, so these MUST be passed
;; via #:timeouts below or the constants do nothing.
(define CURL-MAX-TIME 600)
(define STREAM-IDLE-TIMEOUT 300)

;; Pricing is read from the active provider profile's "pricing" block (USD per
;; 1M tokens).  A profile that declares no pricing yields #f rates, and callers
;; report the cost as unknown instead of inventing a number.

(define (active-pricing)
  (provider-pricing (config-active-provider)))

;; ---------------------------------------------------------------------------
;; Session stats (thread-safe)

(define stats-sema (make-semaphore 1))
(define session-prompt-tokens (box 0))
(define session-completion-tokens (box 0))
(define session-total-tokens (box 0))
(define session-cached-tokens (box 0))

(define (reset-session-stats)
  (call-with-semaphore stats-sema
    (lambda ()
      (set-box! session-prompt-tokens 0)
      (set-box! session-completion-tokens 0)
      (set-box! session-total-tokens 0)
      (set-box! session-cached-tokens 0))))

;; Costs use the active provider's configured rates.  Each returns #f when the
;; profile declares no such rate, so callers can report "unknown" instead of a
;; misleading $0.00.

(define (rate-cost tokens rate)
  (and rate (* tokens rate (/ 1 1000000))))

(define (prompt-cost tokens)
  (rate-cost tokens (pricing-ref (active-pricing) 'input)))

(define (cached-cost tokens)
  (rate-cost tokens (pricing-ref (active-pricing) 'cached_input)))

(define (completion-cost tokens)
  (rate-cost tokens (pricing-ref (active-pricing) 'output)))

;; Cached input tokens are reported by the server in
;; usage.prompt_tokens_details.cached_tokens and are part of prompt_tokens;
;; bill them at the discounted rate and subtract them from the uncached pool.
(define (session-cost)
  ;; -> number, or #f when the active profile declares no pricing at all.
  (define rates (active-pricing))
  (define input (pricing-ref rates 'input))
  (define cached (pricing-ref rates 'cached_input))
  (define output (pricing-ref rates 'output))
  (and (or input cached output)
       (call-with-semaphore stats-sema
         (lambda ()
           (define pt (unbox session-prompt-tokens))
           (define ca (unbox session-cached-tokens))
           (+ (or (rate-cost (max 0 (- pt ca)) input) 0)
              (or (rate-cost ca cached) 0)
              (or (rate-cost (unbox session-completion-tokens) output) 0))))))

(define (print-session-stats)
  (define-values (pt ct tt ca)
    (call-with-semaphore stats-sema
      (lambda ()
        (values (unbox session-prompt-tokens)
                (unbox session-completion-tokens)
                (unbox session-total-tokens)
                (unbox session-cached-tokens)))))
  (define cost (session-cost))
  (define rates (active-pricing))
  (displayln "")
  (displayln "Session token usage:")
  (displayln (format "  Prompt tokens:     ~a" pt))
  (displayln (format "  Completion tokens: ~a" ct))
  (displayln (format "  Total tokens:      ~a" tt))
  (when (> ca 0)
    (define pct (* 100.0 (/ ca (max 1 pt))))
    (displayln (format "  Cached tokens:     ~a (~a% of prompt)" ca (~r pct #:precision 1))))
  (if cost
      (displayln (format "  Estimated cost:    $~a  ($~a/M input, $~a/M cached input, $~a/M output)"
                         (~r cost #:precision 6)
                         (~r (or (pricing-ref rates 'input) 0) #:precision 4)
                         (~r (or (pricing-ref rates 'cached_input) 0) #:precision 4)
                         (~r (or (pricing-ref rates 'output) 0) #:precision 4)))
      (displayln "  Estimated cost:    n/a (no \"pricing\" block for this provider)")))

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
                    (+ (unbox session-cached-tokens) (hash-ref details 'cached_tokens 0))))))))

;; ---------------------------------------------------------------------------
;; API key
;;
;; The env var name comes from the active provider profile's api_key_env when
;; a harness config is loaded; falls back to FIREWORKS_API_KEY.

(define (get-api-key)
  (define env-name
    (or (let ([p (config-active-provider)])
          (and p (provider-api-key-env p)))
        "FIREWORKS_API_KEY"))
  (define key (getenv env-name))
  (unless (and key (not (string=? key "")))
    (error 'fireworks-ai "~a environment variable not set" env-name))
  key)

;; ---------------------------------------------------------------------------
;; SSE streaming helpers

;; Index of the first byte in `bstr` equal to byte `b`, or #f if absent.
(define (bytes-index-of bstr b)
  (let loop ([i 0]
             [len (bytes-length bstr)])
    (cond
      [(= i len) #f]
      [(= (bytes-ref bstr i) b) i]
      [else (loop (add1 i) len)])))

;; Returns a stateful function that reads one line at a time from the SSE
;; response stream `in`. Each read waits up to STREAM-IDLE-TIMEOUT seconds
;; for the next byte (an idle timeout, not a wall-clock cap), so long slow
;; generations never hit a total-time limit as long as tokens keep flowing.
;; Each call returns a line as bytes (newline stripped) or eof at end of
;; stream. Partial lines are buffered between calls.
(define (make-sse-line-reader in)
  (define buf (make-bytes 4096))
  (define acc (box #""))
  (define (read-more!) ;; -> #t at EOF, #f after appending more bytes
    (unless (sync/timeout STREAM-IDLE-TIMEOUT
              (handle-evt in (lambda (_) #t)))
      (error 'fireworks-ai
             "stream idle timeout: no data for ~a seconds"
             STREAM-IDLE-TIMEOUT))
    (define n (read-bytes-avail! buf in))
    (cond
      [(eof-object? n) #t]
      [else
       (when (> n 0)
         (set-box! acc (bytes-append (unbox acc) (subbytes buf 0 n))))
       #f]))
  (lambda ()
    (let loop ()
      (define data (unbox acc))
      (define nl (bytes-index-of data 10))    ; 10 == #\n
      (cond
        [nl
         ;; complete line available; keep the remainder for the next call
         (set-box! acc (subbytes data (add1 nl)))
         (subbytes data 0 nl)]
        [(read-more!)
         ;; EOF: whatever is left is the final unterminated line
         (define rest (unbox acc))
         (set-box! acc #"")
         (if (zero? (bytes-length rest)) eof (subbytes rest 0))]
        [else (loop)]))))

;; Parse one SSE "data: {...}" body into a jsexpr hash (or #f on bad JSON).
(define (parse-sse-chunk body)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (string->jsexpr body)))

;; Reconstruct the equivalent non-streaming chat-completions response hash
;; from an SSE stream:
;;   (hash 'id ... 'model ...
;;         'choices (list (hash 'message <assistant msg>
;;                               'finish_reason ...))
;;         'usage (hash ...))
;; `message` carries accumulated 'content, (deepseek) 'reasoning_content, and
;; (when present) a list of 'tool_calls hashes exactly like a non-streaming
;; response: each (hash 'id ... 'type "function"
;;                       'function (hash 'name ... 'arguments <json string>)).
(define (parse-sse-response in)
  (define content-out (open-output-string))
  (define reasoning-out (open-output-string))
  (define tool-calls-by-index (make-hash)) ; index -> (hash 'id 'type 'name 'arguments-box)
  (define usage #f)
  (define finish-reason #f)
  (define message-id (box ""))
  (define message-model (box ""))
  (define next-line (make-sse-line-reader in))
  (let loop ()
    (define line (next-line))
    (cond
      [(eof-object? line) (void)]
      [else
       ;; line is bytes: convert, then trim whitespace and trailing \r from CRLF
       (define trimmed (string-trim (bytes->string/utf-8 line)))
       (cond
         [(or (string=? trimmed "")
              (string-prefix? trimmed ":"))    ; comment / keep-alive
          (loop)]
         [(string-prefix? trimmed "data:")
          (define body (string-trim (substring trimmed 5)))
          (cond
            [(string=? body "[DONE]") (void)]
            [else
             (define chunk (parse-sse-chunk body))
             (when (hash? chunk)
               ;; API-level error inside the stream
               (when (hash-has-key? chunk 'error)
                 (define err (hash-ref chunk 'error))
                 (define msg
                   (cond
                     [(hash? err) (hash-ref err 'message (format "~a" err))]
                     [else (format "~a" err)]))
                 (error 'fireworks-ai "API error: ~a" msg))
               (when (hash-has-key? chunk 'id)
                 (set-box! message-id (hash-ref chunk 'id "")))
               (when (hash-has-key? chunk 'model)
                 (set-box! message-model (hash-ref chunk 'model "")))
               (define chunk-usage (hash-ref chunk 'usage #f))
               (when (and chunk-usage (hash? chunk-usage))
                 (set! usage chunk-usage))
               (for ([c (in-list (hash-ref chunk 'choices '()))])
                 (define delta (hash-ref c 'delta (hash)))
                 (define fr (hash-ref c 'finish_reason #f))
                 (when (and fr (not (equal? fr finish-reason)))
                   (set! finish-reason fr))
                 (define c-delta (hash-ref delta 'content #f))
                 (when (string? c-delta)
                   (display c-delta content-out))
                 (define r-delta (hash-ref delta 'reasoning_content #f))
                 (when (string? r-delta)
                   (display r-delta reasoning-out))
                 (define tc (hash-ref delta 'tool_calls #f))
                 (when (and tc (list? tc))
                   (for ([t (in-list tc)])
                     (define idx (hash-ref t 'index 0))
                     (define entry (hash-ref tool-calls-by-index idx #f))
                     (unless entry
                       (set! entry (make-hash (list (cons 'id "")
                                                     (cons 'type "function")
                                                     (cons 'name "")
                                                     (cons 'arguments (box "")))))
                       (hash-set! tool-calls-by-index idx entry))
                     (define t-id (hash-ref t 'id #f))
                     (when (and (string? t-id) (not (string=? t-id "")))
                       (hash-set! entry 'id t-id))
                     (define t-type (hash-ref t 'type #f))
                     (when (string? t-type)
                       (hash-set! entry 'type t-type))
                     (define f (hash-ref t 'function #f))
                     (when (hash? f)
                       (define f-name (hash-ref f 'name #f))
                       (when (and (string? f-name) (not (string=? f-name "")))
                         (hash-set! entry 'name f-name))
                       (define f-args (hash-ref f 'arguments #f))
                       (when (and (string? f-args) (not (string=? f-args "")))
                         (define b (hash-ref entry 'arguments))
                         (set-box! b (string-append (unbox b) f-args))))))))
              (loop)])]
         [else (loop)])]))
  (define content (get-output-string content-out))
  (define reasoning (get-output-string reasoning-out))
  (define idxs (sort (hash-keys tool-calls-by-index) <))
  (define tool-calls
    (if (null? idxs)
        #f
        (for/list ([idx (in-list idxs)])
          (define e (hash-ref tool-calls-by-index idx))
          (hash 'id (hash-ref e 'id)
                'type (hash-ref e 'type)
                'function (hash 'name (hash-ref e 'name)
                                'arguments (unbox (hash-ref e 'arguments)))))))
  (define message
    (if tool-calls
        (hash 'role "assistant"
              'content content
              'reasoning_content reasoning
              'tool_calls tool-calls)
        (hash 'role "assistant"
              'content content
              'reasoning_content reasoning)))
  (hash 'id (unbox message-id)
        'model (unbox message-model)
        'choices (list (hash 'message message
                             'finish_reason finish-reason))
        'usage (or usage (hash))))

;; ---------------------------------------------------------------------------
;; Low-level POST (streaming)

(define (post-fireworks payload)
  (define api-key (get-api-key))
  (define p (config-active-provider))
  (define endpoint
    (or (and p (provider-endpoint p))
        (error 'fireworks-ai
               "active provider profile has no \"endpoint\"; set it in the harness config")))
  (define headers
    (hash 'content-type "application/json"
          'accept "application/json"
          'authorization (string-append "Bearer " api-key)))
  (define stream-payload
    (hash-set* payload
               'stream #t
               'stream_options (hash 'include_usage #t)))
  (when (DEBUG-LOG)
    (displayln (format "[DEBUG] request: ~a" (jsexpr->string (hash-remove stream-payload 'messages)))))
  (define data
    (with-handlers ([exn:fail? (lambda (e) (error 'fireworks-ai "HTTP error: ~a" (exn-message e)))])
      (define resp
        (post endpoint
              #:headers headers
              #:json stream-payload
              #:stream? #t
              #:close? #f
              #:timeouts (make-timeout-config #:request CURL-MAX-TIME
                                              #:connect CURL-MAX-TIME)))
      (define j (parse-sse-response (response-output resp)))
      (response-close! resp)
      (when (DEBUG-LOG)
        (displayln (format "[DEBUG] response: ~a" (jsexpr->string j))))
      j))
  (when (hash-has-key? data 'error)
    (define err (hash-ref data 'error))
    (define msg
      (cond
        [(hash? err) (hash-ref err 'message (format "~a" err))]
        [else (format "~a" err)]))
    (error 'fireworks-ai "API error: ~a" msg))
  (accumulate-usage data)
  (unless (hash-has-key? data 'choices)
    (error 'fireworks-ai "response has no 'choices'. Raw: ~a" (jsexpr->string data)))
  data)

;; ---------------------------------------------------------------------------
;; chat / chat-with-tools -- thin wrappers over the shared provider-agnostic
;; loop in chat-loop.rkt (also used by mlx-serve.rkt).
;;
;; Generation defaults come from the active provider profile's "generation"
;; section when a harness config is loaded; explicit keyword args win.

;; Model and generation parameters resolve from the active provider profile.
;; A missing model is an error; missing generation parameters are left out of
;; the request so the server's own default applies.

(define (active-model-id)
  (define p (config-active-provider))
  (or (and p (provider-model p))
      (error 'fireworks-ai
             "active provider profile has no \"model\"; set it in the harness config")))

(define (gen-param key)
  (generation-ref (provider-generation (config-active-provider)) key #f))

(define (chat messages
              #:model-id [model-id (active-model-id)]
              #:max-tokens [max-tokens (gen-param 'max_tokens)]
              #:temperature [temperature (gen-param 'temperature)])
  (chat* post-fireworks messages
         #:model-id model-id
         #:max-tokens max-tokens
         #:temperature temperature))

(define (chat-with-tools messages tools
                         #:model-id [model-id (active-model-id)]
                         #:max-tokens [max-tokens (gen-param 'max_tokens)]
                         #:temperature [temperature (gen-param 'temperature)]
                         #:max-iterations [max-iterations 20])
  (chat-with-tools* post-fireworks messages tools
                    #:model-id model-id
                    #:max-tokens max-tokens
                    #:temperature temperature
                    #:max-iterations max-iterations))
```

### Reassembling the SSE Stream

The SSE stream is a sequence of lines like:

```
data: {"id":"...","choices":[{"delta":{"content":"The"}}]}
data: {"id":"...","choices":[{"delta":{"content":" answer"}}]}
data: {"id":"...","choices":[{"delta":{"content":" is"}}]}
data: {"id":"...","choices":[{"delta":{},"finish_reason":"stop"}]}
data: [DONE]
```

`make-sse-line-reader` returns a stateful function that yields one line at a time, buffering partial lines between calls and applying the idle timeout. `parse-sse-response` then walks those lines and reassembles the deltas:

- **`content`** deltas are appended to a string output port.
- **`reasoning_content`** deltas (for reasoning models) go to a separate port.
- **`tool_calls`** deltas are the tricky part, because a single tool call's name and arguments arrive split across many chunks. The code accumulates them in a hash keyed by the call's `index`, appending argument fragments to a boxed string. At the end it sorts the indices and rebuilds the tool-call list.
- The final `usage` chunk is captured for token accounting.

The output is a single normalized response hash with the same shape the non-streaming MLX backend produces, so `chat-loop.rkt` never knows which backend it is talking to.

### Token Accounting and Cost

Because Fireworks is a paid service, the module tracks usage. The session counters live in boxes guarded by a semaphore, the same defensive pattern used for shared mutable state elsewhere in the harness. Cached input tokens are reported by Fireworks in `usage.prompt_tokens_details.cached_tokens`; they are part of `prompt_tokens` but are billed at the discounted rate. The `session-cost` function subtracts them from the uncached pool and bills them separately, and it returns `#f` when the active profile declares no rates at all. The `/tokens` command prints the breakdown, including the cached-token percentage.

## The MLX Client

The MLX backend, `mlx-serve.rkt`, mirrors the Fireworks interface so that the agent loop and the REPL can swap providers with a single parameter. It targets `mlx_lm.server`, which exposes the OpenAI-compatible `/v1/chat/completions` route. That protocol already returns exactly the shape `chat-loop.rkt` consumes, namely `choices[].message` with optional `tool_calls` and `usage.prompt_tokens` / `completion_tokens`, so unlike Ollama's native `/api/chat` there is no message re-shaping at the boundary: the module forwards the OpenAI payload as it is and returns the response unchanged.

Here is the complete file:

```racket
#lang racket

(require net/http-easy
         json
         racket/string
         "fireworks-ai.rkt"   ; for DEBUG-LOG (shared /debug toggle)
         "chat-loop.rkt"
         "harness-config.rkt")

(provide MLX-THINK
         mlx-active-provider   ; parameter: provider hash to read settings from
         mlx-reset-session-stats
         mlx-print-session-stats
         post-mlx
         mlx-chat
         mlx-chat-with-tools)

;; ---------------------------------------------------------------------------
;; Constants
;;
;; Endpoint, model, api_key_env, and generation parameters all come from the
;; active provider profile in the harness config; nothing provider-specific is
;; compiled in here.

;; The provider hash MLX requests consult for endpoint/model/generation.
;; agent.rkt sets this to the active profile.
(define mlx-active-provider (make-parameter #f))

(define (current-provider-json)
  (or (mlx-active-provider) (hash)))

;; The OpenAI-compatible endpoint returns reasoning in the assistant message's
;; 'reasoning field, which chat-loop.rkt ignores (it only reads 'content and
;; 'tool_calls), so there is no separate thinking toggle to wire up here.
(define MLX-THINK (make-parameter #f))
;; Non-streaming request: the whole generation must complete within this
;; window. Local models on large weights can be slow, so be generous.
(define MLX-MAX-TIME 900)
(define MLX-CONNECT-TIME 10)

;; ---------------------------------------------------------------------------
;; Session stats (thread-safe). mlx_lm.server reports prompt_tokens /
;; completion_tokens on every /v1/chat/completions response. Local inference is
;; free, so stats are informational only -- estimated cost is always $0.

(define stats-sema (make-semaphore 1))
(define session-prompt-tokens (box 0))
(define session-completion-tokens (box 0))

(define (mlx-reset-session-stats)
  (call-with-semaphore stats-sema
    (lambda ()
      (set-box! session-prompt-tokens 0)
      (set-box! session-completion-tokens 0))))

(define (mlx-accumulate-usage usage)
  (when (and (hash? usage) (not (hash-empty? usage)))
    (call-with-semaphore stats-sema
      (lambda ()
        (set-box! session-prompt-tokens
                  (+ (unbox session-prompt-tokens)
                     (hash-ref usage 'prompt_tokens 0)))
        (set-box! session-completion-tokens
                  (+ (unbox session-completion-tokens)
                     (hash-ref usage 'completion_tokens 0)))))))

(define (mlx-print-session-stats)
  (define-values (pt ct)
    (call-with-semaphore stats-sema
      (lambda ()
        (values (unbox session-prompt-tokens)
                (unbox session-completion-tokens)))))
  (displayln "")
  (displayln "Session token usage (local MLX -- no API cost):")
  (displayln (format "  Prompt tokens:     ~a" pt))
  (displayln (format "  Completion tokens: ~a" ct))
  (define mdl (or (provider-model (current-provider-json)) "?"))
  (displayln (format "  Estimated cost:    $0  (local model ~a)" mdl)))

;; ---------------------------------------------------------------------------
;; Low-level POST (non-streaming, OpenAI-compatible)
;;
;; mlx_lm.server and Ollama's OpenAI shim both serve /v1/chat/completions.
;; The request and response already match what chat-loop.rkt / chat-with-tools*
;; consume, so we forward the OpenAI payload as-is and return the response
;; unchanged. An optional Bearer key from the provider profile is sent when set
;; (required by remote Ollama-style endpoints, ignored by a local server).

(define (mlx-api-key provider)
  (define env-name (provider-api-key-env provider))
  (and env-name
       (getenv env-name)))

(define (post-mlx payload)
  (define provider (current-provider-json))
  ;; chat-loop.rkt already builds a complete OpenAI-shaped body, including
  ;; tools/tool_choice when present, so it is forwarded as-is.
  (define request* payload)
  (define endpoint
    (or (provider-endpoint provider)
        (error 'mlx-serve
               "active provider profile has no \"endpoint\"; set it in the harness config")))
  (define key (mlx-api-key provider))
  (define headers
    (if (and key (not (string=? key "")))
        (hash 'content-type "application/json"
              'authorization (string-append "Bearer " key))
        (hash 'content-type "application/json")))
  (when (DEBUG-LOG)
    (displayln (format "[DEBUG] mlx request (~a): ~a"
                       endpoint
                       (jsexpr->string request*))))
  (define data
    (with-handlers ([exn:fail? (lambda (e) (error 'mlx-serve "HTTP error: ~a" (exn-message e)))])
      (define resp
        (post endpoint
              #:headers headers
              #:json request*
              #:timeouts (make-timeout-config #:request MLX-MAX-TIME
                                              #:connect MLX-CONNECT-TIME)))
      (define j (response-json resp))
      (when (DEBUG-LOG)
        (displayln (format "[DEBUG] mlx response: ~a" (jsexpr->string j))))
      j))
  (when (hash-has-key? data 'error)
    (define err (hash-ref data 'error))
    (define msg
      (cond
        [(hash? err) (hash-ref err 'message (format "~a" err))]
        [else (format "~a" err)]))
    (error 'mlx-serve "MLX API error: ~a" msg))
  (unless (hash-has-key? data 'choices)
    (error 'mlx-serve "MLX response has no 'choices'. Raw: ~a" (jsexpr->string data)))
  (mlx-accumulate-usage (hash-ref data 'usage (hash)))
  data)

;; ---------------------------------------------------------------------------
;; mlx-chat / mlx-chat-with-tools -- same signatures as fireworks-ai.rkt
;;
;; Generation defaults resolve through the profile in mlx-active-provider.

;; Generation parameters resolve from the active profile; a missing model is an
;; error, and missing generation parameters are simply left out of the request.

(define (m-gen-param key)
  (generation-ref (provider-generation (current-provider-json)) key #f))

(define (m-model-id)
  (or (provider-model (current-provider-json))
      (error 'mlx-serve
             "active provider profile has no \"model\"; set it in the harness config")))

(define (mlx-chat messages
                  #:model-id [model-id (m-model-id)]
                  #:max-tokens [max-tokens (m-gen-param 'max_tokens)]
                  #:temperature [temperature (m-gen-param 'temperature)])
  (chat* post-mlx messages
         #:model-id model-id
         #:max-tokens max-tokens
         #:temperature temperature))

(define (mlx-chat-with-tools messages tools
                             #:model-id [model-id (m-model-id)]
                             #:max-tokens [max-tokens (m-gen-param 'max_tokens)]
                             #:temperature [temperature (m-gen-param 'temperature)]
                             #:max-iterations [max-iterations 20])
  (chat-with-tools* post-mlx messages tools
                    #:model-id model-id
                    #:max-tokens max-tokens
                    #:temperature temperature
                    #:max-iterations max-iterations))
```

### No Conversion Functions Needed

Because `mlx_lm.server` speaks the OpenAI-compatible protocol end to end, `mlx-serve.rkt` needs no boundary conversion functions. Outgoing messages and incoming responses already carry tool-call `arguments` as JSON strings and token counts as `prompt_tokens` / `completion_tokens`, exactly the shape the shared loop expects.

The module keeps one parameter, `mlx-active-provider`, which holds the provider profile that requests should read. `agent.rkt` binds it with `parameterize` around each call, so the endpoint, the model, and the generation settings all resolve from the active profile instead of from module-level constants. A profile with no `endpoint` or no `model` is an error rather than a silent fallback to a hard-coded default. An optional Bearer key from the profile's `api_key_env` is sent when one is set, which is what a remote Ollama-style endpoint needs; a purely local server ignores it.

Because local inference is free, the cost display is always `$0`. The stats are informational only.

## Hierarchical Provider Configuration

Earlier versions of this harness selected a provider with a single environment variable. This version moves every provider-specific value into configuration, in the rough style of the Pi coding harness. Two JSON layers are merged at startup, and the project-local file wins over the global one:

* **Global:** `~/.coding_harness.json`, the base configuration.
* **Local:** `.local_coding_harness.json` in the current directory, an optional per-project override.

A minimal config declares one or more named providers and a default:

```json
{
  "default_provider": "mlx",
  "providers": {
    "mlx": {
      "type": "mlx",
      "endpoint": "http://localhost:11434/v1/chat/completions",
      "model": "mlx-community/gemma-4-26B-A4B-it-OptiQ-4bit",
      "generation": { "temperature": 0.6, "max_tokens": 32768 }
    },
    "fireworks": {
      "type": "openai",
      "endpoint": "https://api.fireworks.ai/inference/v1/chat/completions",
      "api_key_env": "FIREWORKS_API_KEY",
      "model": "accounts/fireworks/models/deepseek-v4p1-flash",
      "generation": { "temperature": 0.6, "max_tokens": 32768 },
      "pricing": { "input": 0.14, "cached_input": 0.028, "output": 0.28 }
    }
  }
}
```

The `type` field selects the wire format: `"mlx"` for the local OpenAI-compatible server (the strings `"ollama"`, `"omlx"`, and `"sushi"` are accepted as aliases and mapped to the same backend) and `"openai"` for Fireworks and any compatible endpoint. The optional `pricing` block supplies the per-million-token rates that `/tokens` uses. Nothing in the Racket code names a provider, an endpoint, or a price.

Here is the complete module:

```racket
#lang racket

(require json
         racket/file
         racket/string)

(provide load-harness-config
         harness-config
         config-provider-names
         config-provider
         config-active-provider-name
         config-set-active-provider!
         config-active-provider
         provider-type
         provider-endpoint
         provider-model
         provider-api-key-env
         provider-generation
         generation-ref
         provider-pricing
         pricing-ref
         print-config-summary)

;; ---------------------------------------------------------------------------
;; JSON loading helpers

(define (read-json-file path)
  ;; -> jsexpr hash, or #f if the file is missing / unreadable / not an object
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (file-exists? path)
         (let ([v (with-input-from-file path read-json)])
           (and (hash? v) v)))))

(define (global-config-path)
  (build-path (find-system-path 'home-dir) ".coding_harness.json"))

(define (local-config-path)
  (build-path (current-directory) ".local_coding_harness.json"))

;; ---------------------------------------------------------------------------
;; Recursive hash merge (local overrides global)

(define (deep-merge global local)
  (cond
    [(not (hash? global)) local]
    [(not (hash? local)) local]
    [else
     (for/fold ([acc global])
               ([(k v) (in-hash local)])
       (hash-set acc k
                 (if (and (hash? (hash-ref acc k #f)) (hash? v))
                     (deep-merge (hash-ref acc k) v)
                     v)))]))

;; ---------------------------------------------------------------------------
;; The merged config, loaded once at startup (reloadable via load-harness-config)

(define harness-config (make-parameter (hash)))

(define (load-harness-config)
  ;; Load global then local, deep-merge, store in the parameter, and return it.
  (define global (or (read-json-file (global-config-path)) (hash)))
  (define local  (or (read-json-file (local-config-path)) (hash)))
  (define merged (deep-merge global local))
  (harness-config merged)
  merged)

;; ---------------------------------------------------------------------------
;; Providers

(define (config-providers)
  (define p (hash-ref (harness-config) 'providers (hash)))
  (if (hash? p) p (hash)))

(define (config-provider-names)
  (sort (map symbol->string (hash-keys (config-providers))) string<?))

(define (name->key name)
  ;; provider sections are keyed by the profile name; JSON object keys come
  ;; back as symbols, so accept either a string or symbol name.
  (cond
    [(symbol? name) name]
    [(string? name) (string->symbol name)]
    [else (string->symbol (format "~a" name))]))

(define (config-provider name)
  ;; -> provider hash for profile `name`, or #f
  (hash-ref (config-providers) (name->key name) #f))

;; Active provider profile ---------------------------------------------------

;; Mutable cell: the name of the provider section currently in use. Defaults
;; to "default_provider" from config, else "fireworks" if that section exists,
;; else the first declared provider, else #f (use the compiled-in defaults of
;; fireworks-ai.rkt / mlx-serve.rkt).
(define active-provider-name (box #f))

(define (pick-default-provider-name cfg)
  (define declared (hash-ref cfg 'default_provider #f))
  (define names (map symbol->string (hash-keys (config-providers))))
  (cond
    [(and (string? declared)
          (hash-has-key? (config-providers) (string->symbol declared)))
     declared]
    [(member "fireworks" names) "fireworks"]
    [(pair? names) (first (sort names string<?))]
    [else #f]))

(define (config-active-provider-name)
  (or (unbox active-provider-name)
      (let ([n (pick-default-provider-name (harness-config))])
        (set-box! active-provider-name n)
        n)))

(define (config-set-active-provider! name)
  (when (and name (config-provider name))
    (set-box! active-provider-name
              (if (string? name) name (symbol->string name))))
  (unbox active-provider-name))

(define (config-active-provider)
  ;; -> provider hash of the active profile, or #f when there is no config
  (define n (config-active-provider-name))
  (and n (config-provider n)))

;; ---------------------------------------------------------------------------
;; Provider field accessors (all tolerant of missing keys)

(define (provider-type provider)
  ;; -> 'mlx | 'openai -- defaults to 'openai
  ;; "mlx" selects the local mlx-serve backend (formerly "ollama"); "ollama",
  ;; "omlx", and "sushi" are also accepted and mapped to 'mlx for compatibility.
  (define t (and provider (hash-ref provider 'type #f)))
  (define low
    (and (or (string? t) (symbol? t))
         (string-downcase (format "~a" t))))
  (cond
    [(member low '("mlx" "ollama" "omlx" "sushi")) 'mlx]
    [else 'openai]))

(define (provider-endpoint provider)
  (define e (and provider (hash-ref provider 'endpoint #f)))
  (and (string? e) (not (string=? e "")) e))

(define (provider-model provider)
  (define m (and provider (hash-ref provider 'model #f)))
  (and (string? m) (not (string=? m "")) m))

(define (provider-api-key-env provider)
  ;; Name of the env var holding the Bearer key for this endpoint, or #f.
  ;; Absent/empty/null means "no key" (plain local MLX).
  (define k (and provider (hash-ref provider 'api_key_env #f)))
  (and (string? k) (not (string=? k "")) k))

(define (provider-generation provider)
  (define g (and provider (hash-ref provider 'generation #f)))
  (if (hash? g) g (hash)))

(define (provider-pricing provider)
  ;; -> hash of per-1M-token USD rates ('input, 'cached_input, 'output), or an
  ;; empty hash when the profile declares none.  Rates live in the provider
  ;; profile so that none are compiled into the code.
  (define g (and provider (hash-ref provider 'pricing #f)))
  (if (hash? g) g (hash)))

(define (pricing-ref pricing key)
  ;; -> number, or #f when the profile does not declare that rate.  The #f
  ;; result means "unknown", which callers report rather than guessing a value.
  (cond
    [(not (hash? pricing)) #f]
    [(hash-has-key? pricing key) (hash-ref pricing key)]
    [(hash-has-key? pricing (string->symbol (format "~a" key)))
     (hash-ref pricing (string->symbol (format "~a" key)))]
    [(hash-has-key? pricing (format "~a" key)) (hash-ref pricing (format "~a" key))]
    [else #f]))

(define (generation-ref generation key default)
  ;; Fetch a generation parameter ("temperature", "max_tokens", "think", ...)
  ;; accepting symbol or string keys because JSON may give either.
  (cond
    [(not (hash? generation)) default]
    [(hash-has-key? generation key) (hash-ref generation key)]
    [(hash-has-key? generation (string->symbol (format "~a" key)))
     (hash-ref generation (string->symbol (format "~a" key)))]
    [(hash-has-key? generation (format "~a" key))
     (hash-ref generation (format "~a" key))]
    [else default]))

;; ---------------------------------------------------------------------------
;; Debug helper

(define (print-config-summary)
  (define cfg (harness-config))
  (displayln (format "Config files: ~a ~a / ~a ~a"
                     (global-config-path)
                     (if (file-exists? (global-config-path)) "(loaded)" "(absent)")
                     (local-config-path)
                     (if (file-exists? (local-config-path)) "(loaded)" "(absent)")))
  (displayln (format "Providers:    ~a"
                     (string-join (config-provider-names) ", ")))
  (displayln (format "Active:       ~a" (or (config-active-provider-name) "(defaults)"))))
```

### Merging the Two Layers

`load-harness-config` reads both files, deep-merges them, and stores the result in the `harness-config` parameter. `deep-merge` recurses into nested hashes so a local file can override a single field, such as a model id, without restating the whole provider; anything that is not a hash, such as a string, a number, or a list, is replaced wholesale by the local value. A missing or malformed file is treated as an empty hash, so the harness starts with whatever configuration is actually present.

The active provider is a name kept in a box. On first use, `config-active-provider-name` picks `default_provider` if it names a real profile, otherwise `fireworks` if that profile exists, otherwise the alphabetically first profile. `/provider`, `--provider`, and the environment can all change it at run time through `config-set-active-provider!`.

The accessors near the bottom of the file, `provider-endpoint`, `provider-model`, `provider-api-key-env`, `provider-generation`, and `provider-pricing`, are deliberately tolerant: each returns `#f` or an empty hash when the field is absent, and the caller decides whether that is fatal. The two clients treat a missing endpoint or model as an error, so a half-written profile produces a clear message instead of a request sent to `#f`. `pricing-ref` and `generation-ref` accept either a symbol or a string key, because `read-json` returns JSON object keys as symbols while a hand-written config might use strings.

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
         json
         "approval.rkt")

(provide define-tool
         render-tools
         execute-tool-calls
         register-all
         ENABLED-TOOLS
         auto-approve?
         dry-run?
         quiet-mode?)

;; ---------------------------------------------------------------------------
;; Registry

(define registry (make-hash))

(define SHELL-WHITELIST (set "make" "ls" "pwd" "cat" "uv"))
(define MAX-CHECK-OUTPUT-CHARS 2000)

;; CLI-controlled modes
(define auto-approve? (make-parameter #f))
(define dry-run? (make-parameter #f))
(define quiet-mode? (make-parameter #f))

(define (define-tool name params description handler)
  ;; params : list of (list pname ptype pdesc)
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
      ;; required must be a JSON array of strings, not symbols
      (set! required (cons pname required)))
    (hash 'type "function"
          'function (hash 'name (hash-ref tool 'name)
                          'description (hash-ref tool 'description)
                          'parameters (hash 'type "object"
                                            'properties props
                                            'required (reverse required))))))

;; ---------------------------------------------------------------------------
;; Tool dispatch

(define (call-tool name args)
  (define tool (hash-ref registry name #f))
  (unless tool (error 'call-tool "Unknown tool: ~a" name))
  (define params (hash-ref tool 'parameters))
  ;; Missing required args? Return an actionable error describing the expected
  ;; argument list -- small models frequently emit malformed/truncated
  ;; arguments, and silently receiving #f tends to send them into retry loops.
  (define missing
    (for/list ([p (in-list params)]
               #:when (not (hash-ref args (string->symbol (first p)) #f)))
      (first p)))
  (cond
    [(pair? missing)
     (format "Error: tool '~a' missing required argument(s): ~a. Expected arguments (JSON object): ~a"
             name
             (string-join missing ", ")
             (string-join (for/list ([p (in-list params)]) (first p)) ", "))]
    [else
     (define positional
       (for/list ([p (in-list params)])
         (hash-ref args (string->symbol (first p)) #f)))
     (with-handlers ([exn:fail? (lambda (e) (format "Error: tool '~a' raised: ~a  (check argument types/values)"
                                                    name (exn-message e)))])
       (define result (apply (hash-ref tool 'handler) positional))
       (if result (format "~a" result) ""))]))

(define (execute-tool-calls tool-calls)
  ;; tool-calls : list of hashes with 'id, 'function {name, arguments}
  ;; Returns list of (list call-id name result-str)
  (define results '())
  (for ([call (in-list tool-calls)])
    (define call-id (hash-ref call 'id ""))
    (define func (hash-ref call 'function (hash)))
    (define name (hash-ref func 'name ""))
    (define args-json (hash-ref func 'arguments "{}"))
    (define short
      (if (<= (string-length args-json) 120)
          args-json
          (string-append (substring args-json 0 117) "...")))
    (unless (quiet-mode?)
      (displayln (format "* ~a ~a" name short)))
    (define args-parsed
      (with-handlers ([exn:fail? (lambda (_) 'BAD-JSON)])
        (let ([j (string->jsexpr args-json)])
          (if (hash? j) j 'NOT-OBJECT))))
    (define result
      (cond
        ;; Truncated tool call -- the model stopped mid-generation, so no
        ;; function name survived. Feed that back instead of crashing.
        [(string=? (string-trim name) "")
         (format "Error: the model's tool call was truncated mid-generation (no function name provided). Received arguments: ~a"
                 short)]
        [(eq? args-parsed 'BAD-JSON)
         (format "Error: invalid JSON in arguments for tool '~a'. Received: ~a"
                 name short)]
        [(eq? args-parsed 'NOT-OBJECT)
         (format "Error: arguments for tool '~a' must be a JSON object. Received: ~a"
                 name short)]
        [else
         ;; Unknown tool names, contract violations, etc. become feedback to the
         ;; model rather than an uncaught exception that aborts the loop.
         (with-handlers ([exn:fail? (lambda (e)
                                      (format "Error: tool '~a' raised: ~a"
                                              name (exn-message e)))])
           (call-tool name args-parsed))]))
    (set! results (append results (list (list call-id name result)))))
  results)

;; ---------------------------------------------------------------------------
;; Helpers: run subprocess and capture combined output

(define (run-external exe args)
  ;; exe : string, args : (listof string)  -> (values combined-output exit-code)
  (define exe-path (find-executable-path exe))
  (unless exe-path
    (error 'run-external "Executable not found: ~a" exe))
  (define-values (sp stdout stdin stderr)
    (apply subprocess #f #f #f exe-path args))
  (close-output-port stdin)
  (define out-str (port->string stdout))
  (define err-str (port->string stderr))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait sp)
  (define status (subprocess-status sp))
  (define code (if (number? status) status 1))
  (values (string-append out-str err-str) code))

(define (shell-quote s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))

(define (truncate-string s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 max-len)
                     (format "\n... (truncated, ~a total chars)" (string-length s)))
      s))

;; Hidden files (ignored from listings and reject read attempts):
;;   - names ending in ~  (e.g. foo.rkt~)
;;   - names wrapped in #...#  (e.g. #foo.rkt#)
;;   - names starting with .  (e.g. .git, .gitignore, .env)
(define (hidden-file? name)
  (or (string-suffix? name "~")
      (and (string-prefix? name "#")
           (string-suffix? name "#"))
      (string-prefix? name ".")))

;; ---------------------------------------------------------------------------
;; Tool implementations

(define (tool-read-file path)
  (with-handlers ([exn:fail? (lambda (e) (format "Error reading ~a: ~a" path (exn-message e)))])
    (define fname (path->string (file-name-from-path path)))
    (if (hidden-file? fname)
        (format "refusing to read hidden/internal file: ~a" path)
        (file->string path))))

(define (tool-list-dir path)
  (with-handlers ([exn:fail? (lambda (e) (format "Error listing ~a: ~a" path (exn-message e)))])
    (define entries (directory-list path))
    (define lines
      (for/list ([e (in-list (sort (map path->string entries) string<?))]
                 #:unless (hidden-file? (path->string e)))
        (define full (build-path path e))
        (if (directory-exists? full)
            (string-append e "/")
            e)))
    (string-join lines "\n")))

(define (tool-grep pattern path)
  (with-handlers ([exn:fail? (lambda (e) (format "Error running grep: ~a" (exn-message e)))])
    (define-values (out code) (run-external "grep" (list "-rnE" pattern path)))
    out))

(define (strip-shell-quotes s)
  (if (and (>= (string-length s) 2)
           (let ([first (string-ref s 0)]
                 [last  (string-ref s (sub1 (string-length s)))])
             (or (and (char=? first #\") (char=? last #\"))
                 (and (char=? first #\') (char=? last #\')))))
      (substring s 1 (sub1 (string-length s)))
      s))

(define (hidden-arg? s)
  (define cleaned (strip-shell-quotes s))
  (and (not (string-prefix? cleaned "-"))
       (hidden-file? (path->string (file-name-from-path cleaned)))))

(define (filter-ls-output out)
  (define lines (string-split out "\n"))
  (define filtered
    (for/list ([line (in-list lines)]
               #:unless (let ([t (string-trim line)])
                          (or (string-prefix? t "total ")
                              (equal? t ""))))
      (define trimmed (string-trim line))
      (define tokens (string-split trimmed))
      (cond
        [(null? tokens) #f]
        [(regexp-match? #rx"^[-d]" (first tokens))
         (define fname (last tokens))
         (if (or (hidden-file? fname) (member fname '("." "..")))
             #f
             line)]
        [else
         (if (hidden-file? trimmed) #f line)])))
  (define result-lines (for/list ([f (in-list filtered)] #:when f) f))
  (if (null? result-lines) "" (string-join result-lines "\n")))

(define (tool-run-shell command)
  (define tokens (string-split (string-trim command)))
  (cond
    [(null? tokens) "empty command"]
    [else
     (define cmd (first tokens))
     (cond
       [(not (set-member? SHELL-WHITELIST cmd))
        (format "Command '~a' not whitelisted. Allowed: ~a"
                cmd (string-join (sort (set->list SHELL-WHITELIST) string<?) ", "))]
       [(and (not (equal? cmd "ls"))
             (ormap hidden-arg? (rest tokens)))
        => (lambda (bad)
             (format "refusing to run command referencing hidden/internal file: ~a" bad))]
       [else
        (with-handlers ([exn:fail? (lambda (e) (format "Error running command: ~a" (exn-message e)))])
          (define args (rest tokens))
          (define-values (out code) (run-external cmd args))
          (define filtered-out (if (equal? cmd "ls") (filter-ls-output out) out))
          (string-append filtered-out (format "(exit ~a)" code)))])]))

(define (run-make-check)
  (with-handlers ([exn:fail? (lambda (e) (values (format "make check error: ~a" (exn-message e)) 1))])
    (run-external "make" (list "check"))))

(define (tool-propose-edit path old new)
  (define exists? (file-exists? path))
  (define current
    (if exists?
        (with-handlers ([exn:fail? (lambda (e) (format "Error reading ~a: ~a" path (exn-message e)))])
          (file->string path))
        ""))
  ;; If read failed and returned error string, treat as error
  (when (and exists? (string-prefix? current "Error reading"))
    current)
  (cond
    [(and exists? (not (string=? current old)))
     (format "stale base: on-disk contents of ~a do not match the 'old' you provided. Read the file again and retry." path)]
    [(and exists? (string=? current new))
     "no changes (proposed content matches current file)"]
    [(and (not exists?) (string=? new ""))
     "refused: cannot create an empty file"]
    [else
     (define diff-text (unified-diff current new (string-append "a/" path) (string-append "b/" path)))
     (displayln "")
     (unless exists? (displayln (format "(new file: ~a)" path)))
     (print-colored-diff diff-text)
     (cond
       [(dry-run?)
        "dry-run: diff shown, file not written (use without --dry-run to apply)"]
       [(auto-approve?)
        ;; Safety: still show diff above, then auto-apply without prompting
        (unless (quiet-mode?)
          (displayln "[auto-approve: applying change without prompt]"))
        (make-parent-directory* path)
        (call-with-output-file path #:exists 'truncate
          (lambda (out) (display new out)))
        (define-values (out status) (run-make-check))
        (if (= status 0)
            "applied (auto-approved); make check passed"
            (format "applied (auto-approved); make check FAILED (exit ~a):\n~a"
                    status (truncate-string out MAX-CHECK-OUTPUT-CHARS)))]
        [else
         (define answer (prompt-yes-no-skip))
         (cond
           [(eq? answer 'no) "user rejected the change"]
           [(eq? answer 'skip)
           (define reason (prompt-reason))
           (format "user skipped: ~a" reason)]
          [else ; 'yes
           (make-parent-directory* path)
           (call-with-output-file path #:exists 'truncate
             (lambda (out) (display new out)))
           (define-values (out status) (run-make-check))
           (if (= status 0)
               "applied; make check passed"
               (format "applied; make check FAILED (exit ~a):\n~a"
                       status (truncate-string out MAX-CHECK-OUTPUT-CHARS)))])])]))

;; ---------------------------------------------------------------------------
;; Registration

(define (register-all)
  (define-tool
    "read_file"
    (list (list "path" "string" "File path relative to the working directory."))
    "Read and return the contents of a file. Refuses to read hidden/internal files (~, #...#, and dotfiles)."
    tool-read-file)
  (define-tool
    "list_dir"
    (list (list "path" "string" "Directory path. Use \".\" for the working directory."))
    "List files and subdirectories (with trailing /) in a directory. Hidden/internal files (~, #...#, and dotfiles) are excluded."
    tool-list-dir)
  (define-tool
    "grep"
    (list (list "pattern" "string" "Extended regex pattern to search for.")
          (list "path" "string" "Directory or file path to search."))
    "Recursively grep files for PATTERN. Wraps `grep -rnE`."
    tool-grep)
  (define-tool
    "run_shell"
    (list (list "command" "string" "Shell command. Only whitelisted commands may run: make, ls, pwd, cat, uv."))
    "Run a whitelisted shell command and return its combined output. Refuses commands that reference hidden/internal files."
    tool-run-shell)
  (define-tool
    "propose_edit"
    (list (list "path" "string" "Path to the file to edit or create.")
          (list "old" "string" "For an existing file: the exact current contents. For a new file: pass empty string.")
          (list "new" "string" "The proposed new contents of the file, in full."))
    "Propose an edit or new-file creation. The user is shown a unified diff and asked to approve. On approval the file is written and `make check` is run."
    tool-propose-edit))

(define ENABLED-TOOLS (list "read_file" "list_dir" "grep" "run_shell" "propose_edit"))
```

Each tool is stored as a hash with its name, description, parameter list, and handler function. `render-tools` converts the registry entries into OpenAI function-calling schema format: a list of hashes the API understands as callable functions. The model receives these alongside the conversation and decides which, if any, to invoke.

### Tool Dispatch

`execute-tool-calls` receives the list of tool call objects from the API response and dispatches each one:

```racket
(define (execute-tool-calls tool-calls)
  ;; tool-calls : list of hashes with 'id, 'function {name, arguments}
  ;; Returns list of (list call-id name result-str)
  (define results '())
  (for ([call (in-list tool-calls)])
    (define call-id (hash-ref call 'id ""))
    (define func (hash-ref call 'function (hash)))
    (define name (hash-ref func 'name ""))
    (define args-json (hash-ref func 'arguments "{}"))
    (define short
      (if (<= (string-length args-json) 120)
          args-json
          (string-append (substring args-json 0 117) "...")))
    (unless (quiet-mode?)
      (displayln (format "* ~a ~a" name short)))
    (define args-parsed
      (with-handlers ([exn:fail? (lambda (_) 'BAD-JSON)])
        (let ([j (string->jsexpr args-json)])
          (if (hash? j) j 'NOT-OBJECT))))
    (define result
      (cond
        ;; Truncated tool call -- the model stopped mid-generation, so no
        ;; function name survived. Feed that back instead of crashing.
        [(string=? (string-trim name) "")
         (format "Error: the model's tool call was truncated mid-generation (no function name provided). Received arguments: ~a"
                 short)]
        [(eq? args-parsed 'BAD-JSON)
         (format "Error: invalid JSON in arguments for tool '~a'. Received: ~a"
                 name short)]
        [(eq? args-parsed 'NOT-OBJECT)
         (format "Error: arguments for tool '~a' must be a JSON object. Received: ~a"
                 name short)]
        [else
         ;; Unknown tool names, contract violations, etc. become feedback to the
         ;; model rather than an uncaught exception that aborts the loop.
         (with-handlers ([exn:fail? (lambda (e)
                                      (format "Error: tool '~a' raised: ~a"
                                              name (exn-message e)))])
           (call-tool name args-parsed))]))
    (set! results (append results (list (list call-id name result)))))
  results)
```

Each call prints its name and a truncated copy of its arguments unless quiet mode is on, so the user can watch what the model is doing. These error branches matter more than they look. A weak model can truncate a tool call mid-generation, which leaves the function name empty; it can emit arguments that are not valid JSON; or it can emit valid JSON that is not an object. Each of those cases becomes an explanatory string that goes back to the model as the tool result. A raised exception inside a handler is caught the same way. The loop therefore keeps running and the model gets a chance to correct itself, rather than the whole session dying on one malformed call.

### The Five Coding Tools

The agent registers five tools at startup:

| Tool | Purpose |
|---|---|
| `read_file` | Return the full text of a file |
| `list_dir` | List files and subdirectories in a directory |
| `grep` | Recursively search for an extended regex pattern |
| `run_shell` | Run a whitelisted shell command and return its output |
| `propose_edit` | Show a colored diff and ask the user to approve the change |

Do not let the short list suggest that the model can wander the filesystem. `read_file`, `list_dir`, and `run_shell` share a `hidden-file?` predicate that treats editor backups ending in `~`, Emacs lock files wrapped in `#...#`, and dotfiles as off limits. `read_file` refuses them, `list_dir` filters them out, and `run_shell` both refuses any argument that names one and filters `ls` output. `run_shell` also enforces a strict command whitelist so the model cannot run arbitrary shell commands:

```racket
(define (hidden-arg? s)
  (define cleaned (strip-shell-quotes s))
  (and (not (string-prefix? cleaned "-"))
       (hidden-file? (path->string (file-name-from-path cleaned)))))

(define (filter-ls-output out)
  (define lines (string-split out "\n"))
  (define filtered
    (for/list ([line (in-list lines)]
               #:unless (let ([t (string-trim line)])
                          (or (string-prefix? t "total ")
                              (equal? t ""))))
      (define trimmed (string-trim line))
      (define tokens (string-split trimmed))
      (cond
        [(null? tokens) #f]
        [(regexp-match? #rx"^[-d]" (first tokens))
         (define fname (last tokens))
         (if (or (hidden-file? fname) (member fname '("." "..")))
             #f
             line)]
        [else
         (if (hidden-file? trimmed) #f line)])))
  (define result-lines (for/list ([f (in-list filtered)] #:when f) f))
  (if (null? result-lines) "" (string-join result-lines "\n")))

(define (tool-run-shell command)
  (define tokens (string-split (string-trim command)))
  (cond
    [(null? tokens) "empty command"]
    [else
     (define cmd (first tokens))
     (cond
       [(not (set-member? SHELL-WHITELIST cmd))
        (format "Command '~a' not whitelisted. Allowed: ~a"
                cmd (string-join (sort (set->list SHELL-WHITELIST) string<?) ", "))]
       [(and (not (equal? cmd "ls"))
             (ormap hidden-arg? (rest tokens)))
        => (lambda (bad)
             (format "refusing to run command referencing hidden/internal file: ~a" bad))]
       [else
        (with-handlers ([exn:fail? (lambda (e) (format "Error running command: ~a" (exn-message e)))])
          (define args (rest tokens))
          (define-values (out code) (run-external cmd args))
          (define filtered-out (if (equal? cmd "ls") (filter-ls-output out) out))
          (string-append filtered-out (format "(exit ~a)" code)))])]))
```

If the model attempts a disallowed command, or names a file the agent considers internal, it receives an error string describing what is allowed. It can then adapt its approach rather than causing the agent to crash. The whitelist currently allows `make`, `ls`, `pwd`, `cat`, and `uv` (the Python package runner). Everything else is refused.

### The `propose_edit` Approval Gate

`propose_edit` is the most critical tool. Before writing any file it checks for several error conditions, shows the user a diff, waits for approval or applies the change automatically, and then runs `make check`:

```racket
(define (tool-propose-edit path old new)
  (define exists? (file-exists? path))
  (define current
    (if exists?
        (with-handlers ([exn:fail? (lambda (e) (format "Error reading ~a: ~a" path (exn-message e)))])
          (file->string path))
        ""))
  ;; If read failed and returned error string, treat as error
  (when (and exists? (string-prefix? current "Error reading"))
    current)
  (cond
    [(and exists? (not (string=? current old)))
     (format "stale base: on-disk contents of ~a do not match the 'old' you provided. Read the file again and retry." path)]
    [(and exists? (string=? current new))
     "no changes (proposed content matches current file)"]
    [(and (not exists?) (string=? new ""))
     "refused: cannot create an empty file"]
    [else
     (define diff-text (unified-diff current new (string-append "a/" path) (string-append "b/" path)))
     (displayln "")
     (unless exists? (displayln (format "(new file: ~a)" path)))
     (print-colored-diff diff-text)
     (cond
       [(dry-run?)
        "dry-run: diff shown, file not written (use without --dry-run to apply)"]
       [(auto-approve?)
        ;; Safety: still show diff above, then auto-apply without prompting
        (unless (quiet-mode?)
          (displayln "[auto-approve: applying change without prompt]"))
        (make-parent-directory* path)
        (call-with-output-file path #:exists 'truncate
          (lambda (out) (display new out)))
        (define-values (out status) (run-make-check))
        (if (= status 0)
            "applied (auto-approved); make check passed"
            (format "applied (auto-approved); make check FAILED (exit ~a):\n~a"
                    status (truncate-string out MAX-CHECK-OUTPUT-CHARS)))]
        [else
         (define answer (prompt-yes-no-skip))
         (cond
           [(eq? answer 'no) "user rejected the change"]
           [(eq? answer 'skip)
           (define reason (prompt-reason))
           (format "user skipped: ~a" reason)]
          [else ; 'yes
           (make-parent-directory* path)
           (call-with-output-file path #:exists 'truncate
             (lambda (out) (display new out)))
           (define-values (out status) (run-make-check))
           (if (= status 0)
               "applied; make check passed"
               (format "applied; make check FAILED (exit ~a):\n~a"
                       status (truncate-string out MAX-CHECK-OUTPUT-CHARS)))])])]))
```

The stale-base guard is worth understanding carefully. The model reads a file, then constructs a proposed edit based on that content. If the user edits the file externally in between, a naive tool would overwrite those changes silently. By requiring `old` to exactly match what is on disk, the tool forces the model to re-read the file before retrying, and the mismatch is reported as a tool result the model can read and respond to. The neighboring guards reject a no-op edit whose new content already matches the file, and refuse to create an empty file.

The three application paths share the same write and check code. `--dry-run` stops after the diff. `--yes` prints the diff and applies it without prompting. The default path asks the user for `y`, `n`, or `s`, and on `s` it collects a one-line reason so the model learns why the change was skipped.

The `make check` gate closes another important feedback loop. If the edit compiles cleanly, `"applied; make check passed"` goes back into the conversation history and the model can proceed. If `make check` fails, the output goes back as well, giving the model the compiler errors it needs to self-correct on the next turn. The output is truncated to `MAX-CHECK-OUTPUT-CHARS` characters so a huge build log does not blow up the context window.

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
         prompt-reason
         color-enabled?)

;; ---------------------------------------------------------------------------
;; ANSI colours

(define ANSI-RED   "\033[31m")
(define ANSI-GREEN "\033[32m")
(define ANSI-CYAN  "\033[36m")
(define ANSI-RESET "\033[0m")

;; When #f, print diffs without ANSI (for --plain / --no-color / piped output)
(define color-enabled? (make-parameter #t))

;; ---------------------------------------------------------------------------
;; Shell quoting helper (single-quote, escape embedded single quotes)

(define (shell-quote s)
  (string-append "'"
                 (string-replace s "'" "'\\''")
                 "'"))

;; ---------------------------------------------------------------------------
;; unified-diff : string string string string -> string
;; Runs `diff -u` on two temporary files and returns stdout.

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

;; ---------------------------------------------------------------------------
;; print-colored-diff : string -> void

(define (print-colored-diff diff-text)
  (for ([line (in-list (string-split diff-text "\n"))])
    (cond
      [(color-enabled?)
       (cond
         [(or (string-prefix? line "+++")
              (string-prefix? line "---")
              (string-prefix? line "@@"))
          (displayln (string-append ANSI-CYAN line ANSI-RESET))]
         [(string-prefix? line "+")
          (displayln (string-append ANSI-GREEN line ANSI-RESET))]
         [(string-prefix? line "-")
          (displayln (string-append ANSI-RED line ANSI-RESET))]
         [else (displayln line)])]
      [else (displayln line)])))

;; ---------------------------------------------------------------------------
;; prompt-yes-no-skip : -> (or 'yes 'no 'skip)

(define (prompt-yes-no-skip)
  (let loop ()
    (display "\nApply this change? [y]es / [n]o / [s]kip and tell the model why: ")
    (flush-output)
    (define line (read-line (current-input-port)))
    (define norm (if (eof-object? line) "" (string-downcase (string-trim line))))
    (cond
      [(member norm '("y" "yes")) 'yes]
      [(member norm '("n" "no")) 'no]
      [(member norm '("s" "skip")) 'skip]
      [else
       (displayln "Please answer y, n, or s.")
       (loop)])))

;; ---------------------------------------------------------------------------
;; prompt-reason : -> string

(define (prompt-reason)
  (display "Reason (one line): ")
  (flush-output)
  (define line (read-line (current-input-port)))
  (if (eof-object? line) "" line))
```

`dynamic-wind` takes three thunks: a before-thunk (here `void`), a body-thunk, and an after-thunk. The after-thunk runs whether the body completes normally or raises an exception, analogous to Python's `try/finally`. This guarantees the three temporary files are cleaned up regardless of what goes wrong.

### Colorizing the Diff

`print-colored-diff` walks each line of the unified diff output and applies ANSI terminal color codes:

```racket
(define (print-colored-diff diff-text)
  (for ([line (in-list (string-split diff-text "\n"))])
    (cond
      [(color-enabled?)
       (cond
         [(or (string-prefix? line "+++")
              (string-prefix? line "---")
              (string-prefix? line "@@"))
          (displayln (string-append ANSI-CYAN line ANSI-RESET))]
         [(string-prefix? line "+")
          (displayln (string-append ANSI-GREEN line ANSI-RESET))]
         [(string-prefix? line "-")
          (displayln (string-append ANSI-RED line ANSI-RESET))]
         [else (displayln line)])]
      [else (displayln line)])))
```

Lines beginning with `+` are added lines and appear green; lines beginning with `-` are removed and appear red; diff headers (`+++`, `---`, `@@`) appear cyan. This makes it straightforward to review a proposed change without reading both full file versions. The `color-enabled?` parameter turns the codes off for `--plain`, `--no-color`, and piped output, where escape sequences would only corrupt a log.

### The Approval Prompt

`prompt-yes-no-skip` reads a cooked line from stdin and re-prompts until it sees one of the three answers:

```racket
;; prompt-yes-no-skip : -> (or 'yes 'no 'skip)

(define (prompt-yes-no-skip)
  (let loop ()
    (display "\nApply this change? [y]es / [n]o / [s]kip and tell the model why: ")
    (flush-output)
    (define line (read-line (current-input-port)))
    (define norm (if (eof-object? line) "" (string-downcase (string-trim line))))
    (cond
      [(member norm '("y" "yes")) 'yes]
      [(member norm '("n" "no")) 'no]
      [(member norm '("s" "skip")) 'skip]
      [else
       (displayln "Please answer y, n, or s.")
       (loop)])))

;; ---------------------------------------------------------------------------
;; prompt-reason : -> string

(define (prompt-reason)
  (display "Reason (one line): ")
  (flush-output)
  (define line (read-line (current-input-port)))
  (if (eof-object? line) "" line))
```

The prompt uses plain `read-line` rather than the readline-based reader that drives the REPL. Approval happens in the middle of a tool call, where a short `y`/`n`/`s` answer is easier to reason about than an edited line with history, and `read-line` behaves correctly when stdin is a pipe, which is what makes `--stdin` and one-shot scripting work. `prompt-reason` collects the explanation for a skipped edit.

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

;; ---------------------------------------------------------------------------
;; Brave Search
;; Returns (listof (list url title description))

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
  (define resp
    (get url #:headers headers))
  (define data (response-json resp))
  (define web (hash-ref data 'web (hash)))
  (define results (hash-ref web 'results '()))
  (for/list ([r (in-list results)])
    (list (hash-ref r 'url "")
          (hash-ref r 'title "")
          (hash-ref r 'description ""))))

;; ---------------------------------------------------------------------------
;; Exa AI Search
;; Returns (listof (list url title highlight))

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

### Provider Dispatch

`agent.rkt` is the entry point that ties everything together. It no longer holds a provider parameter of its own. The active provider is a profile in the harness config, and every model call goes through two dispatch functions that read it:

```racket
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
```

`require-provider` is the single place that decides whether the harness can run at all. If no config file declares any providers, it raises an error naming both config paths instead of silently falling back to a compiled-in default. `active-provider-type` reduces the profile's `type` field to `'mlx` or `'openai`, and `using-mlx?` uses that answer for `/tokens` and the banner.

Both dispatch functions resolve the model id and the generation parameters the same way: an explicit keyword argument wins, otherwise the value comes from the profile's `generation` block, otherwise the parameter is left out of the request entirely. For MLX the profile is bound into `mlx-active-provider` with `parameterize` around the call, which is how a module-level client learns which endpoint and model to use without reaching for a global variable.

A session-level model override sits in front of the profile's model. `/model <id>` and `--model <id>` set it, and `switch-provider!` clears it so that changing providers always adopts the new profile's own model. The `/provider` command with no argument prints the active profile and lists every profile in the config; with an argument it switches to that profile.

### Intent Classification

Before sending any message to the model, `agent.rkt` classifies the user's intent as one of three categories: `"general"`, `"coding"`, or `"hybrid"`. The classification uses a two-stage approach.

Stage one is a keyword heuristic, free and instant:

```racket
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
```

```racket
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

`for/or` is the Racket comprehension form that returns the first "truthy" value or `#f` if none is found. The general list is checked first, so a query that mentions both a film title and a file extension is routed to the general path. The ordering of the two lists is the tie-breaker.

If the heuristic returns `#f` (the query is ambiguous), stage two calls the model with a minimal two-message conversation and requests a single-word answer:

```racket
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
```

`max-tokens 10` and `temperature 0.0` keep the classifier call cheap and deterministic. Both are passed explicitly, and because the dispatcher prefers an explicit argument over the profile's value, the classifier's tiny budget is not overridden by a profile that declares a large `max_tokens`. If the classifier itself fails, the handler defaults to `"coding"`, a conservative choice that enables the full tool set.

### Routing to the Model

`send-to-model` uses the classification to choose the right system prompt and call path:

```racket
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
```

General questions use a lightweight one-shot call and a simple system prompt, and they always run a web search. Coding requests go through the full agentic tool loop using a system prompt that describes the five tools and the rules for using them. Hybrid requests get the tool loop plus web search results prepended to the message, but only when `/search` is on. The `[intent: ...]` line is suppressed in quiet mode so scripted runs stay clean.

### The System Prompt

The coding system prompt is set once per session and injected as the first message with `role "system"`. It tells the model which tools are available and how to use them correctly:

```racket
(define SYSTEM-PROMPT-TEMPLATE
  "You are an interactive coding assistant working in the directory {cwd}.\n\nRules:\n- Use read_file, list_dir, and grep to understand the code BEFORE proposing edits.\n- To EDIT an existing file: read_file it first, then pass its exact current contents\n  as `old` to propose_edit.\n- To CREATE a new file: call propose_edit with the empty string \"\" as `old` and\n  the full desired contents as `new`. Do not call read_file first for a file that\n  does not exist yet.\n- One file per propose_edit call. Keep diffs small and focused.\n- If the user rejects an edit or `make check` fails, ask for clarification instead\n  of retrying blindly.\n- run_shell only accepts whitelisted commands: make, ls, pwd, cat, uv.\n- When you are done, reply with a short natural-language summary of what changed.")

(define GENERAL-SYSTEM-PROMPT
  "You are a helpful assistant. Answer the user's question clearly and concisely using the web search results provided. Do not reference files, directories, or code editing tools unless the user explicitly asks about code.")

(define COMPACT-SYSTEM-PROMPT
  "You are a context compactor for a coding assistant. Summarize the conversation transcript into a compact brief that will replace it. Preserve: the user's goals and instructions, decisions made, files created or modified (with paths), important code and tool-output details, and outstanding tasks. Write dense bullets, no preamble.")
```

The `{cwd}` placeholder is replaced with the actual working directory at session start. Telling the model the working directory helps it construct relative paths for `read_file` and `list_dir` calls. The prompt also states the rule that matters most in practice: read a file before editing it, and pass its exact current contents as `old`.

### Context Management

As an agentic conversation grows, every tool result is appended to the message list, and the context window fills up. `agent.rkt` provides two commands to manage this. `/context` shows a formatted table of messages with estimated character and token counts, plus a short preview of each message:

```racket
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
```

Each preview is collapsed to a single line and wrapped to at most three 60-character lines, with an ellipsis marking text that still does not fit, so one long tool result cannot flood the table. The token estimate divides the character count by four, which is a rough but useful approximation for English and for code.

`/compact` sends the whole transcript to the model with the compactor system prompt shown above, gets back a dense summary, and replaces everything except the original system prompt with that summary. The final context table is printed again so you can see the size drop. This trades a little fidelity for a lot of context budget, keeping the model inside its window on long sessions.

### Skills

The agent supports loading "skills" from `~/.agents/skills/<name>/SKILL.md`. Each skill file is a Markdown document that is injected into the conversation as a system message, telling the model to treat it as authoritative guidance. `/skills` lists the available skills, parsing a `description:` field from each file's YAML frontmatter, and `/<skill-name>` loads one. This lets you package reusable instructions that the model will follow for the rest of the session.

### Line Editing, History, and Completion

Interactive input is the job of `line-input.rkt`. When stdin is a terminal and Racket's bundled `readline` collection can be loaded, the REPL gets cursor editing, in-session history with the arrow keys and `Ctrl-R`, and Tab completion, all in process and with no external `rlwrap` wrapper. Two things can go wrong, and both degrade to a plain `read-line` instead of failing the harness: the native library may be missing, and stdin may not be a terminal at all (a pipe, a heredoc, or `--stdin`).

The guarded load is the first piece:

```racket
(define (load-backend!)
  ;; Instantiate readline/readline once, tolerating a missing module or a
  ;; missing native library.  All lookups happen before any assignment so a
  ;; failure can never leave the module half-initialised.
  (unless backend-tried?
    (set! backend-tried? #t)
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (define (get name) (dynamic-require 'readline/readline name))
      (define rl (get 'readline))
      (define add-history (get 'add-history))
      (define history-length (get 'history-length))
      (define history-get (get 'history-get))
      (define set-completion (get 'set-completion-function!))
      (define set-completion-append-char (get 'set-completion-append-character!))
      (set! readline-proc rl)
      (set! add-history-proc add-history)
      (set! history-length-proc history-length)
      (set! history-get-proc history-get)
      (set! set-completion-proc set-completion)
      (set! set-completion-append-char-proc set-completion-append-char))))

(define (terminal-stdin?)
  ;; readline captures the input port when its module is instantiated, so only
  ;; engage it when that port really is the terminal's stdin.
  (and (terminal-port? (current-input-port))
       (eq? 'stdin (object-name (current-input-port)))))

(define (line-input-available?)
  ;; -> boolean.  Loads the backend on first use, and only when stdin is a
  ;; terminal, so piped/--stdin runs never touch libedit at all.
  (and (terminal-stdin?)
       (begin (load-backend!) (and readline-proc #t))))
```

`dynamic-require` is called inside `with-handlers`, and every lookup happens before any assignment, so a failure can never leave the module half-initialized. `terminal-stdin?` exists because readline captures the input port when its module is instantiated: the backend is engaged only when that port really is the terminal's stdin, which is what keeps piped output byte-for-byte identical to the old behavior.

History is trickier than it looks. Editline does not add accepted lines to its own history while GNU Readline does, so the module probes once on the first non-empty line and fills the gap itself if the backend left the history unchanged:

```racket
(define history-probed? #f)
(define backend-adds-history? #f)

(define (remember-line! line before-count)
  (define grew? (> (history-length-proc) before-count))
  (cond
    [(not history-probed?)
     (set! history-probed? #t)
     (set! backend-adds-history? grew?)
     (when (and (not grew?) (not (string=? line "")))
       (add-history-proc line))]
    [(and (not backend-adds-history?) (not (string=? line "")))
     (add-history-proc line)]))
```

Persistence goes to `~/.coding_agent_history`. `save-history!` keeps the last `max-history` (1000) entries and writes them atomically, using negative history indices that count back from the newest entry, which sidesteps the zero-versus-one base difference between the two backends.

Completion is installed as a callback that receives the word under the cursor. `Tab` on a word beginning with `/` completes slash commands; elsewhere it completes provider profile names, search engines, and the model ids declared in the harness config:

```racket
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
```

`setup-line-input!` wires the three pieces together and registers a plumber flush hook, so history is saved when the process exits normally. It returns `#t` only when the backend is actually active, and the REPL works unchanged when it returns `#f`.

### The REPL Loop

The main loop in `agent.rkt` is a straightforward tail-recursive function:

```racket
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
```

`handle-slash-command` recognizes `/reset`, `/history`, `/context`, `/compact`, `/model`, `/provider`, `/debug`, `/search`, `/tokens`, `/help`, `/skills`, and any other `/<name>` as a skill lookup, all before any model call is made. Input is read through `read-input-line`, so the same loop gets readline editing on a terminal and a plain prompt otherwise. The `module+ main` form lets `agent.rkt` be both loaded as a library (for testing) and run directly from the command line.

## The Command-Line Interface

Because `agent.rkt` ends in a `module+ main` submodule, it is also an ordinary Unix-style command. If any prompt is supplied, the harness runs a single task and exits; otherwise it starts the REPL. The flags are declared with `racket/cmdline`:

```racket
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
```

`--help` is generated by `racket/cmdline` and exits with status 0. `--version` prints the version and exits before any provider check. A prompt can arrive as positional arguments, as one or more `-p`/`--prompt` values, or on stdin with `--stdin`; `build-prompt` joins every source with blank lines and trims the result, so mixing them works.

Two flags exist for automation. `--yes` prints the diff and applies it without prompting, and `--dry-run` prints the diff and writes nothing. `--quiet` suppresses the banner, the `[intent]` line, and most tool chatter, while `--plain` (or `--no-color`) turns off ANSI codes, which is what you want when redirecting output to a file.

### Exit Codes

One-shot runs are meant to compose with shell scripts, so the harness maps outcomes to exit codes: `0` for a normal finish, `1` for a model or network error, `2` when a `make check` gate failed, `3` when the user rejected or skipped a change, and `5` for bad arguments. The `infer-exit-code` helper derives the code by scanning the tool results in the conversation for the strings the tools produce, which keeps the decision in one place instead of threading a status value through the agentic loop.

## Running the Agent

### Installation

Install Racket 8.11 or later from racket-lang.org. Then install the `http-easy` HTTP client package:

```
raco pkg install --auto http-easy
```

Export your API keys. Which key you need depends on the provider profiles you configure; the Fireworks profile used in this chapter reads `FIREWORKS_API_KEY`, and the search keys are optional:

```
export FIREWORKS_API_KEY=fw_...
export BRAVE_SEARCH_API_KEY=...
export EXA_SEARCH_API_KEY=...
```

Finally, create `~/.coding_harness.json` with at least one provider. A local MLX profile needs no key, only a running server:

```
mlx_lm.server --model mlx-community/gemma-4-26B-A4B-it-OptiQ-4bit --port 11434
```

With no providers configured the harness refuses to start and prints both config paths, which is the intended failure mode: there is no compiled-in provider to fall back to.

### Starting the REPL

```
make run
```

or directly:

```
racket agent.rkt
```

The banner shows the working directory, the active provider profile, and the active model:

```
Coding Agent REPL.  /help for commands, /quit to exit.
  cwd:      /Users/mark/myproject
  provider: fireworks
  model:    accounts/fireworks/models/deepseek-v4p1-flash
```

### Sample Session

The following session asks the agent to add a helper function to an existing file. Lines beginning with `>` are user input; everything else is agent output.

```
> add a function called word-count that takes a string and returns the number of words

[intent: coding → coding tools, no search]
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
  Estimated cost:    $0.000282  ($0.1400/M input, $0.0280/M cached input, $0.2800/M output)

> /quit
```

### Enabling Web Search

Toggle search on with `/search`. Switch between engines with `/search brave` or `/search exa`:

```
> /search brave
Web search ON (engine: brave)

> what is the current version of Racket?

[intent: general → web search, no coding tools]
[Web search results for: what is the current version of Racket?]
1. Racket -- A programmable programming language
   https://racket-lang.org
   Racket 8.14 was released on ...
...

As of mid-2026, the current stable release of Racket is version 8.14.
```

### Switching Providers

The `/provider` command with no argument reports the active profile and everything the config declares. With a profile name it switches, and the next call uses that profile's endpoint, model, and generation settings:

```
> /provider
Current provider: fireworks (model: accounts/fireworks/models/deepseek-v4p1-flash)
Available profiles: deepseek, fireworks, mlx, omlx, sushi

> /provider mlx
Provider set to profile 'mlx' (model: mlx-community/gemma-4-26B-A4B-it-OptiQ-4bit)

> /tokens

Session token usage (local MLX -- no API cost):
  Prompt tokens:     0
  Completion tokens: 0
  Estimated cost:    $0  (local model mlx-community/gemma-4-26B-A4B-it-OptiQ-4bit)
```

### Running One-Shot Commands

Any prompt makes the run non-interactive. This is the form to use from a script or a Makefile:

```
racket agent.rkt -p "add a docstring to word-count"
racket agent.rkt --stdin --quiet --plain < task.txt > out.txt
racket agent.rkt --dry-run -p "rename foo to bar"
racket agent.rkt -y -p "fix the failing test"
echo $?   # 0 ok, 2 make check failed, 3 rejected, 5 bad args
```

### Building an Executable

The `Makefile` also builds and installs a standalone binary, and it regenerates shell completions:

```
make make-executable            # builds ./coding-agent
make install PREFIX=/usr/local  # copy to $PREFIX/bin
make completions                # bash/zsh/fish into ./completions/
make dist                       # raco distribute -> ./dist/
```

The executable runs from any directory on this machine. Because it is built with `++lib readline/readline`, line editing survives the packaging step, while a machine without libedit still falls back to plain input at run time.

## Interpreting the Output

When the agent prints `* tool-name arguments` it is showing a tool call in progress. The tool name and a truncated version of the arguments help you follow the model's reasoning. `read_file utils.rkt` means the model decided it needs to see the file before editing it, a sign it is following the system prompt rules. `propose_edit` always appears after a `read_file` for the same path.

`make check passed` tells you both that the model's proposed syntax was valid Racket and that your project's own compile step accepted it. If you see `make check FAILED`, the failure output follows immediately and appears in the agent's next prompt, giving the model a second chance to correct the error autonomously.

The `/tokens` output shows prompt tokens growing much faster than completion tokens. That is expected in an agentic loop: the conversation history (including tool results, which can be long) is re-sent to the model on every iteration, while the model's replies are comparatively short. The cost estimate uses the rates declared in the active provider's `pricing` block, and a profile without one prints `n/a (no "pricing" block for this provider)` instead of guessing. A local MLX profile always reports `$0`.

The `[intent: ...]` line tells you how the agent routed your request. A general question is answered without touching any tools; a coding task gets the full tool loop. If the routing looks wrong, you can inspect the keyword lists and adjust them. The line is hidden under `--quiet`.

A line such as `(stopped: the model repeated the identical tool call(s) 2 times ...)` means the repetition guard fired. That is a signal that the model is too weak for the task or is emitting malformed arguments, not that the harness has crashed.

## Wrap Up

This chapter built a complete Racket coding agent in roughly 2,800 lines across nine focused modules. The main ideas were:

**Separation of concerns.** The shared agentic loop, the two provider clients, the configuration layer, the tool registry, the approval UI, the search backends, and the line editor each live in their own file. The only module that cannot be required statically is the line editor, and it isolates that fact: it loads readline with `dynamic-require` and falls back to plain input, so a missing native library is a degradation rather than a failure.

**Provider abstraction.** The agentic loop in `chat-loop.rkt` is parameterized by a `post-fn` adapter, so Fireworks (cloud, SSE streaming) and MLX (local, OpenAI-compatible) both run through the identical loop. The only differences are in the transport at the boundary.

**Configuration over compilation.** Endpoints, models, generation parameters, API key variable names, and prices live in a JSON config with two merge layers. Adding a provider or changing a rate is a config edit, and no provider name appears anywhere in the Racket source.

**The stale-base guard in `propose_edit`.** Requiring the model to supply the exact current contents of a file before any edit is accepted prevents silent overwrites when the file changes between the read and edit steps. The mismatch is reported as a tool result the model can read and respond to.

**Two-stage intent classification.** A free keyword heuristic handles the common cases and falls back to a cheap model call only for ambiguous queries. Defaulting to `"coding"` on classifier failure keeps the full tool set available.

**The `make check` feedback loop.** Every accepted edit is immediately verified by the project's own build target. Failures go back into the conversation history, giving the model the information it needs to self-correct on the next iteration.

**A repetition guard in the loop.** Small local models sometimes re-issue the identical failing call. Tracking the last few call signatures and stopping when one repeats turns a wasted run into an immediate, legible explanation.

These patterns (tool registries, approval gates with stale-base guards, intent routing, quality gates, provider adapters, and config-driven backends) apply broadly across languages and LLM providers. The Racket implementation here serves as a concrete reference for how each piece fits together at the system level.

## Optional Practice Problems

**Problem 1: Add a `write_file` tool**

The agent currently has no way for the model to create a file without going through `propose_edit`. Add a `write_file` tool to `tools.rkt` that accepts a `path` and `content` parameter, writes the content directly (without a diff prompt), and returns a confirmation string. Register it in `register-all` and add it to `ENABLED-TOOLS`. Consider what safety constraints, if any, should prevent the model from overwriting files outside the working directory, and whether the existing `hidden-file?` predicate should apply.

**Problem 2: Extend the shell whitelist dynamically**

`SHELL-WHITELIST` is currently a compile-time constant. Add a `/allow-cmd` slash command to `agent.rkt` that lets the user append a command to the whitelist at runtime, so that `/allow-cmd git` would let the model run `git status` and `git diff`. Update `handle-slash-command` to recognize the new command and update the set stored in `tools.rkt`. Think about where the mutable whitelist state should live and how `tools.rkt` should expose it.

**Problem 3: Persistent session history**

At present, `/reset` discards the conversation history and there is no way to resume a previous session. Add two slash commands: `/save <filename>` that writes the current `messages-box` contents to a JSON file using `jsexpr->string`, and `/load <filename>` that reads that file and restores the conversation. Use `string->jsexpr` for loading. Handle file-not-found and malformed JSON gracefully by printing an error and leaving the existing history unchanged.

**Problem 4: Token-budget guard**

`chat-with-tools*` will keep iterating until the model stops calling tools or `max-iterations` is reached. Add a token-budget guard that checks the running token total after each iteration and returns early with a warning message if it exceeds a configurable threshold. The counters live in private boxes in `fireworks-ai.rkt`, and `mlx-serve.rkt` keeps a smaller pair of its own, so you will need to export a reader or thread a check through the `post-fn` adapter. Expose the threshold as a `/budget <n>` slash command that sets it, and a `/budget` command with no argument that prints the current setting and the remaining budget.

**Problem 5: Second search backend -- DuckDuckGo**

Add a `ddg-search` function to `search.rkt` using the DuckDuckGo Instant Answer API at `https://api.duckduckgo.com/?q=QUERY&format=json`. The response contains a `RelatedTopics` array of objects with `Text` and `FirstURL` fields. Return results in the same `(url title description)` triple format as `brave-search` and `exa-search`. Update `agent.rkt` to accept `/search ddg` as a valid engine selection, and add it to the `SEARCH-ENGINES` list so Tab completes it.

**Problem 6: Colored intent label in the REPL prompt**
The line `[intent: coding → coding tools, no search]` is printed in plain text. Use ANSI codes to color the label: green for `"coding"`, cyan for `"general"`, and yellow (`"\033[33m"`) for `"hybrid"`. Update `send-to-model` in `agent.rkt` to apply the color. `approval.rkt` already defines the constants, but they are not in its `provide` list, so decide whether to export them, redefine them locally, or move them to a shared `ansi.rkt` module.

**Problem 7: Retry on `make check` failure**

Currently, when `propose_edit` runs `make check` and it fails, the failure output is returned to the model as a tool result, but the model must then propose a new edit from scratch. Modify `tool-propose-edit` so that on a `make check` failure it offers the user a `[r]etry` option at the approval prompt (in addition to the existing `y/n/s` choices). On retry, revert the file to its previous contents using `call-with-output-file`, print a confirmation, and return a result string that tells the model the file was reverted and includes the check output so it can try again with a corrected edit.

**Problem 8: Stream the SSE deltas to the terminal**

The Fireworks client already reassembles the SSE stream into a single response via `parse-sse-response`, but the user does not see the reply being written in real time. Modify `post-fireworks` so that, while `parse-sse-response` is accumulating the response, each `delta.content` fragment is also `display`ed to the terminal as it arrives. Consider how to do this without double-printing the final text (which the REPL also prints after the call returns), and whether the tool-call argument fragments should be hidden.

**Problem 9: Add a provider profile without touching code**

The provider layer is data, not code. Confirm that by adding a second local profile to `~/.coding_harness.json` that points at a different port, for example oMLX on 8000 or sushi on 12345. Switch to it with `/provider <name>`, check it with `/provider` alone and with `/tokens`, and verify that Tab completion offers the new profile name. Then override one field in `.local_coding_harness.json` and confirm that the deep merge replaces just that field while the rest of the global profile survives.
