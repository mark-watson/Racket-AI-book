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

This chapter builds a complete Racket implementation of such a coding agent. The agent classifies each user request as a coding task, a general question, or a hybrid, and routes it accordingly. It supports live web search via Brave or Exa AI, renders colored unified diffs before any file is written, and lets the user interrupt a running task at any time with a single ESC keypress. It runs against either a cloud provider (Fireworks AI) or a local model server (Ollama) through a single provider-agnostic loop.

## Module Architecture

The project is organized into eight source files, each with a single clear responsibility:

```
chat-loop.rkt      Provider-agnostic agentic tool loop shared by both backends
fireworks-ai.rkt   Fireworks API client (SSE streaming), session stats, chat helpers
ollama-ai.rkt      Local Ollama API client, session stats, chat helpers
tools.rkt          Tool registry, five coding tools, the propose_edit approval gate
approval.rkt       Colored diff printer, ESC-aware y/n/s prompt
search.rkt         Brave Search and Exa AI search backends
interrupt.rkt      Shared thread-safe task-interrupt flag
agent.rkt          REPL, intent classifier, provider dispatch, top-level run
```

The static dependency graph is nearly linear, but there is a subtle circularity: `chat-loop.rkt` needs to dispatch tool calls defined in `tools.rkt`, while `tools.rkt` needs the interrupt flag and the approval prompt. The interrupt flag is the key to resolving this cleanly. It lives in its own tiny module, `interrupt.rkt`, with no dependencies, so every other module can `require` it statically. The approval prompt, which needs the flag, lives in `approval.rkt`; the tools, which need both the flag and the prompt, live in `tools.rkt`. By factoring the shared flag into its own leaf module, no module needs a `dynamic-require` and the graph stays acyclic.

## The Shared Interrupt Flag

The ESC-to-interrupt feature needs a single flag that many modules read and write: the loop checks it before each iteration, the tool executor checks it before each tool call, and the approval prompt sets it when ESC is pressed. Because it is shared so widely, it lives in its own dependency-free module, `interrupt.rkt`:

```racket
#lang racket

(provide task-interrupted?
         task-interrupted-set!
         task-interrupted-clear!)

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

The flag uses a plain `box` (a mutable cell) protected by a semaphore because the main thread writes to it while the worker thread reads from it. `call-with-semaphore` acquires the semaphore, runs the body, and releases the semaphore even if an exception is raised.

## The Provider-Agnostic Agentic Loop

The heart of the agent is a loop that is deliberately independent of any particular model vendor. It lives in `chat-loop.rkt` and is parameterized by a `post-fn` argument: a function that takes an OpenAI-style chat-completions payload and returns a normalized response hash. Both Fireworks and Ollama supply their own `post-fn`, and the loop never talks to a network directly.

Here is the complete file:

```racket
#lang racket

(require racket/string
         "interrupt.rkt"
         "tools.rkt")

(provide chat*
         chat-with-tools*)

(define (without-dangling msgs)
  (if (and (not (null? msgs))
           (hash-has-key? (last msgs) 'tool_calls))
      (drop-right msgs 1)
      msgs))

(define (chat* post-fn messages
               #:model-id model-id
               #:max-tokens max-tokens
               #:temperature temperature)
  (define payload
    (hash 'model model-id
          'max_tokens max-tokens
          'temperature temperature
          'messages messages))
  (define data (post-fn payload))
  (define content
    (hash-ref (hash-ref (first (hash-ref data 'choices)) 'message) 'content ""))
  (if (and (string? content) (not (string=? content "")))
      content
      "No response content"))

(define (chat-with-tools* post-fn messages tools
                          #:model-id model-id
                          #:max-tokens max-tokens
                          #:temperature temperature
                          #:max-iterations max-iterations)
  (define tools-rendered (render-tools tools))
  (define current-messages (box (map (lambda (m) m) messages)))

  (define (loop iter)
    (cond
      [(task-interrupted?)
       (values "(task interrupted by user)" (without-dangling (unbox current-messages)))]
      [(>= iter max-iterations)
       ;; One final no-tools call to generate a summary
       (if (task-interrupted?)
           (values "(task interrupted by user)" (without-dangling (unbox current-messages)))
           (let ()
             (define payload
               (hash 'model model-id
                     'max_tokens max-tokens
                     'temperature temperature
                     'messages (unbox current-messages)))
             (with-handlers ([exn:fail? (lambda (_) (values "(max tool iterations reached)" (unbox current-messages)))])
               (define data (post-fn payload))
               (define msg (hash-ref (first (hash-ref data 'choices)) 'message))
               (define content (hash-ref msg 'content ""))
               (values (if (and (string? content) (not (string=? content "")))
                           content
                           "(no summary from model)")
                       (append (unbox current-messages) (list msg))))))]
      [else
       (define payload
         (let ([base (hash 'model model-id
                           'max_tokens max-tokens
                           'temperature temperature
                           'messages (unbox current-messages))])
           (if (null? tools-rendered)
               base
               (hash-set* base 'tools tools-rendered 'tool_choice "auto"))))
       (define data (post-fn payload))
       (define msg (hash-ref (first (hash-ref data 'choices)) 'message))
       (define tool-calls (hash-ref msg 'tool_calls #f))
       (define content (hash-ref msg 'content ""))
       (set-box! current-messages (append (unbox current-messages) (list msg)))
       (cond
         [(task-interrupted?)
          (values "(task interrupted by user)" (without-dangling (unbox current-messages)))]
         [(and content tool-calls (not (string=? (string-trim content) "")))
          ;; Model narrated its reasoning AND requested tools: show the text, then run.
          (displayln "")
          (displayln (string-trim content))
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
          (loop (add1 iter))]
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

There are two edge cases worth noting. The first is a model that emits *both* text and tool calls in the same turn: the text is printed to the user immediately (so it can narrate "I'll read the file first"), then the tools run, then the loop continues. The second is the iteration cap: at `max-iterations` the loop makes one final call with no tools, asking the model to summarize what it did, rather than returning nothing.

The `without-dangling` helper strips a trailing assistant message that has `tool_calls` but no corresponding `tool` results -- a history in that state would be rejected by the API.

## The Fireworks AI Client

### The API and Pricing

Fireworks AI is a hosted inference platform that serves many open-weight models through an OpenAI-compatible API. This agent defaults to DeepSeek v4 Flash, a fast and cost-effective model well-suited for coding tasks. The pricing at the time of writing is $0.14 per million uncached input tokens, $0.028 per million cached input tokens (an 80 percent cache discount), and $0.28 per million output tokens.

The estimated session cost accumulated over a conversation is:

```$
\text{cost} = (p - k) \times \frac{0.14}{10^6} + k \times \frac{0.028}{10^6} + c \times \frac{0.28}{10^6}
```

where `p`$ is the total prompt tokens, `k`$ is the cached portion of those prompt tokens (billed at the discount), and `c`$ is the total completion tokens. The agent tracks all of these and displays the running total on demand.

### Streaming with Server-Sent Events

Unlike the plain one-shot `chat`, the Fireworks client streams its responses using **server-sent events (SSE)**. The API sends a sequence of `data:` lines, each carrying a small *delta* of the response, terminated by a `data: [DONE]` line. Streaming has two benefits: the user sees the reply being written in real time, and there is no wall-clock cap on generation time. The only timeouts are `CURL-MAX-TIME` (seconds to wait for response headers and the TCP connection) and `STREAM-IDLE-TIMEOUT` (seconds of *silence* from the server before giving up). As long as tokens keep flowing, a request may run for minutes.

Here is the complete file:

```racket
#lang racket

(require net/http-easy
         json
         racket/string
         racket/port
         "interrupt.rkt"
         "tools.rkt"
         "chat-loop.rkt")

(provide FIREWORKS-ENDPOINT
         FIREWORKS-MODEL
         MAX-TOKENS
         DEBUG-LOG
         CURL-MAX-TIME
         PRICE-PER-M-PROMPT
         PRICE-PER-M-CACHED-PROMPT
         PRICE-PER-M-COMPLETION
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

(define FIREWORKS-ENDPOINT "https://api.fireworks.ai/inference/v1/chat/completions")
(define FIREWORKS-MODEL (make-parameter "accounts/fireworks/models/deepseek-v4-flash-0731"))
(define MAX-TOKENS 32768)
(define DEBUG-LOG (make-parameter #f))
(define CURL-MAX-TIME 600)
(define STREAM-IDLE-TIMEOUT 300)

(define PRICE-PER-M-PROMPT 0.14)
(define PRICE-PER-M-CACHED-PROMPT 0.028)
(define PRICE-PER-M-COMPLETION 0.28)

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

(define (prompt-cost tokens)
  (* tokens PRICE-PER-M-PROMPT (/ 1 1000000)))

(define (cached-cost tokens)
  (* tokens PRICE-PER-M-CACHED-PROMPT (/ 1 1000000)))

(define (completion-cost tokens)
  (* tokens PRICE-PER-M-COMPLETION (/ 1 1000000)))

(define (session-cost)
  (call-with-semaphore stats-sema
    (lambda ()
      (define pt (unbox session-prompt-tokens))
      (define ca (unbox session-cached-tokens))
      (+ (prompt-cost (max 0 (- pt ca)))
         (cached-cost ca)
         (completion-cost (unbox session-completion-tokens))))))

(define (print-session-stats)
  (define-values (pt ct tt ca)
    (call-with-semaphore stats-sema
      (lambda ()
        (values (unbox session-prompt-tokens)
                (unbox session-completion-tokens)
                (unbox session-total-tokens)
                (unbox session-cached-tokens)))))
  (define cost (session-cost))
  (displayln "")
  (displayln "Session token usage:")
  (displayln (format "  Prompt tokens:     ~a" pt))
  (displayln (format "  Completion tokens: ~a" ct))
  (displayln (format "  Total tokens:      ~a" tt))
  (when (> ca 0)
    (define pct (* 100.0 (/ ca (max 1 pt))))
    (displayln (format "  Cached tokens:     ~a (~a% of prompt)" ca (~r pct #:precision 1))))
  (displayln (format "  Estimated cost:    $~a  ($~a/M input, $~a/M cached input, $~a/M output)"
                     (~r cost #:precision 6)
                     (~r PRICE-PER-M-PROMPT #:precision 4)
                     (~r PRICE-PER-M-CACHED-PROMPT #:precision 4)
                     (~r PRICE-PER-M-COMPLETION #:precision 4))))

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

(define (get-api-key)
  (define key (getenv "FIREWORKS_API_KEY"))
  (unless (and key (not (string=? key "")))
    (error 'fireworks-ai "FIREWORKS_API_KEY environment variable not set"))
  key)

(define (bytes-index-of bstr b)
  (let loop ([i 0]
             [len (bytes-length bstr)])
    (cond
      [(= i len) #f]
      [(= (bytes-ref bstr i) b) i]
      [else (loop (add1 i) len)])))

(define (make-sse-line-reader in)
  (define buf (make-bytes 4096))
  (define acc (box #""))
  (define (read-more!)
    (unless (sync/timeout STREAM-IDLE-TIMEOUT
              (handle-evt in (lambda (_) #t)))
      (error 'fireworks-ai
             "stream idle timeout: no data from Fireworks for ~a seconds"
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
         (set-box! acc (subbytes data (add1 nl)))
         (subbytes data 0 nl)]
        [(read-more!)
         (define rest (unbox acc))
         (set-box! acc #"")
         (if (zero? (bytes-length rest)) eof (subbytes rest 0))]
        [else (loop)]))))

(define (parse-sse-chunk body)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (string->jsexpr body)))

(define (parse-sse-response in)
  (define content-out (open-output-string))
  (define reasoning-out (open-output-string))
  (define tool-calls-by-index (make-hash))
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
       (define trimmed (string-trim (bytes->string/utf-8 line)))
       (cond
         [(or (string=? trimmed "")
              (string-prefix? trimmed ":"))
          (loop)]
         [(string-prefix? trimmed "data:")
          (define body (string-trim (substring trimmed 5)))
          (cond
            [(string=? body "[DONE]") (void)]
            [else
             (define chunk (parse-sse-chunk body))
             (when (hash? chunk)
               (when (hash-has-key? chunk 'error)
                 (define err (hash-ref chunk 'error))
                 (define msg
                   (cond
                     [(hash? err) (hash-ref err 'message (format "~a" err))]
                     [else (format "~a" err)]))
                 (error 'fireworks-ai "Fireworks API error: ~a" msg))
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

(define (post-fireworks payload)
  (define api-key (get-api-key))
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
        (post FIREWORKS-ENDPOINT
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
    (error 'fireworks-ai "Fireworks API error: ~a" msg))
  (accumulate-usage data)
  (unless (hash-has-key? data 'choices)
    (error 'fireworks-ai "Fireworks response has no 'choices'. Raw: ~a" (jsexpr->string data)))
  data)

(define (chat messages
              #:model-id [model-id (FIREWORKS-MODEL)]
              #:max-tokens [max-tokens MAX-TOKENS]
              #:temperature [temperature 0.6])
  (chat* post-fireworks messages
         #:model-id model-id
         #:max-tokens max-tokens
         #:temperature temperature))

(define (chat-with-tools messages tools
                         #:model-id [model-id (FIREWORKS-MODEL)]
                         #:max-tokens [max-tokens MAX-TOKENS]
                         #:temperature [temperature 0.6]
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

data: [DONE]
```

`make-sse-line-reader` returns a stateful function that yields one line at a time, buffering partial lines between calls and applying the idle timeout. `parse-sse-response` then walks those lines and reassembles the deltas:

- **`content`** deltas are appended to a string output port.
- **`reasoning_content`** deltas (for reasoning models) go to a separate port.
- **`tool_calls`** deltas are the tricky part, because a single tool call's name and arguments arrive split across many chunks. The code accumulates them in a hash keyed by the call's `index`, appending argument fragments to a boxed string. At the end it sorts the indices and rebuilds the tool-call list.
- The final `usage` chunk is captured for token accounting.

The output is a single normalized response hash with the same shape the non-streaming Ollama backend produces, so `chat-loop.rkt` never knows which backend it is talking to.

### Token Accounting and Cost

Because Fireworks is a paid service, the module tracks usage. The session counters live in boxes guarded by a semaphore, because the REPL can run tasks in background threads. Cached input tokens are reported by Fireworks in `usage.prompt_tokens_details.cached_tokens`; they are part of `prompt_tokens` but are billed at the discounted rate. The `session-cost` function subtracts them from the uncached pool and bills them separately. The `/tokens` command prints the breakdown, including the cached-token percentage.

## The Ollama Client

The Ollama backend, `ollama-ai.rkt`, mirrors the Fireworks interface so that the agent loop and the REPL can swap providers with a single parameter. The differences are all about the wire format. Ollama's `/api/chat` endpoint is non-streaming, expects tool-call arguments as *objects* rather than JSON strings, and reports token counts as `prompt_eval_count`/`eval_count` rather than `prompt_tokens`/`completion_tokens`.

Here is the complete file:

```racket
#lang racket

(require net/http-easy
         json
         racket/string
         "fireworks-ai.rkt"   ; for DEBUG-LOG (shared /debug toggle)
         "chat-loop.rkt")

(provide OLLAMA-ENDPOINT
         OLLAMA-MODEL
         OLLAMA-THINK
         ollama-reset-session-stats
         ollama-print-session-stats
         post-ollama
         ollama-chat
         ollama-chat-with-tools)

(define OLLAMA-ENDPOINT "http://localhost:11434/api/chat")
(define OLLAMA-MODEL (make-parameter "nemotron-3.5-lightning:30b-mlx"))
(define OLLAMA-THINK (make-parameter #f))
(define MAX-TOKENS 32768)
(define OLLAMA-MAX-TIME 900)
(define OLLAMA-CONNECT-TIME 10)

(define stats-sema (make-semaphore 1))
(define session-prompt-tokens (box 0))
(define session-completion-tokens (box 0))

(define (ollama-reset-session-stats)
  (call-with-semaphore stats-sema
    (lambda ()
      (set-box! session-prompt-tokens 0)
      (set-box! session-completion-tokens 0))))

(define (ollama-accumulate-usage usage)
  (when (and (hash? usage) (not (hash-empty? usage)))
    (call-with-semaphore stats-sema
      (lambda ()
        (set-box! session-prompt-tokens
                  (+ (unbox session-prompt-tokens)
                     (hash-ref usage 'prompt_tokens 0)))
        (set-box! session-completion-tokens
                  (+ (unbox session-completion-tokens)
                     (hash-ref usage 'completion_tokens 0)))))))

(define (ollama-print-session-stats)
  (define-values (pt ct)
    (call-with-semaphore stats-sema
      (lambda ()
        (values (unbox session-prompt-tokens)
                (unbox session-completion-tokens)))))
  (displayln "")
  (displayln "Session token usage (local Ollama -- no API cost):")
  (displayln (format "  Prompt tokens:     ~a" pt))
  (displayln (format "  Completion tokens: ~a" ct))
  (displayln (format "  Total tokens:      ~a" (+ pt ct)))
  (displayln (format "  Estimated cost:    $0  (local model ~a)" (OLLAMA-MODEL))))

(define (sanitize-message msg)
  (define role (hash-ref msg 'role "user"))
  (define content (hash-ref msg 'content ""))
  (define tcs (hash-ref msg 'tool_calls #f))
  (cond
    [(equal? role "tool")
     (hash 'role "tool"
           'content (if (string? content) content (format "~a" content)))]
    [(and (equal? role "assistant") (list? tcs))
     (hash 'role "assistant"
           'content (if (string? content) content "")
           'tool_calls
           (for/list ([tc (in-list tcs)])
             (define f (hash-ref tc 'function (hash)))
             (define args (hash-ref f 'arguments "{}"))
             (define args-obj
               (cond
                 [(hash? args) args]
                 [(string? args)
                  (with-handlers ([exn:fail? (lambda (_) (hash))])
                    (string->jsexpr args))]
                 [else (hash)]))
             (hash 'function (hash 'name (hash-ref f 'name "")
                                   'arguments args-obj))))]
    [else
     (hash 'role role
           'content (if (string? content) content ""))]))

(define (normalize-response r)
  (define msg (hash-ref r 'message (hash)))
  (define raw-tcs (hash-ref msg 'tool_calls #f))
  (define tool-calls
    (and (list? raw-tcs)
         (not (null? raw-tcs))
         (for/list ([tc (in-list raw-tcs)] [i (in-naturals 1)])
           (define f (hash-ref tc 'function (hash)))
           (define args (hash-ref f 'arguments (hash)))
           (hash 'id (format "call_~a" i)
                 'type "function"
                 'function (hash 'name (hash-ref f 'name "")
                                 'arguments (if (string? args)
                                                args
                                                (jsexpr->string args)))))))
  (define content (hash-ref msg 'content ""))
  (define thinking (hash-ref msg 'thinking ""))
  (define message
    (if tool-calls
        (hash 'role "assistant"
              'content (if (string? content) content "")
              'reasoning_content (if (string? thinking) thinking "")
              'tool_calls tool-calls)
        (hash 'role "assistant"
              'content (if (string? content) content "")
              'reasoning_content (if (string? thinking) thinking ""))))
  (define prompt-tokens (hash-ref r 'prompt_eval_count 0))
  (define completion-tokens (hash-ref r 'eval_count 0))
  (hash 'model (hash-ref r 'model "")
        'choices (list (hash 'message message
                             'finish_reason (hash-ref r 'done_reason #f)))
        'usage (hash 'prompt_tokens prompt-tokens
                     'completion_tokens completion-tokens
                     'total_tokens (+ prompt-tokens completion-tokens))))

(define (post-ollama payload)
  (define request
    (hash 'model (hash-ref payload 'model)
          'messages (map sanitize-message (hash-ref payload 'messages '()))
          'stream #f
          'think (OLLAMA-THINK)
          'options (hash 'num_predict (hash-ref payload 'max_tokens MAX-TOKENS)
                         'temperature (hash-ref payload 'temperature 0.6))))
  (define request*
    (if (hash-has-key? payload 'tools)
        (hash-set request 'tools (hash-ref payload 'tools))
        request))
  (when (DEBUG-LOG)
    (displayln (format "[DEBUG] ollama request: ~a"
                       (jsexpr->string (hash-remove request* 'messages)))))
  (define data
    (with-handlers ([exn:fail? (lambda (e) (error 'ollama-ai "HTTP error: ~a" (exn-message e)))])
      (define resp
        (post OLLAMA-ENDPOINT
              #:json request*
              #:timeouts (make-timeout-config #:request OLLAMA-MAX-TIME
                                              #:connect OLLAMA-CONNECT-TIME)))
      (define j (response-json resp))
      (when (DEBUG-LOG)
        (displayln (format "[DEBUG] ollama response: ~a" (jsexpr->string j))))
      j))
  (unless (hash-has-key? data 'message)
    (error 'ollama-ai "Ollama response has no 'message'. Raw: ~a" (jsexpr->string data)))
  (define normalized (normalize-response data))
  (ollama-accumulate-usage (hash-ref normalized 'usage))
  normalized)

(define (ollama-chat messages
                     #:model-id [model-id (OLLAMA-MODEL)]
                     #:max-tokens [max-tokens MAX-TOKENS]
                     #:temperature [temperature 0.6])
  (chat* post-ollama messages
         #:model-id model-id
         #:max-tokens max-tokens
         #:temperature temperature))

(define (ollama-chat-with-tools messages tools
                                #:model-id [model-id (OLLAMA-MODEL)]
                                #:max-tokens [max-tokens MAX-TOKENS]
                                #:temperature [temperature 0.6]
                                #:max-iterations [max-iterations 20])
  (chat-with-tools* post-ollama messages tools
                    #:model-id model-id
                    #:max-tokens max-tokens
                    #:temperature temperature
                    #:max-iterations max-iterations))
```

### The Two Conversion Functions

The heart of the Ollama module is a pair of conversion functions at the boundary between the two wire formats.

**`sanitize-message`** prepares outgoing messages. It drops keys Ollama does not understand (like `reasoning_content` and `tool_call_id`), and crucially converts assistant tool-call `arguments` from JSON strings back into objects, because that is what Ollama's API expects.

**`normalize-response`** does the reverse. It takes Ollama's response, which has tool-call arguments as objects and no call ids, and rebuilds the OpenAI-style shape: arguments as a JSON string and a synthesized `call_1`, `call_2`, ... id per call. It also maps `prompt_eval_count` to `prompt_tokens` and `eval_count` to `completion_tokens` so the shared token-accounting code in the loop works unchanged.

Because local inference is free, the cost display is always `$0`. The stats are informational only.

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
         "interrupt.rkt"
         "approval.rkt")

(provide define-tool
         render-tools
         execute-tool-calls
         register-all
         ENABLED-TOOLS)

(define registry (make-hash))

(define SHELL-WHITELIST (set "make" "ls" "pwd" "cat" "uv"))
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
         (with-handlers ([exn:fail? (lambda (e) (format "Error running command: ~a" (exn-message e)))])
           (define args (rest tokens))
           (define-values (out code) (run-external cmd args))
           (string-append out (format "(exit ~a)" code))))]))
```

If the model attempts to run a disallowed command, it receives an error string describing what is allowed. It can then adapt its approach rather than causing the agent to crash. The whitelist currently allows `make`, `ls`, `pwd`, `cat`, and `uv` (the Python package runner). Everything else is refused.

### The `propose_edit` Approval Gate

`propose_edit` is the most critical tool. Before writing any file it checks for several error conditions, shows the user a diff, waits for approval, writes the file, and then runs `make check`:

```racket
(define (tool-propose-edit path old new)
  (define exists? (file-exists? path))
  (define current
    (if exists?
        (with-handlers ([exn:fail? (lambda (e) (format "Error reading ~a: ~a" path (exn-message e)))])
          (file->string path))
        ""))
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
     (define answer (prompt-yes-no-skip))
     (cond
       [(eq? answer 'interrupted) "change not applied (task interrupted by user)"]
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
                    status (truncate-string out MAX-CHECK-OUTPUT-CHARS)))]))])
```

The stale-base guard is worth understanding carefully. The model reads a file, then constructs a proposed edit based on that content. If the user edits the file externally in between, a naive tool would overwrite those changes silently. By requiring `old` to exactly match what is on disk, the tool forces the model to re-read the file before retrying -- the mismatch is reported as a tool result the model can read and respond to.

The `make check` gate closes another important feedback loop. If the edit compiles cleanly, `"applied; make check passed"` goes back into the conversation history and the model can proceed. If `make check` fails, the output goes back as well, giving the model the compiler errors it needs to self-correct on the next turn. The output is truncated to `MAX-CHECK-OUTPUT-CHARS` characters so a huge build log does not blow up the context window.

## The Approval and Diff System

### Generating a Unified Diff

`approval.rkt` generates diffs by writing the two file versions to temporary files and calling the system `diff -u` utility. Here is the full source file:

```racket
#lang racket

(require racket/file
         racket/port
         racket/string
         racket/system
         "interrupt.rkt")

(provide unified-diff
         print-colored-diff
         prompt-yes-no-skip
         prompt-reason
         save-stty-state
         restore-stty-state
         enter-cbreak-mode)

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
              [(char=? ch #\u001b)
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
               (display "\r")
               (flush-output)
               (set! done? #t)]
              [(or (char=? ch #\backspace) (char=? ch #\rubout))
               (when (not (null? chars))
                 (set! chars (rest chars))
                 (display "\b \b")
                 (flush-output))
               (loop)]
              [(char=? ch (integer->char 3)) ; Ctrl-C -- treat like ESC
               (set! esc? #t)
               (set! done? #t)]
              [else
               (display ch)
               (flush-output)
               (set! chars (cons ch chars))
               (loop)]))
          (begin (sleep 0.02) (loop)))))
  (values (list->string (reverse chars)) esc?))
```

After reading an ESC character the code waits 50 ms. If the input port has more bytes ready within that window, they belong to an ANSI sequence and are drained. If no bytes arrive, it was a standalone ESC and `esc?` is set to `#t`. The function returns two values: the accumulated character string and the ESC flag.

Because raw mode has echo off, `read-line-raw` echoes printable characters itself and handles backspace by erasing the last buffered character. Ctrl-C is treated like ESC. On newline it emits a bare carriage return so the caller can add the final newline.

The module also provides `save-stty-state`, `restore-stty-state`, and `enter-cbreak-mode`, which capture and restore the terminal state. `enter-cbreak-mode` runs `stty -icanon -echo`, deliberately *not* full `stty raw`, because full raw mode disables output post-processing and would cause multi-line output to "stair-step" on screen.

`prompt-yes-no-skip` wraps `read-line-raw` in `dynamic-wind` to ensure the terminal is always restored, then interprets the result:

```racket
(define (prompt-yes-no-skip)
  (define saved (save-stty-state))
  (define raw? (try-raw-mode))
  (dynamic-wind
    void
    (lambda ()
      (let loop ()
        (when (task-interrupted?) (values 'interrupted))
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
          [(task-interrupted?) 'interrupted]
          [else
           (define norm (string-downcase (string-trim line)))
           (cond
             [(member norm '("y" "yes")) 'yes]
             [(member norm '("n" "no")) 'no]
             [(member norm '("s" "skip")) 'skip]
             [else
              (displayln "Please answer y, n, or s.")
              (loop)])])))
    (lambda () (when raw? (restore-mode saved)))))
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

### Provider Dispatch

`agent.rkt` is the entry point that ties everything together. It holds a `PROVIDER` parameter selecting `'fireworks` (cloud) or `'ollama` (local), defaulting to Fireworks unless the `AGENT_PROVIDER` environment variable is set to `ollama`. All model calls go through two small dispatch functions that pick the right backend:

```racket
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
```

The `/provider` slash command switches the parameter at runtime, and the `/model` command changes the model for the current provider.

### Intent Classification

Before sending any message to the model, `agent.rkt` classifies the user's intent as one of three categories: `"general"`, `"coding"`, or `"hybrid"`. The classification uses a two-stage approach.

Stage one is a keyword heuristic -- free and instant:

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

`for/or` is the Racket comprehension form that returns the first "truthy" value or `#f` if none is found.

If the heuristic returns `#f` (the query is ambiguous), stage two calls the LLM with a minimal two-message conversation and requests a single-word answer:

```racket
(define (llm-classify user-line)
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (format "[Classifier LLM error: ~a — defaulting to coding]" (exn-message e)))
                               "coding")])
    (define msgs
      (list (hash 'role "system"
                  'content "You are a one-word query classifier. Reply with exactly one word and nothing else.")
            (hash 'role "user"
                  'content
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
- run_shell only accepts whitelisted commands: make, ls, pwd, cat, uv.
- When you are done, reply with a short natural-language summary of what changed.")
```

The `{cwd}` placeholder is replaced with the actual working directory at session start. Telling the model the working directory helps it construct relative paths for `read_file` and `list_dir` calls.

### Context Management

As an agentic conversation grows, every tool result is appended to the message list, and the context window fills up. `agent.rkt` provides two commands to manage this. `/context` shows a formatted table of messages with estimated character and token counts:

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
  ...)
```

`/compact` sends the whole transcript to the model with a "compactor" system prompt, gets back a dense summary, and replaces everything except the original system prompt with that summary:

```racket
(define COMPACT-SYSTEM-PROMPT
  "You are a context compactor for a coding assistant. Summarize the conversation transcript into a compact brief that will replace it. Preserve: the user's goals and instructions, decisions made, files created or modified (with paths), important code and tool-output details, and outstanding tasks. Write dense bullets, no preamble.")
```

This trades a little fidelity for a lot of context budget, keeping the model inside its window on long sessions.

### Skills

The agent supports loading "skills" from `~/.agents/skills/<name>/SKILL.md`. Each skill file is a Markdown document that is injected into the conversation as a system message, telling the model to treat it as authoritative guidance. `/skills` lists available skills (parsing a `description:` field from each file's YAML frontmatter), and `/<skill-name>` loads one. This lets you package reusable instructions that the model will follow for the rest of the session.

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
  (define done? (sync/timeout INTERRUPT-WAIT-TIMEOUT worker))
  (unless done?
    (displayln (format "[Task did not stop within ~as; worker may still be running in background]"
                       INTERRUPT-WAIT-TIMEOUT)))
  escaped?)
```

`escape-pressed?` performs a non-blocking `char-ready?` check so the poll loop spends nearly all its time sleeping 50 ms between checks. When ESC is detected, `task-interrupted-set!` flips the shared flag. The worker thread's `chat-with-tools*` loop checks that flag at each iteration boundary and returns early without being killed. This cooperative shutdown approach is cleaner than killing the thread forcefully because it allows the thread to restore any state it owns before exiting.

`sync/timeout` waits up to `INTERRUPT-WAIT-TIMEOUT` (180) seconds for the worker to finish; if it does not stop within that window the user sees a warning but the REPL continues normally. The `dynamic-wind` guarantees the terminal state is restored even if the task throws.

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
```

`handle-slash-command` recognizes `/reset`, `/history`, `/context`, `/compact`, `/model`, `/provider`, `/debug`, `/search`, `/tokens`, `/help`, `/skills`, and skill names before any model call is made. The `module+ main` form lets `agent.rkt` be both loaded as a library (for testing) and run directly from the command line.

## Running the Agent

### Installation

Install Racket 8.11 or later from racket-lang.org. Then install the `http-easy` HTTP client package:

```
raco pkg install --auto http-easy
```

Export your API keys. Only `FIREWORKS_API_KEY` is required for the cloud provider; the search keys are optional:

```
export FIREWORKS_API_KEY=fw_...
export BRAVE_SEARCH_API_KEY=BSA...   # optional
export EXA_SEARCH_API_KEY=...        # optional
```

For the local provider, make sure an Ollama server is running with a model pulled:

```
ollama pull nemotron-3.5-lightning:30b-mlx
export AGENT_PROVIDER=ollama
```

### Starting the REPL

```
make run
```

or directly:

```
racket agent.rkt
```

The banner shows the working directory, the active provider, and the active model:

```
Coding Agent REPL.  /help for commands, /quit to exit.
  cwd:      /Users/mark/myproject
  provider: fireworks
  model:    accounts/fireworks/models/deepseek-v4-flash-0731

> 
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
  Estimated cost:    $0.000482  ($0.1400/M input, $0.0280/M cached input, $0.2800/M output)

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

Switch between Fireworks and Ollama at runtime with `/provider`:

```
> /provider ollama
Provider set to ollama (model: nemotron-3.5-lightning:30b-mlx)

> /tokens

Session token usage (local Ollama -- no API cost):
  Prompt tokens:     0
  Completion tokens: 0
  Total tokens:      0
  Estimated cost:    $0  (local model nemotron-3.5-lightning:30b-mlx)
```

### Interrupting a Long Task

Press ESC during any multi-step operation to stop the agent before the next tool call:

```
> refactor all error handling in the project to use a unified log-error helper

[intent: coding → coding tools, no search]
* list_dir .
* grep "error" .
* read_file src/main.rkt
^[
[ESC — stopping after the current step…]
[Interrupted. Type your next request or /reset.]

>
```

The terminal is restored immediately and the REPL is ready for the next input.

## Interpreting the Output

When the agent prints `* tool-name arguments` it is showing a tool call in progress. The tool name and a truncated version of the arguments help you follow the model's reasoning. `read_file utils.rkt` means the model decided it needs to see the file before editing it -- a sign it is following the system prompt rules. `propose_edit` always appears after a `read_file` for the same path.

`make check passed` tells you both that the model's proposed syntax was valid Racket and that your project's own compile step accepted it. If you see `make check FAILED`, the failure output follows immediately and appears in the agent's next prompt -- giving the model a second chance to correct the error autonomously.

The `/tokens` output shows prompt tokens growing much faster than completion tokens. That is expected in an agentic loop: the conversation history (including tool results, which can be long) is re-sent to the model on every iteration, while the model's replies are comparatively short. The Fireworks cost display also separates cached input tokens, which are billed at an 80 percent discount, from uncached input tokens.

The `[intent: ... → ...]` line tells you how the agent routed your request. A general question is answered without touching any tools; a coding task gets the full tool loop. If the routing looks wrong, you can inspect the keyword lists and adjust them.

## Wrap Up

This chapter built a complete Racket coding agent in roughly 800 lines across eight focused modules. The main ideas were:

**Separation of concerns.** The shared agentic loop, the two provider clients, the tool registry, the approval UI, and the search backends each live in their own file. The interrupt flag lives in its own dependency-free module, which resolves the circular dependency cleanly without `dynamic-require`.

**Provider abstraction.** The agentic loop in `chat-loop.rkt` is parameterized by a `post-fn` adapter, so Fireworks (cloud, SSE streaming) and Ollama (local, non-streaming) both run through the identical loop. The only differences are in the wire-format conversion at the boundary.

**The stale-base guard in `propose_edit`.** Requiring the model to supply the exact current contents of a file before any edit is accepted prevents silent overwrites when the file changes between the read and edit steps. The mismatch is reported as a tool result the model can read and respond to.

**Two-stage intent classification.** A free keyword heuristic handles the common cases and falls back to a cheap LLM call only for ambiguous queries. Defaulting to `"coding"` on classifier failure keeps the full tool set available.

**The `make check` feedback loop.** Every accepted edit is immediately verified by the project's own build target. Failures go back into the conversation history, giving the model the information it needs to self-correct on the next iteration.

**Cooperative ESC interruption.** Racket's native threads make it straightforward to run the model call in the background while the main thread polls for user input. The interrupt flag coordinates graceful shutdown without forcefully killing the worker thread, so terminal state is always restored cleanly.

These patterns -- tool registries, approval gates with stale-base guards, intent routing, quality gates, provider adapters, and graceful interruption -- apply broadly across languages and LLM providers. The Racket implementation here serves as a concrete reference for how each piece fits together at the system level.

## Optional Practice Problems

**Problem 1: Add a `write_file` tool**

The agent currently has no way for the model to create a file without going through `propose_edit`. Add a `write_file` tool to `tools.rkt` that accepts a `path` and `content` parameter, writes the content directly (without a diff prompt), and returns a confirmation string. Register it in `register-all` and add it to `ENABLED-TOOLS`. Consider what safety constraints, if any, should prevent the model from overwriting files outside the working directory.

**Problem 2: Extend the shell whitelist dynamically**

`SHELL-WHITELIST` is currently a compile-time constant. Add a `/allow-cmd` slash command to `agent.rkt` that lets the user append a command to the whitelist at runtime -- for example, `/allow-cmd git` would let the model run `git status` and `git diff`. Update `handle-slash-command` to recognize the new command and update the set stored in `tools.rkt`. Think about where the mutable whitelist state should live and how `tools.rkt` should expose it.

**Problem 3: Persistent session history**

At present, `/reset` discards the conversation history and there is no way to resume a previous session. Add two slash commands: `/save <filename>` that writes the current `messages-box` contents to a JSON file using `jsexpr->string`, and `/load <filename>` that reads that file and restores the conversation. Use `string->jsexpr` for loading. Handle file-not-found and malformed JSON gracefully by printing an error and leaving the existing history unchanged.

**Problem 4: Token-budget guard**

`chat-with-tools*` will keep iterating until the model stops calling tools or `max-iterations` is reached. Add a token-budget guard that checks `session-total-tokens` after each iteration and returns early with a warning message if the running total exceeds a configurable threshold. Expose the threshold as a `/budget <n>` slash command that sets it, and a `/budget` command with no argument that prints the current setting and remaining budget.

**Problem 5: Second search backend -- DuckDuckGo**

Add a `ddg-search` function to `search.rkt` using the DuckDuckGo Instant Answer API at `https://api.duckduckgo.com/?q=QUERY&format=json`. The response contains a `RelatedTopics` array of objects with `Text` and `FirstURL` fields. Return results in the same `(url title description)` triple format as `brave-search` and `exa-search`. Update `agent.rkt` to accept `/search ddg` as a valid engine selection.

**Problem 6: Colored intent label in the REPL prompt**

The line `[intent: coding → ...]` is printed in plain text. Use ANSI codes (already defined in `approval.rkt`) to color the label: green for `"coding"`, cyan for `"general"`, and yellow (`"\033[33m"`) for `"hybrid"`. Update `send-to-model` in `agent.rkt` to apply the color. Since `approval.rkt` already defines the ANSI constants, think about whether to `require` them from there, redefine them locally, or move them to a shared `ansi.rkt` module.

**Problem 7: Retry on `make check` failure**

Currently, when `propose_edit` runs `make check` and it fails, the failure output is returned to the model as a tool result -- but the model must then propose a new edit from scratch. Modify `tool-propose-edit` so that on a `make check` failure it offers the user a `[r]etry` option at the approval prompt (in addition to the existing `y/n/s` choices). On retry, revert the file to its previous contents using `call-with-output-file`, print a confirmation, and return a result string that tells the model the file was reverted and includes the check output so it can try again with a corrected edit.

**Problem 8: Stream the SSE deltas to the terminal**

The Fireworks client already reassembles the SSE stream into a single response via `parse-sse-response`, but the user does not see the reply being written in real time. Modify `post-fireworks` so that, while `parse-sse-response` is accumulating the response, each `delta.content` fragment is also `display`ed to the terminal as it arrives. Consider how to do this without double-printing the final text (which the REPL also prints after the call returns), and whether the tool-call argument fragments should be hidden.
