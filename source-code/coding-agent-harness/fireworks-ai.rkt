#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; fireworks-ai.rkt -- Fireworks AI API client, session stats, chat helpers
;;; Racket port of py-coding-agent/fireworks_ai.py

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

;; ---------------------------------------------------------------------------
;; Constants

(define FIREWORKS-ENDPOINT "https://api.fireworks.ai/inference/v1/chat/completions")
(define FIREWORKS-MODEL (make-parameter "accounts/fireworks/models/deepseek-v4-flash-0731"))
(define MAX-TOKENS 32768)
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

;; deepseek-v4-flash-0731 pricing (Fireworks serverless):
;;   uncached input  $0.14/M
;;   cached input    $0.028/M  (80% cache discount)
;;   output          $0.28/M
(define PRICE-PER-M-PROMPT 0.14)
(define PRICE-PER-M-CACHED-PROMPT 0.028)
(define PRICE-PER-M-COMPLETION 0.28)

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

(define (prompt-cost tokens)
  (* tokens PRICE-PER-M-PROMPT (/ 1 1000000)))

(define (cached-cost tokens)
  (* tokens PRICE-PER-M-CACHED-PROMPT (/ 1 1000000)))

(define (completion-cost tokens)
  (* tokens PRICE-PER-M-COMPLETION (/ 1 1000000)))

;; Cached input tokens are reported by Fireworks in
;; usage.prompt_tokens_details.cached_tokens and are part of prompt_tokens;
;; bill them at the discounted rate and subtract them from the uncached pool.
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
                    (+ (unbox session-cached-tokens) (hash-ref details 'cached_tokens 0))))))))

;; ---------------------------------------------------------------------------
;; API key

(define (get-api-key)
  (define key (getenv "FIREWORKS_API_KEY"))
  (unless (and key (not (string=? key "")))
    (error 'fireworks-ai "FIREWORKS_API_KEY environment variable not set"))
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

;; ---------------------------------------------------------------------------
;; Low-level POST (streaming)

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

;; ---------------------------------------------------------------------------
;; chat / chat-with-tools -- thin wrappers over the shared provider-agnostic
;; loop in chat-loop.rkt (also used by ollama-ai.rkt).

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
