#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; fireworks-ai.rkt -- Fireworks AI API client, session stats, chat helpers
;;; Racket port of py-coding-agent/fireworks_ai.py

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

;; ---------------------------------------------------------------------------
;; Constants

(define FIREWORKS-ENDPOINT "https://api.fireworks.ai/inference/v1/chat/completions")
(define FIREWORKS-MODEL (make-parameter "accounts/fireworks/models/deepseek-v4-flash-0731"))
(define MAX-TOKENS 32768)
(define DEBUG-LOG (make-parameter #f))
(define CURL-MAX-TIME 300)

(define PRICE-PER-M-PROMPT 0.22)
(define PRICE-PER-M-COMPLETION 0.88)

;; ---------------------------------------------------------------------------
;; Interrupt flag -- shared with approval.rkt and tools.rkt
;; Thread-safe via a semaphore.

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

(define (session-cost)
  (call-with-semaphore stats-sema
    (lambda ()
      (+ (* (unbox session-prompt-tokens) PRICE-PER-M-PROMPT (/ 1 1000000))
         (* (unbox session-completion-tokens) PRICE-PER-M-COMPLETION (/ 1 1000000))))))

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
  (displayln (format "  Estimated cost:    $~a  ($~a/M prompt, $~a/M completion)"
                     (~r cost #:precision 6)
                     (~r PRICE-PER-M-PROMPT #:precision 4)
                     (~r PRICE-PER-M-COMPLETION #:precision 4))))

(define (accumulate-usage data)
  (define usage (hash-ref data 'usage (hash)))
  (when (and (hash? usage) (not (hash-empty? usage)))
    (call-with-semaphore stats-sema
      (lambda ()
        (set-box! session-prompt-tokens
                  (+ (unbox session-prompt-tokens) (hash-ref usage 'prompt_tokens 0)))
        (set-box! session-completion-tokens
                  (+ (unbox session-completion-tokens) (hash-ref usage 'completion_tokens 0)))
        (set-box! session-total-tokens
                  (+ (unbox session-total-tokens) (hash-ref usage 'total_tokens 0)))
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
;; Low-level POST

(define (post-fireworks payload)
  (define api-key (get-api-key))
  (define headers
    (hash 'content-type "application/json"
          'accept "application/json"
          'authorization (string-append "Bearer " api-key)))
  (when (DEBUG-LOG)
    (displayln (format "[DEBUG] request: ~a" (jsexpr->string payload))))
  (define data
    (with-handlers ([exn:fail? (lambda (e) (error 'fireworks-ai "HTTP error: ~a" (exn-message e)))])
      (define resp
        (post FIREWORKS-ENDPOINT
              #:headers headers
              #:json payload))
      (define j (response-json resp))
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
;; chat : (listof hash) [#:model-id string ...] -> string

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

;; ---------------------------------------------------------------------------
;; Helpers for chat-with-tools

(define (without-dangling msgs)
  (if (and (not (null? msgs))
           (hash-has-key? (last msgs) 'tool_calls))
      (drop-right msgs 1)
      msgs))

;; ---------------------------------------------------------------------------
;; chat-with-tools : (listof hash) (listof string) ... -> (values string (listof hash))
;; Multi-turn agentic loop. Returns two values: final-text and final-messages.
;; Uses dynamic-require to avoid a circular dependency with tools.rkt.

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
       (values "(task interrupted by user)" (without-dangling (unbox current-messages)))]
      [(>= iter max-iterations)
       ;; Max iterations -- one final no-tools call for summary
       (if (task-interrupted?)
           (values "(task interrupted by user)" (without-dangling (unbox current-messages)))
           (let ()
             (define payload
               (hash 'model model-id
                     'max_tokens max-tokens
                     'temperature temperature
                     'messages (unbox current-messages)))
             (with-handlers ([exn:fail? (lambda (_) (values "(max tool iterations reached)" (unbox current-messages)))])
               (define data (post-fireworks payload))
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
       (define data (post-fireworks payload))
       (define msg (hash-ref (first (hash-ref data 'choices)) 'message))
       (define tool-calls (hash-ref msg 'tool_calls #f))
       (define content (hash-ref msg 'content ""))
       ;; Append the assistant message
       (set-box! current-messages (append (unbox current-messages) (list msg)))
       (cond
         [(task-interrupted?)
          (values "(task interrupted by user)" (without-dangling (unbox current-messages)))]
         [(and content tool-calls (not (string=? (string-trim content) "")))
          (displayln "")
          (displayln (string-trim content))
          (if (not tool-calls)
              (values (or content "(empty response from model)") (unbox current-messages))
              (let ()
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
                (loop (add1 iter))))]
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
