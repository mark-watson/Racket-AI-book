#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; chat-loop.rkt -- provider-agnostic agentic tool-calling loop, shared by
;;; fireworks-ai.rkt and mlx-serve.rkt. The `post-fn` argument adapts an
;;; OpenAI-style chat-completions payload to a specific backend and returns a
;;; normalized response hash:
;;;   (hash 'choices (list (hash 'message <assistant msg> 'finish_reason ...))
;;;         'usage   (hash 'prompt_tokens n 'completion_tokens n ...))
;;; where <assistant msg> is (hash 'role "assistant" 'content <string>)
;;; plus, when the model called tools, 'tool_calls -- a list of
;;;   (hash 'id <string> 'type "function"
;;;         'function (hash 'name <string> 'arguments <json string>))

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
