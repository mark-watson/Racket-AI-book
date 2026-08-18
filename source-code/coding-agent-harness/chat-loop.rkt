#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; chat-loop.rkt -- provider-agnostic agentic tool-calling loop, shared by
;;; fireworks-ai.rkt and ollama-ai.rkt. The `post-fn` argument adapts an
;;; OpenAI-style chat-completions payload to a specific backend and returns a
;;; normalized response hash:
;;;   (hash 'choices (list (hash 'message <assistant msg> 'finish_reason ...))
;;;         'usage   (hash 'prompt_tokens n 'completion_tokens n ...))
;;; where <assistant msg> is (hash 'role "assistant" 'content <string>)
;;; plus, when the model called tools, 'tool_calls -- a list of
;;;   (hash 'id <string> 'type "function"
;;;         'function (hash 'name <string> 'arguments <json string>))

(require racket/string
         "interrupt.rkt"
         "tools.rkt")

(provide chat*
         chat-with-tools*)

;; ---------------------------------------------------------------------------
;; Helpers

(define (without-dangling msgs)
  (if (and (not (null? msgs))
           (hash-has-key? (last msgs) 'tool_calls))
      (drop-right msgs 1)
      msgs))

;; ---------------------------------------------------------------------------
;; chat* : post-fn (listof hash) ... -> string

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

;; ---------------------------------------------------------------------------
;; chat-with-tools* : post-fn (listof hash) (listof string) ... -> (values string (listof hash))
;; Multi-turn agentic loop. Returns two values: final-text and final-messages.

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
       ;; Append the assistant message
       (set-box! current-messages (append (unbox current-messages) (list msg)))
       (cond
         [(task-interrupted?)
          (values "(task interrupted by user)" (without-dangling (unbox current-messages)))]
         [(and content tool-calls (not (string=? (string-trim content) "")))
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
