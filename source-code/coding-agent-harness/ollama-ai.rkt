#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; ollama-ai.rkt -- local Ollama API client (http://localhost:11434), session
;;; stats, chat helpers. Mirrors the interface of fireworks-ai.rkt so agent.rkt
;;; can swap providers with /provider or AGENT_PROVIDER=ollama.
;;;
;;; The HTTP style follows Racket-AI-book/source-code/llmapis/ollama_ai_local.rkt
;;; (http-easy POST to localhost:11434, no API key), but uses the /api/chat
;;; endpoint instead of /api/generate because the coding agent needs multi-turn
;;; message history and tool calling. Responses are normalized into the same
;;; shape post-fireworks returns so chat-loop.rkt works unchanged.

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

;; ---------------------------------------------------------------------------
;; Constants

(define OLLAMA-ENDPOINT "http://localhost:11434/api/chat")
(define OLLAMA-MODEL (make-parameter "nemotron-3.5-lightning:30b-mlx"))
;; Many local models (e.g. nemotron) are "thinking" models: with thinking on,
;; tokens go into a separate 'thinking field and 'content can come back empty
;; (especially with small num_predict caps). Off by default so all generated
;; tokens land in 'content; turn on with (OLLAMA-THINK #t) if wanted.
(define OLLAMA-THINK (make-parameter #f))
(define MAX-TOKENS 32768)
;; Non-streaming request: the whole generation must complete within this
;; window. Local models on large weights can be slow, so be generous.
(define OLLAMA-MAX-TIME 900)
(define OLLAMA-CONNECT-TIME 10)

;; ---------------------------------------------------------------------------
;; Session stats (thread-safe). Ollama reports prompt_eval_count / eval_count
;; on every /api/chat response. Local inference is free, so stats are
;; informational only -- estimated cost is always $0.

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

;; ---------------------------------------------------------------------------
;; Message/response normalization
;;
;; Outgoing: strip keys Ollama does not use (reasoning_content, tool_call_id,
;; ...) and convert assistant tool_calls arguments from JSON strings back into
;; objects, which is what Ollama's /api/chat expects.
;;
;; Incoming: Ollama returns (hash 'message (hash 'role 'content ['tool_calls])
;;                                'prompt_eval_count n 'eval_count n ...)
;; with tool_call arguments as *objects* and no call ids. Normalize to the
;; OpenAI-style shape that chat-loop.rkt and tools.rkt consume: arguments as a
;; JSON string and a synthesized 'id per call.

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

;; ---------------------------------------------------------------------------
;; Low-level POST (non-streaming)

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

;; ---------------------------------------------------------------------------
;; ollama-chat / ollama-chat-with-tools -- same signatures as fireworks-ai.rkt

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
