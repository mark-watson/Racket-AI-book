#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; mlx-serve.rkt -- local MLX (mlx_lm.server) API client
;;; (http://localhost:11434), session stats, chat helpers. Mirrors the
;;; interface of fireworks-ai.rkt so agent.rkt can swap providers with
;;; /provider or AGENT_PROVIDER=mlx.
;;;
;;; The endpoint is the OpenAI-compatible /v1/chat/completions route served by
;;; mlx_lm.server (and Ollama's own OpenAI shim). That protocol already matches
;;; what chat-loop.rkt consumes (choices[].message with optional tool_calls,
;;; usage.prompt_tokens / completion_tokens), so there is no message re-shaping
;;; here: we forward the OpenAI payload and return the response as-is. A Bearer
;;; key from the provider profile is passed along but ignored by a local server.

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
