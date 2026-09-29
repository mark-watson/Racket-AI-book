#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; harness-config.rkt -- hierarchical JSON configuration for the coding
;;; harness, in the rough style of the Pi coding harness config.
;;;
;;; Two config layers are merged (local wins over global on conflicts):
;;;
;;;   Global:  ~/.coding_harness.json            (base configuration)
;;;   Local:   .local_coding_harness.json        (optional per-project override,
;;;                                             loaded from the current
;;;                                             directory at startup)
;;;
;;; Rough format (all sections optional):
;;;
;;; {
;;;   "default_provider": "mlx-local",
;;;   "providers": {
;;;     "mlx-local": {
;;;       "type": "mlx",
;;;       "endpoint": "http://localhost:11434/v1/chat/completions",
;;;       "model": "mlx-community/gemma-4-26B-A4B-it-OptiQ-4bit",
;;;       "generation": { "temperature": 0.6, "max_tokens": 32768 }
;;;     },
;;;     "mlx-cloud": {
;;;       "type": "mlx",
;;;       "endpoint": "https://example.com/v1/chat/completions",
;;;       "api_key_env": "MLX_API_KEY",
;;;       "model": "some-model",
;;;       "generation": { "temperature": 0.3 }
;;;     },
;;;     "fireworks": {
;;;       "type": "openai",
;;;       "endpoint": "https://api.fireworks.ai/inference/v1/chat/completions",
;;;       "api_key_env": "FIREWORKS_API_KEY",
;;;       "model": "accounts/fireworks/models/deepseek-v4p1-flash",
;;;       "generation": { "temperature": 0.6, "max_tokens": 32768 },
;;;       "pricing": { "input": 0.14, "cached_input": 0.028, "output": 0.28 }
;;;     },
;;;     "deepseek": {
;;;       "type": "openai",
;;;       "endpoint": "https://api.deepseek.com/v1/chat/completions",
;;;       "api_key_env": "DEEPSEEK_API_KEY",
;;;       "model": "deepseek-flash",
;;;       "generation": { "temperature": 0.6, "max_tokens": 32768 },
;;;       "pricing": { "input": 0.15, "cached_input": 0.003, "output": 0.60 }
;;;     }
;;;   },
;;;   "search": { "engine": "brave", "enabled": false },
;;;   "debug": false, "quiet": false, "plain": false
;;; }
;;; Provider "type" is either "mlx" (the local mlx-serve backend -- OpenAI-style
;;; /v1/chat/completions served by mlx_lm.server on localhost:11434, oMLX on port 8000,
;;; or sushi on port 12345) or "openai" (OpenAI-style chat completions; Fireworks.ai
;;; and any compatible endpoint). The type strings "ollama", "omlx", and "sushi"
;;; are also accepted and mapped to "mlx".
;;;
;;; Every provider-specific value lives here: endpoint, model, api_key_env,
;;; generation parameters, and the per-1M-token USD "pricing" rates (input,
;;; cached_input, output) that /tokens uses.  Nothing provider-specific is
;;; compiled into the Racket code, so adding or changing a provider needs no
;;; code change.  A profile with no "pricing" block reports token counts
;;; without a cost estimate rather than guessing a rate.
;;;
;;; Merge rules: nested hashes merge recursively, local keys override global
;;; keys; anything that is not a hash (strings, numbers, booleans, lists) is
;;; replaced wholesale by the local value when present.

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
