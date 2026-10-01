#lang racket

;;; llmapis.rkt -- uniform LLM API entry point for all providers in llmapis/
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Modeled on the Common Lisp uniform LLM API in
;;; loving-common-lisp/src/litelm (litelm.lisp, providers.lisp,
;;; messages.lisp): models are addressed as "provider/model-name" strings,
;;; messages and tool definitions use a Racket-friendly nested-list format
;;; (no JSON in user code), and one code path serves every provider.
;;;
;;; Providers with an OpenAI-compatible chat endpoint share one code path
;;; (OpenAI, Gemini via its OpenAI-compatible base URL, Mistral, DeepSeek,
;;; Fireworks AI, local Ollama). Anthropic (native Messages API) and local
;;; llama.cpp (llama-server /completion) have small dedicated adapters behind
;;; the same `llm-completion' / `llm-embedding' interface.
;;;
;;; Tools are Racket functions. Define them with `make-llm-tool', pass them
;;; to `llm-completion' via #:tools, then run what the model asks for with
;;; `execute-tool-calls' -- or let `llm-chat-with-tools' run the whole
;;; request/execute/reply loop for you:
;;;
;;;   (define (get-weather args)
;;;     (format "sunny and 22C in ~a" (hash-ref args 'location)))
;;;   (define weather-tool
;;;     (make-llm-tool "get_weather"
;;;                    "Get the current weather for a location"
;;;                    '(("location" "string" "City name, e.g. Paris"))
;;;                    get-weather))
;;;   (llm-response-content
;;;    (llm-chat-with-tools "ollama/qwen3:1.7b"
;;;                         "What is the weather in Paris?"
;;;                         (list weather-tool)))
;;;
;;; Provider-specific extras (web search, citations, ...) stay in the
;;; per-provider modules (openai.rkt, anthropic.rkt, gemini.rkt, ...);
;;; this module covers the uniform path: chat completion (with Racket
;;; function tools), embeddings, and provider routing.

(require net/http-easy
         json
         racket/string
         racket/list)

(provide
 ;; error hierarchy (mirrors litelm's conditions)
 (struct-out exn:fail:llm)
 (struct-out exn:fail:llm:api)
 (struct-out exn:fail:llm:authentication)
 (struct-out exn:fail:llm:rate-limit)
 (struct-out exn:fail:llm:not-found)
 (struct-out exn:fail:llm:context-window)
 llm-error
 raise-llm-error
 openai-max-tokens-fallback?
 ;; provider registry and routing
 (struct-out llm-provider)
 define-provider
 find-provider
 llm-providers
 parse-model
 provider-api-key
 ;; messages
 (struct-out llm-message)
 normalize-messages
 llm-user-message
 llm-system-message
 llm-assistant-message
 llm-tool-message
 llm-translate-messages
 llm-translate-messages-anthropic
 ;; tools (Racket functions as tools)
 (struct-out llm-param)
 (struct-out llm-tool)
 (struct-out llm-tool-call)
 (struct-out llm-tool-result)
 make-llm-tool
 make-llm-tool-call
 llm-translate-tools
 llm-translate-tools-anthropic
 execute-tool-calls
 ;; responses and entry points
 (struct-out llm-response)
 llm-parse-openai-response
 llm-parse-anthropic-response
 llm-completion
 llm-ask
 llm-embedding
 llm-chat-with-tools)

;;; ---------------------------------------------------------------------------
;;; Error hierarchy (mirrors litelm's condition hierarchy)
;;;
;;;   exn:fail:llm
;;;   └── exn:fail:llm:api  (fields: status body)
;;;       ├── exn:fail:llm:authentication      401, 403
;;;       ├── exn:fail:llm:rate-limit          429
;;;       ├── exn:fail:llm:not-found           404
;;;       └── exn:fail:llm:context-window      400 mentioning "context"

(struct exn:fail:llm exn:fail () #:transparent)
(struct exn:fail:llm:api exn:fail:llm (status body) #:transparent)
(struct exn:fail:llm:authentication exn:fail:llm:api () #:transparent)
(struct exn:fail:llm:rate-limit exn:fail:llm:api () #:transparent)
(struct exn:fail:llm:not-found exn:fail:llm:api () #:transparent)
(struct exn:fail:llm:context-window exn:fail:llm:api () #:transparent)

(define (llm-error fmt . args)
  "Raise a generic uniform-API error (no HTTP status involved)."
  (raise (exn:fail:llm (apply format fmt args)
                       (current-continuation-marks))))

(define (raise-llm-error status body)
  "Map an HTTP failure status to the matching exn:fail:llm subtype."
  (define body-str (if (string? body) body (format "~a" (or body ""))))
  (define msg (format "LLM API error ~a: ~a" status body-str))
  (define marks (current-continuation-marks))
  (cond [(member status '(401 403))
         (raise (exn:fail:llm:authentication msg marks status body-str))]
        [(equal? status 429)
         (raise (exn:fail:llm:rate-limit msg marks status body-str))]
        [(equal? status 404)
         (raise (exn:fail:llm:not-found msg marks status body-str))]
        [(and (equal? status 400)
              (regexp-match? #rx"(?i:context)" body-str))
         (raise (exn:fail:llm:context-window msg marks status body-str))]
        [else
         (raise (exn:fail:llm:api msg marks status body-str))]))

;;; ---------------------------------------------------------------------------
;;; Provider registry and "provider/model-name" routing (cf. providers.lisp)

(struct llm-provider (name base-url env-keys requires-key? kind)
  #:transparent)
;; kind is one of 'openai-compatible, 'anthropic, 'llama-cpp.

(define provider-table (make-hash))

(define (define-provider name base-url
                         #:env-keys [env-keys '()]
                         #:requires-key? [requires-key? #t]
                         #:kind [kind 'openai-compatible])
  "Register a provider. NAME is a symbol or string, BASE-URL an
OpenAI-compatible base URL (or the native base for 'anthropic /
'llama-cpp kinds), ENV-KEYS environment variable name(s) tried in order."
  (define sym (if (symbol? name) name (string->symbol name)))
  (define (key->string k) (if (symbol? k) (symbol->string k) k))
  (define keys
    (cond [(or (string? env-keys) (symbol? env-keys))
           (list (key->string env-keys))]
          [(list? env-keys) (map key->string env-keys)]
          [else (llm-error "define-provider: bad #:env-keys value: ~s"
                           env-keys)]))
  (hash-set! provider-table sym
             (llm-provider sym base-url keys requires-key? kind)))

(define-provider 'openai "https://api.openai.com/v1"
  #:env-keys '("OPENAI_API_KEY" "OPENAI_KEY"))

(define-provider 'gemini "https://generativelanguage.googleapis.com/v1beta/openai"
  #:env-keys '("GEMINI_API_KEY" "GOOGLE_API_KEY"))

(define-provider 'mistral "https://api.mistral.ai/v1"
  #:env-keys '("MISTRAL_API_KEY"))

(define-provider 'deepseek "https://api.deepseek.com/v1"
  #:env-keys '("DEEPSEEK_API_KEY"))

(define-provider 'fireworks-ai "https://api.fireworks.ai/inference/v1"
  #:env-keys '("FIREWORKS_API_KEY"))

(define-provider 'ollama "http://localhost:11434/v1"
  #:env-keys '()
  #:requires-key? #f)

(define-provider 'anthropic "https://api.anthropic.com/v1"
  #:env-keys '("ANTHROPIC_API_KEY")
  #:kind 'anthropic)

(define-provider 'llama-local "http://localhost:8080"
  #:env-keys '()
  #:requires-key? #f
  #:kind 'llama-cpp)

(define (find-provider name)
  "Look up a provider by symbol or string; raise listing known providers."
  (define sym (if (symbol? name) name (string->symbol name)))
  (or (hash-ref provider-table sym #f)
      (llm-error "Unknown provider ~s. Known providers: ~a"
                 name (llm-providers))))

(define (llm-providers)
  "Sorted list of registered provider name symbols."
  (sort (hash-keys provider-table) symbol<?))

(define (parse-model model #:provider [provider #f])
  "Split a \"provider/model-name\" string into (values provider model-name).
Model names may themselves contain slashes (e.g. fireworks-ai/... paths).
A #:provider keyword argument overrides the string prefix."
  (cond [provider
         (values (find-provider provider) model)]
        [(and (string? model)
              (regexp-match #rx"^([^/]+)/(.+)$" model))
         => (lambda (m)
              (values (find-provider (string->symbol (cadr m)))
                      (caddr m)))]
        [else
         (llm-error
          "Model ~s must be of the form \"provider/model-name\"" model)]))

(define (provider-api-key provider explicit-key)
  "Explicit key wins; otherwise scan the provider's env vars in order.
Returns #f for keyless local providers."
  (or explicit-key
      (for/or ([var (in-list (llm-provider-env-keys provider))])
        (define v (getenv var))
        (and v (> (string-length v) 0) v))
      (if (llm-provider-requires-key? provider)
          (llm-error
           "No API key for provider ~a. Pass #:api-key or set one of ~a"
           (llm-provider-name provider)
           (llm-provider-env-keys provider))
          #f)))

;;; ---------------------------------------------------------------------------
;;; Messages: Racket-friendly format <-> wire format (cf. messages.lisp)
;;;
;;; A message is (list role content [option value] ...) where role is a
;;; symbol or string ('system 'user 'assistant 'tool), content is a string
;;; (or #f for assistant messages that only carry tool calls), and options
;;; are #:tool-calls (list of llm-tool-call), #:tool-call-id (string, for
;;; role 'tool), and #:name. A plain string is shorthand for a user message.

(struct llm-message (role content tool-calls tool-call-id name)
  #:transparent)
;; role: "system" | "user" | "assistant" | "tool" (string).

(define valid-roles '("system" "user" "assistant" "tool"))

(define (role->string r)
  (define s (cond [(symbol? r) (symbol->string r)]
                  [(string? r) r]
                  [else (llm-error "message role must be a symbol or string: ~s"
                                   r)]))
  (define down (string-downcase s))
  (unless (member down valid-roles)
    (llm-error "unknown message role ~s (expected one of ~a)" r valid-roles))
  down)

(define (llm-user-message content)
  (llm-message "user" content '() #f #f))

(define (llm-system-message content)
  (llm-message "system" content '() #f #f))

(define (normalize-tool-call-list tool-calls)
  "Accept llm-tool-call structs or (list id name args) specs, where args
is a hash or a JSON object string."
  (for/list ([tc (in-list tool-calls)])
    (cond [(llm-tool-call? tc) tc]
          [(and (list? tc) (= (length tc) 3))
           (make-llm-tool-call (first tc) (second tc) (third tc))]
          [else (llm-error "bad tool call spec (want llm-tool-call or (id name args)): ~s"
                           tc)])))

(define (normalize-one-message m)
  (cond [(string? m) (llm-user-message m)]
        [(llm-message? m) m]
        [(and (list? m) (>= (length m) 2))
         (define role (role->string (first m)))
         (define content (second m))
         (unless (or (string? content) (not content))
           (llm-error "message content must be a string or #f: ~s" m))
         (define-values (tool-calls tool-call-id name)
           (parse-message-opts (cddr m)))
         (when (and (equal? role "tool") (not tool-call-id))
           (llm-error "role 'tool messages need #:tool-call-id: ~s" m))
         (when (and (equal? role "assistant")
                    (not content) (null? tool-calls))
           (llm-error "assistant messages need content or #:tool-calls: ~s" m))
         (llm-message role content tool-calls tool-call-id name)]
        [else (llm-error "bad message (want string, llm-message, or (role content ...)): ~s"
                         m)]))

(define (parse-message-opts opts)
  (let loop ([o opts] [tool-calls '()] [tool-call-id #f] [name #f])
    (cond [(null? o) (values tool-calls tool-call-id name)]
          [(null? (cdr o))
           (llm-error "dangling message option: ~s" (car o))]
          [else
           (define k (car o))
           (define v (cadr o))
           (cond [(eq? k '#:tool-calls)
                  (loop (cddr o) (normalize-tool-call-list v) tool-call-id name)]
                 [(eq? k '#:tool-call-id)
                  (loop (cddr o) tool-calls v name)]
                 [(eq? k '#:name)
                  (loop (cddr o) tool-calls tool-call-id v)]
                 [else (llm-error "unknown message option: ~s" k)])])))

(define (normalize-messages messages)
  "A string (single user message), one llm-message, or a list of strings /
lists / llm-messages  ->  (listof llm-message)."
  (cond [(or (string? messages) (llm-message? messages))
         (list (normalize-one-message messages))]
        [(list? messages)
         (map normalize-one-message messages)]
        [else (llm-error "bad messages value: ~s" messages)]))

;;; ---- OpenAI-compatible wire format ----

(define (tool-call->openai tc)
  (hash 'id (llm-tool-call-id tc)
        'type "function"
        'function (hash 'name (llm-tool-call-name tc)
                        'arguments (or (llm-tool-call-arguments-raw tc)
                                       (jsexpr->string
                                        (tool-args->jsexpr
                                         (llm-tool-call-arguments tc)))))))

(define (tool-args->jsexpr args)
  (cond [(hash? args) args]
        [(not args) (hash)]
        [else (llm-error "tool call arguments must be a hash: ~s" args)]))

(define (llm-message->openai m)
  (define h (hash 'role (llm-message-role m)))
  (define h1 (if (llm-message-content m)
                 (hash-set h 'content (llm-message-content m))
                 h))
  (define h2 (if (pair? (llm-message-tool-calls m))
                 (hash-set h1 'tool_calls
                           (map tool-call->openai (llm-message-tool-calls m)))
                 h1))
  (define h3 (if (llm-message-tool-call-id m)
                 (hash-set h2 'tool_call_id (llm-message-tool-call-id m))
                 h2))
  (if (llm-message-name m)
      (hash-set h3 'name (llm-message-name m))
      h3))

(define (llm-translate-messages messages)
  "Normalize MESSAGES and render OpenAI-compatible wire hashes."
  (map llm-message->openai (normalize-messages messages)))

;;; ---- Anthropic wire format (system is top-level; tool results are blocks) ----

(define (tool-call->anthropic-block tc)
  (hash 'type "tool_use"
        'id (llm-tool-call-id tc)
        'name (llm-tool-call-name tc)
        'input (tool-args->jsexpr (llm-tool-call-arguments tc))))

(define (llm-message->anthropic-blocks m)
  ;; Assistant message -> list of text/tool_use blocks.
  (append (if (llm-message-content m)
              (list (hash 'type "text" 'text (llm-message-content m)))
              '())
          (map tool-call->anthropic-block (llm-message-tool-calls m))))

(define (merge-anthropic-wire wire)
  "Merge consecutive same-role wire messages (Anthropic rejects repeats)."
  (let loop ([rest wire] [acc '()])
    (cond [(null? rest) (reverse acc)]
          [(and (pair? acc)
                (equal? (hash-ref (car acc) 'role)
                        (hash-ref (car rest) 'role)))
           (define prev (car acc))
           (define next (car rest))
           (define (content-blocks h)
             (define c (hash-ref h 'content))
             (if (string? c) (list (hash 'type "text" 'text c)) c))
           (loop (cdr rest)
                 (cons (hash 'role (hash-ref prev 'role)
                             'content (append (content-blocks prev)
                                              (content-blocks next)))
                       (cdr acc)))]
          [else (loop (cdr rest) (cons (car rest) acc))])))

(define (llm-translate-messages-anthropic messages #:system [extra-system #f])
  "Normalize MESSAGES -> (values system-string-or-#f wire-messages)."
  (define normalized (normalize-messages messages))
  (define systems
    (append (filter-map (lambda (m)
                          (and (equal? (llm-message-role m) "system")
                               (llm-message-content m)))
                        normalized)
            (if extra-system (list extra-system) '())))
  (define system-str (and (pair? systems) (string-join systems "\n\n")))
  (define wire
    (for/list ([m (in-list normalized)]
               #:unless (equal? (llm-message-role m) "system"))
      (cond [(equal? (llm-message-role m) "tool")
             (hash 'role "user"
                   'content (list (hash 'type "tool_result"
                                        'tool_use_id (llm-message-tool-call-id m)
                                        'content (or (llm-message-content m) ""))))]
            [(equal? (llm-message-role m) "assistant")
             (define blocks (llm-message->anthropic-blocks m))
             (when (null? blocks)
               (llm-error "assistant message has no content or tool calls"))
             (hash 'role "assistant" 'content blocks)]
            [else
             (hash 'role (llm-message-role m)
                   'content (or (llm-message-content m) ""))])))
  (values system-str (merge-anthropic-wire wire)))

;;; ---------------------------------------------------------------------------
;;; Tools: Racket functions the model can call (cf. translate-tools)
;;;
;;; A tool wraps a Racket procedure of one argument: a hash of decoded JSON
;;; arguments with symbol keys. The procedure's return value is formatted
;;; with ~a and sent back to the model. Errors (unknown tool, missing
;;; argument, handler exception) become "Error: ..." strings -- feedback
;;; for the model, never an uncaught exception (same policy as
;;; coding-agent-harness/tools.rkt).

(struct llm-param (name type description required? enum) #:transparent)
(struct llm-tool (name description parameters proc) #:transparent)
(struct llm-tool-call (id name arguments arguments-raw) #:transparent)
;; arguments: hash with symbol keys; arguments-raw: the original JSON
;; string when the call arrived over the wire (#f when built locally).
(struct llm-tool-result (call-id name result) #:transparent)

(define (parse-param-spec spec)
  (cond [(llm-param? spec) spec]
        [(and (list? spec) (>= (length spec) 3))
         (define name (let ([n (first spec)])
                        (cond [(symbol? n) (symbol->string n)]
                              [(string? n) n]
                              [else (llm-error "bad parameter name: ~s" n)])))
         (define type (let ([t (second spec)])
                        (cond [(symbol? t) (symbol->string t)]
                              [(string? t) t]
                              [else (llm-error "bad parameter type: ~s" t)])))
         (define desc (third spec))
         (unless (string? desc)
           (llm-error "parameter description must be a string: ~s" spec))
         (let loop ([o (cdddr spec)] [required? #t] [enum #f])
           (cond [(null? o) (llm-param name type desc required? enum)]
                 [(null? (cdr o))
                  (llm-error "dangling parameter option: ~s" (car o))]
                 [else
                  (define k (car o))
                  (define v (cadr o))
                  (cond [(eq? k '#:required)
                         (loop (cddr o) (and v #t) enum)]
                        [(eq? k '#:enum)
                         (loop (cddr o) required? v)]
                        [else (llm-error "unknown parameter option: ~s" k)])]))]
        [else (llm-error
               "bad parameter spec (want (name type desc [#:required b] [#:enum l])): ~s"
               spec)]))

(define (make-llm-tool name description params proc)
  "Wrap a Racket function as an LLM tool. PROC takes one hash argument
(symbol keys) and returns any value (formatted with ~a).
PARAMS entries: (list name type desc) -- required by default -- or with
#:required #f / #:enum '(...). Names may be symbols or strings."
  (define n (cond [(symbol? name) (symbol->string name)]
                  [(string? name) name]
                  [else (llm-error "tool name must be a symbol or string: ~s"
                                   name)]))
  (unless (string? description)
    (llm-error "tool description must be a string: ~s" description))
  (unless (procedure? proc)
    (llm-error "tool handler must be a procedure: ~s" proc))
  (llm-tool n description (map parse-param-spec params) proc))

(define (parse-tool-args args)
  "Normalize tool arguments to (values args-hash args-raw-or-#f)."
  (cond [(hash? args) (values args #f)]
        [(string? args)
         (define parsed
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (string->jsexpr args)))
         (if (hash? parsed)
             (values parsed args)
             (values (hash) args))]
        [(not args) (values (hash) #f)]
        [else (llm-error "tool arguments must be a hash or JSON string: ~s"
                         args)]))

(define (make-llm-tool-call id name [args (hash)])
  "Build an llm-tool-call; ARGS is a hash (local call) or JSON string (wire)."
  (define n (if (symbol? name) (symbol->string name) name))
  (define-values (args-hash args-raw) (parse-tool-args args))
  (llm-tool-call (format "~a" id) n args-hash args-raw))

(define (llm-param->property p)
  (define h (hash 'type (llm-param-type p)
                  'description (llm-param-description p)))
  (if (llm-param-enum p)
      (hash-set h 'enum (llm-param-enum p))
      h))

(define (llm-tool->schema tool)
  "Shared JSON-schema fragment for one tool: (values properties required)."
  (define props (make-hash))
  (for ([p (in-list (llm-tool-parameters tool))])
    (hash-set! props
               (string->symbol (llm-param-name p))
               (llm-param->property p)))
  (values props
          (filter-map (lambda (p)
                        (and (llm-param-required? p) (llm-param-name p)))
                      (llm-tool-parameters tool))))

(define (normalize-tools tools)
  (cond [(hash? tools) (hash-values tools)]
        [(list? tools)
         (for/list ([t (in-list tools)])
           (if (llm-tool? t)
               t
               (llm-error "bad tool (want llm-tool): ~s" t)))]
        [else (llm-error "tools must be a list or hash of llm-tool: ~s" tools)]))

(define (llm-translate-tools tools)
  "Render TOOLS (list/hash of llm-tool) as OpenAI-compatible wire hashes."
  (for/list ([tool (in-list (normalize-tools tools))])
    (define-values (props required) (llm-tool->schema tool))
    (hash 'type "function"
          'function (hash 'name (llm-tool-name tool)
                          'description (llm-tool-description tool)
                          'parameters (hash 'type "object"
                                            'properties props
                                            'required required)))))

(define (llm-translate-tools-anthropic tools)
  "Render TOOLS as Anthropic custom-tool wire hashes."
  (for/list ([tool (in-list (normalize-tools tools))])
    (define-values (props required) (llm-tool->schema tool))
    (hash 'name (llm-tool-name tool)
          'description (llm-tool-description tool)
          'input_schema (hash 'type "object"
                              'properties props
                              'required required))))

(define (execute-tool-calls tools tool-calls)
  "Run TOOL-CALLS (llm-tool-call structs) against TOOLS (list or name->tool
hash of llm-tool). Returns (listof llm-tool-result); failures become
\"Error: ...\" result strings for the model."
  (define registry
    (if (hash? tools)
        tools
        (for/hash ([t (in-list (normalize-tools tools))])
          (values (llm-tool-name t) t))))
  (for/list ([call (in-list tool-calls)])
    (unless (llm-tool-call? call)
      (llm-error "bad tool call (want llm-tool-call): ~s" call))
    (define id (llm-tool-call-id call))
    (define name (llm-tool-call-name call))
    (define tool (hash-ref registry name #f))
    (define args (llm-tool-call-arguments call))
    (define result
      (cond [(not tool)
             (format "Error: unknown tool: ~a" name)]
            [(and (llm-tool-call-arguments-raw call)
                  (not (hash? (string->jsexpr-safe
                               (llm-tool-call-arguments-raw call)))))
             (format "Error: invalid JSON arguments for tool '~a'. Received: ~a"
                     name (llm-tool-call-arguments-raw call))]
            [else
             (define missing
               (for/list ([p (in-list (llm-tool-parameters tool))]
                          #:when (and (llm-param-required? p)
                                      (not (hash-has-key?
                                            args
                                            (string->symbol
                                             (llm-param-name p))))))
                 (llm-param-name p)))
             (cond [(pair? missing)
                    (format "Error: tool '~a' missing required argument(s): ~a"
                            name (string-join missing ", "))]
                   [else
                    (with-handlers
                        ([exn:fail?
                          (lambda (e)
                            (format "Error: tool '~a' raised: ~a"
                                    name (exn-message e)))])
                      (define v ((llm-tool-proc tool) args))
                      (cond [(void? v) ""]
                            [(string? v) v]
                            [else (format "~a" v)]))])]))
    (llm-tool-result id name result)))

(define (string->jsexpr-safe s)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define v (string->jsexpr s))
    (and (hash? v) v)))

;;; Message constructors for multi-turn tool loops.

(define (llm-assistant-message response)
  "Build the assistant message (with tool calls) continuing from RESPONSE."
  (unless (llm-response? response)
    (llm-error "want llm-response: ~s" response))
  (llm-message "assistant"
               (llm-response-content response)
               (llm-response-tool-calls response)
               #f #f))

(define (llm-tool-message result)
  "Build a role-'tool message from an llm-tool-result."
  (unless (llm-tool-result? result)
    (llm-error "want llm-tool-result: ~s" result))
  (llm-message "tool"
               (llm-tool-result-result result)
               '()
               (llm-tool-result-call-id result)
               #f))

;;; ---------------------------------------------------------------------------
;;; Responses and entry points (cf. litelm.lisp COMPLETION / EMBEDDING)

(struct llm-response (content tool-calls finish-reason model usage raw)
  #:transparent)
;; content: string or #f (model only made tool calls).
;; tool-calls: (listof llm-tool-call).
;; usage: hash with 'prompt-tokens 'completion-tokens 'total-tokens, or #f.

(define (json-null->false v)
  (if (eq? v 'null) #f v))

(define (response-text-blocks content)
  "Some providers answer with a content-block list instead of a string."
  (cond [(string? content) content]
        [(list? content)
         (string-join
          (filter-map (lambda (b)
                        (and (hash? b) (hash-ref b 'text #f)))
                      content)
          "")]
        [else #f]))

(define (llm-parse-openai-response json model-name)
  "Parse an OpenAI-compatible /chat/completions body into an llm-response."
  (unless (hash? json)
    (llm-error "bad chat response (want JSON object): ~s" json))
  (when (hash-has-key? json 'error)
    (llm-error "LLM API error: ~a" (hash-ref json 'error)))
  (define choices (hash-ref json 'choices '()))
  (when (null? choices)
    (llm-error "LLM API response has no choices: ~s" json))
  (define choice (first choices))
  (define message (hash-ref choice 'message (hash)))
  (define content
    (response-text-blocks
     (json-null->false (hash-ref message 'content #f))))
  (define tool-calls
    (for/list ([tc (in-list (hash-ref message 'tool_calls '()))])
      (define fn (hash-ref tc 'function (hash)))
      (define args (hash-ref fn 'arguments "{}"))
      (make-llm-tool-call (hash-ref tc 'id "")
                          (hash-ref fn 'name "")
                          (if (hash? args) args (format "~a" args)))))
  (define raw-usage (hash-ref json 'usage #f))
  (define usage
    (and (hash? raw-usage)
         (hash 'prompt-tokens (hash-ref raw-usage 'prompt_tokens #f)
               'completion-tokens (hash-ref raw-usage 'completion_tokens #f)
               'total-tokens (hash-ref raw-usage 'total_tokens #f))))
  (llm-response content
                tool-calls
                (json-null->false (hash-ref choice 'finish_reason #f))
                (hash-ref json 'model model-name)
                usage
                json))

(define (llm-parse-anthropic-response json model-name)
  "Parse an Anthropic /messages body into an llm-response."
  (unless (hash? json)
    (llm-error "bad Anthropic response (want JSON object): ~s" json))
  (when (hash-has-key? json 'error)
    (llm-error "Anthropic API error: ~a" (hash-ref json 'error)))
  (define blocks (hash-ref json 'content '()))
  (define texts
    (filter-map (lambda (b)
                  (and (hash? b)
                       (equal? (hash-ref b 'type "") "text")
                       (hash-ref b 'text #f)))
                blocks))
  (define tool-calls
    (filter-map (lambda (b)
                  (and (hash? b)
                       (equal? (hash-ref b 'type "") "tool_use")
                       (let ([input (hash-ref b 'input (hash))])
                         (llm-tool-call
                          (format "~a" (hash-ref b 'id ""))
                          (format "~a" (hash-ref b 'name ""))
                          (if (hash? input) input (hash))
                          (and (hash? input)
                               (with-handlers ([exn:fail?
                                                (lambda (_) #f)])
                                 (jsexpr->string input)))))))
                blocks))
  (define raw-usage (hash-ref json 'usage #f))
  (define usage
    (and (hash? raw-usage)
         (let ([in (hash-ref raw-usage 'input_tokens #f)]
               [out (hash-ref raw-usage 'output_tokens #f)])
           (hash 'prompt-tokens in
                 'completion-tokens out
                 'total-tokens (and in out (+ in out))))))
  (llm-response (and (pair? texts) (string-join texts ""))
                tool-calls
                (json-null->false (hash-ref json 'stop_reason #f))
                (hash-ref json 'model model-name)
                usage
                json))

;;; ---- HTTP ----

(define (response-body-string resp)
  (with-handlers ([exn:fail? (lambda (_) "")])
    (define body (response-body resp))
    (cond [(bytes? body) (bytes->string/utf-8 body #\?)]
          [(string? body) body]
          [else ""])))

(define (llm-post-json url headers payload)
  "POST PAYLOAD as JSON; raise the mapped exn:fail:llm subtype on HTTP error."
  (define resp (post url #:headers headers #:json payload))
  (define code (response-status-code resp))
  (if (and (>= code 200) (< code 300))
      (response-json resp)
      (raise-llm-error code (response-body-string resp))))

(define (bearer-headers key extra)
  (define h (if key
                (hasheq 'authorization (string-append "Bearer " key))
                (hasheq)))
  (for/fold ([h h]) ([(k v) (in-hash extra)])
    (hash-set h (if (symbol? k) k (string->symbol k)) v)))

;;; ---- OpenAI-compatible chat (openai, gemini, mistral, deepseek,
;;;      fireworks-ai, ollama) ----

(define (openai-max-tokens-fallback? err payload)
  "True when ERR is a 400 rejection asking for 'max_completion_tokens
instead of the 'max_tokens in PAYLOAD (newer OpenAI models). The retry
then sends 'max_completion_tokens; the swapped payload no longer carries
'max_tokens, so at most one retry happens."
  (and (exn:fail:llm:api? err)
       (equal? (exn:fail:llm:api-status err) 400)
       (hash-has-key? payload 'max_tokens)
       (let ([body (exn:fail:llm:api-body err)])
         (and (string? body)
              (if (regexp-match? #rx"max_completion_tokens" body) #t #f)))))

(define (openai-send url headers payload model-name)
  (with-handlers ([exn:fail:llm:api?
                   (lambda (e)
                     (if (openai-max-tokens-fallback? e payload)
                         (openai-send
                          url headers
                          (hash-set (hash-remove payload 'max_tokens)
                                    'max_completion_tokens
                                    (hash-ref payload 'max_tokens))
                          model-name)
                         (raise e)))])
    (llm-parse-openai-response (llm-post-json url headers payload)
                               model-name)))

(define (openai-tool-choice tc)
  (case tc
    [(auto) "auto"] [(none) "none"] [(required) "required"]
    [(#f) #f]
    [else (llm-error "bad #:tool-choice (want 'auto 'none 'required or #f): ~s"
                     tc)]))

(define (maybe-set h k v)
  (if v (hash-set h k v) h))

(define (llm-completion-openai provider model-name wire-messages
                               #:tools [tools '()]
                               #:tool-choice [tool-choice 'auto]
                               #:temperature [temperature #f]
                               #:max-tokens [max-tokens #f]
                               #:top-p [top-p #f]
                               #:api-key [api-key #f]
                               #:api-base [api-base #f]
                               #:extra-headers [extra-headers (hash)])
  (define key (provider-api-key provider api-key))
  (define base (or api-base (llm-provider-base-url provider)))
  (define payload
    (maybe-set
     (maybe-set
      (maybe-set
       (maybe-set
        (maybe-set (hash 'model model-name
                         'messages wire-messages
                         'stream #f)
                   'tools (and (pair? tools) (llm-translate-tools tools)))
        'tool_choice (and (pair? tools)
                          (openai-tool-choice tool-choice)))
        'temperature temperature)
       'max_tokens max-tokens)
      'top_p top-p))
  (openai-send (string-append base "/chat/completions")
               (bearer-headers key extra-headers)
               payload
               model-name))

;;; ---- Anthropic native chat ----

(define (anthropic-tool-choice tc)
  (case tc
    [(auto) (hash 'type "auto")]
    [(required) (hash 'type "any")]
    [(none) (hash 'type "none")]
    [(#f) #f]
    [else (llm-error "bad #:tool-choice (want 'auto 'none 'required or #f): ~s"
                     tc)]))

(define (llm-completion-anthropic provider model-name messages
                                 #:tools [tools '()]
                                 #:tool-choice [tool-choice 'auto]
                                 #:max-tokens [max-tokens #f]
                                 #:system [system #f]
                                 #:api-key [api-key #f]
                                 #:api-base [api-base #f]
                                 #:extra-headers [extra-headers (hash)])
  ;; temperature / top-p intentionally omitted: keep the uniform surface to
  ;; options every provider honors the same way.
  (define key (provider-api-key provider api-key))
  (when (not key)
    (llm-error "Anthropic requires an API key"))
  (define base (or api-base (llm-provider-base-url provider)))
  (define normalized-tools (normalize-tools tools))
  (define-values (system-str wire)
    (llm-translate-messages-anthropic messages #:system system))
  (define payload
    (maybe-set
     (maybe-set
      (maybe-set (hash 'model model-name
                       'max_tokens (or max-tokens 1024)
                       'messages wire)
                 'system system-str)
      'tools (and (pair? normalized-tools)
                  (llm-translate-tools-anthropic normalized-tools)))
     'tool_choice (and (pair? normalized-tools)
                       (anthropic-tool-choice tool-choice))))
  (define headers
    (hash-set* (bearer-headers #f extra-headers)
               'x-api-key key
               'anthropic-version "2023-06-01"))
  (llm-parse-anthropic-response
   (llm-post-json (string-append base "/messages") headers payload)
   model-name))

;;; ---- Local llama.cpp chat (llama-server /completion; text only) ----

(define (llama-flatten-message m)
  (define role (llm-message-role m))
  (define body
    (cond [(llm-message-content m) (llm-message-content m)]
          [(pair? (llm-message-tool-calls m))
           (string-join
            (map (lambda (tc)
                   (format "call ~a(~a)" (llm-tool-call-name tc)
                           (or (llm-tool-call-arguments-raw tc) "")))
                 (llm-message-tool-calls m))
            "\n")]
          [else ""]))
  (format "~a: ~a"
          (cond [(equal? role "system") "System"]
                [(equal? role "assistant") "Assistant"]
                [(equal? role "tool") "Tool result"]
                [else "User"])
          body))

(define (llm-completion-llama provider model-name messages
                              #:max-tokens [max-tokens #f]
                              #:system [system #f]
                              #:api-base [api-base #f])
  (define base (or api-base (llm-provider-base-url provider)))
  (define normalized (normalize-messages messages))
  (define prompt
    (string-join
     (append (if system (list (format "System: ~a" system)) '())
             (map llama-flatten-message normalized)
             '("Assistant:"))
     "\n\n"))
  (define json
    (llm-post-json (string-append base "/completion")
                   (hasheq)
                   (hash 'prompt prompt
                         'n_predict (or max-tokens 256)
                         'stream #f)))
  (llm-response (hash-ref json 'content "")
                '()
                (hash-ref json 'stop_type "eos")
                model-name
                #f
                json))

;;; ---- Uniform entry points ----

(define (llm-completion model
                        #:messages [messages "Hello!"]
                        #:tools [tools '()]
                        #:tool-choice [tool-choice 'auto]
                        #:temperature [temperature #f]
                        #:max-tokens [max-tokens #f]
                        #:top-p [top-p #f]
                        #:system [system #f]
                        #:provider [provider #f]
                        #:api-key [api-key #f]
                        #:api-base [api-base #f]
                        #:extra-headers [extra-headers (hash)])
  "Send a chat completion to MODEL, a \"provider/model-name\" string.

MESSAGES is a string or a list of (role content ...) messages. TOOLS is a
list (or name->tool hash) of llm-tool -- Racket functions the model may
call. Tool calls are RETURNED in the response, not executed; use
`execute-tool-calls' (once) or `llm-chat-with-tools' (full loop).

Returns an llm-response; see `llm-response-content' and
`llm-response-tool-calls'."
  (define-values (prov model-name) (parse-model model #:provider provider))
  (case (llm-provider-kind prov)
    [(openai-compatible)
     (define wire (llm-translate-messages messages))
     (define with-system
       (if system (cons (hash 'role "system" 'content system) wire) wire))
     (llm-completion-openai prov model-name with-system
                            #:tools tools
                            #:tool-choice tool-choice
                            #:temperature temperature
                            #:max-tokens max-tokens
                            #:top-p top-p
                            #:api-key api-key
                            #:api-base api-base
                            #:extra-headers extra-headers)]
    [(anthropic)
     (when (or temperature top-p)
       (llm-error (string-append
                   "Anthropic via the uniform API does not take "
                   "#:temperature/#:top-p (use anthropic.rkt directly)")))
     (llm-completion-anthropic prov model-name messages
                               #:tools tools
                               #:tool-choice tool-choice
                               #:max-tokens max-tokens
                               #:system system
                               #:api-key api-key
                               #:api-base api-base
                               #:extra-headers extra-headers)]
    [(llama-cpp)
     (when (pair? (normalize-tools tools))
       (llm-error "provider llama-local (llama.cpp /completion) has no tool API"))
     (llm-completion-llama prov model-name messages
                           #:max-tokens max-tokens
                           #:system system
                           #:api-base api-base)]
    [else (llm-error "unsupported provider kind: ~s"
                     (llm-provider-kind prov))]))

(define (llm-ask model prompt
                 #:system [system #f]
                 #:temperature [temperature #f]
                 #:max-tokens [max-tokens #f]
                 #:provider [provider #f]
                 #:api-key [api-key #f]
                 #:api-base [api-base #f])
  "One-shot question -> answer string."
  (llm-response-content
   (llm-completion model
                   #:messages prompt
                   #:system system
                   #:temperature temperature
                   #:max-tokens max-tokens
                   #:provider provider
                   #:api-key api-key
                   #:api-base api-base)))

(define (llm-embedding model input
                       #:dimensions [dimensions #f]
                       #:provider [provider #f]
                       #:api-key [api-key #f]
                       #:api-base [api-base #f]
                       #:extra-headers [extra-headers (hash)])
  "Embed INPUT (a string or list of strings) with MODEL. Returns a list of
float lists. Only OpenAI-compatible providers support embeddings."
  (define-values (prov model-name) (parse-model model #:provider provider))
  (unless (eq? (llm-provider-kind prov) 'openai-compatible)
    (llm-error "embeddings are not supported for provider ~a"
               (llm-provider-name prov)))
  (define inputs
    (cond [(string? input) (list input)]
          [(and (list? input) (andmap string? input)) input]
          [else (llm-error "embedding input must be a string or list of strings: ~s"
                           input)]))
  (define key (provider-api-key prov api-key))
  (define base (or api-base (llm-provider-base-url prov)))
  (define payload
    (maybe-set (hash 'model model-name 'input inputs)
               'dimensions dimensions))
  (define json
    (llm-post-json (string-append base "/embeddings")
                   (bearer-headers key extra-headers)
                   payload))
  (for/list ([item (in-list (hash-ref json 'data '()))])
    (hash-ref item 'embedding)))

(define (llm-chat-with-tools model messages tools
                             #:max-iterations [max-iterations 10]
                             #:tool-choice [tool-choice 'auto]
                             #:temperature [temperature #f]
                             #:max-tokens [max-tokens #f]
                             #:top-p [top-p #f]
                             #:system [system #f]
                             #:provider [provider #f]
                             #:api-key [api-key #f]
                             #:api-base [api-base #f])
  "Agentic loop: ask MODEL (with TOOLS = Racket functions), execute any
requested tool calls, feed the results back, repeat until the model
answers without tool calls or MAX-ITERATIONS runs out. Returns the final
llm-response. For manual control, loop `llm-completion' +
`execute-tool-calls' yourself with `llm-assistant-message'/`llm-tool-message'."
  (let loop ([msgs (normalize-messages messages)]
             [fuel max-iterations])
    (define resp
      (llm-completion model
                      #:messages msgs
                      #:tools tools
                      #:tool-choice tool-choice
                      #:temperature temperature
                      #:max-tokens max-tokens
                      #:top-p top-p
                      #:system system
                      #:provider provider
                      #:api-key api-key
                      #:api-base api-base))
    (define calls (llm-response-tool-calls resp))
    (if (or (null? calls) (<= fuel 1))
        resp
        (let ([results (execute-tool-calls tools calls)])
          (loop (append msgs
                        (cons (llm-assistant-message resp)
                              (map llm-tool-message results)))
                (sub1 fuel))))))

#| Examples (need network + API keys, or local Ollama / llama.cpp):

(require "llmapis.rkt")

;; basic completion -- returns an llm-response struct
(llm-response-content
 (llm-completion "openai/gpt-5-mini" #:messages "What is 2+2?"))

;; string shorthand + another provider
(llm-ask "gemini/gemini-flash-latest" "Capital of France?")

;; local model, no key needed
(llm-ask "ollama/qwen3:1.7b" "What is 2+2?")
(llm-ask "llama-local/local-model" "What is 2+2?")

;; tools: real Racket functions the model can call
(define (get-weather args)
  (format "sunny and 22C in ~a" (hash-ref args 'location "nowhere")))
(define tools
  (list (make-llm-tool "get_weather"
                       "Get the current weather for a location"
                       '(("location" "string" "City name, e.g. Paris"))
                       get-weather)))
(llm-response-content
 (llm-chat-with-tools "openai/gpt-5-mini"
                      "What is the weather in Paris?"
                      tools))

;; embeddings
(llm-embedding "openai/text-embedding-ada-002" "hello world")

;; register another OpenAI-compatible provider at runtime
(define-provider 'groq "https://api.groq.com/openai/v1"
  #:env-keys '("GROQ_API_KEY"))
(llm-ask "groq/llama-3.1-8b-instant" "Say hi.")
|#
