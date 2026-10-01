#lang racket

;;; test.rkt -- live demo / smoke test for the uniform API in llmapis.rkt.
;;;
;;; Uses ONLY the new uniform API (no per-provider modules). Google tests
;;; need GOOGLE_API_KEY (or GEMINI_API_KEY), OpenAI tests need
;;; OPENAI_API_KEY (or OPENAI_KEY); Ollama tests need a local server with
;;; qwen3:1.7b. Anything unavailable is SKIPped, not failed.
;;;
;;; Run:  racket test.rkt
;;; Optional env overrides: GEMINI_MODEL, OPENAI_MODEL, OLLAMA_MODEL,
;;; OLLAMA_HOST.

(require "llmapis.rkt"
         net/http-easy
         json)

;;; ---------------------------------------------------------------------------
;;; Small demo harness: headers, timing, failure counting.

(define failures 0)

(define (headline fmt . args)
  (displayln (string-append "\n=== " (apply format fmt args) " ===")))

(define (show label value)
  (printf "~a: ~a\n" label value))

(define (timed thunk)
  (define t0 (current-inexact-milliseconds))
  (define v (thunk))
  (values v (/ (- (current-inexact-milliseconds) t0) 1000.0)))

(define (note-failure fmt . args)
  (set! failures (add1 failures))
  (printf "FAILED: ~a\n" (apply format fmt args)))

;;; ---------------------------------------------------------------------------
;;; Environment: keys, Ollama reachability, model picks.

(define google-key?
  (or (getenv "GOOGLE_API_KEY") (getenv "GEMINI_API_KEY")))

(define openai-key?
  (or (getenv "OPENAI_API_KEY") (getenv "OPENAI_KEY")))

(define ollama-host (or (getenv "OLLAMA_HOST") "http://localhost:11434"))
(define ollama-model (or (getenv "OLLAMA_MODEL") "qwen3:1.7b"))

(define (ollama-has-model? model)
  "True when the local Ollama server lists MODEL in /api/tags."
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define resp (get (string-append ollama-host "/api/tags")))
    (define data (response-json resp))
    (for/or ([m (in-list (hash-ref data 'models '()))])
      (define name (hash-ref m 'name ""))
      (or (string=? name model)
          (string-prefix? name (string-append model ":"))))))

(define (pick-google-model)
  "First working chat model out of the override / known-good candidates.
Returns #f when the network/API is unreachable (caller SKIPpes)."
  (define candidates
    (if (getenv "GEMINI_MODEL")
        (list (getenv "GEMINI_MODEL"))
        '("gemini-2.5-flash-lite" "gemini-flash-latest")))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "  probe failed: ~a\n" (exn-message e))
                     #f)])
    (for/or ([m (in-list candidates)])
      (with-handlers ([exn:fail:llm:not-found?
                       (lambda (_) (printf "  model gemini/~a not found, trying next\n" m) #f)])
        (llm-ask (string-append "gemini/" m) "Reply with exactly: ok"
                 #:max-tokens 16)
        (printf "  using model gemini/~a\n" m)
        m))))

(define (pick-openai-model)
  "First working chat model out of the override / default candidates.
Returns #f when the network/API is unreachable (caller SKIPpes)."
  (define candidates
    (if (getenv "OPENAI_MODEL")
        (list (getenv "OPENAI_MODEL"))
        '("gpt-5-mini")))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "  probe failed: ~a\n" (exn-message e))
                     #f)])
    (for/or ([m (in-list candidates)])
      (with-handlers ([exn:fail:llm:not-found?
                       (lambda (_) (printf "  model openai/~a not found, trying next\n" m) #f)])
        (llm-ask (string-append "openai/" m) "Reply with exactly: ok"
                 #:max-tokens 16)
        (printf "  using model openai/~a\n" m)
        m))))

;;; ---------------------------------------------------------------------------
;;; Demo tools: real Racket functions the model may call.

(define (demo-get-weather args)
  (format "sunny and 22C in ~a" (hash-ref args 'location "nowhere")))

(define demo-tools
  (list (make-llm-tool
         "get_weather"
         "Get the current weather for a location"
         '(("location" "string" "City name, e.g. Paris"))
         demo-get-weather)
        (make-llm-tool
         "add_numbers"
         "Add two numbers together"
         '((a "number" "First addend") (b "number" "Second addend"))
         (lambda (args) (+ (hash-ref args 'a 0) (hash-ref args 'b 0))))))

;;; ---------------------------------------------------------------------------
;;; The demos. Each prints the prompt, the answer, and the interesting
;;; metadata (tool calls, token usage, elapsed time).

(define (demo-basic-answer tag model prompt)
  (headline "~a: one-shot question" tag)
  (show "model" model)
  (show "prompt" prompt)
  (define-values (answer secs)
    (timed (lambda () (llm-ask model prompt #:max-tokens 256))))
  (show "answer" answer)
  (show "elapsed" (format "~as" (real->decimal-string secs 1)))
  (unless (and (string? answer) (> (string-length answer) 0))
    (note-failure "~a answer was empty" tag)))

(define (demo-full-response tag model prompt)
  (headline "~a: full llm-response inspection" tag)
  (define resp (llm-completion model #:messages prompt #:max-tokens 256))
  (show "content" (llm-response-content resp))
  (show "model" (llm-response-model resp))
  (show "finish-reason" (llm-response-finish-reason resp))
  (show "tool-calls" (llm-response-tool-calls resp))
  (show "usage" (llm-response-usage resp)))

(define (demo-manual-tool-round tag model prompt tools)
  (headline "~a: manual tool round (inspect every step)" tag)
  (show "prompt" prompt)
  (define first (llm-completion model #:messages prompt #:tools tools
                                #:max-tokens 512))
  (show "model asked for ~a tool call(s)"
        (length (llm-response-tool-calls first)))
  (for ([tc (in-list (llm-response-tool-calls first))])
    (show "  call" (format "~a id=~a args=~a"
                           (llm-tool-call-name tc)
                           (llm-tool-call-id tc)
                           (jsexpr->string (llm-tool-call-arguments tc)))))
  (when (null? (llm-response-tool-calls first))
    (note-failure "~a: model made no tool call for ~s" tag prompt))
  (define results (execute-tool-calls tools (llm-response-tool-calls first)))
  (for ([r (in-list results)])
    (show "  result" (format "~a => ~a"
                             (llm-tool-result-name r)
                             (llm-tool-result-result r))))
  (define follow-up
    (llm-completion
     model
     #:messages (append (list (llm-message "user" prompt '() #f #f)
                              (llm-assistant-message first))
                        (map llm-tool-message results))
     #:tools tools #:max-tokens 256))
  (show "final answer" (llm-response-content follow-up)))

(define (demo-auto-loop tag model prompt tools)
  (headline "~a: automatic agentic loop (llm-chat-with-tools)" tag)
  (show "prompt" prompt)
  (define-values (resp secs)
    (timed (lambda () (llm-chat-with-tools model prompt tools
                                           #:max-tokens 512))))
  (show "final answer" (llm-response-content resp))
  (show "usage" (llm-response-usage resp))
  (show "elapsed" (format "~as" (real->decimal-string secs 1)))
  (unless (string? (llm-response-content resp))
    (note-failure "~a: loop ended with no text answer" tag)))

(define (demo-embedding tag model text)
  (headline "~a: embeddings (experimental)" tag)
  (define vecs
    (with-handlers
        ([exn:fail:llm?
          (lambda (e)
            (show "note" (format "embeddings unavailable here: ~a"
                                 (exn-message e)))
            #f)])
      (llm-embedding model text)))
  (when vecs
    (show "vectors" (length vecs))
    (show "dimensions" (length (first vecs)))
    (show "first 5" (take (first vecs) 5))))

;;; ---------------------------------------------------------------------------
;;; Run.

(headline "llmapis.rkt uniform API demo")
(show "registered providers" (llm-providers))
(show "GOOGLE_API_KEY/GEMINI_API_KEY" (if google-key? "set" "missing"))
(show "OPENAI_API_KEY/OPENAI_KEY" (if openai-key? "set" "missing"))
(show "Ollama server" ollama-host)

;; Google (cloud).
(define google-model #f)
(when google-key?
  (headline "Google: model probe")
  (set! google-model (pick-google-model)))
(if google-model
    (let ([m (string-append "gemini/" google-model)])
      (with-handlers ([exn:fail?
                       (lambda (e) (note-failure "Google demo: ~a" (exn-message e)))])
        (demo-basic-answer "Google" m "What is 2+2? Reply with just the number.")
        (demo-full-response "Google" m "Name the capital of France, and nothing else.")
        (demo-manual-tool-round "Google" m "What is the weather in Paris?" demo-tools)
        (demo-auto-loop "Google" m
                        "Add 17 and 25, then tell me the weather in Paris."
                        demo-tools)
        (demo-embedding "Google" "gemini/text-embedding-004" "hello world")))
    (headline "Google: SKIP (no key or no working model)"))

;; OpenAI (cloud).
(define openai-model #f)

(when openai-key?
  (headline "OpenAI: model probe")
  (set! openai-model (pick-openai-model)))

(if openai-model
    (let ([m (string-append "openai/" openai-model)])
      (with-handlers ([exn:fail?
                       (lambda (e) (note-failure "OpenAI demo: ~a" (exn-message e)))])
        (demo-basic-answer "OpenAI" m "What is 2+2? Reply with just the number.")
        (demo-full-response "OpenAI" m "Name the capital of France, and nothing else.")
        (demo-manual-tool-round "OpenAI" m "What is the weather in Paris?" demo-tools)
        (demo-auto-loop "OpenAI" m
                        "Add 17 and 25, then tell me the weather in Paris."
                        demo-tools)
        (demo-embedding "OpenAI" "openai/text-embedding-ada-002" "hello world")))
    (headline "OpenAI: SKIP (no key or no working model)"))

;; Ollama (local).
(if (ollama-has-model? ollama-model)
    (let ([m (string-append "ollama/" ollama-model)])
      (with-handlers ([exn:fail?
                       (lambda (e) (note-failure "Ollama demo: ~a" (exn-message e)))])
        (demo-basic-answer "Ollama" m "What is 2+2? Reply with just the number.")
        (demo-full-response "Ollama" m "Name the capital of France, and nothing else.")
        (demo-auto-loop "Ollama" m "What is the weather in Paris?" demo-tools)))
    (headline "Ollama: SKIP (~a not found on ~a)" ollama-model ollama-host))

(headline "done: ~a failure(s)" failures)
(exit (if (zero? failures) 0 1))
