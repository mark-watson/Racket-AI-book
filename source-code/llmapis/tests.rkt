#lang racket

;;; tests.rkt — Offline unit tests for the uniform API in llmapis.rkt
;;; Apache 2 License
;;;
;;; Nothing here touches the network: these tests cover model routing,
;;; message and tool translation, response parsing, and error mapping.
;;; For a live smoke test against real providers see live-test.rkt.
;;; Run with:  raco test tests.rkt

(require rackunit json)
(require "llmapis.rkt")

(test-case "parse-model routes on the provider prefix"
  (define-values (prov model) (parse-model "openai/gpt-5-mini"))
  (check-equal? (llm-provider-name prov) 'openai)
  (check-equal? model "gpt-5-mini")
  (check-equal? (llm-provider-kind prov) 'openai-compatible)
  ;; only the first slash separates provider from model; colons are kept
  (define-values (prov2 model2) (parse-model "ollama/qwen3:1.7b"))
  (check-equal? (llm-provider-name prov2) 'ollama)
  (check-equal? model2 "qwen3:1.7b"))

(test-case "parse-model rejects bad model strings"
  (check-exn #rx"Unknown provider" (lambda () (parse-model "nosuch/x")))
  (check-exn #rx"provider/model-name" (lambda () (parse-model "noslash"))))

(test-case "default models can be overridden from the environment"
  (parameterize ([current-environment-variables
                  (make-environment-variables)])
    (check-equal? (default-chat-model) "openai/gpt-5-mini")
    (check-equal? (default-embedding-model) "openai/text-embedding-3-small")
    (putenv "LLM_CHAT_MODEL" "ollama/qwen3:1.7b")
    (putenv "LLM_EMBEDDING_MODEL" "ollama/nomic-embed-text")
    (check-equal? (default-chat-model) "ollama/qwen3:1.7b")
    (check-equal? (default-embedding-model) "ollama/nomic-embed-text")
    ;; whatever the defaults are, they must be routable
    (define-values (prov _m) (parse-model (default-chat-model)))
    (check-true (llm-provider? prov))))

(test-case "messages: a bare string is one user message"
  (define msgs (normalize-messages "hi"))
  (check-equal? (length msgs) 1)
  (check-equal? (llm-message-role (first msgs)) "user")
  (check-equal? (llm-message-content (first msgs)) "hi")
  (check-equal? (llm-translate-messages msgs)
                (list (hash 'role "user" 'content "hi"))))

(test-case "messages: role/content pairs keep their order"
  (define msgs
    (normalize-messages '(("system" "be brief") ("user" "hi"))))
  (check-equal? (map llm-message-role msgs) '("system" "user")))

(define weather-tool
  (make-llm-tool "get_weather" "Get the weather"
                 '(("location" "string" "City name"))
                 (lambda (args)
                   (format "sunny in ~a" (hash-ref args 'location)))))

(test-case "tools translate to the OpenAI function schema"
  (define schema (first (llm-translate-tools (list weather-tool))))
  (check-equal? (hash-ref schema 'type) "function")
  (define fn (hash-ref schema 'function))
  (check-equal? (hash-ref fn 'name) "get_weather")
  (define params (hash-ref fn 'parameters))
  (check-equal? (hash-ref params 'required) '("location"))
  (check-equal? (hash-ref (hash-ref (hash-ref params 'properties) 'location)
                          'type)
                "string"))

(test-case "execute-tool-calls runs the Racket function"
  (define results
    (execute-tool-calls
     (list weather-tool)
     (list (make-llm-tool-call "c1" "get_weather" (hash 'location "Paris")))))
  (check-equal? (llm-tool-result-result (first results)) "sunny in Paris"))

(test-case "an unknown tool becomes an error result, not an exception"
  (define results
    (execute-tool-calls (list weather-tool)
                        (list (make-llm-tool-call "c2" "nope" (hash)))))
  (check-true (string-contains? (llm-tool-result-result (first results))
                                "unknown tool")))

(test-case "parsing an OpenAI-style chat response"
  (define body
    (string->jsexpr #<<JSON
{"choices": [{"message": {"role": "assistant", "content": "4"},
              "finish_reason": "stop"}],
 "usage": {"prompt_tokens": 3, "completion_tokens": 1}}
JSON
                    ))
  (define resp (llm-parse-openai-response body "gpt-5-mini"))
  (check-equal? (llm-response-content resp) "4")
  (check-equal? (llm-response-tool-calls resp) '())
  (check-equal? (llm-response-model resp) "gpt-5-mini"))

(test-case "an error body raises exn:fail:llm"
  (check-exn exn:fail:llm?
             (lambda ()
               (llm-parse-openai-response
                (hash 'error (hash 'message "boom")) "m"))))

(test-case "HTTP status codes map to the error hierarchy"
  (define (raised status)
    (with-handlers ([exn:fail:llm? values])
      (raise-llm-error status "{\"error\": {\"message\": \"x\"}}")))
  (check-true (exn:fail:llm:authentication? (raised 401)))
  (check-true (exn:fail:llm:rate-limit? (raised 429)))
  (check-true (exn:fail:llm:not-found? (raised 404)))
  (check-true (exn:fail:llm:api? (raised 500)))
  (check-false (exn:fail:llm:authentication? (raised 500))))
