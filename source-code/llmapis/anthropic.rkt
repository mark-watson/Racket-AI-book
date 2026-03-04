#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(require net/http-easy)
(require json)

(provide generate
         question-anthropic-with-search
         question-anthropic-with-search-and-citations)

(define *claude-endpoint* "https://api.anthropic.com/v1/messages")
(define *claude-model* "claude-sonnet-4-6")
(define *claude-max-tokens* 1000)

(define (make-auth-proc [extra-headers '()])
  (lambda (uri headers params)
    (values
     (apply hash-set* headers
            (append (list 'x-api-key (getenv "ANTHROPIC_API_KEY")
                          'anthropic-version "2023-06-01"
                          'content-type "application/json")
                    extra-headers))
     params)))

(define (call-claude data [extra-headers '()])
  (response-json
   (post *claude-endpoint*
         #:auth (make-auth-proc extra-headers)
         #:json data)))

(define (generate prompt max-tokens)
  (let* ((data (hash 'model *claude-model*
                     'max_tokens max-tokens
                     'messages (list (hash 'role "user" 'content prompt))))
         (r (call-claude data))
         (content (hash-ref r 'content '()))
         (first-block (if (null? content) (hash) (car content))))
    (hash-ref first-block 'text "No response")))

(define (question-anthropic-with-search prompt)
  (let* ((data (hash 'model *claude-model*
                     'max_tokens *claude-max-tokens*
                     'messages (list (hash 'role "user" 'content prompt))
                     'tools (list (hash 'type "web_search_20250305" 'name "web_search"))))
         (r (call-claude data (list 'anthropic-beta "web-search-2025-03-05")))
         (content (hash-ref r 'content '()))
         (text-blocks (filter (lambda (b) (equal? (hash-ref b 'type "") "text")) content))
         (last-block (and (pair? text-blocks) (last text-blocks))))
    (if last-block
        (hash-ref last-block 'text "No response content")
        "No response content")))

(define (question-anthropic-with-search-and-citations prompt)
  (let* ((data (hash 'model *claude-model*
                     'max_tokens *claude-max-tokens*
                     'messages (list (hash 'role "user" 'content prompt))
                     'tools (list (hash 'type "web_search_20250305" 'name "web_search"))))
         (r (call-claude data (list 'anthropic-beta "web-search-2025-03-05")))
         (content (hash-ref r 'content '()))
         (text-blocks (filter (lambda (b) (equal? (hash-ref b 'type "") "text")) content))
         (last-block (and (pair? text-blocks) (last text-blocks)))
         (text (if last-block (hash-ref last-block 'text "No response content") "No response content"))
         (result-blocks (filter (lambda (b) (equal? (hash-ref b 'type "") "web_search_tool_result")) content))
         (citations (for*/list ([block result-blocks]
                                [result (hash-ref block 'content '())]
                                #:when (equal? (hash-ref result 'type "") "web_search_result"))
                      (cons (hash-ref result 'title "") (hash-ref result 'url "")))))
    (values text citations)))

;; Examples:
;; (displayln (generate "Mary is 30 and Harry is 25. Who is older?" 100))
;; (displayln (generate "Finish this story: Frank bought a new sports car. Frank drove" 200))
;; (displayln (question-anthropic-with-search "What are the latest developments in AI?"))
;;(let-values ([(text citations) (question-anthropic-with-search-and-citations "Latest AI news")])
;;  (displayln text) (displayln citations))
;; (displayln (generate "What is the capital of France?" 50))
