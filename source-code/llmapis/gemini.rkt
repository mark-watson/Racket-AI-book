#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(require net/http-easy)
(require json)

(provide generate
         generate-with-search
         generate-with-search-and-citations)

(define *gemini-model* "gemini-3-flash-preview")
(define *gemini-max-tokens* 8192)

(define *google-api-key*
  (or (getenv "GOOGLE_API_KEY")
      (error "GOOGLE_API_KEY environment variable is not set")))

(define *interactions-url*
  "https://generativelanguage.googleapis.com/v1beta/interactions")

(define (auth-proc uri headers params)
  (values
   (hash-set* headers
              'x-goog-api-key *google-api-key*
              'content-type "application/json"
              'Api-Revision "2026-05-20")
   params))

(define (call-interactions data)
  (response-json
   (post *interactions-url*
         #:auth auth-proc
         #:json data)))

(define (extract-text-from-steps response)
  "Extract text from the last model_output step in an Interactions API response."
  (when (hash-has-key? response 'error)
    (error "Gemini Interactions API error" (hash-ref response 'error)))
  (let* ((steps (hash-ref response 'steps '())))
    (for/last ([step steps]
               #:when (equal? (hash-ref step 'type "") "model_output"))
      (let* ((content (hash-ref step 'content '()))
             (first-content (if (null? content) (hash) (car content))))
        (hash-ref first-content 'text "No response")))))

(define (generate prompt [model *gemini-model*])
  (let* ((data (hash 'model model 'input prompt))
         (r (call-interactions data)))
    (extract-text-from-steps r)))

(define (generate-with-search prompt [model *gemini-model*])
  (let* ((data (hash 'model model
                     'input prompt
                     'tools (list (hash 'type "google_search"))))
         (r (call-interactions data)))
    (extract-text-from-steps r)))

(define (generate-with-search-and-citations prompt [model *gemini-model*])
  (let* ((data (hash 'model model
                     'input prompt
                     'tools (list (hash 'type "google_search"))))
         (r (call-interactions data))
         (text (extract-text-from-steps r))
         (steps (hash-ref r 'steps '()))
         (citations
          (for*/list ([step steps]
                      #:when (equal? (hash-ref step 'type "") "model_output")
                      [content-item (hash-ref step 'content '())]
                      #:when (hash-has-key? content-item 'annotations)
                      [annotation (hash-ref content-item 'annotations '())]
                      #:when (equal? (hash-ref annotation 'type "") "url_citation"))
            (cons (hash-ref annotation 'title "")
                  (hash-ref annotation 'url "")))))
    (values text citations)))

#| Examples:
(displayln (generate "What is the capital of France?"))
(displayln (generate "Mary is 30 and Harry is 25. Who is older?"))
(displayln (generate-with-search "What are the latest developments in AI?"))
(let-values ([(text citations) (generate-with-search-and-citations "Latest AI news")])
  (displayln text) (displayln citations))
(let-values ([(text citations) (generate-with-search-and-citations "Sci-fi movies playing in Flagstaff Arizona today?")]) (displayln text) (displayln citations))
|#
