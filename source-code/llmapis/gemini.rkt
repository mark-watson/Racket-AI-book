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

(define (gemini-endpoint [model *gemini-model*])
  (string-append
   "https://generativelanguage.googleapis.com/v1beta/models/"
   model ":generateContent"))

(define (auth-proc uri headers params)
  (values
   (hash-set* headers
              'x-goog-api-key *google-api-key*
              'content-type "application/json")
   params))

(define (make-generation-config)
  (hash 'maxOutputTokens *gemini-max-tokens*))

(define (call-gemini data [model *gemini-model*])
  (response-json
   (post (gemini-endpoint model)
         #:auth auth-proc
         #:json (hash-set data 'generationConfig (make-generation-config)))))

(define (extract-text response)
  (when (hash-has-key? response 'error)
    (error "Gemini API error" (hash-ref response 'error)))
  (let* ((candidates (hash-ref response 'candidates '()))
         (candidate (if (null? candidates) (hash) (car candidates)))
         (content (hash-ref candidate 'content (hash)))
         (parts (hash-ref content 'parts '()))
         (first-part (if (null? parts) (hash) (car parts))))
    (hash-ref first-part 'text "No response")))

(define (make-search-request prompt)
  (hash 'contents (list (hash 'parts (list (hash 'text prompt))))
        'tools (list (hash 'googleSearch (hash)))))

(define (generate prompt [model *gemini-model*])
  (let* ((data (hash 'contents (list (hash 'parts (list (hash 'text prompt))))))
         (r (call-gemini data model)))
    (extract-text r)))

(define (generate-with-search prompt [model *gemini-model*])
  (extract-text (call-gemini (make-search-request prompt) model)))

(define (generate-with-search-and-citations prompt [model *gemini-model*])
  (let* ((r (call-gemini (make-search-request prompt) model))
         (text (extract-text r))
         (candidates (hash-ref r 'candidates '()))
         (candidate (if (null? candidates) (hash) (car candidates)))
         (metadata (hash-ref candidate 'groundingMetadata (hash)))
         (chunks (hash-ref metadata 'groundingChunks '()))
         (citations (for*/list ([chunk chunks]
                                #:when (hash-has-key? chunk 'web))
                      (let ((web (hash-ref chunk 'web (hash))))
                        (cons (hash-ref web 'title "")
                              (hash-ref web 'uri ""))))))
    (values text citations)))

#| Examples:
(displayln (generate "What is the capital of France?"))
(displayln (generate "Mary is 30 and Harry is 25. Who is older?"))
(displayln (generate-with-search "What are the latest developments in AI?"))
(let-values ([(text citations) (generate-with-search-and-citations "Latest AI news")])
  (displayln text) (displayln citations))
(let-values ([(text citations) (generate-with-search-and-citations "Sci-fi movies playing in Flagstaff Arizona today?")]) (displayln text) (displayln citations))
|#
