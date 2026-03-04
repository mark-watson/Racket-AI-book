#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(require net/http-easy)
(require json)

(provide generate
         generate-with-search
         generate-with-search-and-citations)

(define *gemini-model* "gemini-2.0-flash")
(define *gemini-max-tokens* 1000)

(define (gemini-endpoint [model *gemini-model*])
  (string-append "https://generativelanguage.googleapis.com/v1beta/models/" model ":generateContent"))

(define (make-auth-proc)
  (lambda (uri headers params)
    (values
     (hash-set* headers
                'x-goog-api-key (getenv "GOOGLE_API_KEY")
                'content-type "application/json")
     params)))

(define (call-gemini data [model *gemini-model*])
  (response-json
   (post (gemini-endpoint model)
         #:auth (make-auth-proc)
         #:json data)))

(define (extract-text response)
  (let* ((candidates (hash-ref response 'candidates '()))
         (candidate (if (null? candidates) (hash) (car candidates)))
         (content (hash-ref candidate 'content (hash)))
         (parts (hash-ref content 'parts '()))
         (first-part (if (null? parts) (hash) (car parts))))
    (hash-ref first-part 'text "No response")))

(define (generate prompt)
  (let* ((data (hash 'contents (list (hash 'parts (list (hash 'text prompt))))))
         (r (call-gemini data)))
    (extract-text r)))

(define (generate-with-search prompt)
  (let* ((data (hash 'contents (list (hash 'parts (list (hash 'text prompt))))
                     'tools (list (hash 'google_search (hash)))))
         (r (call-gemini data)))
    (extract-text r)))

(define (generate-with-search-and-citations prompt)
  (let* ((data (hash 'contents (list (hash 'parts (list (hash 'text prompt))))
                     'tools (list (hash 'google_search (hash)))))
         (r (call-gemini data))
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

;; Examples:
;;(displayln (generate "What is the capital of France?"))
;; (displayln (generate "Mary is 30 and Harry is 25. Who is older?"))
;; (displayln (generate-with-search "What are the latest developments in AI?"))
;; (let-values ([(text citations) (generate-with-search-and-citations "Latest AI news")])
;;   (displayln text) (displayln citations))
