#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(require net/http-easy)
(require json)

(provide generate
         generate-with-search
         generate-with-search-and-citations)

(define *gemini-model* "gemini-flash-latest")
(define *gemini-max-tokens* 8192)

(define *google-api-key*
  (or (getenv "GOOGLE_API_KEY")
      (error "GOOGLE_API_KEY environment variable is not set")))

(define *base-url*
  "https://generativelanguage.googleapis.com/v1beta/models")

(define (auth-proc uri headers params)
  (values
   (hash-set* headers
              'x-goog-api-key *google-api-key*
              'content-type "application/json")
   params))

(define (call-generate-content model data)
  (let ((url (string-append *base-url* "/" model ":generateContent")))
    (response-json
     (post url
           #:auth auth-proc
           #:json data))))

(define (extract-text response)
  "Extract text from a generateContent API response."
  (when (hash-has-key? response 'error)
    (error "Gemini API error" (hash-ref response 'error)))
  (let* ((candidates (hash-ref response 'candidates '()))
         (first-cand (if (null? candidates) (hash) (car candidates)))
         (content (hash-ref first-cand 'content (hash)))
         (parts (hash-ref content 'parts '()))
         (first-part (if (null? parts) (hash) (car parts))))
    (hash-ref first-part 'text "No response")))

(define (generate prompt [model *gemini-model*])
  (let* ((data (hash 'contents
                     (list (hash 'parts
                                 (list (hash 'text prompt))))))
         (r (call-generate-content model data)))
    (extract-text r)))

(define (generate-with-search prompt [model *gemini-model*])
  (let* ((data (hash 'contents
                     (list (hash 'parts
                                 (list (hash 'text prompt))))
                     'tools (list (hash 'googleSearch (hash)))))
         (r (call-generate-content model data)))
    (extract-text r)))

(define (generate-with-search-and-citations prompt [model *gemini-model*])
  (let* ((data (hash 'contents
                     (list (hash 'parts
                                 (list (hash 'text prompt))))
                     'tools (list (hash 'googleSearch (hash)))))
         (r (call-generate-content model data))
         (text (extract-text r))
         (candidates (hash-ref r 'candidates '()))
         (first-cand (if (null? candidates) (hash) (car candidates)))
         (grounding (hash-ref first-cand 'groundingMetadata (hash)))
         (grounding-chunks (hash-ref grounding 'groundingChunks '()))
         (citations
          (for/list ([chunk grounding-chunks])
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
