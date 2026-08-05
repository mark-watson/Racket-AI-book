#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; search.rkt -- Brave Search and Exa AI search backends
;;; Racket port of py-coding-agent/search.py

(require net/http-easy
         net/uri-codec
         json
         racket/string)

(provide brave-search
         exa-search)

(define EXA-ENDPOINT "https://api.exa.ai/search")

;; ---------------------------------------------------------------------------
;; Brave Search
;; Returns (listof (list url title description))

(define (brave-search query [num-results 5])
  (define api-key (getenv "BRAVE_SEARCH_API_KEY"))
  (unless (and api-key (not (string=? api-key "")))
    (error 'brave-search "BRAVE_SEARCH_API_KEY environment variable not set"))
  (define encoded (uri-encode query))
  (define url (format "https://api.search.brave.com/res/v1/web/search?q=~a&count=~a"
                      encoded num-results))
  (define headers
    (hash 'X-Subscription-Token api-key
          'content-type "application/json"
          'accept "application/json"))
  (define resp
    (get url #:headers headers))
  (define data (response-json resp))
  (define web (hash-ref data 'web (hash)))
  (define results (hash-ref web 'results '()))
  (for/list ([r (in-list results)])
    (list (hash-ref r 'url "")
          (hash-ref r 'title "")
          (hash-ref r 'description ""))))

;; ---------------------------------------------------------------------------
;; Exa AI Search
;; Returns (listof (list url title highlight))

(define (exa-search query [num-results 5])
  (define api-key (getenv "EXA_SEARCH_API_KEY"))
  (unless (and api-key (not (string=? api-key "")))
    (error 'exa-search "EXA_SEARCH_API_KEY environment variable not set"))
  (define payload
    (hash 'query query
          'type "auto"
          'numResults num-results
          'contents (hash 'highlights #t)))
  (define headers
    (hash 'content-type "application/json"
          'authorization (string-append "Bearer " api-key)))
  (define resp
    (post EXA-ENDPOINT
          #:headers headers
          #:json payload))
  (define data (response-json resp))
  (define results (hash-ref data 'results '()))
  (for/list ([r (in-list results)])
    (list (hash-ref r 'url "")
          (hash-ref r 'title "")
          (let ([hl (hash-ref r 'highlights '())])
            (if (and (list? hl) (not (null? hl))) (first hl) "")))))
