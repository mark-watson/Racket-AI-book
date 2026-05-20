#lang racket

(require net/http-easy)
(require racket/set)
 (require racket/pretty)
(require json)

(provide tavily-search)

(define (json-helper query)
  (define api-key (getenv "TAVILY_API_KEY"))
  (unless api-key
    (error 'tavily-search "TAVILY_API_KEY environment variable is not set"))
  (jsexpr->string
   (hash 'api_key api-key
         'query query
         'max_results 5)))

(define (filter-response a-hash-eq)
  (list
   (hash-ref a-hash-eq 'url)
   (hash-ref a-hash-eq 'title)
   (hash-ref a-hash-eq 'content)))

(define (tavily-search query)
  (let* ((prompt-data (json-helper query))
         (p
          (post
           "https://api.tavily.com/search"
           #:data prompt-data
           #:headers (hash 'content-type "application/json")))
         (r (response-json p)))
    (map filter-response (hash-ref r 'results '()))))

(module+ main
  (displayln (json-helper "1 + 2?"))
  (pretty-print (tavily-search "Fun things to do in Sedona Arizona")))
