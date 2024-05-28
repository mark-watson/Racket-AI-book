#lang racket

(require net/http-easy)
(require racket/set)
 (require racket/pretty)
(require json)

(provide brave-search)

BROKEN - TBD: fix!

(define (json-helper query)
  (format "{\"X-Subscription-Token\": \"~a\", \"q\": \"~a\"}"
          (getenv "BRAVE_SEARCH_API_KEY")
          query))

(define (filter-response a-hash-eq)
  (list
   (hash-ref a-hash-eq 'url)
   (hash-ref a-hash-eq 'title)
   (hash-ref a-hash-eq 'content)))

(displayln (json-helper "1 + 2?"))
                       

(define (brave-search query)
  (let* ((prompt-data (json-helper query))
         (p
          (post
           "https://api.search.brave.com/res/v1/web/search"
           #:data prompt-data))
         (r (response-json p)))
    (displayln prompt-data)
    (pretty-print r)
    (map filter-response (hash-ref r  'results))))

(pretty-print (brave-search "Fun things to do in Sedona Arizona"))

;;curl -s --compressed "https://api.search.brave.com/res/v1/web/search?q=brave+search" -H "Accept:
;;      application/json" -H "Accept-Encoding: gzip" -H "X-Subscription-Token: <YOUR_API_KEY>"