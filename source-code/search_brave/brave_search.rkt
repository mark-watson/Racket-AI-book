#lang racket

(require net/http-easy)
(require racket/set)
(require racket/pretty)
(require json)
(require net/url)
(require net/uri-codec)

(provide brave-search)

(define (brave-search query [num-results 3])
  (define api-key (getenv "BRAVE_SEARCH_API_KEY"))
  (unless api-key
    (error 'brave-search "BRAVE_SEARCH_API_KEY environment variable is not set"))
  (let* ([url "https://api.search.brave.com/res/v1/web/search"]
         [headers 
          (hash 'X-Subscription-Token api-key
                'Content-Type "application/json")]
         [params
          (list (cons 'q query)
                (cons 'count (number->string num-results)))]
         [response 
          (get url 
               #:headers headers
               #:params params)])
    
    (if (= (response-status-code response) 200)
        (let* ([json-response (response-json response)]
               [web (hash-ref json-response 'web #f)]
               [web-results (if web (hash-ref web 'results '()) '())])
          (for/list ([result web-results])
            (hash
             'title (hash-ref result 'title "")
             'url (hash-ref result 'url "")
             'description (hash-ref result 'description ""))))
        (error 'brave-search (format "HTTP request failed with status ~a: ~a" 
                                     (response-status-code response)
                                     (response-body response))))))

; Example usage
(pretty-print 
 (brave-search "Fun things to do in Sedona Arizona"))