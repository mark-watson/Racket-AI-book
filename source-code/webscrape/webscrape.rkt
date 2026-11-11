#lang racket

;;; webscrape.rkt — Web scraping utilities for Racket
;;; Copyright (C) 2022-2026 Mark Watson
;;; Apache 2 License
;;;
;;; The library is split into two layers:
;;;   - pure functions that work on an already-parsed page (an xexp),
;;;     which can be tested without a network connection, and
;;;   - web-uri->... convenience functions that fetch a page first.

(require net/http-easy)
(require html-parsing)
(require net/url xml xml/path)

(provide web-uri->xexp
         web-uri->text
         web-uri->links
         web-uri->html-headers
         html-string->xexp
         xexp->text
         xexp->links
         xexp->html-headers)

; Parse a string of HTML into an XExp structure
(define (html-string->xexp html)
  (html->xexp html))

; Fetch a web page and return its HTML as an XExp structure.
; A-URI is a URL string or bytes. Raises exn:fail:network on connection
; problems and exn:fail on any HTTP status other than 2xx.
(define (web-uri->xexp a-uri)
  (define resp
    (get a-uri
         #:stream? #t
         #:timeouts (make-timeout-config #:connect 10 #:request 30)))
  (define status (response-status-code resp))
  (cond
    [(<= 200 status 299)
     (define xexp (html->xexp (response-output resp)))
     (response-close! resp)
     xexp]
    [else
     (response-close! resp)
     (error 'web-uri->xexp "HTTP status ~a fetching ~a" status a-uri)]))

; Extract all paragraph text from a parsed page
(define (xexp->text a-xexp)
  (define p-elements (se-path*/list '(p) a-xexp))
  ; Keep only strings (text content) and normalize whitespace
  (define text-strings
    (for/list ([elem p-elements]
               #:when (string? elem))
      elem))
  (string-normalize-spaces
   (string-join text-strings "\n")))

; Extract links from a parsed page.
; With no BASE-URI only absolute links (starting with http) are returned.
; With a BASE-URI string, relative links such as "/about.html" are
; resolved against it and returned as absolute URLs too.
(define (xexp->links a-xexp [base-uri #f])
  (define all-hrefs
    (filter string? (se-path*/list '(href) a-xexp)))
  (define (absolute? href) (string-prefix? href "http"))
  (cond
    [base-uri
     (define base (string->url base-uri))
     (for/list ([href all-hrefs]
                #:unless (or (string-prefix? href "#")
                             (string-prefix? href "mailto:")
                             (string-prefix? href "javascript:")))
       (if (absolute? href)
           href
           (url->string (combine-url/relative base href))))]
    [else (filter absolute? all-hrefs)]))

; Extract HTML headers (H1, H2, H3) from a parsed page.
; Returns a list of lists: (list (h1-headers...) (h2-headers...) (h3-headers...))
(define (xexp->html-headers a-xexp)
  (for/list ([tag '(h1 h2 h3)])
    (for/list ([h (se-path*/list (list tag) a-xexp)]
               #:when (string? h))
      h)))

; Fetch a page and extract all paragraph text
(define (web-uri->text a-uri)
  (xexp->text (web-uri->xexp a-uri)))

; Fetch a page and extract all external links (starting with http).
; Pass #:resolve-relative? #t to also get relative links as absolute URLs.
(define (web-uri->links a-uri #:resolve-relative? [resolve-relative? #f])
  (xexp->links (web-uri->xexp a-uri)
               (and resolve-relative?
                    (if (bytes? a-uri) (bytes->string/utf-8 a-uri) a-uri))))

; Fetch a page and extract H1, H2, and H3 headers
(define (web-uri->html-headers a-uri)
  (xexp->html-headers (web-uri->xexp a-uri)))

(module+ main
  ; (web-uri->xexp "https://knowledgebooks.com")
  ; (web-uri->text "https://knowledgebooks.com")
  ; (web-uri->links "https://knowledgebooks.com")
  (void))
