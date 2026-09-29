#lang racket

;;; webscrape.rkt — Web scraping utilities for Racket
;;; Copyright (C) 2022-2025 Mark Watson
;;; Apache 2 License

(require net/http-easy)
(require html-parsing)
(require net/url xml xml/path)
(require srfi/13) ; for strings

(provide web-uri->xexp
         web-uri->text
         web-uri->links
         web-uri->html-headers)

; Fetch a web page and return its HTML as an XExp structure
; A-URI is a URL string or bytes
(define (web-uri->xexp a-uri)
  ; GET the URL and parse HTML to XExp
  (define resp (get a-uri #:stream? #t))
  (define xexp (html->xexp (response-output resp)))
  (response-close! resp)
  xexp)

; Extract all paragraph text from a web page
; A-URI is a URL string or bytes
(define (web-uri->text a-uri)
  ; Fetch page and extract paragraph content
  (define a-xexp (web-uri->xexp a-uri))
  (define p-elements (se-path*/list '(p) a-xexp))
  ; Filter to only strings (text content) and normalize whitespace
  (define text-strings
    (for/list ([elem p-elements]
               #:when (string? elem))
      elem))
  (string-normalize-spaces
   (string-join text-strings "\n")))

; Extract all external links from a web page
; A-URI is a URL string or bytes
(define (web-uri->links a-uri)
  ; Fetch page and extract href attributes
  (define a-xexp (web-uri->xexp a-uri))
  (define all-hrefs (se-path*/list '(href) a-xexp))
  ; Filter to only external links (starting with http)
  (for/list ([href all-hrefs]
             #:when (and (string? href)
                         (string-prefix? "http" href)))
    href))

; Extract HTML headers (H1, H2, H3) from a web page
; A-URI is a URL string or bytes
; Returns a list of lists: (list (h1-headers...) (h2-headers...) (h3-headers...))
(define (web-uri->html-headers a-uri)
  ; Fetch page and extract headers
  (define a-xexp (web-uri->xexp a-uri))
  ; Extract H1 headers
  (define h1-headers
    (for/list ([h (se-path*/list '(h1) a-xexp)]
               #:when (string? h))
      h))
  ; Extract H2 headers
  (define h2-headers
    (for/list ([h (se-path*/list '(h2) a-xexp)]
               #:when (string? h))
      h))
  ; Extract H3 headers
  (define h3-headers
    (for/list ([h (se-path*/list '(h3) a-xexp)]
               #:when (string? h))
      h))
  (list h1-headers h2-headers h3-headers))

(module+ main
  ; (web-uri->xexp "https://knowledgebooks.com")
  ; (web-uri->text "https://knowledgebooks.com")
  ; (web-uri->links "https://knowledgebooks.com")
  (void))