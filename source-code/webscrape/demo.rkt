#lang racket

;;; demo.rkt — Demo of web scraping utilities
;;; Copyright (C) 2022-2025 Mark Watson
;;; Apache 2 License

(require "webscrape.rkt")

(module+ main
  (displayln "=== Web Scraping Demo ===")
  (newline)
  
  ; Demo URL - Mark Watson's website
  (define demo-url "https://markwatson.com")
  
  (printf "Fetching: ~a~n" demo-url)
  (newline)
  
  ; Get the HTML structure
  (displayln "1. Extracting XExp structure (first 500 chars):")
  (define xexp (web-uri->xexp demo-url))
  (printf "XExp type: ~a~n" (if (list? xexp) (car xexp) xexp))
  (newline)
  
  ; Get text content
  (displayln "2. Extracting paragraph text:")
  (define text (web-uri->text demo-url))
  (when (> (string-length text) 0)
    (displayln (substring text 0 (min 300 (string-length text))))
    (when (> (string-length text) 300)
      (displayln "...")))
  (newline)
  
  ; Get links
  (displayln "3. Extracting external links:")
  (define links (web-uri->links demo-url))
  (for ([link (take links (min 5 (length links)))])
    (printf "  ~a~n" link))
  (when (> (length links) 5)
    (printf "  ... and ~a more links~n" (- (length links) 5)))
  (newline)
  
  ; Get headers
  (displayln "4. Extracting HTML headers (H1, H2, H3):")
  (define headers (web-uri->html-headers demo-url))
  (define h1-headers (first headers))
  (define h2-headers (second headers))
  (define h3-headers (third headers))
  (when (> (length h1-headers) 0)
    (displayln "   H1 headers:")
    (for ([h (take h1-headers (min 3 (length h1-headers)))])
      (printf "     - ~a~n" h)))
  (when (> (length h2-headers) 0)
    (displayln "   H2 headers:")
    (for ([h (take h2-headers (min 3 (length h2-headers)))])
      (printf "     - ~a~n" h)))
  (when (> (length h3-headers) 0)
    (displayln "   H3 headers:")
    (for ([h (take h3-headers (min 3 (length h3-headers)))])
      (printf "     - ~a~n" h)))
  (newline)
  
  (displayln "=== Demo Complete ==="))