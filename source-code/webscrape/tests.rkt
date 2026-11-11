#lang racket

;;; tests.rkt — Offline unit tests for the web scrape library
;;; Copyright (C) 2022-2026 Mark Watson
;;; Apache 2 License
;;;
;;; These tests parse a fixed HTML string, so no network is needed.
;;; Run with:  raco test tests.rkt   (or: racket tests.rkt)

(require rackunit)
(require "webscrape.rkt")

(define sample-html #<<HTML
<!DOCTYPE html>
<html><head><title>Sample</title></head>
<body>
  <h1>Main Title</h1>
  <p>First   paragraph.</p>
  <h2>Section One</h2>
  <p>Second paragraph with a <a href="https://example.com/page">link</a>.</p>
  <h2>Section Two</h2>
  <ul>
    <li><a href="/about.html">About</a></li>
    <li><a href="docs/intro.html">Intro</a></li>
    <li><a href="#top">Top</a></li>
    <li><a href="mailto:someone@example.com">Mail</a></li>
    <li><a href="http://other.example.org/">Other</a></li>
  </ul>
  <h3>Details</h3>
</body></html>
HTML
  )

(define page (html-string->xexp sample-html))

(test-case "html-string->xexp returns a *TOP* xexp"
  (check-equal? (car page) '*TOP*))

(test-case "xexp->text joins paragraph strings and normalizes spaces"
  (define text (xexp->text page))
  (check-true (string-prefix? text "First paragraph."))
  (check-true (string-contains? text "Second paragraph with a")))

(test-case "xexp->links keeps only absolute links by default"
  (check-equal? (xexp->links page)
                '("https://example.com/page" "http://other.example.org/")))

(test-case "xexp->links resolves relative links against a base URI"
  (check-equal? (xexp->links page "https://site.test/blog/index.html")
                '("https://example.com/page"
                  "https://site.test/about.html"
                  "https://site.test/blog/docs/intro.html"
                  "http://other.example.org/")))

(test-case "xexp->html-headers returns h1, h2, h3 lists"
  (check-equal? (xexp->html-headers page)
                '(("Main Title") ("Section One" "Section Two") ("Details"))))

(test-case "an empty page yields empty results"
  (define empty (html-string->xexp "<html><body></body></html>"))
  (check-equal? (xexp->text empty) "")
  (check-equal? (xexp->links empty) '())
  (check-equal? (xexp->html-headers empty) '(() () ())))
