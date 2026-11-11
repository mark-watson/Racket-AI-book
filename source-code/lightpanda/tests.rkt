#lang racket

;;; tests.rkt — Offline unit tests for the Lightpanda client
;;; Apache 2 License
;;;
;;; These tests do not need the lightpanda binary or a network: they check
;;; link extraction on a fixed HTML string and the behavior when the
;;; binary cannot be found. Run with:  raco test tests.rkt

(require rackunit)
(require "lightpanda.rkt")

(define-syntax-rule (quietly body ...)
  (parameterize ([current-error-port (open-output-nowhere)])
    body ...))

(test-case "extract-links returns href values in document order"
  (check-equal?
   (extract-links
    "<html><body><a href=\"https://a.example/\">A</a>
     <p>text <a href=\"/local\">B</a></p></body></html>")
   '("https://a.example/" "/local")))

(test-case "extract-links on a page with no links"
  (check-equal? (extract-links "<html><body><p>none</p></body></html>") '())
  (check-equal? (extract-links "") '()))

(test-case "a missing binary gives #f and an empty link list, not an exception"
  (parameterize ([lightpanda-binary "no-such-lightpanda-binary-xyz"])
    (check-false (quietly (fetch-url "https://example.com/")))
    (check-equal? (quietly (fetch-and-extract-links "https://example.com/"))
                  '())))
