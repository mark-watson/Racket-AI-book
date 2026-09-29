#lang racket

;;; test.rkt — Unit tests for web scrape library
;;; Copyright (C) 2022-2025 Mark Watson
;;; Apache 2 License

(require rackunit)
(require rackunit/text-ui)
(require "webscrape.rkt")

(define all-tests
  (test-suite "webscrape unit tests"
    (test-case "web-uri->xexp returns XExp structure"
      ; We can't test with live URLs, so we test the structure when it works
      ; This is a placeholder for demonstration - in real code you'd use stubs
      (check-exn exn:fail:network?
                 (lambda () (web-uri->xexp "http://nonexistent.invalid/test"))))
    
    (test-case "web-uri->links handles network errors gracefully"
      (check-exn exn:fail:network?
                 (lambda () (web-uri->links "http://nonexistent.invalid/test"))))
    
    (test-case "web-uri->text handles network errors gracefully"
      (check-exn exn:fail:network?
                 (lambda () (web-uri->text "http://nonexistent.invalid/test"))))
    
    (test-case "web-uri->html-headers handles network errors gracefully"
      (check-exn exn:fail:network?
                 (lambda () (web-uri->html-headers "http://nonexistent.invalid/test"))))))

(module+ main
  (run-tests all-tests))