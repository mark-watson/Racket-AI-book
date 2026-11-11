#lang racket

;;; tests.rkt — Offline unit tests for Knowledge Graph Navigator query parsing
;;; Apache 2 License
;;;
;;; Only the parsing step is tested; it uses the local NLP library and
;;; needs no network. Run with:  raco test tests.rkt

(require rackunit)
(require "main.rkt")

;; parse-query prints trace output; keep the test log quiet
(define (quiet-parse s)
  (parameterize ([current-output-port (open-output-nowhere)])
    (parse-query s)))

(test-case "a who question finds a person"
  (check-equal? (quiet-parse "who is Bill Gates?") '(person "Bill Gates")))

(test-case "a where question finds a place"
  (check-equal? (quiet-parse "where is San Francisco?") '(place "San Francisco")))

(test-case "questions with no recognized entity are unknown"
  (check-equal? (first (quiet-parse "who is xyzzy?")) 'unknown)
  (check-equal? (first (quiet-parse "where is xyzzy?")) 'unknown)
  (check-equal? (first (quiet-parse "what time is it?")) 'unknown))
