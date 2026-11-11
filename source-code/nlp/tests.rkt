#lang racket

;;; tests.rkt — Offline unit tests for the NLP library
;;; Apache 2 License
;;;
;;; The tagger and name finders are deterministic and use only the local
;;; data files. Run with:  raco test tests.rkt

(require rackunit)
(require "main.rkt")

;; The library prints trace output while it works; keep the test log quiet
(define-syntax-rule (quietly body ...)
  (parameterize ([current-output-port (open-output-nowhere)])
    body ...))

(test-case "part-of-speech tagging"
  (check-equal? (quietly (parts-of-speech (vector "the" "cat" "ran")))
                (vector "DT" "NN" "VBD")))

(test-case "one tag is returned per input word"
  (define words (vector "banking" "in" "Europe" "is" "a" "good" "business"))
  (define tags (quietly (parts-of-speech words)))
  (check-equal? (vector-length tags) (vector-length words))
  (check-true (for/and ([t tags]) (string? t))))

(test-case "place names"
  (define found
    (quietly
     (find-place-names
      (vector "George" "Bush" "went" "to" "San" "Diego" "and" "London") '())))
  (check-equal? (sort found string<?) '("London" "San Diego")))

(test-case "human names"
  (define (names . words)
    (quietly (find-human-names (list->vector words) '())))
  (check-not-false (member "Bill Gates" (names "who" "is" "Bill" "Gates")))
  ;; a title in front of a name is kept as part of the name
  (check-not-false
   (member "President George Bush"
           (names "President" "George" "Bush" "went" "to" "San" "Diego")))
  (define two (names "John" "Smith" "visited" "Mary" "Jones" "in" "London"))
  (check-not-false (member "John Smith" two))
  (check-not-false (member "Mary Jones" two)))

(test-case "text with no names"
  (check-equal? (quietly (find-place-names (vector "qqqq" "eeee" "gggg") '())) '())
  (check-equal? (quietly (find-human-names (vector "xxx" "eee" "tt") '())) '()))
