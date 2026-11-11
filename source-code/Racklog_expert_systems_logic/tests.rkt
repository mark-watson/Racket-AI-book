#lang racket

;;; tests.rkt — Regression tests for the two Racklog examples
;;; Apache 2 License
;;;
;;; draw.rkt and monkey.rkt are scripts that print their answers, so the
;;; tests run each one and check the output. Run with:  raco test tests.rkt

(require rackunit racket/runtime-path)

(define-runtime-path here ".")

(define (run-script file)
  (define out (open-output-string))
  (parameterize ([current-directory here]
                 [current-output-port out])
    (system* (find-executable-path "racket") file))
  (get-output-string out))

(test-case "draw.rkt: three pairs and one three of a kind, no duplicates"
  (define lines (string-split (run-script "draw.rkt") "\n"))
  (define pairs (filter (lambda (l) (string-prefix? l "pair:")) lines))
  (define threes
    (filter (lambda (l) (string-prefix? l "three of a kind:")) lines))
  (check-equal? (length pairs) 3)
  (check-equal? (length (remove-duplicates pairs)) 3)
  (check-equal? threes
                '("three of a kind: 10 of club, diamond, and heart")))

(test-case "monkey.rkt: the plan is the nine steps from couch to bananas"
  (define output (run-script "monkey.rkt"))
  (define steps
    (for/list ([l (string-split output "\n")]
               #:when (regexp-match? #px"^\\d+\\. " l))
      (second (regexp-match #px"^\\d+\\. (.*)$" l))))
  (check-equal? steps
                '("(jump-down-from couch)"
                  "(walk 5-7 to 2-2)"
                  "(walk 2-2 to 9-5)"
                  "(pick-up ladder)"
                  "(carry ladder to 5-7)"
                  "(carry ladder to 2-2)"
                  "(drop ladder)"
                  "(climb ladder)"
                  "(grasp bananas)")))
