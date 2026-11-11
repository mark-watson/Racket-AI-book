#lang racket

;;; tests.rkt — Regression tests for the OPS5 interpreter
;;; Apache 2 License
;;;
;;; ops5.rkt is one large program with a REPL, so the tests drive it the
;;; way a reader does: start it on an .ops file, type commands, and check
;;; what it prints. Run with:  raco test tests.rkt

(require rackunit racket/runtime-path)

(define-runtime-path here ".")

;; Run ops5.rkt on OPS-FILE, feed it COMMANDS on standard input, and
;; return everything it printed.
(define (run-ops5 ops-file . commands)
  (define racket-exe (find-executable-path "racket"))
  (define out (open-output-string))
  (parameterize ([current-directory here]
                 [current-output-port out]
                 [current-error-port out]
                 [current-input-port
                  (open-input-string
                   (string-append (string-join commands "\n") "\n(exit)\n"))])
    (system* racket-exe "ops5.rkt" ops-file))
  (get-output-string out))

(define (firings output)
  (match (regexp-match #rx"\\(([0-9]+) firings" output)
    [(list _ n) (string->number n)]
    [_ #f]))

(test-case "draw.ops finds the pairs and three of a kind"
  (define output (run-ops5 "draw.ops" "(run)"))
  (check-true (string-contains? output "found a pair"))
  (check-true (string-contains? output "three of a kind"))
  (check-true (string-contains? output "End -- no production true"))
  (check-true (string-contains? output "(2 productions"))
  (check-equal? (firings output) 28))

(test-case "monkey.ops plans its way to the bananas"
  (define output (run-ops5 "monkey.ops" "(make start 1)" "(run)"))
  ;; the plan must carry the ladder, climb it, and end at the bananas
  (define steps
    (list "I will carry ladder to 2-2"
          "I will now climb onto ladder"
          "what I want to do now is get the bananas"
          "End -- no production true"))
  (for ([step steps])
    (check-true (string-contains? output step) step))
  ;; and the steps must come in that order
  (define positions
    (for/list ([step steps])
      (caar (regexp-match-positions (regexp-quote step) output))))
  (check-equal? positions (sort positions <))
  (check-true (string-contains? output "(19 productions"))
  (check-equal? (firings output) 16))

(test-case "with no start fact nothing fires"
  (define output (run-ops5 "monkey.ops" "(run)"))
  (check-equal? (firings output) 0))
