#lang racket

;;; tests.rkt — Offline tests for the autograd engine in microgpt.rkt
;;; Apache 2 License
;;;
;;; The engine's functions are not exported, so require/expose is used to
;;; reach them. Each analytic gradient is compared with a numerical
;;; finite-difference estimate. Run with:  raco test tests.rkt

(require rackunit)
(require/expose "microgpt.rkt"
                (make-val val-data val-grad
                 u+ u* u- u/ upow ulog uexp urelu back usum usoftmax))

;; d f / d x_i estimated by central differences
(define (numeric-grad f xs i [h 1e-5])
  (define (at delta)
    (val-data
     (apply f (for/list ([x xs] [j (in-naturals)])
                (make-val (if (= i j) (+ x delta) x))))))
  (/ (- (at h) (at (- h))) (* 2 h)))

;; Check every input's backprop gradient against the numerical estimate
(define (check-gradients f . xs)
  (define vals (map make-val xs))
  (back (apply f vals))
  (for ([v vals] [i (in-naturals)])
    (check-= (val-grad v) (numeric-grad f xs i) 1e-4
             (format "input ~a of ~a" i xs))))

(test-case "forward values"
  (check-= (val-data (u+ 2.0 3.0)) 5.0 1e-12)
  (check-= (val-data (u* 2.0 3.0)) 6.0 1e-12)
  (check-= (val-data (u- 2.0 3.0)) -1.0 1e-12)
  (check-= (val-data (u/ 3.0 2.0)) 1.5 1e-12)
  (check-= (val-data (urelu -2.0)) 0.0 1e-12)
  (check-= (val-data (usum (map make-val '(1.0 2.0 3.0)))) 6.0 1e-12))

(test-case "gradients of the primitive operations"
  (check-gradients u+ 2.0 3.0)
  (check-gradients u* 2.0 3.0)
  (check-gradients u- 2.0 3.0)
  (check-gradients u/ 3.0 2.0)
  (check-gradients (lambda (x) (upow x 3)) 1.7)
  (check-gradients ulog 2.5)
  (check-gradients uexp 0.3)
  (check-gradients urelu 1.2)
  (check-gradients urelu -1.2))

(test-case "a value used twice accumulates both gradient paths"
  (define x (make-val 3.0))
  (back (u* x x))                        ; d(x^2)/dx = 2x
  (check-= (val-grad x) 6.0 1e-9))

(test-case "gradient of a composite expression"
  (check-gradients
   (lambda (a b c)
     (u+ (u* (ulog a) (uexp b)) (u/ (urelu c) (u+ a 1.0))))
   1.5 0.2 2.0))

(test-case "softmax sums to one and has correct gradients"
  (define probs (usoftmax (map make-val '(1.0 2.0 0.5))))
  (check-= (apply + (map val-data probs)) 1.0 1e-9)
  ;; negative log-likelihood of the second class, as in training
  (check-gradients
   (lambda (a b c) (u* (ulog (second (usoftmax (list a b c)))) -1.0))
   1.0 2.0 0.5))
