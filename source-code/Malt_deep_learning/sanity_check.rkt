#lang racket
(require malt)
(require (only-in racket/base [+ r+] [- r-] [* r*] [min r-min] [max r-max]))
(define (realize x) (if (dual? x) (ρ x) x))

(define cust-dim 3) (define prod-dim 4)
(define num-samples 400)
(random-seed 5)
(define (random-vec n) (list->tensor (for/list ((_ (in-range n))) (random))))
(define rec-xs
  (list->tensor (for/list ((_ (in-range num-samples)))
    (concat (random-vec cust-dim) (random-vec prod-dim)))))
(define rec-ys
  (list->tensor (for/list ((i (in-range num-samples)))
    (let ((x (tref rec-xs i)))
      (tensor (+ (+ 0.1 (* 0.8 (+ (+ (* (tref x 0) (tref x 3))
                                     (* (tref x 1) (tref x 4)))
                                  (* (tref x 2) (tref x 6)))))
                 (* 0.05 (- (random) 0.5))))))))

(printf "xs shape: ~a  ys shape: ~a~%" (shape rec-xs) (shape rec-ys))

(for ((i (in-range 5)))
  (let* ((x (tref rec-xs i))
         (v (for/list ((j (in-range 7))) (realize (tref x j))))
         (expected (r+ 0.1 (r* 0.8 (r+ (r* (list-ref v 0) (list-ref v 3))
                                       (r+ (r* (list-ref v 1) (list-ref v 4))
                                           (r* (list-ref v 2) (list-ref v 6)))))))
         (actual (realize (tref (tref rec-ys i) 0))))
    (printf "sample ~a: expected(no-noise)=~a actual=~a |diff|=~a~%"
            i expected actual (abs (r- actual expected)))))

(define vals (for/list ((i (in-range num-samples))) (realize (tref (tref rec-ys i) 0))))
(printf "y: min=~a max=~a mean=~a~%"
        (apply r-min vals) (apply r-max vals) (/ (apply r+ vals) num-samples))
