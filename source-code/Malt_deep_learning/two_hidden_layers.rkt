#lang racket

(require malt)

;; Two hidden layers: 2 -> 8 -> 8 -> 1
;; Synthetic task: classify whether a 2-D point lies inside a circle of radius 0.6

(define circle-network
  (stack-blocks
   (list
    (block relu (list (list 8 2) (list 8)))    ; hidden layer 1
    (block relu (list (list 8 8) (list 8)))    ; hidden layer 2
    (block relu (list (list 1 8) (list 1)))))) ; output layer

(define circle-theta-shapes (block-ls circle-network))

;; Generate 300 synthetic training points in [-1,1] x [-1,1]
(define num-samples 300)

(random-seed 7)

(define circle-xs
  (list->tensor
   (for/list ((_ (in-range num-samples)))
     (tensor (- (* 2.0 (random)) 1.0) (- (* 2.0 (random)) 1.0)))))

(define circle-ys
  (list->tensor
   (for/list ((i (in-range num-samples)))
     (let ((x (tref (tref circle-xs i) 0))
           (y (tref (tref circle-xs i) 1)))
       (tensor (if (< (+ (* x x) (* y y)) 0.36) 1.0 0.0))))))

(random-seed 1) ; re-seed so initial weights are reproducible regardless of data generation

(define trained-theta
  (with-hypers ((revs 8000)
                (alpha 0.005)
                (batch-size 16))
    (naked-gradient-descent
     (sampling-obj (l2-loss (block-fn circle-network)) circle-xs circle-ys)
     (init-theta circle-theta-shapes))))

(define circle-model (model (block-fn circle-network) trained-theta))

;; Quick self-check: accuracy on the training data
(define correct
  (for/sum ((i (in-range num-samples)))
    (let ((pred (tref (circle-model (tref circle-xs i)) 0))
          (truth (tref (tref circle-ys i) 0)))
      (if (equal? (> pred 0.5) (> truth 0.5)) 1 0))))

(printf "Training accuracy: ~a/~a~%" correct num-samples)

(printf "Sample predictions:~%")
(for ((x (in-list (list (tensor 0.0 0.0) (tensor 0.9 0.9)
                        (tensor 0.3 0.2) (tensor -0.8 0.5)))))
  (printf "  ~a -> ~a~%" x (tref (circle-model x) 0)))
