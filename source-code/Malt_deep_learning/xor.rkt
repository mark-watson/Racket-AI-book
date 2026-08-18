#lang racket

(require malt)

;; Simple XOR network: 2 -> 8 -> 1

(define xor-network
  (stack-blocks
   (list
    (block relu (list (list 8 2) (list 8)))    ; hidden layer: 2 inputs, 8 units
    (block relu (list (list 1 8) (list 1)))))) ; output layer: 8 inputs, 1 unit

(define xor-theta-shapes (block-ls xor-network))

(define xor-xs
  (tensor (tensor 0.0 0.0)
          (tensor 0.0 1.0)
          (tensor 1.0 0.0)
          (tensor 1.0 1.0)))

(define xor-ys
  (tensor (tensor 0.0)
          (tensor 1.0)
          (tensor 1.0)
          (tensor 0.0)))

(random-seed 42)

(define trained-theta
  (with-hypers ((revs 4000)
                (alpha 0.01)
                (batch-size 4))
    (naked-gradient-descent
     (sampling-obj (l2-loss (block-fn xor-network)) xor-xs xor-ys)
     (init-theta xor-theta-shapes))))

(define xor-model (model (block-fn xor-network) trained-theta))

(printf "XOR predictions (expect 0, 1, 1, 0):~%")
(for ((x (in-list (list (tensor 0.0 0.0) (tensor 0.0 1.0)
                        (tensor 1.0 0.0) (tensor 1.0 1.0)))))
  (printf "  ~a -> ~a~%" x (xor-model x)))
