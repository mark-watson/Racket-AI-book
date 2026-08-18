#lang racket

(require malt)
(require (only-in racket/base [+ r+] [- r-] [/ r/] [abs r-abs]))

;; Jointly learned recommendation model.
;; Each sample combines TWO kinds of input:
;;   a. customer features (bargain-hunter, quality-seeker, novelty-seeker) -> 3 numbers
;;   b. product features  (inverse-price, quality, popularity, novelty)    -> 4 numbers
;; A "customer tower" and a "product tower" each learn an embedding;
;; the embeddings are concatenated and a final linear layer predicts the rating.

(define cust-dim 3)
(define prod-dim 4)
(define input-dim 7) ; cust-dim + prod-dim (literal: malt's + makes duals)
(define emb-dim 4)

;; Slice out the two input types with constant 0/1 selection matrices
;; (malt has no slice op; dot-product with a constant matrix is differentiable).
(define (selection-matrix n offset)
  (list->tensor
   (for/list ((i (in-range n)))
     (list->tensor
      (for/list ((j (in-range input-dim)))
        ;; NB: base + via r+ and equal? -- malt's + breaks eq? here
        (if (equal? j (r+ i offset)) 1.0 0.0))))))

(define select-customer (selection-matrix cust-dim 0))
(define select-product  (selection-matrix prod-dim cust-dim))

;; Custom block function.
;; theta layout: [cust-W cust-b prod-W prod-b out-W out-b]
(define rec-block-fn
  (λ (t)
    (λ (theta)
      (let ((cust-emb (rectify ((linear (dot-product-2-1 select-customer t)) theta)))
            (prod-emb (rectify ((linear (dot-product-2-1 select-product t)) (refr theta 2)))))
        ((linear (concat cust-emb prod-emb)) (refr theta 4))))))

(define rec-theta-shapes
  (list (list emb-dim cust-dim) (list emb-dim)     ; customer tower
        (list emb-dim prod-dim) (list emb-dim)     ; product tower
        (list 1 8) (list 1)))                      ; output layer (8 = 2 x emb-dim)

;;*------ Synthetic training data ------
;; Hidden "ground truth": rating = customer tastes . matching product attrs + noise.

(define num-samples 400)

(random-seed 5)

(define (random-vec n)
  (list->tensor (for/list ((_ (in-range n))) (random))))

(define rec-xs
  (list->tensor
   (for/list ((_ (in-range num-samples)))
     (concat (random-vec cust-dim) (random-vec prod-dim)))))

(define rec-ys
  (list->tensor
   (for/list ((i (in-range num-samples)))
     (let ((x (tref rec-xs i)))
       (tensor (+ (+ 0.1
                     (* 0.8 (+ (+ (* (tref x 0) (tref x 3)) ; bargain-hunter x cheap
                                  (* (tref x 1) (tref x 4))) ; quality-seeker x quality
                               (* (tref x 2) (tref x 6))))) ; novelty-seeker x novel
                  (* 0.05 (- (random) 0.5))))))))

(random-seed 5) ; re-seed so initial weights are reproducible

(define trained-theta
  (with-hypers ((revs 8000)
                (alpha 0.01)
                (batch-size 16))
    (naked-gradient-descent
     (sampling-obj (l2-loss rec-block-fn) rec-xs rec-ys)
     (init-theta rec-theta-shapes))))

(define rec-model (model rec-block-fn trained-theta))

;; trained theta holds duals; unwrap to plain numbers
(define (realize x) (if (dual? x) (ρ x) x))

;; Quick self-check: mean absolute error on training data
(define total-error
  (for/fold ((acc 0.0)) ((i (in-range num-samples)))
    (r+ acc (r-abs (r- (realize (tref (rec-model (tref rec-xs i)) 0))
                       (realize (tref (tref rec-ys i) 0)))))))

(printf "Mean absolute error over ~a samples: ~a~%"
        num-samples (r/ total-error num-samples))

;; Recommend: score a new customer against three candidate products
(define new-customer (tensor 0.9 0.8 0.1)) ; bargain hunter + quality seeker

(define candidate-products
  (list (tensor 0.9 0.9 0.2 0.1)   ; cheap, high quality
        (tensor 0.1 0.9 0.9 0.9)   ; expensive, high quality, popular, novel
        (tensor 0.5 0.2 0.1 0.9))) ; mid price, low quality, novel

(printf "Predicted ratings for new customer ~a:~%" new-customer)
(for ((p (in-list candidate-products)))
  (printf "  product ~a -> ~a~%" p
          (realize (tref (rec-model (concat new-customer p)) 0))))
