#lang racket

;;; tests.rkt — Offline tests for deep_learning_category_theory.rkt
;;; Apache 2 License
;;;
;;; The file exports nothing, so require/expose is used to reach its
;;; functions. Run with:  raco test tests.rkt

(require rackunit)
(require/expose "deep_learning_category_theory.rkt"
                (make-network network-forward train train-step predict
                 model-layers layer-params-W layer-params-b layer-params model
                 mse-loss permutation-invariant-pool k-means nearest-centroid
                 update-centroids sheaf-section sheaf-glue
                 heyting-and heyting-or heyting-not heyting-implies
                 make-nat-transform apply-nat-transform))

(define xor-data '(((0.0 0.0) 0.0) ((0.0 1.0) 1.0) ((1.0 0.0) 1.0) ((1.0 1.0) 0.0)))

(define (total-loss m)
  (for/sum ([pair xor-data])
    (mse-loss (predict m (first pair)) (second pair))))

(test-case "Para: the backward pass matches a numerical gradient"
  (random-seed 7)
  (define m (make-network '((2 3) (3 1))))
  (define xs '(0.3 -0.8))
  (define y 1.0)
  (define η 1e-3)
  ;; one SGD step moves weight w by -η * dL/dw, so the step itself
  ;; reveals the analytic gradient
  (define-values (m2 _loss) (train-step m xs y η))
  (define (first-weight mm) (caar (layer-params-W (first (model-layers mm)))))
  (define analytic (/ (- (first-weight m) (first-weight m2)) η))
  ;; numerical gradient for the same weight
  (define (loss-with w)
    (define l1 (first (model-layers m)))
    (define W (layer-params-W l1))
    (define W* (cons (cons w (cdr (car W))) (cdr W)))
    (define m* (model (cons (layer-params W* (layer-params-b l1))
                            (cdr (model-layers m)))))
    (mse-loss (predict m* xs) y))
  (define h 1e-5)
  (define w0 (first-weight m))
  (define numeric (/ (- (loss-with (+ w0 h)) (loss-with (- w0 h))) (* 2 h)))
  (check-= analytic numeric 1e-5))

(test-case "Para: training reduces the XOR loss"
  (random-seed 42)
  (define m (make-network '((2 4) (4 1))))
  (define before (total-loss m))
  (define trained
    (parameterize ([current-output-port (open-output-nowhere)])
      (train m xor-data 0.5 3000 1000)))
  (check-true (< (total-loss trained) (* 0.5 before))))

(test-case "invariance: sum pooling ignores the order of its inputs"
  (check-= (permutation-invariant-pool '(1.0 2.0 3.5))
           (permutation-invariant-pool '(3.5 1.0 2.0)) 1e-12))

(test-case "colimits: k-means separates two well-separated clusters"
  (random-seed 1)
  (define data '((0.0 0.1) (0.1 0.0) (0.05 0.05) (5.0 5.1) (5.1 5.0) (4.9 5.0)))
  (define-values (centroids labels) (k-means data 2 50))
  (check-equal? (length centroids) 2)
  ;; the first three points share a label, the last three share the other
  (check-equal? (length (remove-duplicates (take labels 3))) 1)
  (check-equal? (length (remove-duplicates (drop labels 3))) 1)
  (check-not-equal? (first labels) (last labels)))

(test-case "nearest-centroid and update-centroids"
  (check-equal? (nearest-centroid '(1.0 1.0) '((0.0 0.0) (9.0 9.0))) 0)
  (check-equal? (update-centroids '((0.0 0.0) (2.0 2.0) (10.0 10.0)) '(0 0 1) 2)
                '((1.0 1.0) (10.0 10.0))))

(test-case "sheaf gluing: consistent sections glue, inconsistent ones do not"
  (check-= (sheaf-glue (list (sheaf-section 'a 0.80) (sheaf-section 'b 0.82)) 0.05)
           0.81 1e-9)
  (check-false
   (sheaf-glue (list (sheaf-section 'a 0.2) (sheaf-section 'b 0.9)) 0.05)))

(test-case "Heyting algebra on truth values in [0,1]"
  (check-= (heyting-and 0.3 0.7) 0.3 1e-12)
  (check-= (heyting-or 0.3 0.7) 0.7 1e-12)
  (check-= (heyting-not 0.3) 0.7 1e-12)
  (check-= (heyting-implies 0.3 0.6) 0.7 1e-12)
  ;; on the classical values 0 and 1 it agrees with Boolean logic
  (check-= (heyting-implies 1.0 0.0) 0.0 1e-12)
  (check-= (heyting-implies 0.0 0.0) 1.0 1e-12))

(test-case "natural transformation: the adapter maps between layer sizes"
  (define nt (make-nat-transform 4 2))
  (check-equal? (length (apply-nat-transform nt '(0.1 0.2 0.3 0.4))) 2))
