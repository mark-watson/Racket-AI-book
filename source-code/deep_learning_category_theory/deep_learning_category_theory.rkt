#lang racket

;; Category-Theory Deep Learning Framework in Racket
;; Reference: Jia, Peng, Yang & Chen (2025) "Category-Theoretical and
;; Topos-Theoretical Frameworks in Machine Learning" Axioms 14(3):204.
;; https://doi.org/10.3390/axioms14030204
;;
;; Five categorical perspectives, each self-contained:
;;   I.   Para category + lens composition   (compositional backprop)
;;   II.  Markov categories                  (dropout, Bayesian MC)
;;   III. Invariance / equivariance          (colimit pooling, k-means)
;;   IV.  Topos                              (subobject classifier, sheaves)
;;   V.   Natural transformations            (knowledge distillation)

(require racket/list racket/format racket/random)

;; ── I. Para category: layers are lenses, backprop is pullback composition ──

(struct layer-params (W b))
(struct layer-grads  (dW db))
(struct model (layers))                    ; list of layer-params

(define (sigmoid z)       (/ 1.0 (+ 1.0 (exp (- z)))))
(define (sigmoid-deriv a) (* a (- 1.0 a)))

(define (dot ws xs)     (for/sum ([w ws] [x xs]) (* w x)))
(define (matvec M v)    (map (λ (row) (dot row v)) M))
(define (vec-add u v)   (map + u v))
(define (outer d x)     (map (λ (di) (map (λ (xi) (* di xi)) x)) d))
(define (transpose M)   (apply map list M))
(define (matT-vec M v)  (matvec (transpose M) v))

;; forward-para : params × act × act-deriv × inputs
;;              → (values outputs pullback)   where pullback : ∇out → (∇params ∇in)
(define (forward-para params act act-deriv inputs)
  (define W (layer-params-W params))
  (define b (layer-params-b params))
  (define zs   (vec-add (matvec W inputs) b))
  (define acts (map act zs))
  (define (pullback upstream)
    (define delta (map (λ (u z) (* u (act-deriv z))) upstream zs))
    (values (layer-grads (outer delta inputs) delta) (matT-vec W delta)))
  (values acts pullback))

;; Forward pass through all layers, stacking pullbacks for reversal.
(define (network-forward m xs)
  (let loop ([params-list (model-layers m)] [current-input xs] [pullbacks '()])
    (if (null? params-list)
        (values (car current-input) (reverse pullbacks))
        (let-values ([(acts pb) (forward-para (car params-list) sigmoid
                                              (compose sigmoid-deriv sigmoid)
                                              current-input)])
          (loop (cdr params-list) acts (cons pb pullbacks))))))

(define (mse-loss y-hat y)      (sqr (- y-hat y)))
(define (mse-loss-grad y-hat y) (* 2.0 (- y-hat y)))

;; Backprop = compose the pullbacks in reverse order.
(define (model-backward pullbacks dl-dy)
  (let go ([pbs (reverse pullbacks)] [upstream (list dl-dy)] [acc '()])
    (if (null? pbs)
        acc
        (let-values ([(grads dx) ((car pbs) upstream)])
          (go (cdr pbs) dx (cons grads acc))))))

;; SGD update θ ← θ − η∇θ — an endomorphism on the model.
(define (update-layer params grads η)
  (layer-params
   (map (λ (wi dWi) (map (λ (w dw) (- w (* η dw))) wi dWi))
        (layer-params-W params) (layer-grads-dW grads))
   (map (λ (bi dbi) (- bi (* η dbi)))
        (layer-params-b params) (layer-grads-db grads))))

(define (model-update m grads-list η)
  (model (map (λ (p g) (update-layer p g η)) (model-layers m) grads-list)))

(define (train-step m xs y η)
  (define-values (pred pullbacks) (network-forward m xs))
  (values (model-update m (model-backward pullbacks (mse-loss-grad pred y)) η)
          (mse-loss pred y)))

(define (glorot fan-in fan-out)
  (define lim (sqrt (/ 6.0 (+ fan-in fan-out))))
  (- (* 2.0 lim (random)) lim))

(define (make-layer fan-in fan-out)
  (layer-params (for/list ([_ fan-out]) (for/list ([__ fan-in]) (glorot fan-in fan-out)))
                (make-list fan-out 0.0)))

(define (make-network arch)
  (model (map (λ (spec) (make-layer (car spec) (cadr spec))) arch)))

(define (train m dataset η epochs print-every)
  (let loop ([cur m] [epoch 0])
    (if (= epoch epochs)
        cur
        (let-values ([(m* total)
                      (for/fold ([acc-m cur] [acc-loss 0.0]) ([pair dataset])
                        (define-values (m** loss) (train-step acc-m (car pair) (cadr pair) η))
                        (values m** (+ acc-loss loss)))])
          (when (zero? (modulo epoch print-every))
            (printf "  Epoch ~a  loss: ~a\n" epoch (~r total #:precision 6)))
          (loop m* (add1 epoch))))))

(define (predict m xs)
  (define-values (pred _) (network-forward m xs))
  pred)

;; ── II. Markov categories: stochastic morphisms ──

;; Dropout as a stochastic lens: the forward pass samples a Bernoulli(p)
;; mask, and the backward pass reuses the same mask (closed optic).
(define (make-dropout-lens keep-prob)
  (λ (inputs)
    (define mask (for/list ([x inputs]) (if (< (random) keep-prob) 1.0 0.0)))
    (define scale (/ 1.0 keep-prob))
    (define (pullback upstream) (map (λ (u m) (* u m scale)) upstream mask))
    (values (map (λ (x m) (* x m scale)) inputs mask) pullback)))

(define (gaussian-sample μ σ)              ; Box-Muller
  (define u1 (+ 1e-10 (random)))
  (define u2 (random))
  (+ μ (* σ (sqrt (* -2.0 (log u1))) (cos (* 2.0 pi u2)))))

;; Bayesian layer: W ~ N(μ, σ) sampled at each forward pass.
(struct bayesian-layer (mu sigma fan-out))

(define (make-bayesian-layer fan-in fan-out #:sigma [σ 0.1])
  (bayesian-layer (for/list ([_ fan-out]) (for/list ([__ fan-in]) (glorot fan-in fan-out)))
                  σ fan-out))

(define (bayesian-forward bl inputs)
  (define σ (bayesian-layer-sigma bl))
  (define W (map (λ (row) (map (λ (w) (gaussian-sample w σ)) row)) (bayesian-layer-mu bl)))
  (map sigmoid (vec-add (matvec W inputs) (make-list (bayesian-layer-fan-out bl) 0.0))))

;; Monte Carlo estimate of predictive mean and epistemic uncertainty.
(define (bayesian-predict-mc bl inputs n)
  (define samples (for/list ([_ n]) (car (bayesian-forward bl inputs))))
  (define mean (/ (apply + samples) n))
  (values mean (sqrt (/ (apply + (map (λ (s) (sqr (- s mean))) samples)) n))))

;; ── III. Invariance / equivariance ──

;; Σ xᵢ is the colimit over the set diagram — permutation-invariant by
;; commutativity of addition (basis of DeepSets).
(define (permutation-invariant-pool xs) (apply + xs))

(define (euclidean-dist u v) (sqrt (apply + (map (λ (a b) (sqr (- a b))) u v))))

(define (nearest-centroid point centroids)
  (define dists (map (λ (c) (euclidean-dist point c)) centroids))
  (define min-d (apply min dists))
  (for/first ([i (in-naturals)] [d dists] #:when (= d min-d)) i))

;; K-means: each centroid is the colimit (average) of its cluster.
(define (update-centroids data labels k)
  (for/list ([c (in-range k)])
    (define cluster (for/list ([p data] [l labels] #:when (= l c)) p))
    (if (null? cluster)
        (make-list (length (car data)) 0.0)
        (let ([n (length cluster)])
          (map (λ (vs) (/ (apply + vs) n)) (apply map list cluster))))))

(define (k-means data k max-iter)
  (let loop ([cents (take (shuffle data) k)] [iter 0])
    (define labels (map (λ (p) (nearest-centroid p cents)) data))
    (define new-cents (update-centroids data labels k))
    (if (or (= iter max-iter) (equal? cents new-cents))
        (values new-cents labels)
        (loop new-cents (add1 iter)))))

;; ── IV. Topos: subobject classifier, sheaf gluing ──

;; The sigmoid output IS the characteristic morphism χ : X → Ω; the
;; decision boundary is χ⁻¹(0.5).
(define (subobject-classify model xs)
  (define prob (predict model xs))
  (values prob (if (>= prob 0.5) 1 0)))

(struct sheaf-section (context prediction))

;; Glue local predictions into a global one iff all pairwise predictions
;; agree within tol (the sheaf condition); #f means inconsistent.
(define (sheaf-glue sections tol)
  (define n (length sections))
  (define (consistent? i j)
    (< (abs (- (sheaf-section-prediction (list-ref sections i))
               (sheaf-section-prediction (list-ref sections j)))) tol))
  (if (for*/and ([i (in-range n)] [j (in-range (add1 i) n)]) (consistent? i j))
      (/ (apply + (map sheaf-section-prediction sections)) n)
      #f))

;; Ω carries a Heyting algebra (intuitionistic logic) — min/max/1−x.
(define (heyting-and p q)        (min p q))
(define (heyting-or  p q)        (max p q))
(define (heyting-not p)          (- 1.0 p))
(define (heyting-implies p q)    (heyting-or (heyting-not p) q))

;; ── V. Natural transformation η : F ⇒ G (knowledge distillation) ──

(struct nat-transform (adapter-W adapter-b))

(define (make-nat-transform source-size target-size)
  (nat-transform (for/list ([_ target-size]) (for/list ([__ source-size]) (glorot source-size target-size)))
                 (make-list target-size 0.0)))

(define (apply-nat-transform nt v)
  (map sigmoid (vec-add (matvec (nat-transform-adapter-W nt) v) (nat-transform-adapter-b nt))))

;; ── Demos ──

(module+ main
  (random-seed 42)
  (displayln "Category-Theory Deep Learning Framework in Racket")
  (displayln "Reference: Jia et al. (2025) Axioms 14(3):204")

  ;; I. XOR via compositional backprop
  (displayln "\n== I. Para category + lens composition (XOR) ==")
  (define xor-data '(((0.0 0.0) 0.0) ((0.0 1.0) 1.0) ((1.0 0.0) 1.0) ((1.0 1.0) 0.0)))
  (define trained-xor (train (make-network '((2 4) (4 4) (4 1))) xor-data 0.5 6000 2000))
  (for ([pair xor-data])
    (define y-hat (predict trained-xor (car pair)))
    (printf "  ~a -> target ~a, pred ~a, class ~a\n"
            (car pair) (cadr pair) (~r y-hat #:precision 4) (if (> y-hat 0.5) 1 0)))

  ;; II-A. Dropout as a stochastic lens
  (displayln "\n== II. Markov categories ==")
  (define-values (masked-vec pb-fn) ((make-dropout-lens 0.7) '(1.0 2.0 3.0 4.0 5.0)))
  (printf "  dropout(0.7): ~a -> ~a\n" '(1.0 2.0 3.0 4.0 5.0)
          (map (λ (x) (~r x #:precision 3)) masked-vec))
  (printf "  grad (same mask): ~a\n"
          (map (λ (x) (~r x #:precision 3)) (pb-fn '(1.0 1.0 1.0 1.0 1.0))))

  ;; II-B. Bayesian layer, MC uncertainty
  (define-values (μ-est σ-est)
    (bayesian-predict-mc (make-bayesian-layer 3 1 #:sigma 0.3) '(1.0 0.5 -0.5) 200))
  (printf "  Bayesian MC (200 samples): mean ~a, std ~a\n"
          (~r μ-est #:precision 4) (~r σ-est #:precision 4))

  ;; III-A. Permutation-invariant pooling
  (displayln "\n== III. Invariance / equivariance ==")
  (printf "  pool(1 3 5 2) = ~a, pool(3 1 2 5) = ~a (same result)\n"
          (permutation-invariant-pool '(1.0 3.0 5.0 2.0))
          (permutation-invariant-pool '(3.0 1.0 2.0 5.0)))

  ;; III-B. K-means as colimit computation
  (define cluster-data
    '((1.0 1.0) (1.2 0.8) (0.9 1.1) (5.0 5.0) (5.1 4.9) (4.8 5.2) (1.0 5.0) (0.9 4.8) (1.1 5.1)))
  (define-values (centroids labels) (k-means cluster-data 3 50))
  (for ([c centroids] [i (in-naturals)])
    (printf "  cluster ~a centroid: (~a, ~a)\n" i (~r (car c) #:precision 2) (~r (cadr c) #:precision 2)))
  (printf "  labels: ~a\n" labels)

  ;; IV-A. Subobject classifier on the trained XOR model
  (displayln "\n== IV. Topos ==")
  (for ([pair xor-data])
    (define-values (prob cls) (subobject-classify trained-xor (car pair)))
    (printf "  χ(~a) = ~a -> class ~a\n" (car pair) (~r prob #:precision 4) cls))

  ;; IV-B. Sheaf gluing
  (define s1 (sheaf-section "A" 0.72))
  (define s2 (sheaf-section "B" 0.68))
  (define s3 (sheaf-section "C" 0.91))
  (printf "  glue A+B (tol 0.1): ~a\n"
          (let ([g (sheaf-glue (list s1 s2) 0.1)]) (if g (~r g #:precision 4) "INCONSISTENT")))
  (printf "  glue A+C (tol 0.1): ~a\n"
          (let ([g (sheaf-glue (list s1 s3) 0.1)]) (if g (~r g #:precision 4) "INCONSISTENT")))

  ;; IV-C. Heyting algebra
  (printf "  Heyting: ~a ∧ ~a = ~a, ¬~a = ~a, ~a ⇒ ~a = ~a\n"
          0.8 0.3 (heyting-and 0.8 0.3) 0.8 (heyting-not 0.8) 0.8 0.3 (heyting-implies 0.8 0.3))

  ;; V. Natural transformation (teacher hidden → student)
  (displayln "\n== V. Natural transformation (knowledge distillation) ==")
  (define teacher-hidden
    (let* ([layers (model-layers trained-xor)]
           [l1 (list-ref layers 0)]
           [l2 (list-ref layers 1)])
      (define-values (a1 _pb1) (forward-para l1 sigmoid (compose sigmoid-deriv sigmoid) '(1.0 0.0)))
      (define-values (a2 _pb2) (forward-para l2 sigmoid (compose sigmoid-deriv sigmoid) a1))
      a2))
  (printf "  teacher hidden: ~a\n" (map (λ (x) (~r x #:precision 4)) teacher-hidden))
  (printf "  student rep:    ~a\n"
          (map (λ (x) (~r x #:precision 4)) (apply-nat-transform (make-nat-transform 4 2) teacher-hidden))))
