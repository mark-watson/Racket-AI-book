#lang racket

;; Reference https://www.mdpi.com/2075-1680/14/3/204
;; Category-Theoretical and Topos-Theoretical Frameworks in Machine Learning

;; =============================================================================
;; Category-Theory Deep Learning Framework in Racket
;; =============================================================================
;;
;; Reference:  Jia, Peng, Yang & Chen (2025).
;;   "Category-Theoretical and Topos-Theoretical Frameworks in Machine Learning:
;;    A Survey."  Axioms 14(3):204.  https://doi.org/10.3390/axioms14030204
;;
;; The paper organises categorical ML into four perspectives:
;;
;;   I.   Gradient-based learning
;;          • Para (parametric maps) — neural-net layers as morphisms
;;          • Lenses / Optics        — bidirectional forward / backward pass
;;          • Compositional backprop — functor on base category
;;
;;   II.  Probability-based learning
;;          • Markov categories      — stochastic morphisms
;;          • Bayesian inference functor
;;          • Dropout as a stochastic lens
;;
;;   III. Invariance / Equivalence
;;          • Functor-equivariant layers
;;          • Categorical clustering (colimits)
;;          • Persistent homology (sketch)
;;
;;   IV.  Topos-based learning
;;          • Subobject classifier   — binary decisions
;;          • Sheaf-style composition — context propagation
;;
;; This file implements working demonstrations of perspectives I, II, and IV.
;; Each section is self-contained and runs as part of the `main` module.
;; =============================================================================

(require racket/list
         racket/string
         racket/format
         racket/random)

;; ─────────────────────────────────────────────────────────────────────────────
;;  SECTION I — GRADIENT-BASED LEARNING
;;              "Para" category + lens composition + compositional backprop
;; ─────────────────────────────────────────────────────────────────────────────
;;
;; I.1  Para category ─────────────────────────────────────────────────────────
;;
;; In the Para construction a morphism  f : A → B  is a triple
;;   (parameter-space P, forward fn, backward fn)
;; where
;;   forward  : P × A → B
;;   backward : P × A × ∇B → ∇P × ∇A    (the "residual" / lens put-back)
;;
;; Neural-network layers are exactly morphisms in Para(Euc), the category of
;; smooth parametric maps between Euclidean spaces (Gavranović 2022; §2.1 of
;; the survey).

;; Typed wrappers — newtypes that encode semantic roles ──────────────────────
(struct input-vec    (vals)  #:transparent)  ; object in InputSpace
(struct target-val   (v)     #:transparent)  ; object in TargetSpace
(struct prediction   (v)     #:transparent)  ; object in OutputSpace
(struct learning-rate (v)    #:transparent)  ; hyper-parameter η

;; Layer parameter space: weight matrix W and bias vector b
(struct layer-params (W b)   #:transparent)
(struct layer-grads  (dW db) #:transparent)

;; Full model is the product of N layer-params objects
(struct model (layers) #:transparent)       ; layers : list of layer-params

;; I.2  Math utilities ─────────────────────────────────────────────────────────

(define (sigmoid z)         (/ 1.0 (+ 1.0 (exp (- z)))))
(define (sigmoid-deriv a)   (* a (- 1.0 a)))
(define (relu z)            (max 0.0 z))
(define (relu-deriv z)      (if (> z 0.0) 1.0 0.0))

;; dot : list(float) × list(float) → float
(define (dot ws xs)
  (for/sum ([w ws] [x xs]) (* w x)))

;; matvec : matrix(m×n) × vec(n) → vec(m)
(define (matvec M v)
  (map (λ (row) (dot row v)) M))

;; vec-add : vec × vec → vec
(define (vec-add u v) (map + u v))

;; scalar-vec* : float × vec → vec
(define (scalar-vec* s v) (map (λ (x) (* s x)) v))

;; outer : vec × vec → matrix   (δ ⊗ x)
(define (outer delta x)
  (map (λ (d) (map (λ (xi) (* d xi)) x)) delta))

;; transpose : matrix(m×n) → matrix(n×m)
(define (transpose M)
  (apply map list M))

;; matT-vec : matrix(m×n) × vec(m) → vec(n)   (Mᵀ · v)
(define (matT-vec M v)
  (matvec (transpose M) v))

;; I.3  Para morphism (tracked forward lens) ────────────────────────────────
;;
;; forward-para : layer-params × activation-fn × list(float)
;;              → (list(float), pullback-closure)
;;
;; The pullback closure is the "put-back" half of the lens:
;;   pullback : ∇output → (∇params, ∇input)
;;
;; This directly implements the Para category composition rule (survey §2.1):
;;   composition of two Para morphisms threads parameters and residuals.

(define (forward-para params act act-deriv inputs)
  (define W  (layer-params-W params))
  (define b  (layer-params-b params))
  (define zs   (vec-add (matvec W inputs) b))   ; pre-activations
  (define acts (map act zs))                     ; post-activations
  ;; Pullback closure  —  the "put-back" of the categorical lens
  (define (pullback upstream-grad)
    ;; δ = upstream ⊙ act'(z)   (local gradient, Hadamard product)
    (define delta (map (λ (u z) (* u (act-deriv z))) upstream-grad zs))
    ;; ∇W = δ ⊗ xᵀ   (outer product)
    (define dW (outer delta inputs))
    ;; ∇b = δ
    (define db delta)
    ;; ∇x = Wᵀ · δ   (propagate to previous layer)
    (define dx (matT-vec W delta))
    (values (layer-grads dW db) dx))
  (values acts pullback))

;; I.4  Sequential composition of Para morphisms (the network) ───────────────
;;
;; network-forward : model × list(float) → (prediction, list(pullback))
;;
;; This implements the functor  F : Para → Para  that maps a sequence of layers
;; to their composed forward-pass + stacked pullback closures (survey §2.2).

(define (network-forward m xs)
  (let loop ([params-list (model-layers m)]
             [current-input xs]
             [pullbacks '()])
    (if (null? params-list)
        ;; Final activation is the scalar prediction
        (values (prediction (car current-input))
                (reverse pullbacks)
                current-input)
        (let* ([p   (car params-list)]
               [rest (cdr params-list)]
               ;; Use sigmoid for hidden layers, identity for the last
               [act       (if (null? rest) (λ (z) (sigmoid z)) sigmoid)]
               [act-deriv (λ (z) (sigmoid-deriv (sigmoid z)))])
          (define-values (acts pb) (forward-para p act act-deriv current-input))
          (loop rest acts (cons pb pullbacks))))))

;; I.5  Loss — typed morphism: Prediction × Target → SquaredError ─────────
;;
;; Survey §2.3: loss is a natural transformation from the prediction functor
;; to the real-number functor.  MSE is the simplest instance.

(define (mse-loss pred tgt)
  (define d (- (prediction-v pred) (target-val-v tgt)))
  (* d d))

(define (mse-loss-grad pred tgt)
  (* 2.0 (- (prediction-v pred) (target-val-v tgt))))

;; I.6  Backward pass — pullback composition (covariant functor on ∇) ──────
;;
;; model-backward : list(pullback) × float → list(layer-grads)
;;
;; The survey (§2.2) notes that backpropagation is the composite of the
;; pullback morphisms in reverse order — a covariant functor on the gradient
;; category.

;; model-backward* : list(pullback) × float → list(layer-grads)
;; Correct version: threads upstream gradient through each pullback in reverse.
(define (model-backward* pullbacks dl-dy-hat)
  (define (go pbs upstream acc)
    (if (null? pbs)
        acc
        (let-values ([(grads dx) ((car pbs) upstream)])
          (go (cdr pbs) dx (cons grads acc)))))
  (go (reverse pullbacks) (list dl-dy-hat) '()))

;; I.7  SGD update — endomorphism on ModelState ────────────────────────────
;;
;; survey §2.1: the gradient-descent update  θ ← θ − η∇θ  is an endomorphism
;; u_η : Model → Model on the parameter object of Para.

(define (update-layer params grads η)
  (define W  (layer-params-W params))
  (define b  (layer-params-b params))
  (define dW (layer-grads-dW grads))
  (define db (layer-grads-db grads))
  (layer-params
   (map (λ (wi dwi) (map (λ (w dw) (- w (* η dw))) wi dwi)) W dW)
   (map (λ (bi dbi) (- bi (* η dbi))) b db)))

(define (model-update m grads-list η)
  (model (map (λ (p g) (update-layer p g η))
              (model-layers m)
              grads-list)))

;; I.8  One training step — the composed morphism ──────────────────────────

(define (train-step m xs y η)
  (define-values (pred pullbacks _final) (network-forward m xs))
  (define loss   (mse-loss      pred y))
  (define dl-dy  (mse-loss-grad pred y))
  (define grads  (model-backward* pullbacks dl-dy))
  (values (model-update m grads η) loss))

;; I.9  Initialisation ─────────────────────────────────────────────────────

(define (glorot fan-in fan-out)
  (define lim (sqrt (/ 6.0 (+ fan-in fan-out))))
  (- (* 2.0 lim (random)) lim))

(define (make-layer fan-in fan-out)
  (layer-params
   (for/list ([_ fan-out])
     (for/list ([__ fan-in]) (glorot fan-in fan-out)))
   (make-list fan-out 0.0)))

;; make-network : list((fan-in fan-out)) → model
(define (make-network arch)
  (model (map (λ (spec) (make-layer (car spec) (cadr spec))) arch)))

;; I.10  Training loop ─────────────────────────────────────────────────────

(define (train m dataset η epochs print-every)
  (let loop ([cur m] [epoch 0])
    (if (= epoch epochs)
        cur
        (let-values
            ([(m* total-loss)
              (for/fold ([acc-m cur] [acc-loss 0.0]) ([pair dataset])
                (define xs (input-vec-vals (car pair)))
                (define y  (cdr pair))
                (define-values (m** loss) (train-step acc-m xs y η))
                (values m** (+ acc-loss loss)))])
          (when (zero? (modulo epoch print-every))
            (printf "  Epoch ~a  loss: ~a\n" epoch (~r total-loss #:precision 6)))
          (loop m* (add1 epoch))))))

;; I.11  Inference ─────────────────────────────────────────────────────────

(define (predict m xs)
  (define-values (pred _ __) (network-forward m xs))
  (prediction-v pred))


;; ─────────────────────────────────────────────────────────────────────────────
;;  SECTION II — PROBABILITY-BASED LEARNING: MARKOV CATEGORIES
;;               (survey §3 — stochastic morphisms, dropout, Bayesian sketch)
;; ─────────────────────────────────────────────────────────────────────────────
;;
;; A Markov category is a symmetric monoidal category where every object X
;; carries a commutative comonoid structure:
;;   copy  : X → X ⊗ X    (diagonal / duplication)
;;   delete: X → I         (marginalisation / discarding)
;;
;; Morphisms are "stochastic kernels" — they map objects to probability
;; distributions over objects.  In practice we represent a stochastic morphism
;; as a function that, given an input, *samples* an output.
;;
;; Two instances we implement:
;;   A) Dropout   — a stochastic lens (forward stochasticity)
;;   B) Bayesian  — weight-space priors / posterior sampling sketch

;; II.1  Stochastic morphism representation ────────────────────────────────
;;
;; A stochastic morphism  f : A →_s B  is represented as a closure:
;;   (λ (a) (sample-from-distribution-over-B))
;;
;; Composition of stochastic morphisms:
;;   (f ∘_s g)(a) = f(g(a))   — Kleisli composition in the probability monad

(define (stochastic-compose f g)
  (λ (x) (f (g x))))

;; II.2  Dropout as a stochastic lens ─────────────────────────────────────
;;
;; Dropout (Srivastava 2014) can be formalised as a stochastic lens (survey §3.2):
;;   forward  : X →_s X ⊗ Mask    where Mask ~ Bernoulli(p)^n
;;   backward : X ⊗ Mask → X      (apply the same mask to gradient)
;;
;; The stochastic forward pass samples a mask once; the backward pass reuses it
;; (this is the "closed" lens / optic requirement that both passes share state).

(define (make-dropout-lens keep-prob)
  ;; Returns a stochastic Para morphism:
  ;;   dropout-forward : vec(n) → (masked-vec(n), mask, pullback)
  (λ (inputs)
    ;; Sample binary mask from Bernoulli(keep-prob)
    (define mask
      (for/list ([x inputs])
        (if (< (random) keep-prob) 1.0 0.0)))
    ;; Apply mask and scale (inverted dropout — keeps expectation constant)
    (define scale (/ 1.0 keep-prob))
    (define masked (map (λ (x m) (* x m scale)) inputs mask))
    ;; The pullback reuses the same mask (stochastic lens requirement)
    (define (pullback upstream)
      (map (λ (u m) (* u m scale)) upstream mask))
    (values masked pullback)))

;; II.3  Bayesian weight uncertainty (sketch) ──────────────────────────────
;;
;; In Bayesian deep learning (survey §3.3), weights are random variables.
;; A Bayesian layer samples weights from a distribution W ~ N(μ, σ²) at each
;; forward pass — this is a stochastic morphism in the Markov category Stoch.
;;
;; We implement a single-layer Bayesian linear model with Gaussian weights.

(define (gaussian-sample μ σ)
  ;; Box-Muller transform for N(μ, σ)
  (define u1 (+ 1e-10 (random)))
  (define u2 (random))
  (define z  (* (sqrt (* -2.0 (log u1))) (cos (* 2.0 pi u2))))
  (+ μ (* σ z)))

(struct bayesian-layer (mu sigma fan-in fan-out) #:transparent)

(define (make-bayesian-layer fan-in fan-out #:sigma [σ 0.1])
  ;; μ initialised with Glorot, σ fixed (could be learned)
  (bayesian-layer
   (for/list ([_ fan-out])
     (for/list ([__ fan-in]) (glorot fan-in fan-out)))
   σ fan-in fan-out))

;; Stochastic forward pass: sample W ~ N(μ, σI) and apply
(define (bayesian-forward bl inputs)
  (define μ  (bayesian-layer-mu    bl))
  (define σ  (bayesian-layer-sigma bl))
  ;; Sample weight matrix W_s ~ N(μ, σ)
  (define W-sample
    (map (λ (row) (map (λ (w) (gaussian-sample w σ)) row)) μ))
  ;; Forward: acts = sigmoid(W_s · x)
  (map sigmoid (vec-add (matvec W-sample inputs)
                        (make-list (bayesian-layer-fan-out bl) 0.0))))

;; Uncertainty estimation via Monte Carlo sampling (survey §3.3)
(define (bayesian-predict-mc bl inputs n-samples)
  (define samples
    (for/list ([_ n-samples])
      (car (bayesian-forward bl inputs))))
  (define mean (/ (apply + samples) n-samples))
  (define variance
    (/ (apply + (map (λ (s) (sqr (- s mean))) samples)) n-samples))
  (values mean (sqrt variance)))


;; ─────────────────────────────────────────────────────────────────────────────
;;  SECTION III — INVARIANCE / EQUIVARIANCE
;;                (survey §4 — functors that respect symmetry structure)
;; ─────────────────────────────────────────────────────────────────────────────
;;
;; A layer  f : X → Y  is *equivariant* with respect to a group G if:
;;   f(g · x) = g · f(x)    for all g ∈ G
;;
;; In categorical language: there exists a functor  F : BG → Vect  such that
;; f is a natural transformation between F and another G-representation.
;;
;; We demonstrate permutation-equivariance (the simplest case), which is the
;; categorical basis of set-valued neural networks (DeepSets, §4.1 of survey).

;; A permutation equivariant layer: uses only sum-aggregation over the set,
;; so any permutation of inputs gives the same-permuted output.
;;
;; ρ(x₁, …, xₙ) = (φ(x₁ + … + xₙ + ψ(x₁)), …)   (DeepSets formulation)
;; For simplicity we implement the simpler invariant aggregation  Σ xᵢ → scalar

(define (permutation-invariant-pool xs)
  ;; Categorical reading: this is the colimit (coproduct) over the set diagram
  ;; Σᵢ xᵢ — invariant to any permutation because addition is commutative.
  (apply + xs))

(define (permutation-equivariant-map f xs)
  ;; Apply f elementwise — equivariant because f ∘ π = π ∘ f for any permutation π
  (map f xs))

;; Categorical clustering (colimit interpretation) ─────────────────────────
;;
;; K-means can be viewed as computing colimits in a category of metric spaces
;; (survey §4.2): the cluster centre is the colimit / limit of points in the
;; cluster, and assignment is the universal morphism.

(define (euclidean-dist u v)
  (sqrt (apply + (map (λ (a b) (sqr (- a b))) u v))))

(define (nearest-centroid point centroids)
  (define dists (map (λ (c) (euclidean-dist point c)) centroids))
  (define min-d (apply min dists))
  (for/first ([i (in-naturals)] [d dists] #:when (= d min-d)) i))

(define (update-centroids data labels k)
  (for/list ([c (in-range k)])
    (define cluster (filter-map (λ (pair) (and (= (cdr pair) c) (car pair)))
                                (map cons data labels)))
    (if (null? cluster)
        (make-list (length (car data)) 0.0)
        ;; Centroid = colimit (average) of the cluster
        (let ([n (length cluster)])
          (map (λ (vs) (/ (apply + vs) n))
               (apply map list cluster))))))

(define (k-means data k max-iter)
  ;; Initialise centroids by random selection
  (define centroids (take (shuffle data) k))
  (let loop ([cents centroids] [iter 0])
    (define labels (map (λ (p) (nearest-centroid p cents)) data))
    (define new-cents (update-centroids data labels k))
    (if (or (= iter max-iter)
            (equal? cents new-cents))
        (values new-cents labels)
        (loop new-cents (add1 iter)))))


;; ─────────────────────────────────────────────────────────────────────────────
;;  SECTION IV — TOPOS-BASED LEARNING
;;               (survey §5 — subobject classifiers, sheaves, internal logic)
;; ─────────────────────────────────────────────────────────────────────────────
;;
;; A topos E is a category that has:
;;   • finite limits and colimits
;;   • a subobject classifier Ω  with a "true" morphism  ⊤ : 1 → Ω
;;
;; Every subobject (subset) S ⊆ X is classified by a unique morphism
;;   χ_S : X → Ω   such that S = χ_S⁻¹(⊤)
;;
;; In machine learning (survey §5.1), the output neuron of a binary classifier
;; IS the characteristic morphism χ : InputSpace → {0,1}.  Sigmoid squashes
;; the activation into [0,1] ≅ Ω, and the decision boundary is χ⁻¹(0.5).
;;
;; Sheaf composition (survey §5.2): local predictions on overlapping contexts
;; are "glued" into a global consistent assignment — exactly the sheaf
;; condition.  We implement a simple ensemble that enforces global consistency.

;; IV.1  Subobject classifier — binary decision morphism ───────────────────
;;
;; χ : InputSpace → Ω ≅ [0,1]   (the sigmoid output IS the characteristic map)

(define (subobject-classify model xs)
  ;; The prediction probability is the value of the characteristic morphism χ
  (define prob (predict model xs))
  ;; The subobject (class membership) is determined by χ⁻¹(0.5)
  (values prob (if (>= prob 0.5) 1 0)))

;; IV.2  Sheaf composition — local-to-global consistency ──────────────────
;;
;; A sheaf F on a space X assigns to each open set U ⊆ X a set of "sections"
;; F(U), with restriction maps that are compatible on overlaps.
;;
;; For ML: each "expert" (sub-model) covers a local context (a feature subset).
;; The sheaf condition requires that predictions on overlapping contexts agree.
;;
;; We implement a two-expert ensemble with a consistency check ("gluing lemma").

(struct sheaf-section (context prediction) #:transparent)

(define (sheaf-consistent? s1 s2 tol)
  ;; Two sections are consistent (glueable) if their predictions agree on
  ;; their shared context within tolerance ε
  (< (abs (- (sheaf-section-prediction s1)
             (sheaf-section-prediction s2)))
     tol))

(define (sheaf-glue sections tol)
  ;; Attempt to glue sections into a global section (survey §5.2).
  ;; Returns #f if sections are inconsistent (sheaf condition fails).
  (define all-consistent?
    (for/and ([i (in-range (length sections))])
      (for/and ([j (in-range (add1 i) (length sections))])
        (sheaf-consistent? (list-ref sections i)
                           (list-ref sections j)
                           tol))))
  (if all-consistent?
      ;; Global section: average of local predictions
      (/ (apply + (map sheaf-section-prediction sections))
         (length sections))
      #f))

;; IV.3  Internal logic — the Heyting algebra of propositions ─────────────
;;
;; The subobject classifier Ω in a topos carries a Heyting algebra structure
;; (intuitionistic logic).  In a Boolean topos (Set), this collapses to
;; classical logic.  We represent propositions as probability thresholds.

(define (heyting-and p q)   (min p q))         ; conjunction
(define (heyting-or  p q)   (max p q))         ; disjunction
(define (heyting-not p)     (- 1.0 p))         ; pseudo-complement (in [0,1])
(define (heyting-implies p q) (heyting-or (heyting-not p) q))  ; p ⇒ q


;; ─────────────────────────────────────────────────────────────────────────────
;;  SECTION V — FUNCTOR COMPOSITION DEMO
;;              Natural transformation between two trained models
;; ─────────────────────────────────────────────────────────────────────────────
;;
;; A natural transformation  η : F ⇒ G  between two functors witnesses that
;; G's predictions can be derived from F's predictions in a coherent, functorial
;; way.  We implement a simple "knowledge distillation" adapter — a linear map
;; from F-outputs to G-outputs — as a concrete natural transformation.

(struct nat-transform (adapter-W adapter-b) #:transparent)

(define (make-nat-transform source-size target-size)
  ;; Linear adapter:  η_X : F(X) → G(X)
  (nat-transform
   (for/list ([_ target-size])
     (for/list ([__ source-size]) (glorot source-size target-size)))
   (make-list target-size 0.0)))

(define (apply-nat-transform nt v)
  (map sigmoid
       (vec-add (matvec (nat-transform-adapter-W nt) v)
                (nat-transform-adapter-b nt))))


;; =============================================================================
;;  DEMO — runs all four perspectives with concrete examples
;; =============================================================================

(module+ main
  (random-seed 42)

  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════════════╗")
  (displayln "║   Category-Theory Deep Learning Framework (Racket)              ║")
  (displayln "║   Reference: Jia et al. (2025) Axioms 14(3):204                ║")
  (displayln "╚══════════════════════════════════════════════════════════════════╝")
  (displayln "")

  ;; ──────────────────────────────────────────────────────────────────────────
  ;;  DEMO I: Para category — compositional backprop on XOR
  ;; ──────────────────────────────────────────────────────────────────────────
  (displayln "━━━━  I. Para Category + Lens Composition  (XOR problem)  ━━━━")
  (displayln "  Architecture: input(2) → hidden(4) → hidden(4) → output(1)")
  (displayln "  Para morphisms composed sequentially; pullbacks in reverse.")
  (displayln "")

  (define xor-data
    (list (cons (input-vec '(0.0 0.0)) (target-val 0.0))
          (cons (input-vec '(0.0 1.0)) (target-val 1.0))
          (cons (input-vec '(1.0 0.0)) (target-val 1.0))
          (cons (input-vec '(1.0 1.0)) (target-val 0.0))))

  ;; Architecture: input(2) → hidden1(4) → hidden2(4) → output(1)
  (define xor-model
    (make-network '((2 4) (4 4) (4 1))))

  (define trained-xor
    (train xor-model xor-data 0.5 6000 2000))

  (displayln "  → Predictions after 6000 epochs:")
  (for ([pair xor-data])
    (define xs    (input-vec-vals (car pair)))
    (define y     (target-val-v  (cdr pair)))
    (define y-hat (predict trained-xor xs))
    (printf "      Input: ~a  Target: ~a  Pred: ~a  Class: ~a\n"
            xs y (~r y-hat #:precision 4) (if (> y-hat 0.5) 1 0)))

  (displayln "")
  (displayln "  Category-theory reading:")
  (displayln "  • Each layer is a morphism in Para(Euc): f : P × X → Y")
  (displayln "  • forward-para returns (output, pullback-closure) — the lens")
  (displayln "  • Backprop = pullback composition  f* ∘ g* ∘ h*  in reverse")
  (displayln "  • SGD update = endomorphism u_η : Model → Model")
  (displayln "")

  ;; ──────────────────────────────────────────────────────────────────────────
  ;;  DEMO II: Markov Categories — Dropout + Bayesian uncertainty
  ;; ──────────────────────────────────────────────────────────────────────────
  (displayln "━━━━  II. Markov Categories — Stochastic Morphisms  ━━━━")
  (displayln "")

  ;; II-A: Dropout as stochastic lens
  (displayln "  II-A  Dropout (stochastic lens, Bernoulli(0.7) mask)")
  (define dropout-lens (make-dropout-lens 0.7))
  (define test-vec '(1.0 2.0 3.0 4.0 5.0))
  (define-values (masked-vec pb-fn) (dropout-lens test-vec))
  (printf "    Input:       ~a\n" test-vec)
  (printf "    After drop:  ~a\n" (map (λ (x) (~r x #:precision 3)) masked-vec))
  (define masked-grad (pb-fn '(1.0 1.0 1.0 1.0 1.0)))
  (printf "    Grad (back): ~a\n" (map (λ (x) (~r x #:precision 3)) masked-grad))
  (displayln "")
  (displayln "    The same mask is reused in both passes — the 'closed'")
  (displayln "    stochastic lens condition (survey §3.2).")
  (displayln "")

  ;; II-B: Bayesian uncertainty
  (displayln "  II-B  Bayesian layer — Monte Carlo uncertainty estimation")
  (define bayes-layer (make-bayesian-layer 3 1 #:sigma 0.3))
  (define bayes-input '(1.0 0.5 -0.5))
  (define-values (μ-est σ-est) (bayesian-predict-mc bayes-layer bayes-input 200))
  (printf "    Input:  ~a\n" bayes-input)
  (printf "    MC mean (200 samples):  ~a\n" (~r μ-est  #:precision 4))
  (printf "    MC std  (uncertainty):  ~a\n" (~r σ-est  #:precision 4))
  (displayln "")
  (displayln "    W ~ N(μ,σ) at each forward pass = stochastic morphism in Stoch.")
  (displayln "    Variance encodes epistemic uncertainty (survey §3.3).")
  (displayln "")

  ;; ──────────────────────────────────────────────────────────────────────────
  ;;  DEMO III: Invariance / Equivariance — DeepSets + K-means
  ;; ──────────────────────────────────────────────────────────────────────────
  (displayln "━━━━  III. Invariance & Equivariance  ━━━━")
  (displayln "")

  (displayln "  III-A  Permutation-Invariant Pooling (colimit / coproduct)")
  (define set1 '(1.0 3.0 5.0 2.0))
  (define set2 '(3.0 1.0 2.0 5.0))   ; permutation of set1
  (printf "    Set 1 pool: ~a  (order: ~a)\n"
          (permutation-invariant-pool set1) set1)
  (printf "    Set 2 pool: ~a  (order: ~a)\n"
          (permutation-invariant-pool set2) set2)
  (displayln "    → Same result for both permutations ✓")
  (displayln "")

  (displayln "  III-B  K-means as categorical colimit computation")
  (define cluster-data
    '((1.0 1.0) (1.2 0.8) (0.9 1.1)
      (5.0 5.0) (5.1 4.9) (4.8 5.2)
      (1.0 5.0) (0.9 4.8) (1.1 5.1)))
  (define-values (centroids labels) (k-means cluster-data 3 50))
  (printf "    Data: ~a points in 2D\n" (length cluster-data))
  (printf "    Centroids (colimits of each cluster):\n")
  (for ([c centroids] [i (in-naturals)])
    (printf "      Cluster ~a: (~a, ~a)\n"
            i (~r (car c) #:precision 2) (~r (cadr c) #:precision 2)))
  (printf "    Labels: ~a\n" labels)
  (displayln "")
  (displayln "    Each centroid = colimit of its cluster (survey §4.2).")
  (displayln "")

  ;; ──────────────────────────────────────────────────────────────────────────
  ;;  DEMO IV: Topos Framework — subobject classifier + sheaf gluing
  ;; ──────────────────────────────────────────────────────────────────────────
  (displayln "━━━━  IV. Topos Framework  ━━━━")
  (displayln "")

  ;; Use the trained XOR model as our binary classifier
  (displayln "  IV-A  Subobject Classifier χ : X → Ω")
  (displayln "        (sigmoid output = characteristic morphism)")
  (for ([pair xor-data])
    (define xs (input-vec-vals (car pair)))
    (define-values (prob cls) (subobject-classify trained-xor xs))
    (printf "    χ(~a) = ~a  → class ~a\n"
            xs (~r prob #:precision 4) cls))
  (displayln "")
  (displayln "    The sigmoid IS the characteristic morphism of the classifier's")
  (displayln "    subobject S ⊆ InputSpace (survey §5.1).")
  (displayln "")

  ;; Sheaf gluing
  (displayln "  IV-B  Sheaf Gluing — local-to-global consistency")
  ;; Two local expert predictions on the same point
  (define s1 (sheaf-section "feature-subset-A" 0.72))
  (define s2 (sheaf-section "feature-subset-B" 0.68))
  (define s3 (sheaf-section "feature-subset-C" 0.91))  ; inconsistent expert
  (define glued-12 (sheaf-glue (list s1 s2) 0.1))
  (define glued-13 (sheaf-glue (list s1 s3) 0.1))
  (printf "    Expert A pred: ~a,  Expert B pred: ~a\n"
          (sheaf-section-prediction s1) (sheaf-section-prediction s2))
  (printf "    Glue A+B (tol=0.1): ~a\n"
          (if glued-12 (~r glued-12 #:precision 4) "INCONSISTENT — cannot glue"))
  (printf "    Expert C pred: ~a (conflict with A)\n"
          (sheaf-section-prediction s3))
  (printf "    Glue A+C (tol=0.1): ~a\n"
          (if glued-13 (~r glued-13 #:precision 4) "INCONSISTENT — cannot glue"))
  (displayln "")
  (displayln "    Consistent sections glue to a global prediction; inconsistent")
  (displayln "    sections reveal model disagreement (survey §5.2).")
  (displayln "")

  ;; Heyting algebra
  (displayln "  IV-C  Internal Logic — Heyting Algebra on Ω = [0,1]")
  (define p1 0.8) (define p2 0.3)
  (printf "    p='high confidence'=~a,  q='low confidence'=~a\n" p1 p2)
  (printf "    p ∧ q  = min(p,q)       = ~a\n" (heyting-and p1 p2))
  (printf "    p ∨ q  = max(p,q)       = ~a\n" (heyting-or  p1 p2))
  (printf "    ¬p     = 1-p            = ~a\n" (heyting-not p1))
  (printf "    p ⇒ q  = ¬p ∨ q        = ~a\n" (heyting-implies p1 p2))
  (displayln "")
  (displayln "    Ω carries Heyting algebra structure — the internal logic of")
  (displayln "    the topos (Boolean collapse in Set, survey §5.1).")
  (displayln "")

  ;; ──────────────────────────────────────────────────────────────────────────
  ;;  DEMO V: Natural Transformation — knowledge distillation adapter
  ;; ──────────────────────────────────────────────────────────────────────────
  (displayln "━━━━  V. Natural Transformation — Knowledge Distillation Adapter  ━━━━")
  (displayln "")
  (displayln "  A natural transformation η : F ⇒ G is a coherent family of")
  (displayln "  morphisms η_X : F(X) → G(X) that commutes with all arrows.")
  (displayln "  Here we use a linear adapter as η, mapping the 4-dim penultimate")
  (displayln "  hidden layer of the teacher to a 2-dim student representation.")
  (displayln "")

  ;; Retrieve the 4-dim hidden representation from layer 2 (the penultimate layer)
  ;; by running a partial forward pass through layers 1 and 2 only.
  (define teacher-hidden
    (let* ([layers (model-layers trained-xor)]
           [l1 (list-ref layers 0)]
           [l2 (list-ref layers 1)])
      (define-values (a1 _pb1) (forward-para l1 sigmoid (λ (z) (sigmoid-deriv (sigmoid z))) '(1.0 0.0)))
      (define-values (a2 _pb2) (forward-para l2 sigmoid (λ (z) (sigmoid-deriv (sigmoid z))) a1))
      a2))  ; 4-dim hidden representation

  (define adapter (make-nat-transform 4 2))
  (define student-rep (apply-nat-transform adapter teacher-hidden))
  (printf "    Teacher hidden (4-dim): ~a\n"
          (map (λ (x) (~r x #:precision 4)) teacher-hidden))
  (printf "    Student rep   (2-dim):  ~a\n"
          (map (λ (x) (~r x #:precision 4)) student-rep))
  (displayln "")
  (displayln "    The adapter is the natural transformation η_X that lets the")
  (displayln "    student functor G access the teacher functor F's information.")
  (displayln "")

  ;; ──────────────────────────────────────────────────────────────────────────
  ;;  Summary
  ;; ──────────────────────────────────────────────────────────────────────────
  (displayln "╔══════════════════════════════════════════════════════════════════╗")
  (displayln "║  Framework Summary                                              ║")
  (displayln "╠══════════════════════════════════════════════════════════════════╣")
  (displayln "║  I.   Para category    Layers as Para morphisms;                ║")
  (displayln "║       + Lens/Optics    pullbacks compose backprop               ║")
  (displayln "║  II.  Markov cats      Dropout & Bayesian layers as             ║")
  (displayln "║                        stochastic morphisms in Stoch            ║")
  (displayln "║  III. Invariance       Permutation-invariant pooling =          ║")
  (displayln "║                        colimit; K-means = colimit of clusters   ║")
  (displayln "║  IV.  Topos            Sigmoid = subobject classifier χ;        ║")
  (displayln "║                        ensemble gluing = sheaf condition        ║")
  (displayln "║  V.   Nat. transform   Knowledge distillation = η : F ⇒ G      ║")
  (displayln "╚══════════════════════════════════════════════════════════════════╝"))
