#lang racket

;; Reference: https://rust-ml.com/materials/one-training-step-end-to-end.html

;; =============================================================================
;; Category-Theory-Inspired 2-Hidden-Layer Neural Network in Racket
;; =============================================================================
;;
;; Reference: https://rust-ml.com/materials/one-training-step-end-to-end.html
;;
;; Category-theory lens: every layer is a morphism that carries BOTH
;;   • a forward output  (the "get")
;;   • a backward context (the "put-back" / pullback)
;;
;; The four training-step stages, viewed through two lenses:
;;
;;   Stage         | Algebra lens          | Category theory lens
;;   --------------|------------------------|-----------------------------
;;   Forward       | z = Wx + b; σ(z)      | ParameterSpace × InputSpace
;;                 |                        |   → Prediction × Context
;;   Loss          | L = (ŷ - y)²          | Prediction × Target
;;                 |                        |   → SquaredError
;;   Backward      | ∇w = upstream · x     | Context × ∇ŷ → ∇P × ∇X
;;                 | chain rule all layers  | (pullback / covariant functor)
;;   Update        | w := w - η · ∇w       | ModelState → ModelState
;;                 |                        | (endomorphism on parameter space)
;;
;; Architecture: input(2) → hidden1(3) → hidden2(3) → output(1)
;; Activation:   sigmoid on every layer
;; Loss:         mean-squared error
;; Optimiser:    vanilla SGD
;; =============================================================================

;; ---------------------------------------------------------------------------
;; 1.  TYPED WRAPPERS  (newtypes that encode categorical roles)
;; ---------------------------------------------------------------------------
;; We use structs so that Prediction, Target, LearningRate etc. cannot be
;; accidentally confused — the type carries the semantic role of the value.

(struct input-vec  (vals)  #:transparent)  ; InputSpace  vector
(struct target-val (v)     #:transparent)  ; Target      scalar
(struct prediction (v)     #:transparent)  ; Prediction  scalar
(struct learning-rate (v)  #:transparent)  ; Hyper-parameter

;; A layer's parameter space: weight matrix W (rows × cols) and bias vector b.
(struct layer-params (W b) #:transparent)

;; Gradients live in the same shape as parameters.
(struct layer-grads  (dW db) #:transparent)

;; The full model is a product of three layer-params.
(struct model (l1 l2 l3) #:transparent)

;; ---------------------------------------------------------------------------
;; 2.  UTILITY MATH
;; ---------------------------------------------------------------------------

(define (sigmoid z)
  (/ 1.0 (+ 1.0 (exp (- z)))))

(define (sigmoid-deriv sig-val)
  (* sig-val (- 1.0 sig-val)))

;; Vector dot product
(define (dot ws xs)
  (for/sum ([w ws] [x xs]) (* w x)))

;; Hadamard (elementwise) product
(define (vec* xs ys)
  (map * xs ys))

;; Scalar × vector
(define (scalar-vec* s xs)
  (map (λ (x) (* s x)) xs))

;; ---------------------------------------------------------------------------
;; 3.  LAYER LENS
;; ---------------------------------------------------------------------------
;;
;; A Lens morphism for one fully-connected sigmoid layer:
;;
;;   forward-lens : LayerParams × input-vec → (output-vec × LayerContext)
;;
;; The LayerContext is the exact cached state needed to pull gradients back
;; through this layer during backprop — it is the "residual" of the lens.
;;
;; Category-theory reading:
;;   The layer is an arrow  f : P × X → Y × Ctx
;;   The pullback  f* : Ctx × ∇Y → ∇P × ∇X  is computed in backward-pullback.

(struct layer-context
  (inputs       ; input vector  (x for this layer)
   pre-act      ; pre-activation  z = Wx + b
   activations) ; post-activation a = σ(z)
  #:transparent)

;; forward-lens : layer-params × list(float) → (list(float) × layer-context)
;; Returns the pair  (activations, context).
(define (forward-lens params inputs)
  (define W  (layer-params-W params))
  (define b  (layer-params-b params))
  ;; Each row of W is the weight vector for one neuron.
  ;; z_i = dot(W[i], inputs) + b[i]
  (define zs   (map (λ (wi bi) (+ (dot wi inputs) bi)) W b))
  (define acts (map sigmoid zs))
  (values acts (layer-context inputs zs acts)))

;; backward-pullback : layer-context × upstream-gradient-vec → (layer-grads × input-grad-vec)
;;
;; Category-theory reading:
;;   f* (ctx, ∇Y) = (∇P, ∇X)
;;   ∇P = gradient with respect to layer parameters (W and b)
;;   ∇X = gradient to propagate to the previous layer
(define (backward-pullback ctx upstream-grad)
  ;; δ_i = upstream_i · σ'(z_i)    (local gradient)
  (define delta
    (map (λ (u z a) (* u (sigmoid-deriv a)))
         upstream-grad
         (layer-context-pre-act ctx)
         (layer-context-activations ctx)))
  ;; ∇W[i][j] = δ_i · x_j   (outer product)
  (define dW
    (map (λ (d) (map (λ (x) (* d x))
                     (layer-context-inputs ctx)))
         delta))
  ;; ∇b[i] = δ_i
  (define db delta)
  ;; ∇X[j] = Σ_i  δ_i · W[i][j]   (transpose-multiply)
  (define W-ctx
    ;; We need the original W; store params in context for this demo
    ;; We reconstruct it from outer products — but simpler: pass W separately.
    ;; For clarity we re-derive: ∇x_j = Σ_i delta_i * W_ij.
    ;; Since we don't store W in layer-context we compute dX symbolically.
    ;; (See note: in a real framework, you'd store W in the closure.)
    #f)  ; placeholder — see below
  (values (layer-grads dW db) delta))

;; We need W to compute ∇X.  A clean categorical solution closes over W
;; inside the pullback.  We do exactly that with a closure.

;; forward-lens/tracked : layer-params × input → (activations, pullback-fn)
;; The returned pullback-fn : upstream-grad → (layer-grads, input-grad)
(define (forward-lens/tracked params inputs)
  (define W  (layer-params-W params))
  (define b  (layer-params-b params))
  (define zs   (map (λ (wi bi) (+ (dot wi inputs) bi)) W b))
  (define acts (map sigmoid zs))
  ;; Build the pullback closure (the "put-back" of the lens).
  ;; This closure captures W, inputs, zs, acts from the forward pass.
  (define (pullback upstream-grad)
    ;; δ = elementwise:  upstream · σ'(a)
    (define delta (map (λ (u a) (* u (sigmoid-deriv a)))
                       upstream-grad acts))
    ;; ∇W  (outer product δ ⊗ inputs)
    (define dW (map (λ (d) (map (λ (x) (* d x)) inputs)) delta))
    ;; ∇b
    (define db delta)
    ;; ∇X = Wᵀ · δ
    (define dX
      (for/list ([j (in-range (length inputs))])
        (for/sum ([i (in-range (length delta))])
          (* (list-ref delta i)
             (list-ref (list-ref W i) j)))))
    (values (layer-grads dW db) dX))
  (values acts pullback))

;; ---------------------------------------------------------------------------
;; 4.  FULL NETWORK – FORWARD PASS (lens composition)
;; ---------------------------------------------------------------------------
;;
;; The network is the sequential composition of three lenses:
;;
;;   model-forward : Model × InputVec → (Prediction × NetworkContext)
;;
;; NetworkContext is a product of three layer contexts (one per layer),
;; together with their pullback closures, so the backward pass can traverse
;; them in reverse order.  This is the categorical composition of lenses.

(struct network-context
  (pb1 pb2 pb3  ; pullback closures for layers 1, 2, 3
   final-act)   ; final scalar activation (= prediction)
  #:transparent)

;; model-forward : model × list(float) → (prediction × network-context)
(define (model-forward m xs)
  (define-values (a1 pb1) (forward-lens/tracked (model-l1 m) xs))
  (define-values (a2 pb2) (forward-lens/tracked (model-l2 m) a1))
  (define-values (a3 pb3) (forward-lens/tracked (model-l3 m) a2))
  ;; Output layer has one neuron → a3 is a list of one element
  (define y-hat (car a3))
  (values (prediction y-hat)
          (network-context pb1 pb2 pb3 y-hat)))

;; ---------------------------------------------------------------------------
;; 5.  LOSS  (typed morphism: Prediction × Target → SquaredError)
;; ---------------------------------------------------------------------------

(define (mse-loss pred tgt)
  (define diff (- (prediction-v pred) (target-val-v tgt)))
  (* diff diff))

;; Loss gradient with respect to prediction:  dL/dŷ = 2(ŷ - y)
(define (mse-loss-grad pred tgt)
  (* 2.0 (- (prediction-v pred) (target-val-v tgt))))

;; ---------------------------------------------------------------------------
;; 6.  BACKWARD PASS (pullback composition in reverse)
;; ---------------------------------------------------------------------------
;;
;; Category-theory reading:
;;   The backward pass is the composition of the three pullback morphisms
;;   in reverse order, threading the upstream gradient through each.
;;
;;   f* ∘ g* ∘ h*  :  ∇Y → ∇P₃ × ∇P₂ × ∇P₁ × ∇X

;; model-backward : network-context × float → (layer-grads, layer-grads, layer-grads)
(define (model-backward ctx dl-dy-hat)
  ;; Layer 3 pullback (output layer: upstream is dL/da3 = dL/dŷ)
  (define-values (grads3 dX3)
    ((network-context-pb3 ctx) (list dl-dy-hat)))
  ;; Layer 2 pullback (upstream is dX3 — gradient arriving from layer 3)
  (define-values (grads2 dX2)
    ((network-context-pb2 ctx) dX3))
  ;; Layer 1 pullback (upstream is dX2)
  (define-values (grads1 dX1)
    ((network-context-pb1 ctx) dX2))
  (values grads1 grads2 grads3))

;; ---------------------------------------------------------------------------
;; 7.  UPDATE STEP  (endomorphism on ModelState)
;; ---------------------------------------------------------------------------
;;
;; Category-theory reading:
;;   SGD update is an endomorphism  u : Model → Model.
;;   It is closed on the parameter space — it maps a model to a new model
;;   of the same type.  Training is iterated application of this endomorphism.
;;
;;   u_η(θ) = θ - η · ∇θ

(define (update-layer params grads lr)
  (define η  (learning-rate-v lr))
  (define W  (layer-params-W params))
  (define b  (layer-params-b params))
  (define dW (layer-grads-dW grads))
  (define db (layer-grads-db grads))
  (layer-params
   ;; W := W - η · dW  (elementwise)
   (map (λ (wi dwi) (map (λ (w dw) (- w (* η dw))) wi dwi)) W dW)
   ;; b := b - η · db
   (map (λ (bi dbi) (- bi (* η dbi))) b db)))

;; model-update : model × grads1 × grads2 × grads3 × lr → model
;; This is the endomorphism u_η : Model → Model.
(define (model-update m g1 g2 g3 lr)
  (model (update-layer (model-l1 m) g1 lr)
         (update-layer (model-l2 m) g2 lr)
         (update-layer (model-l3 m) g3 lr)))

;; ---------------------------------------------------------------------------
;; 8.  ONE TRAINING STEP  (the composed morphism)
;; ---------------------------------------------------------------------------
;;
;; train-step : Model × InputVec × Target × LearningRate → Model × Loss
;;
;; This is the single end-to-end arrow that composes:
;;   forward-lens ∘ loss ∘ backward-pullback ∘ update

(define (train-step m xs y lr)
  ;; Forward pass (lens)
  (define-values (pred ctx) (model-forward m xs))
  ;; Loss and its gradient w.r.t. prediction
  (define loss  (mse-loss pred y))
  (define dl-dy (mse-loss-grad pred y))
  ;; Backward pass (pullback composition)
  (define-values (g1 g2 g3) (model-backward ctx dl-dy))
  ;; Update (endomorphism)
  (define m* (model-update m g1 g2 g3 lr))
  (values m* loss))

;; ---------------------------------------------------------------------------
;; 9.  PARAMETER INITIALISATION
;; ---------------------------------------------------------------------------

;; Glorot-uniform-ish initialisation: U(−limit, limit)
;; limit = sqrt(6 / (fan-in + fan-out))
(define (glorot-rand fan-in fan-out)
  (define limit (sqrt (/ 6.0 (+ fan-in fan-out))))
  (- (* 2.0 limit (random)) limit))

(define (make-layer fan-in fan-out)
  (layer-params
   ;; Weight matrix: fan-out rows, each of length fan-in
   (for/list ([_ fan-out])
     (for/list ([__ fan-in])
       (glorot-rand fan-in fan-out)))
   ;; Bias vector: zeros (common choice)
   (make-list fan-out 0.0)))

;; Build the full model: input(2) → hidden1(3) → hidden2(3) → output(1)
(define (make-model)
  (model (make-layer 2 3)   ; Layer 1: 2 inputs  → 3 neurons
         (make-layer 3 3)   ; Layer 2: 3 inputs  → 3 neurons
         (make-layer 3 1))) ; Layer 3: 3 inputs  → 1 output

;; ---------------------------------------------------------------------------
;; 10.  TRAINING LOOP  (iterated endomorphism)
;; ---------------------------------------------------------------------------

;; XOR dataset: the classic non-linearly-separable binary problem.
;; Inputs and targets live in their typed wrappers.
(define xor-data
  (list (cons (input-vec '(0.0 0.0)) (target-val 0.0))
        (cons (input-vec '(0.0 1.0)) (target-val 1.0))
        (cons (input-vec '(1.0 0.0)) (target-val 1.0))
        (cons (input-vec '(1.0 1.0)) (target-val 0.0))))

(define (train model dataset lr epochs)
  (define η (learning-rate lr))
  (let loop ([m model] [epoch 0])
    (if (= epoch epochs)
        m
        (let-values
            ([(m* total-loss)
              ;; One epoch = one pass over all training examples
              (for/fold ([cur-m m] [acc-loss 0.0])
                        ([pair dataset])
                (define xs (input-vec-vals (car pair)))
                (define y  (cdr pair))
                (define-values (m** loss) (train-step cur-m xs y η))
                (values m** (+ acc-loss loss)))])
          (when (zero? (modulo epoch 1000))
            (printf "Epoch ~a  total-loss: ~a\n" epoch (~r total-loss #:precision 6)))
          (loop m* (add1 epoch))))))

;; ---------------------------------------------------------------------------
;; 11.  INFERENCE
;; ---------------------------------------------------------------------------

(define (predict m xs)
  (define-values (pred _ctx) (model-forward m xs))
  (prediction-v pred))

;; ---------------------------------------------------------------------------
;; 12.  DEMO
;; ---------------------------------------------------------------------------

(module+ main
  (random-seed 42)

  (displayln "=== Category-Theory Neural Network (Racket) ===")
  (displayln "Architecture: input(2) → hidden1(3) → hidden2(3) → output(1)")
  (displayln "Problem:      XOR")
  (newline)

  ;; Build model
  (define init-model (make-model))

  ;; Train
  (define trained-model
    (train init-model xor-data 0.5 5001))

  ;; Evaluate
  (newline)
  (displayln "=== Predictions after training ===")
  (for ([pair xor-data])
    (define xs  (input-vec-vals (car pair)))
    (define y   (target-val-v  (cdr pair)))
    (define y-hat (predict trained-model xs))
    (printf "  Input: ~a  Target: ~a  Prediction: ~a  Class: ~a\n"
            xs
            y
            (~r y-hat #:precision 4)
            (if (> y-hat 0.5) 1 0)))

  (newline)
  (displayln "=== Category-theory concepts demonstrated ===")
  (displayln "  • Newtypes (input-vec, target-val, prediction, learning-rate)")
  (displayln "    prevent mixing up roles — the type IS the semantic tag.")
  (displayln "  • forward-lens/tracked returns (output, pullback-closure)")
  (displayln "    modelling the categorical Lens pattern: P × X → Y × Ctx")
  (displayln "  • backward-pullback composition threads ∇Y back through")
  (displayln "    each layer in reverse, implementing f* ∘ g* ∘ h*.")
  (displayln "  • model-update is an endomorphism u_η : Model → Model;")
  (displayln "    training is iterated application of this arrow."))
