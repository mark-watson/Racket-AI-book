#lang racket

;;; neural_net.rkt -- A tiny feed-forward network for the DQN example.
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; This is deliberately small and dependency-free: a fully connected
;;; multilayer perceptron with ReLU or tanh hidden units, a linear output
;;; layer, backpropagation, and the Adam optimizer.  It is the function
;;; approximator that Deep Q-Networks replace the Q table with.
;;;
;;; Representation
;;; --------------
;;; `sizes` is a vector such as #(4 64 64 2): the input width, each hidden
;;; width, and the output width.  For layer l (0-based) the weights are a
;;; flat flvector of length (* out in) laid out row major, so the weight
;;; from input i to output o lives at (+ (* o in) i), and the bias of
;;; output o lives at o in the layer's bias flvector.  Activations are
;;; kept as flvectors: fast to index and to update in place.
;;;
;;; Everything is float arithmetic through racket/flonum's fl+, fl* etc.
;;; (a plain Racket + on flonums checks for exact rationals on every
;;; operation; the flonum operators skip that).  A hand written network
;;; like this cannot compete with a GPU library, but it makes every step of
;;; the DQN update visible, which is the point of the book example.

(require racket/flonum
         racket/list
         racket/math)

(provide
 (struct-out mlp)
 make-mlp
 mlp-input-size
 mlp-output-size
 mlp-num-params
 mlp-params
 mlp-param-ref
 mlp-param-set!
 mlp-forward
 mlp-forward-cache
 mlp-backprop
 mlp-predict
 mlp-copy
 mlp-set-weight!
 mlp-get-weight
 mlp-set-bias!
 mlp-mse
 mlp-mse-gradient
 mlp-huber
 mlp-dqn-gradient
 mlp-grad-add!
 mlp-grad-add-scaled!
 mlp-grad-scale!
 mlp-zero-grads

 ;; optimizer
 (struct-out adam)
 make-adam
 adam-step!
 adam-reset!
 sgd-step!

 ;; activation helpers
 relu
 relu-derivative
 huber
 huber-derivative
 clip)

;; =====================================================================
;; Network
;; =====================================================================

;; hidden-activation is 'relu or 'tanh.  The output layer is always linear:
;; Q-values are unbounded real numbers.
(struct mlp (sizes weights biases hidden-activation) #:transparent)

(define (mlp-input-size net) (vector-ref (mlp-sizes net) 0))

(define (mlp-output-size net)
  (vector-ref (mlp-sizes net) (sub1 (vector-length (mlp-sizes net)))))

(define (mlp-num-params net)
  (for/sum ([p (in-list (mlp-params net))]) (flvector-length p)))

;; Uniform initialization in [-limit, limit].  He scaling for ReLU,
;; Xavier/Glorot for tanh; biases start at zero.
(define (random-uniform limit)
  (* limit (- (* 2.0 (random)) 1.0)))

(define (make-mlp sizes #:hidden-activation [hidden-activation 'relu])
  (define sizes* (list->vector sizes))
  (define L (sub1 (vector-length sizes*)))
  (define weights
    (for/vector ([l (in-range L)])
      (define in (vector-ref sizes* l))
      (define out (vector-ref sizes* (add1 l)))
      (define limit
        (case hidden-activation
          [(relu) (flsqrt (fl/ 6.0 (exact->inexact in)))]
          [(tanh) (flsqrt (fl/ 6.0 (exact->inexact (+ in out))))]
          [else (error 'make-mlp "unknown activation: ~a" hidden-activation)]))
      (for/flvector ([i (in-range (* in out))])
        (random-uniform limit))))
  (define biases
    (for/vector ([l (in-range L)])
      (make-flvector (vector-ref sizes* (add1 l)) 0.0)))
  (mlp sizes* weights biases hidden-activation))

;; Parameter and gradient lists are ordered layer by layer:
;; W0, W1, ..., W(L-1), b0, b1, ..., b(L-1).  The flvectors handed out are
;; the live ones, so an optimizer that mutates them mutates the network.
(define (mlp-params net)
  (append (vector->list (mlp-weights net))
          (vector->list (mlp-biases net))))

(define (mlp-zero-grads net)
  (for/list ([p (in-list (mlp-params net))])
    (make-flvector (flvector-length p) 0.0)))

;; Flat parameter access, used by the finite-difference gradient check.
(define (mlp-param-ref net i)
  (let loop ([ps (mlp-params net)] [i i])
    (cond
      [(< i (flvector-length (car ps))) (flvector-ref (car ps) i)]
      [else (loop (cdr ps) (- i (flvector-length (car ps))))])))

(define (mlp-param-set! net i v)
  (let loop ([ps (mlp-params net)] [i i])
    (cond
      [(< i (flvector-length (car ps))) (flvector-set! (car ps) i v)]
      [else (loop (cdr ps) (- i (flvector-length (car ps))))])))

;; ---- activations ----------------------------------------------------

(define (relu x) (if (fl> x 0.0) x 0.0))
(define (relu-derivative x) (if (fl> x 0.0) 1.0 0.0))

(define (apply-activation act x)
  (case act
    [(relu) (relu x)]
    [(tanh) (tanh x)]
    [(linear) x]
    [else (error 'apply-activation "unknown activation: ~a" act)]))

(define (activation-derivative act z a)
  (case act
    [(relu) (relu-derivative z)]
    [(tanh) (fl- 1.0 (fl* a a))]
    [(linear) 1.0]
    [else (error 'activation-derivative "unknown activation: ~a" act)]))

;; ---- forward pass ---------------------------------------------------

(define (mlp-forward net x)
  (define-values (acts zs) (mlp-forward-cache net x))
  (vector-ref acts (sub1 (vector-length acts))))

(define (mlp-predict net x) (mlp-forward net x))

;; Returns (values acts zs):
;;   acts[0] = input, acts[l+1] = activation of layer l (output layer linear)
;;   zs[l+1] = the pre-activation that produced acts[l+1]
(define (mlp-forward-cache net x)
  (define sizes (mlp-sizes net))
  (define weights (mlp-weights net))
  (define biases (mlp-biases net))
  (define L (sub1 (vector-length sizes)))
  (define acts (make-vector (add1 L)))
  (define zs (make-vector (add1 L)))
  (vector-set! acts 0 x)
  (vector-set! zs 0 x)
  (for ([l (in-range L)])
    (define in-size (vector-ref sizes l))
    (define out-size (vector-ref sizes (add1 l)))
    (define W (vector-ref weights l))
    (define b (vector-ref biases l))
    (define prev (vector-ref acts l))
    (define z (make-flvector out-size 0.0))
    (for ([o (in-range out-size)])
      (define acc (flvector-ref b o))
      (define base (* o in-size))
      (for ([i (in-range in-size)])
        (set! acc (fl+ acc (fl* (flvector-ref W (+ base i)) (flvector-ref prev i)))))
      (flvector-set! z o acc))
    (define act (if (= l (sub1 L)) 'linear (mlp-hidden-activation net)))
    (define a (for/flvector ([o (in-range out-size)])
                (apply-activation act (flvector-ref z o))))
    (vector-set! zs (add1 l) z)
    (vector-set! acts (add1 l) a))
  (values acts zs))

;; ---- backward pass --------------------------------------------------

;; dout is dLoss/d(output), an flvector as long as the output layer.
;; Returns a fresh gradient list ordered like (mlp-params net).
(define (mlp-backprop net x acts zs dout)
  (define sizes (mlp-sizes net))
  (define weights (mlp-weights net))
  (define L (sub1 (vector-length sizes)))
  (define dW (for/vector ([l (in-range L)])
               (make-flvector (flvector-length (vector-ref weights l)) 0.0)))
  (define db (for/vector ([l (in-range L)])
               (make-flvector (vector-ref sizes (add1 l)) 0.0)))
  (define delta dout)
  (for ([l (in-range (sub1 L) -1 -1)])
    (define in-size (vector-ref sizes l))
    (define out-size (vector-ref sizes (add1 l)))
    (define W (vector-ref weights l))
    (define a-prev (vector-ref acts l))
    (for ([o (in-range out-size)])
      (define d (flvector-ref delta o))
      (flvector-set! (vector-ref db l) o d)
      (define base (* o in-size))
      (for ([i (in-range in-size)])
        (flvector-set! (vector-ref dW l) (+ base i)
                       (fl* d (flvector-ref a-prev i)))))
    (when (> l 0)
      (define dprev (make-flvector in-size 0.0))
      (for ([o (in-range out-size)])
        (define d (flvector-ref delta o))
        (define base (* o in-size))
        (for ([i (in-range in-size)])
          (flvector-set! dprev i
                         (fl+ (flvector-ref dprev i)
                              (fl* d (flvector-ref W (+ base i)))))))
      (define z-prev (vector-ref zs l))
      (define a-prev-act (vector-ref acts l))
      (set! delta
            (for/flvector ([i (in-range in-size)])
              (fl* (flvector-ref dprev i)
                   (activation-derivative (mlp-hidden-activation net)
                                          (flvector-ref z-prev i)
                                          (flvector-ref a-prev-act i)))))))
  (append (vector->list dW) (vector->list db)))

;; ---- gradient accumulation helpers ---------------------------------

;; Local in-place flvector helpers (kept private so this file has no
;; dependency on the environments module).
(define (flvector-add-scaled! v u scale)
  (for ([i (in-range (flvector-length v))])
    (flvector-set! v i (fl+ (flvector-ref v i) (fl* scale (flvector-ref u i)))))
  v)

(define (flvector-scale! v scale)
  (for ([i (in-range (flvector-length v))])
    (flvector-set! v i (fl* scale (flvector-ref v i))))
  v)

;; acc += g
(define (mlp-grad-add! acc g)
  (for ([a (in-list acc)] [b (in-list g)]) (flvector-add-scaled! a b 1.0))
  acc)

;; acc += scale * g
(define (mlp-grad-add-scaled! acc g scale)
  (for ([a (in-list acc)] [b (in-list g)]) (flvector-add-scaled! a b scale))
  acc)

(define (mlp-grad-scale! acc scale)
  (for ([a (in-list acc)]) (flvector-scale! a scale))
  acc)

;; ---- losses and their gradients ------------------------------------

;; Half the mean squared error, 0.5 * ||pred - y||^2, averaged over samples
;; when a batch is given.
(define (mlp-mse net xs ys)
  (define n (length xs))
  (define total
    (for/sum ([x (in-list xs)] [y (in-list ys)])
      (define pred (mlp-forward net x))
      (for/sum ([p (in-flvector pred)] [t (in-flvector y)])
        (fl* 0.5 (fl* (fl- p t) (fl- p t))))))
  (fl/ total (exact->inexact n)))

;; Mean gradient of the MSE over the batch.
(define (mlp-mse-gradient net xs ys)
  (define n (length xs))
  (define acc (mlp-zero-grads net))
  (for ([x (in-list xs)] [y (in-list ys)])
    (define-values (acts zs) (mlp-forward-cache net x))
    (define pred (vector-ref acts (sub1 (vector-length acts))))
    (define dout (for/flvector ([p (in-flvector pred)] [t (in-flvector y)])
                   (fl/ (fl- p t) (exact->inexact n))))
    (mlp-grad-add! acc (mlp-backprop net x acts zs dout)))
  acc)

;; ---- Huber / clipped TD error -------------------------------------
;;
;; DQN's loss from the Nature paper: the TD error is treated as squared
;; error inside +/- delta and as absolute error outside, and its gradient
;; is the error clipped to +/- delta.  That keeps one very surprising
;; transition from producing a huge weight update.

(define (huber error [delta 1.0])
  (define a (flabs error))
  (if (fl<= a delta) (fl* 0.5 error error) (fl* delta (fl- a (fl* 0.5 delta)))))

(define (huber-derivative error [delta 1.0])
  (define a (flabs error))
  (cond [(fl< error (fl- delta)) (fl- delta)]
        [(fl> error delta) delta]
        [(fl<= a delta) error]
        [else 0.0]))

(define (clip x lo hi)
  (cond [(fl< x lo) lo] [(fl> x hi) hi] [else x]))

;; Gradient of the mean Huber DQN loss over a batch.  Only the Q-value of
;; the action that was actually taken carries gradient; the other outputs
;; are ignored, exactly as in the original DQN target
;;     y = r                     if the episode ended there
;;     y = r + gamma * max_a' Q(s', a')   otherwise
(define (mlp-dqn-gradient net xs actions targets #:clip [delta 1.0])
  (define n (length xs))
  (define acc (mlp-zero-grads net))
  (for ([x (in-list xs)] [a (in-list actions)] [target (in-list targets)])
    (define-values (acts zs) (mlp-forward-cache net x))
    (define q (vector-ref acts (sub1 (vector-length acts))))
    (define dout (make-flvector (flvector-length q) 0.0))
    (define e (fl- (flvector-ref q a) target))
    (flvector-set! dout a (fl/ (huber-derivative e delta) (exact->inexact n)))
    (mlp-grad-add! acc (mlp-backprop net x acts zs dout)))
  acc)

(define (mlp-huber net xs actions targets #:clip [delta 1.0])
  (define n (length xs))
  (define total
    (for/sum ([x (in-list xs)] [a (in-list actions)] [target (in-list targets)])
      (define q (mlp-forward net x))
      (huber (fl- (flvector-ref q a) target) delta)))
  (fl/ total (exact->inexact n)))

;; ---- copying and manual weight access (tests, target networks) ------

(define (mlp-copy net)
  (mlp (mlp-sizes net)
       (for/vector ([w (in-vector (mlp-weights net))]) (flvector-copy w))
       (for/vector ([b (in-vector (mlp-biases net))]) (flvector-copy b))
       (mlp-hidden-activation net)))

(define (checked-layer l net)
  (unless (and (exact-integer? l) (<= 0 l (sub1 (vector-length (mlp-sizes net)))))
    (error 'mlp "layer out of range: ~a" l)))

(define (mlp-set-weight! net l i v)
  (checked-layer l net)
  (flvector-set! (vector-ref (mlp-weights net) l) i v))

(define (mlp-get-weight net l i)
  (checked-layer l net)
  (flvector-ref (vector-ref (mlp-weights net) l) i))

(define (mlp-set-bias! net l o v)
  (checked-layer l net)
  (flvector-set! (vector-ref (mlp-biases net) l) o v))

;; =====================================================================
;; Adam
;; =====================================================================
;;
;; Adam keeps a first and a second moment estimate for every parameter.
;; It is the optimizer used by the DQN examples (and, with plain SGD, by
;; the REINFORCE examples).

(struct adam (m v t) #:transparent #:mutable)

(define (make-adam params)
  (adam (for/list ([p (in-list params)]) (make-flvector (flvector-length p) 0.0))
        (for/list ([p (in-list params)]) (make-flvector (flvector-length p) 0.0))
        0))

(define (adam-reset! opt params)
  (set-adam-m! opt (for/list ([p (in-list params)]) (make-flvector (flvector-length p) 0.0)))
  (set-adam-v! opt (for/list ([p (in-list params)]) (make-flvector (flvector-length p) 0.0)))
  (set-adam-t! opt 0)
  opt)

;; params and grads are parallel lists of flvectors; params are updated in
;; place.  Bias correction uses the step counter so the first few updates
;; are not tiny.
(define (adam-step! opt params grads
                    #:lr [lr 1e-3]
                    #:beta1 [beta1 0.9]
                    #:beta2 [beta2 0.999]
                    #:eps [eps 1e-8])
  (set-adam-t! opt (add1 (adam-t opt)))
  (define t (adam-t opt))
  (define bias1 (fl- 1.0 (flexpt beta1 (exact->inexact t))))
  (define bias2 (fl- 1.0 (flexpt beta2 (exact->inexact t))))
  (for ([p (in-list params)]
        [g (in-list grads)]
        [m (in-list (adam-m opt))]
        [v (in-list (adam-v opt))])
    (for ([i (in-range (flvector-length p))])
      (define gi (flvector-ref g i))
      (define mi (fl+ (fl* beta1 (flvector-ref m i)) (fl* (fl- 1.0 beta1) gi)))
      (define vi (fl+ (fl* beta2 (flvector-ref v i)) (fl* (fl- 1.0 beta2) gi gi)))
      (flvector-set! m i mi)
      (flvector-set! v i vi)
      (define mhat (fl/ mi bias1))
      (define vhat (fl/ vi bias2))
      (flvector-set! p i (fl- (flvector-ref p i)
                              (fl/ (fl* lr mhat) (fl+ (flsqrt vhat) eps))))))
  opt)

;; Plain stochastic gradient descent, params -= lr * grads.
(define (sgd-step! params grads #:lr [lr 1e-2])
  (for ([p (in-list params)] [g (in-list grads)])
    (for ([i (in-range (flvector-length p))])
      (flvector-set! p i (fl- (flvector-ref p i) (fl* lr (flvector-ref g i))))))
  params)

(module+ test
  (require rackunit)

  (define (grads->list gs)
    (append* (for/list ([g (in-list gs)]) (for/list ([x (in-flvector g)]) x))))

  (test-case "forward pass shapes"
    (random-seed 1)
    (define net (make-mlp '(3 5 2)))
    (define out (mlp-forward net (flvector 1.0 2.0 3.0)))
    (check-true (flvector? out))
    (check-equal? (flvector-length out) 2)
    (check-equal? (mlp-input-size net) 3)
    (check-equal? (mlp-output-size net) 2))

  (test-case "finite-difference gradient check (relu and tanh)"
    (for ([act (in-list '(relu tanh))])
      (random-seed 11)
      (define net (make-mlp '(2 4 1) #:hidden-activation act))
      ;; use an input where the ReLU units are not exactly at the kink
      (define x (flvector 0.3 -0.7))
      (define y (flvector 1.5))
      (define grads (mlp-mse-gradient net (list x) (list y)))
      (define g (grads->list grads))
      (for ([i (in-list '(0 3 7 12))])
        (define eps 1e-6)
        (define orig (mlp-param-ref net i))
        (mlp-param-set! net i (fl+ orig eps))
        (define lp (mlp-mse net (list x) (list y)))
        (mlp-param-set! net i (fl- orig eps))
        (define lm (mlp-mse net (list x) (list y)))
        (mlp-param-set! net i orig)
        (define numeric (fl/ (fl- lp lm) (fl* 2.0 eps)))
        (check-= (list-ref g i) numeric 1e-5))))

  (test-case "adam reduces a quadratic"
    (define params (list (flvector 5.0 -3.0)))
    (define opt (make-adam params))
    (for ([i (in-range 400)])
      (define grads (list (flvector (* 2.0 (flvector-ref (car params) 0))
                                    (* 2.0 (flvector-ref (car params) 1)))))
      (adam-step! opt params grads #:lr 0.05))
    (check-= (flvector-ref (car params) 0) 0.0 1e-4)
    (check-= (flvector-ref (car params) 1) 0.0 1e-4))

  (test-case "DQN gradient touches only the taken action"
    (random-seed 3)
    (define net (make-mlp '(2 3 2)))
    (define x (flvector 0.4 0.2))
    (define grads (mlp-dqn-gradient net (list x) (list 1) (list 2.0)))
    ;; The output layer is the second layer: 2 output units x 3 hidden
    ;; units = 6 weights, rows indexed by the action.  Action 1 was taken,
    ;; so the whole row for action 0 (indices 0 1 2) must stay zero and the
    ;; row for action 1 must carry gradient.
    (define w1 (list-ref grads 1))
    (check-= (flvector-ref w1 0) 0.0 1e-12)
    (check-= (flvector-ref w1 1) 0.0 1e-12)
    (check-= (flvector-ref w1 2) 0.0 1e-12)
    (check-true (not (= 0.0 (flvector-ref w1 3))))))
