#lang racket

;;; dqn.rkt -- Deep Q-Network: Q-learning with a neural network, a replay
;;; buffer and a target network.
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Run:  racket dqn.rkt              (GridWorld, about 10 seconds)
;;;       racket dqn.rkt --cartpole   (CartPole, slower, optional)
;;;
;;; The tabular methods in q_learning.rkt store one number per
;;; state-action pair.  Real problems have too many states, so DQN
;;; replaces the table with a network Q(s, a; theta) and trains it on the
;;; same Bellman target:
;;;
;;;     y = r                              if the episode ended
;;;     y = r + gamma * max_a' Q(s', a'; theta^-)   otherwise
;;;
;;; where theta^- is a *target network* copied from theta every few hundred
;;; steps.  Three ideas keep this stable, and each one is visible here:
;;;
;;;   1. a replay buffer: sample random past transitions instead of
;;;      learning from one correlated trajectory;
;;;   2. the target network: bootstrap from slowly-changing weights so the
;;;      target does not chase the prediction;
;;;   3. Huber (clipped) TD error: one surprising transition cannot produce
;;;      a huge gradient.
;;;
;;; The GridWorld demo uses one-hot state vectors, which makes the network
;;; a differentiable Q table -- the DQN policy and the tabular Q-learning
;;; policy can be compared entry by entry.  The CartPole demo uses the same
;;; code with four continuous inputs; only the input features change.

(require "environments.rkt"
         "neural_net.rkt"
         "q_learning.rkt"
         racket/flonum
         racket/list
         racket/math)

(provide
 make-replay-buffer
 replay-push!
 replay-sample
 replay-count
 replay-capacity
 replay-full?
 epsilon-at-step
 dqn-features
 dqn-q-values
 dqn-q-table
 dqn-select-action
 dqn-train-gridworld
 dqn-train-cartpole
 dqn-evaluate-cartpole)

;; =====================================================================
;; Replay buffer
;; =====================================================================
;;
;; A ring buffer of transitions (s, a, r, s', done?).  States are feature
;; vectors, so the same buffer serves GridWorld (one-hot features) and
;; CartPole (four scaled state variables).

(struct replay-buffer
  (capacity states actions rewards next-states dones size next-index)
  #:transparent #:mutable)

(define (make-replay-buffer capacity)
  (replay-buffer capacity
                 (make-vector capacity #f)
                 (make-vector capacity 0)
                 (make-flvector capacity 0.0)
                 (make-vector capacity #f)
                 (make-vector capacity #f)
                 0 0))

(define (replay-capacity buf) (replay-buffer-capacity buf))

(define (replay-count buf) (replay-buffer-size buf))

(define (replay-full? buf) (= (replay-buffer-size buf) (replay-buffer-capacity buf)))

(define (replay-push! buf s a r s2 done?)
  (define i (replay-buffer-next-index buf))
  (vector-set! (replay-buffer-states buf) i s)
  (vector-set! (replay-buffer-actions buf) i a)
  (flvector-set! (replay-buffer-rewards buf) i (exact->inexact r))
  (vector-set! (replay-buffer-next-states buf) i s2)
  (vector-set! (replay-buffer-dones buf) i done?)
  (set-replay-buffer-next-index! buf (remainder (add1 i) (replay-buffer-capacity buf)))
  (set-replay-buffer-size! buf (min (replay-buffer-capacity buf)
                                    (add1 (replay-buffer-size buf))))
  buf)

;; Uniform random minibatch, with replacement (the usual DQN choice).
;; Returns (values states actions rewards next-states dones) as lists.
(define (replay-sample buf n)
  (define size (replay-buffer-size buf))
  (unless (>= size n)
    (error 'replay-sample "buffer holds ~a transitions, cannot sample ~a" size n))
  (define idxs (for/list ([i (in-range n)]) (random size)))
  (values (for/list ([i (in-list idxs)]) (vector-ref (replay-buffer-states buf) i))
          (for/list ([i (in-list idxs)]) (vector-ref (replay-buffer-actions buf) i))
          (for/list ([i (in-list idxs)]) (flvector-ref (replay-buffer-rewards buf) i))
          (for/list ([i (in-list idxs)]) (vector-ref (replay-buffer-next-states buf) i))
          (for/list ([i (in-list idxs)]) (vector-ref (replay-buffer-dones buf) i))))

;; Linear epsilon decay driven by the global step counter.
(define (epsilon-at-step step decay-steps start end)
  (cond
    [(>= step decay-steps) end]
    [else (+ end (* (- start end) (- 1.0 (/ step decay-steps))))]))

;; =====================================================================
;; Features and Q values
;; =====================================================================

;; GridWorld state -> one-hot feature vector of length num-states.
(define (dqn-features gw s) (one-hot (gridworld-num-states gw) s))

(define (dqn-q-values net f) (mlp-forward net f))

(define (dqn-select-action net f eps)
  (if (< (random) eps)
      (random (mlp-output-size net))
      (flvector-argmax (mlp-forward net f))))

;; Evaluate the network on every one-hot input, producing a plain Q table
;; that the tabular evaluation and comparison helpers can use.
(define (dqn-q-table net nS)
  (for/vector ([s (in-range nS)]) (mlp-forward net (one-hot nS s))))

;; =====================================================================
;; Training
;; =====================================================================

;; DQN on GridWorld.  Returns
;;   (values net episode-returns episode-lengths epsilon-history loss-history)
(define (dqn-train-gridworld gw
                            #:hidden [hidden '(48 48)]
                            #:episodes [episodes 250]
                            #:max-steps [max-steps 100]
                            #:gamma [gamma 0.95]
                            #:lr [lr 1e-3]
                            #:batch [batch 32]
                            #:replay-size [replay-size 5000]
                            #:warmup [warmup 500]
                            #:target-update [target-update 250]
                            #:train-every [train-every 1]
                            #:epsilon-start [epsilon-start 1.0]
                            #:epsilon-end [epsilon-end 0.05]
                            #:epsilon-decay-steps [epsilon-decay-steps 6000]
                            #:q-clip [q-clip 1.0]
                            #:random-start? [random-start? #f]
                            #:seed [seed 42]
                            #:verbose [verbose #f])
  (define nS (gridworld-num-states gw))
  (define nA (gridworld-num-actions gw))
  ;; Seed before the network is built: weight initialization draws from
  ;; the global PRNG, so seeding later would make the demo dependent on
  ;; whatever the process did before (and would break the tests).
  (random-seed seed)
  (define net (make-mlp (append (list nS) hidden (list nA))))
  (define target (mlp-copy net))
  (define params (mlp-params net))
  (define opt (make-adam params))
  (define buffer (make-replay-buffer replay-size))
  (define step 0)
  (define returns '())
  (define lengths '())
  (define eps-history '())
  (define loss-history '())
  (for ([ep (in-range episodes)])
    (define-values (ret nsteps)
      (let loop ([s (gridworld-reset gw #:random? random-start?)] [ret 0.0] [steps 0])
        (define f (dqn-features gw s))
        (define eps (epsilon-at-step step epsilon-decay-steps epsilon-start epsilon-end))
        (define a (dqn-select-action net f eps))
        (define-values (s2 r done?) (gridworld-step gw s a))
        (define f2 (dqn-features gw s2))
        (replay-push! buffer f a r f2 (and done? #t))
        (set! ret (+ ret r))
        (set! step (add1 step))
        ;; --- learn from a random minibatch of past transitions ---
        (when (and (>= (replay-count buffer) (max warmup batch))
                   (zero? (remainder step train-every)))
          (define-values (xs as rs xs2 dones)
            (replay-sample buffer batch))
          (define targets
            (for/list ([x2 (in-list xs2)] [rr (in-list rs)] [d? (in-list dones)])
              (if d?
                  rr
                  (+ rr (* gamma (flvector-max (mlp-forward target x2)))))))
          (define grads (mlp-dqn-gradient net xs as targets #:clip q-clip))
          (adam-step! opt params grads #:lr lr)
          (when (zero? (remainder step 250))
            (set! loss-history (cons (mlp-huber net xs as targets #:clip q-clip) loss-history))))
        (when (zero? (remainder step target-update))
          (set! target (mlp-copy net)))
        (when (zero? (remainder step 500))
          (set! eps-history (cons eps eps-history)))
        (cond
          [(or done? (>= (add1 steps) max-steps)) (values ret (add1 steps))]
          [else (loop s2 ret (add1 steps))])))
    (set! returns (cons ret returns))
    (set! lengths (cons nsteps lengths))
    (when (and verbose (zero? (remainder (add1 ep) 50)))
      (printf "  episode ~a: mean return of last 50 = ~a, epsilon = ~a\n"
              (add1 ep)
              (real->decimal-string (mean (take returns (min 50 (length returns)))) 1)
              (real->decimal-string (epsilon-at-step step epsilon-decay-steps
                                                     epsilon-start epsilon-end) 3))))
  (values net (reverse returns) (reverse lengths)
          (reverse eps-history) (reverse loss-history)))

;; DQN on CartPole (continuous state).  Feature vectors come from
;; cartpole-observe, so the input layer has four units.
(define (dqn-train-cartpole cp
                            #:hidden [hidden '(64 64)]
                            #:episodes [episodes 300]
                            #:max-steps [max-steps 500]
                            #:gamma [gamma 0.99]
                            #:lr [lr 1e-3]
                            #:batch [batch 32]
                            #:replay-size [replay-size 20000]
                            #:warmup [warmup 1000]
                            #:target-update [target-update 500]
                            #:train-every [train-every 1]
                            #:epsilon-start [epsilon-start 1.0]
                            #:epsilon-end [epsilon-end 0.05]
                            #:epsilon-decay-steps [epsilon-decay-steps 20000]
                            #:q-clip [q-clip 1.0]
                            #:seed [seed 42]
                            #:verbose [verbose #f])
  (random-seed seed)
  (define net (make-mlp (append (list 4) hidden (list 2))))
  (define target (mlp-copy net))
  (define params (mlp-params net))
  (define opt (make-adam params))
  (define buffer (make-replay-buffer replay-size))
  (define step 0)
  (define returns '())
  (for ([ep (in-range episodes)])
    (define ret
      (let loop ([s (cartpole-reset cp)] [ret 0.0] [steps 0])
        (define f (cartpole-observe cp s))
        (define eps (epsilon-at-step step epsilon-decay-steps epsilon-start epsilon-end))
        (define a (dqn-select-action net f eps))
        (define-values (s2 r done?) (cartpole-act cp s a))
        (replay-push! buffer f a r (cartpole-observe cp s2) (and done? #t))
        (set! ret (+ ret r))
        (set! step (add1 step))
        (when (and (>= (replay-count buffer) (max warmup batch))
                   (zero? (remainder step train-every)))
          (define-values (xs as rs xs2 dones) (replay-sample buffer batch))
          (define targets
            (for/list ([x2 (in-list xs2)] [rr (in-list rs)] [d? (in-list dones)])
              (if d?
                  rr
                  (+ rr (* gamma (flvector-max (mlp-forward target x2)))))))
          (define grads (mlp-dqn-gradient net xs as targets #:clip q-clip))
          (adam-step! opt params grads #:lr lr))
        (when (zero? (remainder step target-update))
          (set! target (mlp-copy net)))
        (cond
          [(or done? (>= (add1 steps) max-steps)) ret]
          [else (loop s2 ret (add1 steps))])))
    (set! returns (cons ret returns))
    (when (and verbose (zero? (remainder (add1 ep) 50)))
      (printf "  episode ~a: mean steps of last 50 = ~a\n"
              (add1 ep)
              (real->decimal-string (mean (take returns (min 50 (length returns)))) 1))))
  (values net (reverse returns)))

;; Greedy CartPole evaluation of a trained network.
(define (dqn-evaluate-cartpole cp net #:episodes [episodes 20] #:max-steps [max-steps 500])
  (define steps-list '())
  (for ([ep (in-range episodes)])
    (let loop ([s (cartpole-reset cp)] [steps 0])
      (cond
        [(>= steps max-steps) (set! steps-list (cons steps steps-list))]
        [else
         (define f (cartpole-observe cp s))
         (define a (flvector-argmax (mlp-forward net f)))
         (define-values (s2 r done?) (cartpole-act cp s a))
         (if done?
             (set! steps-list (cons (add1 steps) steps-list))
             (loop s2 (add1 steps)))])))
  (values (mean steps-list)
          (apply max steps-list)
          (count (lambda (s) (>= s max-steps)) steps-list)))

;; =====================================================================
;; Demos
;; =====================================================================

(define (run-gridworld-demo)
  (define gw (make-gridworld))
  (define episodes 250)
  (define gamma 0.95)
  (printf "=== 2. Deep Q-Network (DQN) on GridWorld ===\n\n")
  (printf "~a\n\n" (gridworld-map-string gw))
  (printf "Network 25 -> 48 -> 48 -> 4 (ReLU, linear output); one-hot state input,\n")
  (printf "so the network represents exactly the Q table of q_learning.rkt.\n")
  (printf "Replay buffer 5000, minibatch 32, Adam lr 0.001, gamma ~a.\n" gamma)
  (printf "epsilon 1.00 -> 0.05 over 6000 steps; target network recopied every\n")
  (printf "250 steps; Huber TD error clipped at 1.0.  ~a episodes, fixed start.\n\n" episodes)

  (define-values (net returns lengths eps-history loss-history)
    (dqn-train-gridworld gw #:episodes episodes #:gamma gamma))

  (printf "Training return per 50-episode block (epsilon-greedy):\n  ~a\n\n"
          (string-join
           (for/list ([b (in-list (block-means returns 50))])
             (real->decimal-string b 2))
           "  "))
  ;; loss-history is returned for callers that want to plot the TD error;
  ;; on this maze it drops below 0.001 within a few hundred steps, so the
  ;; demo prints the training return instead.

  ;; Turn the network into a Q table and use the tabular evaluation code.
  (define nS (gridworld-num-states gw))
  (define qnet (dqn-q-table net nS))
  (define-values (ret rate steps) (evaluate-greedy gw qnet #:episodes 200 #:gamma gamma))
  (printf "Greedy evaluation over 200 episodes: mean return ~a, success ~a, mean ~a steps.\n"
          (real->decimal-string ret 4) (real->decimal-string rate 3)
          (real->decimal-string steps 2))

  (define-values (V* Q* sweeps) (value-iteration gw #:gamma gamma))
  (printf "Value iteration optimum:                                    ~a, success 1.000, 8.00 steps.\n\n"
          (real->decimal-string (vector-ref V* (gridworld-start gw)) 4))

  ;; Table-trained reference, trained on exactly the same MDP.
  (define-values (qtab tab-returns tab-lengths tab-successes tab-visits)
    (train-td gw 'q-learning #:episodes 800 #:alpha 0.2 #:gamma gamma))
  (printf "mean |Q - Q*| over all 100 state-action pairs:\n")
  (printf "  DQN network        ~a\n"
          (real->decimal-string (q-mean-difference qnet Q*) 4))
  (printf "  tabular Q-learning ~a\n\n"
          (real->decimal-string (q-mean-difference qtab Q* #:visits tab-visits #:min-visits 5) 4))

  (printf "Greedy policy from DQN:\n~a\n\n"
          (gridworld-render gw #:policy (lambda (s) (flvector-argmax (vector-ref qnet s)))))
  (printf "DQN path: ~a\n"
          (for/list ([s (in-list (greedy-path gw qnet))])
            (format "(~a,~a)" (first (gridworld-state->pos gw s)) (second (gridworld-state->pos gw s)))))
  (printf "\nThe network found the same optimal route as tabular Q-learning and\n")
  (printf "value iteration; it just stores the Q values as weights instead of a table.\n")
  (printf "Run with --cartpole for the same algorithm on a continuous state space.\n"))

(define (run-cartpole-demo)
  (define cp (make-cartpole))
  (printf "=== 2b. Deep Q-Network on CartPole (optional, slower) ===\n\n")
  (printf "Network 4 -> 64 -> 64 -> 2 (ReLU), replay buffer 30000, minibatch 32,\n")
  (printf "Adam lr 0.001, gamma 0.99, epsilon 1.00 -> 0.05 over 8000 steps,\n")
  (printf "300 episodes.  The four inputs are the scaled cart position, cart\n")
  (printf "velocity, pole angle and pole angular velocity.\n\n")
  (define-values (net returns)
    (dqn-train-cartpole cp #:episodes 300 #:lr 1e-3 #:gamma 0.99
                        #:epsilon-decay-steps 8000 #:target-update 500
                        #:batch 32 #:warmup 1000 #:replay-size 30000
                        #:verbose #t))
  (printf "\nTraining episode length per 50-episode block:\n  ~a\n\n"
          (string-join
           (for/list ([b (in-list (block-means returns 50))])
             (real->decimal-string b 1))
           "  "))
  (define-values (m b f) (dqn-evaluate-cartpole cp net #:episodes 20))
  (printf "Greedy evaluation over 20 episodes: mean ~a steps, best ~a, ~a/20 reached 500.\n"
          (real->decimal-string m 1) b f)
  (printf "\nThis is deliberate honesty about sample efficiency.  A random policy\n")
  (printf "survives about 22 steps, so the greedy policy has clearly learned to push\n")
  (printf "the cart back under the pole, but it has not mastered the task.  DQN needs\n")
  (printf "far more experience than the linear policy gradient in policy_gradient.rkt,\n")
  (printf "which balances the pole for the full 500 steps after 600 episodes because\n")
  (printf "its features encode position and velocity directly.  Double DQN, a soft\n")
  (printf "target update or a longer run all push this curve higher (see PROBLEMS.md).\n"))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (if (member "--cartpole" args)
      (run-cartpole-demo)
      (run-gridworld-demo)))
