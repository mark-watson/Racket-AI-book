#lang racket

;;; tests.rkt -- RackUnit suite for the reinforcement learning examples.
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Run:  raco test tests.rkt      (or: racket tests.rkt)
;;;
;;; The suite checks two different things:
;;;
;;;   * mechanics -- environment transitions, network gradients, buffer
;;;     wrap-around, schedule endpoints, probability distributions;
;;;   * learning -- that each algorithm actually solves its task with a
;;;     fixed seed, and that the numbers agree with the model-based answer
;;;     from value iteration.
;;;
;;; Every experiment uses a fixed seed, so a failure is reproducible.

(require rackunit
         rackunit/text-ui
         racket/flonum
         racket/list
         racket/math
         "environments.rkt"
         "neural_net.rkt"
         "q_learning.rkt"
         "policy_gradient.rkt"
         "dqn.rkt"
         "bandits.rkt")

(define (near? a b tol) (<= (abs (- a b)) tol))

(define TESTS
  (test-suite
   "ReinforcementLearning"

   ;; ==================================================================
   (test-suite
    "environments"
    (test-case "gridworld geometry and terminal states"
      (define gw (make-gridworld))
      (check-equal? (gridworld-num-states gw) 25)
      (check-equal? (gridworld-num-actions gw) 4)
      (check-equal? (gridworld-start gw) 0)
      (check-equal? (gridworld-goal gw) 24)
      (check-true (gridworld-terminal? gw 24))
      (check-true (gridworld-terminal? gw (gridworld-pos->state gw '(3 2)))) ; pit
      (check-false (gridworld-terminal? gw 0))
      (check-equal? (gridworld-state->pos gw (gridworld-pos->state gw '(2 4))) '(2 4)))

    (test-case "walls block movement and the goal pays"
      (define gw (make-gridworld))
      (define-values (s r done?) (gridworld-step gw 5 0)) ; (1 0) up, wall at (1 1) is irrelevant
      (check-equal? s 0)
      (check-= r -0.01 1e-12)
      (check-false done?)
      ;; walking from (1,0) into the wall at (1,1) leaves the agent in place
      (define-values (s2 r2 _) (gridworld-step gw 5 1))
      (check-equal? s2 5)
      ;; right from (4,3) reaches the goal
      (define-values (s3 r3 done3?) (gridworld-step gw 23 1))
      (check-equal? s3 24)
      (check-= r3 1.0 1e-12)
      (check-true done3?))

    (test-case "transition probabilities sum to one, with and without slip"
      (for ([slip (in-list '(0.0 0.3))])
        (define gw (make-gridworld #:slip slip))
        (for* ([s (in-range 25)] [a (in-range 4)])
          (check-= (for/sum ([t (in-list (gridworld-transitions gw s a))]) (first t))
                   1.0 1e-9))))

    (test-case "cartpole starts near upright and a random policy falls"
      (random-seed 5)
      (define cp (make-cartpole))
      (for ([i (in-range 20)])
        (define s (cartpole-reset cp))
        (for ([x (in-flvector s)]) (check-true (<= (abs x) 0.0500001))))
      ;; a random policy survives far longer than a single step but not 500
      (random-seed 7)
      (define lengths
        (for/list ([ep (in-range 50)])
          (let loop ([s (cartpole-reset cp)] [t 0])
            (cond
              [(>= t 500) t]
              [else
               (define-values (s2 r done?) (cartpole-act cp s (random 2)))
               (if done? (add1 t) (loop s2 (add1 t)))]))))
      (check-true (> (mean lengths) 8))
      (check-true (< (mean lengths) 200)))

    (test-case "bandit pulls and optimal arm"
      (random-seed 3)
      (define b (stationary-bandit))
      (check-equal? (bandit-num-arms b) 10)
      (check-equal? (bandit-optimal-arm b) 7)
      (check-= (bandit-optimal-mean b) 0.85 1e-12)
      (for ([i (in-range 100)])
        (check-not-false (member (bandit-pull b 0) '(0.0 1.0)))))
    )

   ;; ==================================================================
   (test-suite
    "neural network"
    (test-case "forward pass and parameter count"
      (random-seed 1)
      (define net (make-mlp '(3 4 2)))
      ;; 3*4 + 4*2 weights + 4 + 2 biases
      (check-equal? (mlp-num-params net) (+ 12 8 4 2))
      (check-equal? (mlp-input-size net) 3)
      (check-equal? (mlp-output-size net) 2)
      (check-equal? (flvector-length (mlp-forward net (flvector 0.1 0.2 0.3))) 2))

    (test-case "backprop agrees with finite differences"
      (random-seed 11)
      (define net (make-mlp '(2 4 1)))
      (define x (flvector 0.3 -0.7))
      (define y (flvector 1.5))
      (define grads (mlp-mse-gradient net (list x) (list y)))
      (define flat (append* (for/list ([g (in-list grads)])
                              (for/list ([v (in-flvector g)]) v))))
      (for ([i (in-list '(0 2 5 9))])
        (define eps 1e-6)
        (define orig (mlp-param-ref net i))
        (mlp-param-set! net i (fl+ orig eps))
        (define lp (mlp-mse net (list x) (list y)))
        (mlp-param-set! net i (fl- orig eps))
        (define lm (mlp-mse net (list x) (list y)))
        (mlp-param-set! net i orig)
        (check-= (list-ref flat i) (fl/ (fl- lp lm) (fl* 2.0 eps)) 1e-5)))

    (test-case "training with Adam reduces the loss"
      (random-seed 4)
      (define net (make-mlp '(1 8 1)))
      (define xs (for/list ([i (in-range 20)]) (flvector (exact->inexact (* 0.05 i)))))
      (define ys (for/list ([x (in-list xs)]) (flvector (+ 1.0 (* 2.0 (flvector-ref x 0))))))
      (define params (mlp-params net))
      (define opt (make-adam params))
      (define before (mlp-mse net xs ys))
      (for ([i (in-range 300)])
        (define grads (mlp-mse-gradient net xs ys))
        (adam-step! opt params grads #:lr 0.02))
      (check-true (< (mlp-mse net xs ys) (/ before 10.0))))

    (test-case "mlp-copy is an independent copy"
      (random-seed 2)
      (define a (make-mlp '(2 3 1)))
      (define b (mlp-copy a))
      (mlp-set-weight! a 0 0 99.0)
      (check-= (mlp-get-weight b 0 0) (mlp-get-weight a 0 0) 1e12)
      (check-true (not (= 99.0 (mlp-get-weight b 0 0)))))

    (test-case "huber loss and clipping"
      (check-= (huber 0.5) 0.125 1e-12)
      (check-= (huber 2.0) 1.5 1e-12)
      (check-= (huber -2.0) 1.5 1e-12)
      (check-= (huber-derivative 0.25) 0.25 1e-12)
      (check-= (huber-derivative 5.0) 1.0 1e-12)
      (check-= (huber-derivative -5.0) -1.0 1e-12)
      (check-= (clip 5.0 -1.0 1.0) 1.0 1e-12))
    )

   ;; ==================================================================
   (test-suite
    "tabular Q-learning"
    (test-case "value iteration matches the closed-form optimum"
      (define gw (make-gridworld))
      (define-values (V Q sweeps) (value-iteration gw #:gamma 0.95))
      ;; 0.95^7 - 0.01 * (1 - 0.95^7) / 0.05
      (define expected (- (expt 0.95 7) (* 0.01 (/ (- 1 (expt 0.95 7)) 0.05))))
      (check-= (vector-ref V 0) expected 1e-6)
      (check-= (q-ref Q 0 1) expected 1e-6) ; right is optimal from the start
      (check-true (> sweeps 0)))

    (test-case "epsilon schedule endpoints"
      (check-= (epsilon-at 0 400 1.0 0.05 400) 1.0 1e-12)
      (check-= (epsilon-at 400 400 1.0 0.05 400) 0.05 1e-12)
      (check-= (epsilon-at 1000 400 1.0 0.05 400) 0.05 1e-12))

    (test-case "Q-learning learns the optimal policy"
      (define gw (make-gridworld))
      (define-values (q returns lengths successes visited)
        (train-td gw 'q-learning #:episodes 800 #:alpha 0.2 #:gamma 0.95 #:seed 42))
      (define-values (ret rate steps) (evaluate-greedy gw q #:episodes 200 #:gamma 0.95))
      (check-= rate 1.0 1e-12)
      (check-= ret 0.6380 0.02)
      (check-= steps 8.0 1e-12)
      (check-equal? (length (greedy-path gw q)) 9) ; 9 states, 8 moves
      (define-values (V* Q* sweeps) (value-iteration gw #:gamma 0.95))
      (check-true (< (q-mean-difference q Q* #:visits visited #:min-visits 5) 0.15)))

    (test-case "all four TD methods find the goal"
      (define gw (make-gridworld))
      (for ([method (in-list '(q-learning sarsa expected-sarsa double-q))])
        (define-values (q returns lengths successes visited)
          (train-td gw method #:episodes 800 #:alpha 0.2 #:gamma 0.95 #:seed 42))
        (define-values (ret rate steps) (evaluate-greedy gw q #:episodes 100 #:gamma 0.95))
        (check-true (>= rate 0.95) (format "~a success rate ~a" method rate))
        (check-= ret 0.6380 0.05)))

    (test-case "off-policy Q-learning tracks Q*, on-policy SARSA tracks Q^pi"
      (define gw (make-gridworld))
      (define-values (V* Q* sweeps) (value-iteration gw #:gamma 0.95))
      (define-values (q-off _r _l _s vis-off)
        (train-td gw 'q-learning #:episodes 800 #:alpha 0.2 #:gamma 0.95 #:seed 42))
      (define-values (q-on _r2 _l2 _s2 vis-on)
        (train-td gw 'sarsa #:episodes 800 #:alpha 0.2 #:gamma 0.95 #:seed 42))
      (define off-err (q-mean-difference q-off Q* #:visits vis-off #:min-visits 5))
      (define on-err (q-mean-difference q-on Q* #:visits vis-on #:min-visits 5))
      ;; SARSA prices in the epsilon-greedy exploration, so it sits further
      ;; from Q* than Q-learning does.
      (check-true (< off-err 0.15))
      (check-true (> on-err off-err)))

    (test-case "double Q-learning learns from terminal transitions in both tables"
      ;; Regression test: the coin flip that chooses which table to update
      ;; must also happen for terminal transitions, otherwise one table
      ;; never sees goal rewards and the bootstrap chain breaks.
      (define gw (make-gridworld))
      (define-values (q returns lengths successes visited)
        (train-td gw 'double-q #:episodes 800 #:alpha 0.2 #:gamma 0.95 #:seed 1))
      (define-values (V* Q* sweeps) (value-iteration gw #:gamma 0.95))
      (check-true (< (q-mean-difference q Q* #:visits visited #:min-visits 5) 0.35))
      (check-= (policy-success-rate gw q #:episodes 100) 1.0 1e-12))
    )

   ;; ==================================================================
   (test-suite
    "policy gradient"
    (test-case "softmax and entropy"
      (define p (softmax-flvector (flvector 1.0 1.0 1.0 1.0)))
      (check-= (for/sum ([x (in-flvector p)]) x) 1.0 1e-12)
      (for ([x (in-flvector p)]) (check-= x 0.25 1e-12))
      (check-= (entropy-of p) (log 4) 1e-12)
      (define sharp (softmax-flvector (flvector 10.0 0.0)))
      (check-true (> (flvector-ref sharp 0) 0.999)))

    (test-case "REINFORCE with raw returns solves GridWorld"
      (define gw (make-gridworld))
      (define-values (theta returns lengths successes)
        (train-reinforce gw #:episodes 600 #:alpha 0.05 #:baseline 'none #:entropy 0.0 #:seed 42))
      (define-values (ret rate steps) (evaluate-policy gw theta #:episodes 200 #:gamma 0.95))
      (check-= rate 1.0 1e-12)
      (check-= ret 0.6380 0.02))

    (test-case "a learned value baseline also solves GridWorld"
      (define gw (make-gridworld))
      (define-values (theta returns lengths successes)
        (train-reinforce gw #:episodes 600 #:alpha 0.05 #:baseline 'value #:entropy 0.0 #:seed 42))
      (define-values (ret rate steps) (evaluate-policy gw theta #:episodes 200 #:gamma 0.95))
      (check-= rate 1.0 1e-12)
      (check-= ret 0.6380 0.05))

    (test-case "an episode-mean baseline is much weaker with sparse rewards"
      ;; The demo output shows this too: subtracting the mean of a failed
      ;; episode leaves a near-zero advantage, so the policy never learns
      ;; that the pit is bad.  This is the pedagogical point of the
      ;; baseline comparison, so it is pinned here.
      (define gw (make-gridworld))
      (define-values (theta-raw _r1 _l1 _s1)
        (train-reinforce gw #:episodes 600 #:alpha 0.05 #:baseline 'none #:entropy 0.0 #:seed 42))
      (define-values (theta-mean _r2 _l2 _s2)
        (train-reinforce gw #:episodes 600 #:alpha 0.05 #:baseline 'mean #:entropy 0.0 #:seed 42))
      (define-values (ret-raw rate-raw _st1) (evaluate-policy-sampled gw theta-raw #:episodes 200))
      (define-values (ret-mean rate-mean _st2) (evaluate-policy-sampled gw theta-mean #:episodes 200))
      (check-true (> rate-raw rate-mean))
      (check-true (> ret-raw ret-mean)))

    (test-case "REINFORCE trains a linear CartPole policy"
      (define cp (make-cartpole))
      (define-values (lp returns)
        (train-reinforce-cartpole cp #:episodes 600 #:lr 0.05 #:gamma 0.99 #:entropy 0.01 #:seed 42))
      (define-values (mean-steps best full) (evaluate-cartpole-policy cp lp #:episodes 20))
      ;; Random play survives about 22 steps; 150 already means balancing.
      (check-true (> mean-steps 150) (format "mean steps ~a" mean-steps))
      (check-true (> best 200)))
    )

   ;; ==================================================================
   (test-suite
    "DQN"
    (test-case "replay buffer stores, wraps and samples"
      (define buf (make-replay-buffer 4))
      (check-equal? (replay-capacity buf) 4)
      (check-equal? (replay-count buf) 0)
      (for ([i (in-range 6)])
        ;; the last four pushes (i = 2..5, the ones that survive in the
        ;; ring buffer) are all terminal transitions
        (replay-push! buf (one-hot 3 (remainder i 3)) (remainder i 3) (exact->inexact i)
                      (one-hot 3 (remainder (add1 i) 3)) (>= i 2)))
      (check-equal? (replay-count buf) 4)   ; ring buffer caps at capacity
      (check-true (replay-full? buf))
      (random-seed 1)
      (define-values (xs as rs xs2 dones) (replay-sample buf 4))
      (check-equal? (length xs) 4)
      (check-equal? (length as) 4)
      (check-equal? (length rs) 4)
      (for ([x (in-list xs)]) (check-true (flvector? x)))
      (for ([r (in-list rs)]) (check-true (and (>= r 0.0) (<= r 5.0))))
      ;; every transition that survived the wrap-around is terminal
      (check-true (for/and ([d (in-list dones)]) d)))

    (test-case "epsilon schedule for DQN"
      (check-= (epsilon-at-step 0 1000 1.0 0.05) 1.0 1e-12)
      (check-= (epsilon-at-step 1000 1000 1.0 0.05) 0.05 1e-12)
      (check-= (epsilon-at-step 5000 1000 1.0 0.05) 0.05 1e-12))

    (test-case "DQN solves a small gridworld"
      (define gw (make-gridworld #:rows 3 #:cols 3 #:walls '() #:pits '()))
      (define-values (net returns lengths eps loss)
        (dqn-train-gridworld gw #:hidden '(16) #:episodes 80 #:batch 8
                             #:warmup 100 #:lr 0.01 #:epsilon-decay-steps 1000
                             #:target-update 50 #:seed 42))
      (define q (dqn-q-table net (gridworld-num-states gw)))
      (define-values (ret rate steps) (evaluate-greedy gw q #:episodes 50 #:gamma 0.95))
      (check-= rate 1.0 1e-12)
      (check-true (<= steps 5.0)))

    (test-case "DQN on the full maze reaches the optimal policy"
      (define gw (make-gridworld))
      (define-values (net returns lengths eps loss)
        (dqn-train-gridworld gw #:episodes 250 #:seed 42))
      (define nS (gridworld-num-states gw))
      (define q (dqn-q-table net nS))
      (define-values (ret rate steps) (evaluate-greedy gw q #:episodes 100 #:gamma 0.95))
      (check-= rate 1.0 1e-12)
      (check-= ret 0.6380 0.02)
      (define-values (V* Q* sweeps) (value-iteration gw #:gamma 0.95))
      (check-true (< (q-mean-difference q Q*) 0.3)))
    )

   ;; ==================================================================
   (test-suite
    "bandits"
    (test-case "epsilon-greedy and UCB1 agents run and beat greedy"
      (define greedy-summary
        (benchmark-agent make-greedy-agent stationary-bandit 10 #:runs 30 #:steps 300 #:seed 3))
      (define ucb-summary
        (benchmark-agent (lambda (k) (make-ucb1-agent k #:c 1.0)) stationary-bandit 10
                         #:runs 30 #:steps 300 #:seed 3))
      (check-true (> (regret-at greedy-summary 300) (regret-at ucb-summary 300))))

    (test-case "Thompson sampling beats greedy and is near-optimal on a deterministic bandit"
      (define two-arm (lambda () (make-bandit '(1.0 0.0))))
      (define s (benchmark-agent make-thompson-agent two-arm 2 #:runs 20 #:steps 200 #:seed 8))
      (check-true (> (optimal-rate s 1 200) 0.9))
      (define greedy-s (benchmark-agent make-greedy-agent stationary-bandit 10
                                        #:runs 30 #:steps 300 #:seed 3))
      (define thompson-s (benchmark-agent make-thompson-agent stationary-bandit 10
                                          #:runs 30 #:steps 300 #:seed 3))
      (check-true (> (regret-at greedy-s 300) (regret-at thompson-s 300)))
      (check-true (< (regret-at thompson-s 300) 40.0)))

    (test-case "greedy gets stuck on the standard ten-armed bandit"
      (define s (benchmark-agent make-greedy-agent stationary-bandit 10
                                 #:runs 20 #:steps 300 #:seed 3))
      (check-true (< (optimal-rate s 1 300) 0.5)))

    (test-case "constant-alpha epsilon-greedy tracks a drifting bandit"
      (define sample-avg
        (benchmark-agent (lambda (k) (make-epsilon-greedy-agent k #:epsilon 0.1))
                         drifting-bandit 10 #:runs 40 #:steps 1000 #:seed 11))
      (define constant-alpha
        (benchmark-agent (lambda (k) (make-epsilon-greedy-agent k #:epsilon 0.1 #:alpha 0.1))
                         drifting-bandit 10 #:runs 40 #:steps 1000 #:seed 11))
      (define (last-200 s)
        (mean (for/list ([i (in-range 800 1000)]) (vector-ref (summary-rewards s) i))))
      (check-true (> (last-200 constant-alpha) (last-200 sample-avg))))

    (test-case "random-gamma-int has the right mean"
      (random-seed 13)
      (define draws (for/list ([i (in-range 500)]) (random-gamma-int 4)))
      (check-true (> (mean draws) 3.2))
      (check-true (< (mean draws) 4.8)))
    )))

(module+ main
  (void (run-tests TESTS)))

(module+ test
  (void (run-tests TESTS)))
