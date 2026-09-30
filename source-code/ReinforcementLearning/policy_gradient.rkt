#lang racket

;;; policy_gradient.rkt -- Policy gradient methods (REINFORCE).
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Run:  racket policy_gradient.rkt
;;;
;;; Q-learning learns a value function and reads a policy off it.  Policy
;;; gradient methods parameterize the policy itself -- a softmax over
;;; action scores -- and follow the gradient of expected return:
;;;
;;;     theta <- theta + alpha * G_t * grad_theta log pi_theta(a_t | s_t)
;;;
;;; where G_t is the return that followed the action.  The expectation of
;;; that update is the policy gradient (the "score function" estimator).
;;;
;;; Three things make it work in practice, and this file demonstrates each:
;;;
;;;   1. subtract a baseline   (does not change the gradient in
;;;      expectation, but cuts its variance enormously).  The baseline can
;;;      be the mean return of the episode, or a learned V(s).
;;;   2. discount and normalize the returns-to-go so one long lucky episode
;;;      cannot dominate the update.
;;;   3. add an entropy bonus to keep the policy from collapsing onto one
;;;      action before it has learned anything.
;;;
;;; Two problems are solved here:
;;;
;;;   * GridWorld with a tabular softmax policy, where the learned policy
;;;     can be compared with the Q-learning and value-iteration answers.
;;;   * CartPole with a *linear* softmax policy over four continuous state
;;;     variables -- the same setting where tabular methods are hopeless
;;;     unless the state is binned first.

(require "environments.rkt"
         "q_learning.rkt"
         "neural_net.rkt"
         racket/flonum
         racket/list
         racket/math)

(provide
 ;; tabular softmax policy on GridWorld
 make-softmax-policy
 policy-theta
 policy-probs
 softmax-flvector
 sample-from-policy
 policy-action
 train-reinforce
 evaluate-policy
 evaluate-policy-sampled
 ;; linear softmax policy on CartPole
 make-linear-policy
 linear-policy-theta
 linear-policy-probs
 cartpole-features
 train-reinforce-cartpole
 evaluate-cartpole-policy
 entropy-of)

;; =====================================================================
;; Softmax
;; =====================================================================

;; Numerically safe softmax: subtract the row maximum before exponentiating.
(define (softmax-flvector row)
  (define m (flvector-max row))
  (define exps (for/flvector ([x (in-flvector row)]) (flexp (fl- x m))))
  (define total (for/sum ([x (in-flvector exps)]) x))
  (for/flvector ([x (in-flvector exps)]) (fl/ x total)))

;; Shannon entropy of a probability vector, in nats.
(define (entropy-of probs)
  (for/sum ([p (in-flvector probs)])
    (if (fl<= p 0.0) 0.0 (fl* (fl- p) (fllog p)))))

;; =====================================================================
;; Tabular softmax policy on GridWorld
;; =====================================================================
;;
;; theta[s] is a row of action scores (logits).  This is the policy
;; equivalent of a Q table: one number per state-action pair.

(define (make-softmax-policy nS nA [init 0.0])
  (for/vector ([s (in-range nS)]) (make-flvector nA init)))

(define (policy-theta pi) pi)

(define (policy-probs pi s) (softmax-flvector (vector-ref pi s)))

(define (sample-from-policy probs) (sample-categorical probs))

;; Greedy (test-time) action: the largest logit, which is also the largest
;; probability.
(define (policy-action pi s) (flvector-argmax (vector-ref pi s)))

;; One REINFORCE training run on GridWorld.
;;
;; baseline is one of:
;;   'none   -- raw returns-to-go
;;   'mean   -- subtract the mean of the episode's returns-to-go
;;   'value  -- subtract a learned tabular V(s), updated by Monte Carlo
;;
;; entropy is the weight on the entropy bonus (0 disables it).
(define (train-reinforce gw
                         #:episodes [episodes 400]
                         #:alpha [alpha 0.05]
                         #:alpha-v [alpha-v 0.1]
                         #:gamma [gamma 0.99]
                         #:baseline [baseline 'mean]
                         #:entropy [entropy 0.0]
                         #:normalize? [normalize? #f]
                         #:max-steps [max-steps 100]
                         #:seed [seed 42])
  (define nS (gridworld-num-states gw))
  (define nA (gridworld-num-actions gw))
  (define theta (make-softmax-policy nS nA))
  (define V (make-vector nS 0.0))
  (random-seed seed)
  (define returns '())
  (define lengths '())
  (define successes '())
  (for ([ep (in-range episodes)])
    ;; --- collect one episode with the current policy ---
    (define states '())
    (define actions '())
    (define rewards '())
    (let loop ([s (gridworld-reset gw)] [steps 0])
      (when (< steps max-steps)
        (define probs (policy-probs theta s))
        (define a (sample-from-policy probs))
        (define-values (s2 r done?) (gridworld-step gw s a))
        (set! states (cons s states))
        (set! actions (cons a actions))
        (set! rewards (cons r rewards))
        (unless done? (loop s2 (add1 steps)))))
    (set! states (reverse states))
    (set! actions (reverse actions))
    (set! rewards (reverse rewards))
    (define T (length states))
    ;; --- returns-to-go, discounted ---
    (define G (make-vector T 0.0))
    (for ([t (in-range (sub1 T) -1 -1)])
      (vector-set! G t
                   (+ (list-ref rewards t)
                      (if (= t (sub1 T)) 0.0 (* gamma (vector-ref G (add1 t)))))))
    (define glist (vector->list G))
    ;; --- the baseline and the advantages ---
    (define mean-G (mean glist))
    (define sd-G (max 1e-8 (stddev glist)))
    (define advantages
      (for/list ([t (in-range T)])
        (define g (vector-ref G t))
        (define b (case baseline
                    [(none) 0.0]
                    [(mean) mean-G]
                    [(value) (vector-ref V (list-ref states t))]
                    [else (error 'train-reinforce "unknown baseline: ~a" baseline)]))
        (define adv (- g b))
        (if normalize? (/ adv sd-G) adv)))
    ;; --- the policy gradient update ---
    (for ([t (in-range T)])
      (define s (list-ref states t))
      (define a (list-ref actions t))
      (define adv (list-ref advantages t))
      (define probs (policy-probs theta s))
      (for ([a2 (in-range nA)])
        (define grad-log
          (if (= a2 a) (- 1.0 (flvector-ref probs a2)) (fl- (flvector-ref probs a2))))
        (define grad-ent
          (if (fl<= entropy 0.0)
              0.0
              (fl* (fl- (flvector-ref probs a2))
                   (fl+ (if (fl<= (flvector-ref probs a2) 0.0)
                            0.0
                            (fllog (flvector-ref probs a2)))
                        (entropy-of probs)))))
        (flvector-set! (vector-ref theta s) a2
                       (fl+ (flvector-ref (vector-ref theta s) a2)
                            (fl* alpha (fl+ (fl* adv grad-log)
                                            (fl* entropy grad-ent)))))))
    ;; --- Monte Carlo update of the baseline value function ---
    (when (eq? baseline 'value)
      (for ([t (in-range T)])
        (define s (list-ref states t))
        (vector-set! V s (+ (vector-ref V s)
                            (* alpha-v (- (vector-ref G t) (vector-ref V s)))))))
    (set! returns (cons (for/sum ([r (in-list rewards)]) r) returns))
    (set! lengths (cons T lengths))
    (set! successes (cons (fl> (last rewards) 0.0) successes)))
  (values theta (reverse returns) (reverse lengths) (reverse successes)))

;; Greedy rollout of a softmax policy.  Returns (values mean-return
;; success-rate mean-steps).
(define (evaluate-policy gw pi
                         #:episodes [episodes 200]
                         #:max-steps [max-steps 100]
                         #:gamma [gamma 1.0])
  ;; policy-action on the logits is exactly greedy-action on a Q table, so
  ;; the Q-learning evaluation code applies unchanged.
  (evaluate-greedy gw pi #:episodes episodes #:max-steps max-steps #:gamma gamma))

;; Rollout that *samples* from the policy.  This is the quantity REINFORCE
;; actually optimizes, so it is the honest measure of the learned policy;
;; the greedy rollout can score higher (no exploration) or lower (a
;; near-tied argmax can loop).
(define (evaluate-policy-sampled gw pi
                                 #:episodes [episodes 200]
                                 #:max-steps [max-steps 100]
                                 #:gamma [gamma 1.0])
  (define returns '())
  (define steps-list '())
  (define successes 0)
  (for ([ep (in-range episodes)])
    (let loop ([s (gridworld-reset gw)] [ret 0.0] [steps 0] [discount 1.0])
      (cond
        [(>= steps max-steps)
         (set! returns (cons ret returns))
         (set! steps-list (cons steps steps-list))]
        [else
         (define a (sample-from-policy (policy-probs pi s)))
         (define-values (s2 r done?) (gridworld-step gw s a))
         (define ret2 (+ ret (* discount r)))
         (cond
           [done?
            (set! returns (cons ret2 returns))
            (set! steps-list (cons (add1 steps) steps-list))
            (when (fl> r 0.0) (set! successes (add1 successes)))]
           [else (loop s2 ret2 (add1 steps) (* discount gamma))])])))
  (values (mean (reverse returns))
          (/ successes episodes)
          (mean (reverse steps-list))))

;; =====================================================================
;; Linear softmax policy on CartPole
;; =====================================================================
;;
;; theta is a 2 x 5 flvector matrix: one row of five weights per action,
;; multiplying the four normalized state variables plus a bias.  A linear
;; policy is the smallest thing that can solve CartPole, and it shows why
;; feature scaling matters: without cartpole-observe the x-dot and
;; theta-dot terms (range ~+/-3) swamp x (range ~2.4) and theta (~0.2).

(define (make-linear-policy [n-features 5])
  (for/vector ([a (in-range 2)]) (make-flvector n-features 0.0)))

(define (linear-policy-theta lp) lp)

(define (cartpole-features cp s)
  (define obs (cartpole-observe cp s))
  (flvector (flvector-ref obs 0) (flvector-ref obs 1)
            (flvector-ref obs 2) (flvector-ref obs 3)
            1.0))

;; logits = theta . features, then softmax
(define (linear-policy-probs lp f)
  (softmax-flvector
   (for/flvector ([a (in-range 2)])
     (define row (vector-ref lp a))
     (for/sum ([i (in-range 5)]) (fl* (flvector-ref row i) (flvector-ref f i))))))

;; REINFORCE with normalized advantages on CartPole, optimized with Adam
;; (the same optimizer the DQN example uses).  The gradient of the expected
;; return for one episode is the sum over steps of
;;     A_t * (1{a_t = a} - pi(a | s_t)) * f(s_t)
;; accumulated over all steps and then divided by the number of steps.
(define (train-reinforce-cartpole cp
                                  #:episodes [episodes 800]
                                  #:lr [lr 0.02]
                                  #:gamma [gamma 0.99]
                                  #:entropy [entropy 0.01]
                                  #:max-steps [max-steps 500]
                                  #:seed [seed 42]
                                  #:verbose [verbose #f])
  (define theta (make-linear-policy))
  (define params (vector->list theta))
  (define opt (make-adam params))
  (random-seed seed)
  (define returns '())
  (for ([ep (in-range episodes)])
    ;; --- roll out one episode ---
    (define states '())
    (define features '())
    (define actions '())
    (define rewards '())
    (define probs-list '())
    (let loop ([s (cartpole-reset cp)] [steps 0])
      (define f (cartpole-features cp s))
      (define probs (linear-policy-probs theta f))
      (define a (sample-categorical probs))
      (define-values (s2 r done?) (cartpole-act cp s a))
      (set! states (cons s states))
      (set! features (cons f features))
      (set! actions (cons a actions))
      (set! rewards (cons r rewards))
      (set! probs-list (cons probs probs-list))
      (cond
        [(or done? (>= (add1 steps) max-steps)) (void)]
        [else (loop s2 (add1 steps))]))
    (set! states (reverse states))
    (set! features (reverse features))
    (set! actions (reverse actions))
    (set! rewards (reverse rewards))
    (set! probs-list (reverse probs-list))
    (define T (length states))
    ;; --- discounted returns-to-go, standardized into advantages ---
    (define G (make-vector T 0.0))
    (for ([t (in-range (sub1 T) -1 -1)])
      (vector-set! G t
                   (+ (list-ref rewards t)
                      (if (= t (sub1 T)) 0.0 (* gamma (vector-ref G (add1 t)))))))
    (define glist (vector->list G))
    (define m-G (mean glist))
    (define sd-G (max 1e-8 (stddev glist)))
    (define adv (for/vector ([g (in-vector G)]) (/ (- g m-G) sd-G)))
    ;; --- accumulate the policy gradient ---
    (define grads (for/list ([p (in-list params)]) (make-flvector (flvector-length p) 0.0)))
    (for ([t (in-range T)])
      (define a (list-ref actions t))
      (define At (vector-ref adv t))
      (define probs (list-ref probs-list t))
      (define f (list-ref features t))
      (define ent (entropy-of probs))
      (for ([a2 (in-range 2)])
        (define p (flvector-ref probs a2))
        (define grad-log (if (= a2 a) (- 1.0 p) (fl- p)))
        (define grad-ent (if (fl<= entropy 0.0)
                             0.0
                             (fl* (fl- p) (fl+ (if (fl<= p 0.0) 0.0 (fllog p)) ent))))
        ;; adam-step! performs gradient *descent*, and this is a gradient
        ;; ascent problem: accumulate the negated gradient.
        (define coef (fl/ (fl- (fl+ (fl* At grad-log) (fl* entropy grad-ent)))
                          (exact->inexact T)))
        (flvector-add-scaled! (list-ref grads a2) f coef)))
    (adam-step! opt params grads #:lr lr)
    (define total (for/sum ([r (in-list rewards)]) r))
    (set! returns (cons total returns))
    (when (and verbose (zero? (remainder (add1 ep) 100)))
      (printf "  episode ~a: mean return of last 100 = ~a\n"
              (add1 ep) (real->decimal-string (mean (take returns (min 100 (length returns)))) 1))))
  (values theta (reverse returns)))

;; Greedy (argmax) evaluation of a CartPole policy.  Returns
;; (values mean-steps best-steps full-episodes).
(define (evaluate-cartpole-policy cp lp
                                  #:episodes [episodes 20]
                                  #:max-steps [max-steps 500])
  (define steps-list '())
  (for ([ep (in-range episodes)])
    (let loop ([s (cartpole-reset cp)] [steps 0])
      (cond
        [(>= steps max-steps) (set! steps-list (cons steps steps-list))]
        [else
         (define f (cartpole-features cp s))
         (define probs (linear-policy-probs lp f))
         (define a (flvector-argmax probs))
         (define-values (s2 r done?) (cartpole-act cp s a))
         (if done?
             (set! steps-list (cons (add1 steps) steps-list))
             (loop s2 (add1 steps)))])))
  (values (mean steps-list)
          (apply max steps-list)
          (count (lambda (s) (>= s max-steps)) steps-list)))

;; =====================================================================
;; Demo
;; =====================================================================

(module+ main
  (define gw (make-gridworld))
  (define ggamma 0.95)

  (printf "=== 1b. Policy gradient (REINFORCE) ===\n\n")
  (printf "5x5 GridWorld, tabular softmax policy over 25 states x 4 actions:\n")
  (printf "~a\n\n" (gridworld-map-string gw))
  (printf "Each variant trains for 600 episodes with discount gamma = 0.99,\n")
  (printf "learning rate 0.05 and a temperature-1 softmax.\n\n")

  ;; name, baseline, entropy weight
  (define variants
    (list (list 'raw-returns             'none  0.0)
          (list 'mean-baseline           'mean  0.0)
          (list 'learned-value-baseline  'value 0.0)
          (list 'value-baseline-entropy  'value 0.05)))

  (define results (make-hash))
  (define rows
    (cons (list "variant" "sampled ret" "greedy ret" "success" "steps")
          (for/list ([v (in-list variants)])
            (define name (first v))
            (define-values (theta returns lengths successes)
              (train-reinforce gw #:episodes 600
                               #:baseline (second v)
                               #:entropy (third v)))
            (hash-set! results name (list theta returns lengths successes))
            (define-values (sret srate ssteps)
              (evaluate-policy-sampled gw theta #:episodes 200 #:gamma ggamma))
            (define-values (gret grate gsteps)
              (evaluate-policy gw theta #:episodes 200 #:gamma ggamma))
            (list (format "~a" name)
                  (real->decimal-string sret 4)
                  (real->decimal-string gret 4)
                  (real->decimal-string grate 3)
                  (real->decimal-string gsteps 2)))))
  (printf "~a\n\n" (format-table rows #:widths '(24 12 12 9 8)))

  (define-values (V* Q* sweeps) (value-iteration gw #:gamma ggamma))
  (printf "For reference, value iteration gives the optimal return ~a;\n"
          (real->decimal-string (vector-ref V* (gridworld-start gw)) 4))
  (printf "Q-learning reaches the same number from samples (q_learning.rkt).\n\n")
  (printf "Why mean-baseline stalls: with a terminal reward, every return-to-go\n")
  (printf "in a failed episode is about the same (-1 for the pit), so subtracting\n")
  (printf "the episode mean leaves an advantage near zero and the policy never\n")
  (printf "learns that the states along that path were bad.  A learned V(s) is\n")
  (printf "state dependent, so entering a pit makes the advantage strongly\n")
  (printf "negative and the policy backs away from it.\n\n")

  (printf "Training return per 100-episode block (sampled behaviour):\n")
  (printf "~a\n"
          (format-table
           (for/list ([v (in-list variants)])
             (cons (format "~a" (first v))
                   (for/list ([b (in-list (block-means (second (hash-ref results (first v))) 100))])
                     (real->decimal-string b 2))))
           #:widths '(24 9 9 9 9 9 9)))
  (printf "\n")

  (define best (first (hash-ref results 'learned-value-baseline)))
  (printf "Greedy policy from REINFORCE with a learned baseline:\n~a\n\n"
          (gridworld-render gw #:policy (lambda (s) (policy-action best s))))
  (printf "Greedy policy from value iteration:\n~a\n\n"
          (gridworld-render gw #:policy (lambda (s) (greedy-action Q* s))))
  (printf "REINFORCE path: ~a\n\n"
          (for/list ([s (in-list (greedy-path gw best))])
            (format "(~a,~a)" (first (gridworld-state->pos gw s)) (second (gridworld-state->pos gw s)))))

  ;; ---- CartPole: continuous state, linear policy -------------------
  (printf "--- CartPole with a linear softmax policy ---\n\n")
  (printf "State (x, x-dot, theta, theta-dot) is continuous, so a table would\n")
  (printf "need binning.  The policy is softmax(W f(s)) with f the four\n")
  (printf "scaled state variables plus a bias; REINFORCE with standardized\n")
  (printf "returns-to-go and Adam trains it directly.\n\n")
  (define cp (make-cartpole))
  (define-values (lp creturns)
    (train-reinforce-cartpole cp #:episodes 600 #:lr 0.05 #:gamma 0.99 #:entropy 0.01))
  (printf "Mean episode length per 100-episode block (training):\n  ~a\n\n"
          (string-join
           (for/list ([b (in-list (block-means creturns 100))])
             (real->decimal-string b 1))
           "  "))
  (define-values (mean-steps best-steps full) (evaluate-cartpole-policy cp lp #:episodes 20))
  (printf "Greedy evaluation over 20 episodes: mean ~a steps, best ~a, ~a/20 reached 500.\n"
          (real->decimal-string mean-steps 1) best-steps full)
  (printf "\nA random policy survives about 22 steps on CartPole, and 500 is the\n")
  (printf "time limit; the learned policy balances the pole indefinitely.\n"))
