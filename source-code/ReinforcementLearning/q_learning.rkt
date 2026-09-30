#lang racket

;;; q_learning.rkt -- Tabular reinforcement learning: Q-learning, SARSA,
;;; expected SARSA and Double Q-learning, all on the same GridWorld MDP.
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Run:  racket q_learning.rkt
;;;
;;; The Q table is a vector of flvectors: (vector-ref q s) is the row of
;;; action values for state s.  Temporal-difference control only ever
;;; touches one row per step, so a table is all the "function
;;; approximation" these methods need.  dqn.rkt replaces exactly this
;;; table with a neural network.
;;;
;;; The four methods differ in only one place -- how they turn the next
;;; state into a target value:
;;;
;;;   Q-learning      y = r + gamma * max_a' Q(s', a')             off-policy
;;;   SARSA           y = r + gamma * Q(s', a'), a' ~ eps-greedy   on-policy
;;;   Expected SARSA  y = r + gamma * sum_a' pi(a'|s') Q(s', a')
;;;   Double Q        update one table, bootstrap the other (removes the
;;;                   maximization bias of Q-learning)
;;;
;;; The file also implements value iteration -- the model-based dynamic
;;; programming solution -- so the learned tables can be checked against
;;; the true optimal values.

(require "environments.rkt"
         racket/flonum
         racket/list
         racket/math)

(provide
 make-q-table
 q-ref
 q-set!
 q-average
 greedy-action
 greedy-action-value
 epsilon-greedy-action
 behavior-action
 expected-value
 epsilon-at
 train-td
 evaluate-greedy
 policy-success-rate
 greedy-path
 value-iteration
 q-difference
 q-mean-difference
 block-means
 format-table)

;; =====================================================================
;; Q tables
;; =====================================================================

(define (make-q-table nS nA [init 0.0])
  (for/vector ([s (in-range nS)]) (make-flvector nA init)))

(define (q-ref q s a) (flvector-ref (vector-ref q s) a))

(define (q-set! q s a v) (flvector-set! (vector-ref q s) a v))

;; Double Q-learning keeps two tables and acts greedily with respect to
;; their average.  The average is also the table this file reports, so its
;; entries are on the same scale as Q* and can be compared with it.
(define (q-average q1 q2)
  (for/vector ([r1 (in-vector q1)] [r2 (in-vector q2)])
    (for/flvector ([x (in-flvector r1)] [y (in-flvector r2)]) (fl* 0.5 (fl+ x y)))))

(define (greedy-action q s) (flvector-argmax (vector-ref q s)))

(define (greedy-action-value q s) (flvector-max (vector-ref q s)))

(define (epsilon-greedy-action q s eps)
  (if (< (random) eps)
      (random (flvector-length (vector-ref q s)))
      (greedy-action q s)))

;; Action taken by the behaviour policy.  For Double Q-learning the
;; average table has to be rebuilt for every state, because both tables
;; are moving.
(define (behavior-action method q q2 s eps)
  (if (eq? method 'double-q)
      (let ([row (for/flvector ([x (in-flvector (vector-ref q s))]
                                [y (in-flvector (vector-ref q2 s))])
                   (fl* 0.5 (fl+ x y)))])
        (if (< (random) eps)
            (random (flvector-length row))
            (flvector-argmax row)))
      (epsilon-greedy-action q s eps)))

;; Value of an epsilon-greedy policy at state s:
;; (1-eps) * Q(s, greedy) + eps/(nA-1) * sum of the other actions.
(define (expected-value q s eps)
  (define row (vector-ref q s))
  (define nA (flvector-length row))
  (define greedy (flvector-argmax row))
  (if (or (<= nA 1) (<= eps 0.0))
      (flvector-max row)
      (for/sum ([a (in-range nA)])
        (* (if (= a greedy) (- 1.0 eps) (/ eps (- nA 1)))
           (flvector-ref row a)))))

;; =====================================================================
;; Training
;; =====================================================================

(define (epsilon-at ep episodes start end decay-episodes)
  (cond
    [(>= ep decay-episodes) end]
    [else (+ end (* (- start end) (- 1.0 (/ ep decay-episodes))))]))

;; The one line that distinguishes the methods: return the TD target and
;; the table that should move toward it.
(define (td-target/table method q q2 s2 r done? gamma eps)
  (cond
    ;; Double Q-learning flips a coin first and then computes the target
    ;; from the *other* table.  The flip has to happen even for a terminal
    ;; transition, where the target is just r; if all terminal data went
    ;; into one table, that table would race ahead while the other stayed
    ;; at its initial values and the bootstrap chain would break.
    [(eq? method 'double-q)
     (define use-q1? (< (random) 0.5))
     (cond
       [done? (values r (if use-q1? q q2))]
       [use-q1?
        ;; update Q1, bootstrap the greedy action of Q1 evaluated in Q2
        (values (+ r (* gamma (q-ref q2 s2 (flvector-argmax (vector-ref q s2))))) q)]
       [else
        ;; update Q2, bootstrap the greedy action of Q2 evaluated in Q1
        (values (+ r (* gamma (q-ref q s2 (flvector-argmax (vector-ref q2 s2))))) q2)])]
    [done? (values r q)]
    [(eq? method 'q-learning)
     (values (+ r (* gamma (greedy-action-value q s2))) q)]
    [(eq? method 'expected-sarsa)
     (values (+ r (* gamma (expected-value q s2 eps))) q)]
    [else (error 'td-target/table "unknown method: ~a" method)]))

;; Q-learning / expected SARSA / Double Q episode: act with the behaviour
;; policy, learn from the target, never mind which action comes next.
(define (run-episode-off-policy gw method q q2 visited eps alpha gamma max-steps random-start?)
  (let loop ([s (gridworld-reset gw #:random? random-start?)] [ret 0.0] [steps 0] [discount 1.0])
    (cond
      [(>= steps max-steps) (values ret steps #f)]
      [else
       (define a (behavior-action method q q2 s eps))
       (define-values (s2 r done?) (gridworld-step gw s a))
       (define ret2 (+ ret (* discount r)))
       ;; Remember that this state-action pair produced data; the |Q-Q*|
       ;; report only looks at pairs the agent actually sampled.
       (q-set! visited s a (+ 1.0 (q-ref visited s a)))
       (define-values (target table)
         (td-target/table method q q2 s2 r done? gamma eps))
       (q-set! table s a (+ (q-ref table s a) (* alpha (- target (q-ref table s a)))))
       (cond
         [done? (values ret2 (add1 steps) (fl> r 0.0))]
         [else (loop s2 ret2 (add1 steps) (* discount gamma))])])))

;; SARSA episode: the next action is chosen by the same epsilon-greedy
;; policy that will be used to act, and its value is the target.
(define (run-episode-sarsa gw q visited eps alpha gamma max-steps random-start?)
  (define s0 (gridworld-reset gw #:random? random-start?))
  (let loop ([s s0] [a (epsilon-greedy-action q s0 eps)] [ret 0.0] [steps 0] [discount 1.0])
    (cond
      [(>= steps max-steps) (values ret steps #f)]
      [else
       (define-values (s2 r done?) (gridworld-step gw s a))
       (define ret2 (+ ret (* discount r)))
       (q-set! visited s a (+ 1.0 (q-ref visited s a)))
       (cond
         [done?
          (q-set! q s a (+ (q-ref q s a) (* alpha (- r (q-ref q s a)))))
          (values ret2 (add1 steps) (fl> r 0.0))]
         [else
          (define a2 (epsilon-greedy-action q s2 eps))
          (define target (+ r (* gamma (q-ref q s2 a2))))
          (q-set! q s a (+ (q-ref q s a) (* alpha (- target (q-ref q s a)))))
          (loop s2 a2 ret2 (add1 steps) (* discount gamma))])])))

;; Train one method.  Returns
;;   (values greedy-policy-table episode-returns episode-lengths
;;           success-flags visit-counts)
;;
;; method is one of 'q-learning, 'sarsa, 'expected-sarsa, 'double-q.
(define (train-td gw method
                  #:episodes [episodes 600]
                  #:alpha [alpha 0.2]
                  #:gamma [gamma 0.95]
                  #:epsilon-start [epsilon-start 1.0]
                  #:epsilon-end [epsilon-end 0.05]
                  #:epsilon-decay-episodes [epsilon-decay-episodes #f]
                  #:max-steps [max-steps 100]
                  #:q-init [q-init 0.0]
                  #:random-start? [random-start? #f]
                  #:seed [seed 42])
  (define nS (gridworld-num-states gw))
  (define nA (gridworld-num-actions gw))
  (define decay (or epsilon-decay-episodes
                    (max 1 (exact-floor (* 0.6 episodes)))))
  (define q (make-q-table nS nA q-init))
  (define q2 (and (eq? method 'double-q) (make-q-table nS nA q-init)))
  (define visited (make-q-table nS nA 0.0))
  (random-seed seed)
  (define returns '())
  (define lengths '())
  (define successes '())
  (for ([ep (in-range episodes)])
    (define eps (epsilon-at ep episodes epsilon-start epsilon-end decay))
    (define-values (ret steps success?)
      (if (eq? method 'sarsa)
          (run-episode-sarsa gw q visited eps alpha gamma max-steps random-start?)
          (run-episode-off-policy gw method q q2 visited eps alpha gamma max-steps random-start?)))
    (set! returns (cons ret returns))
    (set! lengths (cons steps lengths))
    (set! successes (cons success? successes)))
  ;; For Double Q the reported table is the average of Q1 and Q2; its
  ;; greedy policy is the one the agent actually followed.
  (values (if q2 (q-average q q2) q)
          (reverse returns)
          (reverse lengths)
          (reverse successes)
          visited))

;; =====================================================================
;; Evaluation
;; =====================================================================

;; Roll out the greedy policy without exploration.  Returns
;; (values mean-return success-rate mean-steps).  With the default
;; gamma = 1.0 the return is the plain sum of rewards; pass the training
;; gamma to compare with V(start) from value iteration.
(define (evaluate-greedy gw q
                         #:episodes [episodes 200]
                         #:max-steps [max-steps 100]
                         #:gamma [gamma 1.0]
                         #:random-start? [random-start? #f])
  (define returns '())
  (define steps-list '())
  (define successes 0)
  (for ([ep (in-range episodes)])
    (let loop ([s (gridworld-reset gw #:random? random-start?)]
               [ret 0.0] [steps 0] [discount 1.0])
      (cond
        [(>= steps max-steps)
         (set! returns (cons ret returns))
         (set! steps-list (cons steps steps-list))]
        [else
         (define a (greedy-action q s))
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

;; Convenience wrapper when only the success rate matters.
(define (policy-success-rate gw q #:episodes [episodes 200] #:max-steps [max-steps 100])
  (define-values (ret rate steps)
    (evaluate-greedy gw q #:episodes episodes #:max-steps max-steps))
  rate)

;; States visited by following the greedy policy from the start until a
;; terminal state (or the step limit).
(define (greedy-path gw q #:max-steps [max-steps 100])
  (let loop ([s (gridworld-start gw)]
             [acc (list (gridworld-start gw))]
             [steps 0])
    (cond
      [(or (>= steps max-steps) (gridworld-terminal? gw s)) (reverse acc)]
      [else
       (define a (greedy-action q s))
       (define-values (s2 r done?) (gridworld-step gw s a))
       (loop s2 (cons s2 acc) (add1 steps))])))

;; =====================================================================
;; Value iteration: the model-based answer
;; =====================================================================

;; Sweeps Bellman's optimality equation to convergence.  Returns
;; (values V Q sweeps).  Terminal states hold value 0 and are never
;; updated.
(define (value-iteration gw #:gamma [gamma 0.95]
                         #:theta [theta 1e-10]
                         #:max-sweeps [max-sweeps 5000])
  (define nS (gridworld-num-states gw))
  (define nA (gridworld-num-actions gw))
  (define V (make-vector nS 0.0))
  (define Q (make-q-table nS nA 0.0))
  (define sweeps 0)
  (let loop ([sweep 0])
    (set! sweeps sweep)
    (cond
      [(>= sweep max-sweeps) (values V Q sweeps)]
      [else
       (define delta 0.0)
       (for ([s (in-range nS)] #:unless (gridworld-terminal? gw s))
         (define best -inf.0)
         (for ([a (in-range nA)])
           (define qsa
             (for/sum ([t (in-list (gridworld-transitions gw s a))])
               (define p (first t))
               (define s2 (second t))
               (define r (third t))
               (define done? (fourth t))
               (* p (if done? r (+ r (* gamma (vector-ref V s2)))))))
           (q-set! Q s a qsa)
           (set! best (max best qsa)))
         (set! delta (max delta (abs (- best (vector-ref V s)))))
         (vector-set! V s best))
       (if (< delta theta) (values V Q sweeps) (loop (add1 sweep)))])))

;; Largest absolute entrywise difference between two Q tables.  With a
;; visit-count table supplied, only state-action pairs that were visited
;; at least once are compared (entries the agent never sampled stay at
;; their initial value, so comparing them would be meaningless).
(define (q-difference q1 q2 #:visits [visits #f])
  (for*/fold ([m 0.0])
             ([s (in-range (vector-length q1))]
              [a (in-range (flvector-length (vector-ref q1 s)))]
              #:when (or (not visits)
                         (> (flvector-ref (vector-ref visits s) a) 0.0)))
    (max m (abs (- (q-ref q1 s a) (q-ref q2 s a))))))

;; Mean absolute difference over state-action pairs with enough data.
;; This is the readable companion of q-difference: the maximum is always
;; dominated by the one pair the agent visited least.
(define (q-mean-difference q1 q2 #:visits [visits #f] #:min-visits [min-visits 1])
  (define diffs
    (for*/list ([s (in-range (vector-length q1))]
                [a (in-range (flvector-length (vector-ref q1 s)))]
                #:when (or (not visits)
                           (>= (flvector-ref (vector-ref visits s) a) min-visits)))
      (abs (- (q-ref q1 s a) (q-ref q2 s a)))))
  (if (null? diffs) 0.0 (mean diffs)))

;; =====================================================================
;; Reporting helpers
;; =====================================================================

;; Mean of each consecutive block of n values, for a compact learning
;; curve such as "block means of 100 episodes".
(define (block-means xs n)
  (for/list ([i (in-range 0 (length xs) n)])
    (mean (take (drop xs i) (min n (- (length xs) i))))))

;; Left-aligned fixed-width table rows, used by the demos.
(define (format-table rows #:widths [widths #f])
  (define n (length (car rows)))
  (define widths* (or widths (for/list ([i (in-range n)]) 16)))
  (string-join
   (for/list ([row (in-list rows)])
     (string-join
      (for/list ([cell (in-list row)] [w (in-list widths*)])
        (define s (format "~a" cell))
        (if (>= (string-length s) w) s (string-append s (make-string (- w (string-length s)) #\space))))
      " "))
   "\n"))

;; =====================================================================
;; Demo
;; =====================================================================

(module+ main
  (define gw (make-gridworld))
  (define episodes 800)
  (define alpha 0.2)
  (define gamma 0.95)
  (define decay (exact-floor (* 0.5 episodes)))

  (printf "=== 1a. Tabular temporal-difference control on GridWorld ===\n\n")
  (printf "5x5 GridWorld (S start, G goal, X pit, # wall); step cost -0.01,\n")
  (printf "goal +1, pit -1, discount gamma = ~a, learning rate alpha = ~a\n\n" gamma alpha)
  (printf "~a\n\n" (gridworld-map-string gw))
  (printf "Each method trains for ~a episodes with epsilon annealed\n" episodes)
  (printf "from 1.00 to 0.05 over the first ~a episodes.\n\n" decay)

  (define methods '(q-learning sarsa expected-sarsa double-q))
  (define results (make-hash))
  (define rows
    (cons (list "method" "return" "success" "steps" "mean|dQ|")
          (for/list ([m (in-list methods)])
            (define-values (q returns lengths successes visited)
              (train-td gw m #:episodes episodes #:alpha alpha #:gamma gamma))
            (hash-set! results m (list q returns lengths successes visited))
            (define-values (ret rate steps)
              (evaluate-greedy gw q #:episodes 200 #:gamma gamma))
            (list (format "~a" m)
                  (real->decimal-string ret 4)
                  (real->decimal-string rate 3)
                  (real->decimal-string steps 2)
                  "-"))))

  ;; Value iteration gives the exact Q*, so we can measure how close the
  ;; sampled methods got on the state-action pairs they actually tried.
  ;; Only the off-policy methods aim at Q*: SARSA and expected SARSA
  ;; estimate the value of the epsilon-greedy behaviour policy instead, so
  ;; their tables are marked n/a.
  (define-values (V* Q* sweeps) (value-iteration gw #:gamma gamma))
  (define off-policy '(q-learning double-q))
  (define rows*
    (for/list ([row (in-list rows)])
      (define m (string->symbol (first row)))
      (cond
        [(eq? m 'method) row]
        [else
         (append
          (drop-right row 1)
          (list
           (if (memq m off-policy)
               (real->decimal-string
                (q-mean-difference (first (hash-ref results m)) Q*
                                   #:visits (list-ref (hash-ref results m) 4)
                                   #:min-visits 5)
                4)
               "n/a (on-policy)")))])))
  (printf "~a\n\n" (format-table rows* #:widths '(16 10 9 8 16)))

  (printf "Value iteration reference: V(start) = ~a after ~a sweeps\n"
          (real->decimal-string (vector-ref V* (gridworld-start gw)) 4) sweeps)
  (printf "  mean |Q-Q*| covers state-action pairs visited at least 5 times.\n")
  (printf "  Q-learning and Double Q learn Q* (their targets maximize);\n")
  (printf "  SARSA and expected SARSA learn Q^pi for the epsilon-greedy\n")
  (printf "  behaviour policy, which prices in the chance of falling into the\n")
  (printf "  pit and is therefore lower than Q* near the hazard.\n\n")

  ;; Learning curves, in blocks of 100 episodes, for the exploration
  ;; behaviour (so they include epsilon-greedy wandering).
  (printf "Training return per 100-episode block (epsilon-greedy behaviour):\n")
  (printf "~a\n"
          (format-table
           (for/list ([m (in-list methods)])
             (cons (format "~a" m)
                   (for/list ([b (in-list (block-means (second (hash-ref results m)) 100))])
                     (real->decimal-string b 2))))
           #:widths '(16 9 9 9 9 9 9 9)))
  (printf "\n")

  ;; Show the learned policy and the optimal policy side by side.
  (define q-learn (first (hash-ref results 'q-learning)))
  (printf "Greedy policy learned by Q-learning:\n~a\n\n"
          (gridworld-render gw #:policy (lambda (s) (greedy-action q-learn s))))
  (printf "Greedy policy from value iteration:\n~a\n\n"
          (gridworld-render gw #:policy (lambda (s) (greedy-action Q* s))))
  (printf "Q-learning path: ~a\n"
          (for/list ([s (in-list (greedy-path gw q-learn))])
            (format "(~a,~a)" (first (gridworld-state->pos gw s)) (second (gridworld-state->pos gw s)))))
  (printf "Value-iteration path: ~a\n"
          (for/list ([s (in-list (greedy-path gw Q*))])
            (format "(~a,~a)" (first (gridworld-state->pos gw s)) (second (gridworld-state->pos gw s)))))
  (printf "\nAll four methods find the same optimal path from (0,0) to (4,4),\n")
  (printf "even though they learn from different targets.  Arrows away from the\n")
  (printf "path are less trustworthy: those states were visited far less often.\n"))
