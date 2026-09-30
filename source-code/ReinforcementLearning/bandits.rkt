#lang racket

;;; bandits.rkt -- Multi-armed bandits: exploration, exploitation and regret.
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Run:  racket bandits.rkt
;;;
;;; A k-armed bandit is the smallest reinforcement learning problem: there
;;; is one state, k actions, and pulling arm a returns a random reward with
;;; mean mu_a.  Everything is about the exploration/exploitation trade-off,
;;; and regret -- the reward given up compared with always pulling the best
;;; arm -- is the scoreboard:
;;;
;;;     regret after T pulls = sum_t ( max_a mu_a(t) - mu_{a_t}(t) )
;;;
;;; Seven strategies are implemented and compared:
;;;
;;;   greedy              always take the best estimate (gets stuck early)
;;;   epsilon-greedy      explore at random with probability epsilon
;;;     - sample average  average all rewards (best when stationary)
;;;     - constant alpha  exponential recency weighting (tracks drift)
;;;   optimistic initial  start every arm high, so each looks worth trying
;;;   UCB1                add a sqrt(log t / n) bonus for under-tried arms
;;;   Thompson sampling   sample each arm's mean from its posterior
;;;   softmax             Boltzmann exploration at temperature tau
;;;
;;; The second half of the demo repeats the comparison on a *non-stationary*
;;; bandit, where the arm means take a small random walk after every pull.

(require "environments.rkt"
         "q_learning.rkt"
         racket/flonum
         racket/list
         racket/math)

(provide
 bandit-agent
 bandit-agent-name
 bandit-agent-choose
 bandit-agent-update!
 make-greedy-agent
 make-epsilon-greedy-agent
 make-optimistic-agent
 make-ucb1-agent
 make-thompson-agent
 make-softmax-agent
 run-bandit-agent
 benchmark-agent
 summary
 summary-name
 summary-rewards
 summary-optimal
 summary-cumulative
 regret-at
 optimal-rate
 random-gamma-int
 stationary-bandit
 drifting-bandit)

;; =====================================================================
;; Agents
;; =====================================================================
;;
;; An agent is two closures over its own state: `choose` returns the next
;; arm to pull, and `update!` folds in the reward that came back.  Keeping
;; state in closures means a single benchmark loop can run any strategy,
;; and a fresh agent per run keeps the statistics clean.

(struct bandit-agent (name choose update!)
  #:transparent)

;; The simplest possible learner: always take the arm with the highest
;; average reward so far.  With all arms starting at zero this is greedy
;; for the first arm that happens to pay, which is exactly its weakness.
(define (make-greedy-agent k)
  (make-epsilon-greedy-agent k #:epsilon 0.0))

;; epsilon of the time pick a uniformly random arm; otherwise pick the
;; best estimate.  alpha #f uses the sample average 1/n (best when the
;; problem is stationary); a number uses a constant step size, which
;; forgets old rewards and so can follow a moving target.
(define (make-epsilon-greedy-agent k
                                   #:epsilon [epsilon 0.1]
                                   #:alpha [alpha #f]
                                   #:init [init 0.0])
  (define Q (make-flvector k (exact->inexact init)))
  (define N (make-flvector k 0.0))
  (bandit-agent
   (if alpha
       (format "epsilon-greedy ~a (alpha ~a)" epsilon alpha)
       (format "epsilon-greedy ~a (sample avg)" epsilon))
   (lambda ()
     (if (< (random) epsilon)
         (random k)
         (flvector-argmax Q)))
   (lambda (a r)
     (cond
       [alpha
        (flvector-set! Q a (fl+ (flvector-ref Q a)
                                (fl* alpha (fl- r (flvector-ref Q a)))))]
       [else
        (flvector-set! N a (fl+ 1.0 (flvector-ref N a)))
        (flvector-set! Q a (fl+ (flvector-ref Q a)
                                (fl/ (fl- r (flvector-ref Q a)) (flvector-ref N a))))]))))

;; Optimistic initial values: pretend every arm has already paid init, so
;; each one looks good until it is tried.  Pure exploitation then does the
;; exploring, as long as the optimism is larger than any real mean.
(define (make-optimistic-agent k #:init [init 5.0])
  (define Q (make-flvector k (exact->inexact init)))
  (define N (make-flvector k 0.0))
  (bandit-agent
   (format "optimistic init ~a" init)
   (lambda ()
     (define untried (for/list ([i (in-range k)] #:when (zero? (flvector-ref N i))) i))
     (cond
       [(pair? untried) (list-ref untried (random (length untried)))]
       [else (flvector-argmax Q)]))
   (lambda (a r)
     (flvector-set! N a (fl+ 1.0 (flvector-ref N a)))
     (flvector-set! Q a (fl+ (flvector-ref Q a)
                             (fl/ (fl- r (flvector-ref Q a)) (flvector-ref N a)))))))

;; UCB1: pull every arm once, then pick the arm maximizing
;;     Q(a) + c * sqrt(log t / N(a)).
;; The bonus shrinks as an arm is pulled, so under-explored arms stay
;; attractive and the regret grows only logarithmically.  The Hoeffding
;; bound gives c = sqrt(2); c = 1 explores less and does better on a bandit
;; whose means are well separated, which is why it is used here.
(define (make-ucb1-agent k #:c [c 1.0])
  (define Q (make-flvector k 0.0))
  (define N (make-flvector k 0.0))
  (define t 0)
  (bandit-agent
   (format "ucb1 c=~a" c)
   (lambda ()
     (set! t (add1 t))
     (define untried (for/list ([i (in-range k)] #:when (zero? (flvector-ref N i))) i))
     (cond
       [(pair? untried) (list-ref untried (random (length untried)))]
       [else
        (define scores
          (for/flvector ([i (in-range k)])
            (fl+ (flvector-ref Q i)
                 (fl* c (flsqrt (fl/ (fllog (exact->inexact t))
                                     (flvector-ref N i)))))))
        (flvector-argmax scores)]))
   (lambda (a r)
     (flvector-set! N a (fl+ 1.0 (flvector-ref N a)))
     (flvector-set! Q a (fl+ (flvector-ref Q a)
                             (fl/ (fl- r (flvector-ref Q a)) (flvector-ref N a)))))))

;; Gamma(shape, 1) for a positive integer shape, used to build Beta
;; samples.  A Gamma(n, 1) variable is the sum of n unit exponentials, and
;; a unit exponential is -log U for U uniform on (0, 1).
;;
;; Summing the logs matters: after a few hundred pulls the shape parameter
;; is in the hundreds, and computing -log(U_1 * ... * U_n) directly would
;; underflow to zero and hand every arm the same saturated sample.
(define (random-gamma-int n)
  (cond
    [(<= n 0) 0.0]
    [else
     (for/sum ([i (in-range n)])
       (- (fllog (max 1e-12 (random)))))]))

;; Thompson sampling for Bernoulli rewards: keep a Beta(successes + 1,
;; failures + 1) posterior for each arm, draw one sample per arm from that
;; posterior and pull the arm with the largest draw.  Exploration happens
;; automatically: an arm about which little is known has a wide posterior.
(define (make-thompson-agent k)
  (define alpha (make-flvector k 1.0))
  (define beta (make-flvector k 1.0))
  (bandit-agent
   "thompson sampling"
   (lambda ()
     (define samples
       (for/flvector ([i (in-range k)])
         (define x (random-gamma-int (exact-round (flvector-ref alpha i))))
         (define y (random-gamma-int (exact-round (flvector-ref beta i))))
         (if (fl= (fl+ x y) 0.0) 0.0 (fl/ x (fl+ x y)))))
     (flvector-argmax samples))
   (lambda (a r)
     (if (fl> r 0.5)
         (flvector-set! alpha a (fl+ 1.0 (flvector-ref alpha a)))
         (flvector-set! beta a (fl+ 1.0 (flvector-ref beta a)))))))

;; Boltzmann/softmax exploration: probability of arm a is proportional to
;; exp(Q(a) / tau).  Low tau approaches greedy; high tau approaches uniform.
(define (make-softmax-agent k #:tau [tau 0.1])
  (define Q (make-flvector k 0.0))
  (define N (make-flvector k 0.0))
  (bandit-agent
   (format "softmax tau=~a" tau)
   (lambda ()
     (define m (flvector-max Q))
     (define exps (for/flvector ([x (in-flvector Q)]) (flexp (fl/ (fl- x m) tau))))
     (sample-categorical exps))
   (lambda (a r)
     (flvector-set! N a (fl+ 1.0 (flvector-ref N a)))
     (flvector-set! Q a (fl+ (flvector-ref Q a)
                             (fl/ (fl- r (flvector-ref Q a)) (flvector-ref N a)))))))

;; =====================================================================
;; Running one agent on one bandit
;; =====================================================================

(struct bandit-result (name rewards optimal regret cumulative)
  #:transparent)

;; Pull for `steps` steps.  Regret is measured against the best arm just
;; before each pull, which is the right definition when the means drift.
(define (run-bandit-agent agent b steps)
  (define rewards (make-vector steps 0.0))
  (define optimal (make-vector steps 0))
  (define regret (make-vector steps 0.0))
  (for ([t (in-range steps)])
    (define best-mean (bandit-optimal-mean b))
    (define best-arm (bandit-optimal-arm b))
    (define a ((bandit-agent-choose agent)))
    (define r (bandit-pull b a))
    ((bandit-agent-update! agent) a r)
    (vector-set! rewards t r)
    (vector-set! optimal t (if (= a best-arm) 1 0))
    (vector-set! regret t (fl- best-mean r)))
  (define-values (_ignored cumulative)
    (for/fold ([acc 0.0] [out '()]) ([x (in-vector regret)])
      (define s (fl+ acc x))
      (values s (cons s out))))
  (bandit-result (bandit-agent-name agent) rewards optimal regret
                 (list->vector (reverse cumulative))))

;; =====================================================================
;; Averaging over runs
;; =====================================================================

(struct summary (name rewards optimal cumulative)
  #:transparent)

(define (vector-mean-vectors vs)
  (define n (vector-length (car vs)))
  (for/vector ([i (in-range n)])
    (mean (for/list ([v (in-list vs)]) (vector-ref v i)))))

;; Run `make-agent` against `make-bandit` `runs` times with different
;; seeds and average the curves.  Returns a summary whose vectors are
;; indexed by step.
(define (benchmark-agent make-agent make-bandit k
                         #:runs [runs 200]
                         #:steps [steps 1000]
                         #:seed [seed 1])
  (define results
    (for/list ([run (in-range runs)])
      (random-seed (+ seed run))
      (define b (make-bandit))
      (run-bandit-agent (make-agent k) b steps)))
  (summary (bandit-result-name (car results))
           (vector-mean-vectors (for/list ([r (in-list results)]) (bandit-result-rewards r)))
           (vector-mean-vectors (for/list ([r (in-list results)]) (bandit-result-optimal r)))
           (vector-mean-vectors (for/list ([r (in-list results)]) (bandit-result-cumulative r)))))

;; Cumulative regret after t pulls (t is 1-based, like the plots).
(define (regret-at s t)
  (vector-ref (summary-cumulative s) (sub1 t)))

;; Fraction of pulls that chose the best arm between steps from and to
;; (1-based and inclusive).
(define (optimal-rate s from to)
  (define xs (for/list ([i (in-range (sub1 from) to)]) (vector-ref (summary-optimal s) i)))
  (mean xs))

;; =====================================================================
;; The demo bandits
;; =====================================================================

;; The ten arm means used in Sutton and Barto's Figure 2.2; the best arm
;; pays 0.85 and the worst 0.10, so there is a real gap to find.
(define (stationary-bandit)
  (make-bandit '(0.10 0.25 0.40 0.55 0.70 0.35 0.20 0.85 0.60 0.45)))

;; Same arms, but after every pull each mean takes a small Gaussian step.
;; A sample-average learner keeps averaging in ancient history and falls
;; behind; a constant-alpha learner follows the drift.
(define (drifting-bandit)
  (make-bandit '(0.10 0.25 0.40 0.55 0.70 0.35 0.20 0.85 0.60 0.45)
               #:drift 0.02))

;; =====================================================================
;; Demo
;; =====================================================================

(module+ main
  (define runs 200)
  (define steps 1000)

  (printf "=== 3. Multi-armed bandits ===\n\n")
  (printf "10-armed Bernoulli bandit with means\n  ~a\n"
          (string-join (for/list ([m (in-list (bandit-arm-means (stationary-bandit)))])
                         (real->decimal-string m 2))
                       "  "))
  (printf "Best arm: ~a with mean ~a.  Pulls return 1 with probability mu_a.\n\n"
          (bandit-optimal-arm (stationary-bandit))
          (real->decimal-string (bandit-optimal-mean (stationary-bandit)) 2))
  (printf "~a runs of ~a pulls each; regret is averaged over runs.\n\n" runs steps)

  (define specs
    (list (list "greedy"                      make-greedy-agent)
          (list "epsilon-greedy 0.1"          (lambda (k) (make-epsilon-greedy-agent k #:epsilon 0.1)))
          (list "epsilon-greedy 0.01"         (lambda (k) (make-epsilon-greedy-agent k #:epsilon 0.01)))
          (list "optimistic init 5"           (lambda (k) (make-optimistic-agent k #:init 5.0)))
          (list "ucb1 c=1"                    (lambda (k) (make-ucb1-agent k #:c 1.0)))
          (list "thompson sampling"           make-thompson-agent)
          (list "softmax tau=0.1"             (lambda (k) (make-softmax-agent k #:tau 0.1)))))

  (define summaries (make-hash))
  (define rows
    (cons (list "strategy" "regret@100" "regret@500" "regret@1000" "% best arm")
          (for/list ([spec (in-list specs)])
            (define name (first spec))
            (define s (benchmark-agent (second spec) stationary-bandit 10
                                       #:runs runs #:steps steps))
            (hash-set! summaries name s)
            (list name
                  (real->decimal-string (regret-at s 100) 2)
                  (real->decimal-string (regret-at s 500) 2)
                  (real->decimal-string (regret-at s 1000) 2)
                  (real->decimal-string (optimal-rate s 1 1000) 3)))))
  (printf "~a\n\n" (format-table rows #:widths '(22 12 11 11 10)))

  (printf "Fraction of pulls on the best arm, per 100-pull block:\n")
  (printf "~a\n\n"
          (format-table
           (for/list ([spec (in-list specs)])
             (define name (first spec))
             (define s (hash-ref summaries name))
             (cons name
                   (for/list ([b (in-range 1 1001 100)])
                     (real->decimal-string (optimal-rate s b (+ b 99)) 2))))
           #:widths '(22 6 6 6 6 6 6 6 6 6 6)))

  ;; ---- non-stationary bandit --------------------------------------
  (printf "--- Non-stationary bandit (means drift after every pull) ---\n\n")
  (printf "Here the best arm changes over time, so total regret is a moving\n")
  (printf "target.  The table reports the average reward over the last 200 pulls,\n")
  (printf "where a learner that keeps up with the drift wins.\n\n")
  (define drift-specs
    (list (list "epsilon-greedy 0.1 (sample avg)"
                (lambda (k) (make-epsilon-greedy-agent k #:epsilon 0.1)))
          (list "epsilon-greedy 0.1 (alpha 0.1)"
                (lambda (k) (make-epsilon-greedy-agent k #:epsilon 0.1 #:alpha 0.1)))
          (list "optimistic init 5" (lambda (k) (make-optimistic-agent k #:init 5.0)))
          (list "ucb1 c=1" (lambda (k) (make-ucb1-agent k #:c 1.0)))
          (list "thompson sampling" make-thompson-agent)))
  (printf "~a\n\n"
          (format-table
           (cons (list "strategy" "reward last 200" "% best arm last 200")
                 (for/list ([spec (in-list drift-specs)])
                   (define s (benchmark-agent (second spec) drifting-bandit 10
                                              #:runs runs #:steps steps))
                   (list (first spec)
                         (real->decimal-string
                          (mean (for/list ([i (in-range 800 1000)])
                                  (vector-ref (summary-rewards s) i))) 4)
                         (real->decimal-string (optimal-rate s 801 1000) 3))))
           #:widths '(34 16 20)))

  (printf "Takeaways:\n")
  (printf "  * greedy locks onto the first arm that pays and its regret grows linearly;\n")
  (printf "  * optimistic initialization explores on its own but only early;\n")
  (printf "  * on the stationary bandit UCB1 and Thompson sampling keep total regret\n")
  (printf "    far below greedy, and Thompson has the lowest of all because its\n")
  (printf "    posterior knows how uncertain each arm is;\n")
  (printf "  * constant-alpha epsilon-greedy beats the sample average once the means\n")
  (printf "    move, because it can forget stale evidence.\n"))
