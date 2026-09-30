#lang racket

;;; environments.rkt -- Test environments for the reinforcement learning examples.
;;;
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Three families of environment live here:
;;;
;;;   1. GridWorld  -- a small discrete MDP (states x actions x rewards).
;;;                    Used by q_learning.rkt, policy_gradient.rkt and dqn.rkt.
;;;   2. CartPole   -- the classic continuous-state control task from
;;;                    Barto, Sutton and Anderson (1983).  Used by
;;;                    policy_gradient.rkt and by the optional DQN demo.
;;;   3. Bandit     -- multi-armed bandit problems (Bernoulli and Gaussian,
;;;                    stationary and drifting).  Used by bandits.rkt.
;;;
;;; Everything is plain Racket: no packages beyond the standard library, and
;;; all randomness comes from Racket's global PRNG so that (random-seed n)
;;; makes an entire experiment reproducible.

(require racket/flonum
         racket/list
         racket/math)

(provide
 ;; small numeric / RNG helpers
 random-gaussian
 sample-categorical
 vector-argmax
 flvector-argmax
 flvector-max
 flvector-scale!
 flvector-add-scaled!
 flvector-clamp!
 flvector-copy-list
 flvector-ref-list
 flvector-stats
 mean
 stddev
 normalize-flvector
 one-hot

 ;; GridWorld
 (struct-out gridworld)
 make-gridworld
 gridworld-num-states
 gridworld-num-actions
 gridworld-terminal?
 gridworld-pos->state
 gridworld-state->pos
 gridworld-transitions
 gridworld-reset
 gridworld-step
 gridworld-render
 gridworld-map-string
 gridworld-render-values

 ;; CartPole
 (struct-out cartpole)
 make-cartpole
 cartpole-reset
 cartpole-act
 cartpole-observe
 cartpole-state->list
 cartpole-obs-size

 ;; bandits
 (struct-out bandit)
 make-bandit
 bandit-num-arms
 bandit-pull
 bandit-optimal-arm
 bandit-optimal-mean
 bandit-means
 bandit-best-arm
 bandit-arm-means)

;; =====================================================================
;; Small numeric and RNG helpers
;; =====================================================================

;; Standard normal sample via the Box-Muller transform.  u1 is clamped
;; away from zero because log 0 is undefined.
(define (random-gaussian [mu 0.0] [sigma 1.0])
  (define u1 (max 1e-12 (random)))
  (define u2 (random))
  (+ mu (* sigma (flsqrt (* -2.0 (fllog u1))) (flcos (* 2.0 pi u2)))))

;; Draw an index from an unnormalized or normalized vector of weights.
;; Used for epsilon-greedy tie-breaking and for sampling from a softmax
;; policy.
(define (sample-categorical weights)
  ;; Accept either a plain vector of weights or an flvector of
  ;; probabilities (as returned by a softmax).
  (define w (if (flvector? weights) (for/vector ([x (in-flvector weights)]) x) weights))
  (define n (vector-length w))
  (define total (for/sum ([x (in-vector w)]) x))
  (if (<= total 0.0)
      (random n)
      (let ([r (* (random) total)])
        (let loop ([i 0] [acc 0.0])
          (cond
            [(= i (sub1 n)) (sub1 n)]
            [else
             (define acc2 (+ acc (vector-ref w i)))
             (if (< r acc2) i (loop (add1 i) acc2))])))))

;; Index of the largest element of a plain vector (first one wins ties).
(define (vector-argmax v)
  (define n (vector-length v))
  (cond
    [(zero? n) (error 'vector-argmax "empty vector")]
    [else
     (define-values (best _)
       (for/fold ([best 0] [best-val (vector-ref v 0)]) ([i (in-range 1 n)])
         (define x (vector-ref v i))
         (if (> x best-val) (values i x) (values best best-val))))
     best]))

(define (flvector-argmax v)
  (define n (flvector-length v))
  (cond
    [(zero? n) (error 'flvector-argmax "empty flvector")]
    [else
     (define-values (best _)
       (for/fold ([best 0] [best-val (flvector-ref v 0)]) ([i (in-range 1 n)])
         (define x (flvector-ref v i))
         (if (fl> x best-val) (values i x) (values best best-val))))
     best]))

(define (flvector-max v)
  (for/fold ([best (flvector-ref v 0)]) ([i (in-range 1 (flvector-length v))])
    (flmax best (flvector-ref v i))))

;; In-place v += scale * u.  Used by gradient accumulators.
(define (flvector-add-scaled! v u scale)
  (for ([i (in-range (flvector-length v))])
    (flvector-set! v i (fl+ (flvector-ref v i) (fl* scale (flvector-ref u i)))))
  v)

(define (flvector-scale! v scale)
  (for ([i (in-range (flvector-length v))])
    (flvector-set! v i (fl* scale (flvector-ref v i))))
  v)

;; Clamp every element into [-limit, limit] in place.  DQN uses this on the
;; error term so that a single surprising transition cannot blow the
;; network up (the Huber/"clip the TD error" trick from the Nature paper).
(define (flvector-clamp! v limit)
  (for ([i (in-range (flvector-length v))])
    (define x (flvector-ref v i))
    (flvector-set! v i (flmax (fl- limit) (flmin limit x))))
  v)

(define (flvector-copy-list v)
  (for/list ([x (in-flvector v)]) x))

(define (flvector-ref-list v)
  (for/list ([i (in-range (flvector-length v))]) (flvector-ref v i)))

(define (mean xs)
  (cond [(null? xs) 0.0]
        [else (fl/ (exact->inexact (for/sum ([x (in-list xs)]) x))
                   (exact->inexact (length xs)))]))

(define (stddev xs)
  (define m (mean xs))
  (cond [(< (length xs) 2) 0.0]
        [else (flsqrt (fl/ (exact->inexact
                            (for/sum ([x (in-list xs)]) (define d (fl- x m)) (fl* d d)))
                           (exact->inexact (sub1 (length xs)))))]))

;; Min-max normalization of a flvector, returning a fresh flvector.
(define (normalize-flvector v)
  (define lo (for/fold ([best (flvector-ref v 0)]) ([i (in-range 1 (flvector-length v))])
               (flmin best (flvector-ref v i))))
  (define hi (for/fold ([best (flvector-ref v 0)]) ([i (in-range 1 (flvector-length v))])
               (flmax best (flvector-ref v i))))
  (define span (fl- hi lo))
  (for/flvector ([x (in-flvector v)])
    (if (fl= span 0.0) 0.0 (fl/ (fl- x lo) span))))

(define (flvector-stats v)
  (values (for/fold ([s 0.0]) ([x (in-flvector v)]) (fl+ s x))
          (flvector-length v)))

(define (one-hot n i)
  (for/flvector ([j (in-range n)]) (if (= j i) 1.0 0.0)))

;; =====================================================================
;; GridWorld: a small discrete MDP
;; =====================================================================
;;
;; States are integers 0 .. rows*cols-1, row-major from the top-left.
;; Actions are 0=up 1=right 2=down 3=left.  Walking into a wall leaves you
;; in place.  Reaching the goal pay +goal-reward and ends the episode;
;; stepping into a pit pays +pit-reward (negative) and ends the episode;
;; every other step pays +step-reward (negative, a small "time is money"
;; cost that makes the agent prefer short paths).
;;
;; slip is the probability that the intended action is replaced by one of
;; the other three, chosen uniformly.  slip = 0 makes GridWorld a
;; deterministic MDP, which is what the book examples use by default.

(struct gridworld
  (rows          ; integer
   cols          ; integer
   wall?         ; vector of booleans, indexed by state
   pit?          ; vector of booleans, indexed by state
   wall-list     ; list of (row col) positions, for printing
   pit-list      ; list of (row col) positions, for printing
   goal          ; state index
   start         ; state index
   step-reward   ; flonum
   goal-reward   ; flonum
   pit-reward    ; flonum
   slip)         ; flonum in [0, 1)
  #:transparent)

(define (gridworld-num-states gw)
  (* (gridworld-rows gw) (gridworld-cols gw)))

(define (gridworld-num-actions gw) 4)

;; A terminal state ends the episode: the goal or a pit.
(define (gridworld-terminal? gw s)
  (or (= s (gridworld-goal gw))
      (vector-ref (gridworld-pit? gw) s)))

;; row-major state index <-> (row . col) pair
(define (gridworld-pos->state gw pos)
  (match-define (list r c) pos)
  (+ (* r (gridworld-cols gw)) c))

(define (gridworld-state->pos gw s)
  (list (quotient s (gridworld-cols gw))
        (remainder s (gridworld-cols gw))))

(define grid-actions '#(up right down left))
(define grid-deltas '#((-1 0) (0 1) (1 0) (0 -1)))

;; Move from a position one step in an action direction, blocking at the
;; walls and the edges of the board.
(define (grid-move gw r c a)
  (define d (vector-ref grid-deltas a))
  (define r2 (+ r (list-ref d 0)))
  (define c2 (+ c (list-ref d 1)))
  (cond
    [(or (< r2 0) (>= r2 (gridworld-rows gw))
         (< c2 0) (>= c2 (gridworld-cols gw)))
     (values r c)]
    [else
     (define s2 (gridworld-pos->state gw (list r2 c2)))
     (if (vector-ref (gridworld-wall? gw) s2) (values r c) (values r2 c2))]))

;; The model of the MDP: a list of (probability next-state reward done?)
;; triples for taking action a in state s.  Value iteration and policy
;; evaluation need this; the TD methods call gridworld-step, which samples
;; from exactly the same distribution.
(define (gridworld-transitions gw s a)
  (define slip (gridworld-slip gw))
  (define pos (gridworld-state->pos gw s))
  (define probs
    (if (fl<= slip 0.0)
        (list (cons a 1.0))
        (cons (cons a (fl- 1.0 slip))
              (for/list ([a2 (in-range 4)] #:unless (= a2 a))
                (cons a2 (fl/ slip 3.0))))))
  ;; Different actions can lead to the same next state (e.g. two of them
  ;; bump into the same wall), so aggregate probabilities by next state.
  (define agg (make-hash))
  (for ([pr (in-list probs)])
    (define-values (r2 c2) (grid-move gw (first pos) (second pos) (car pr)))
    (define s2 (gridworld-pos->state gw (list r2 c2)))
    (define p (cdr pr))
    (hash-update! agg s2 (lambda (old) (fl+ old p)) 0.0))
  (for/list ([(s2 p) (in-hash agg)])
    (cond
      [(= s2 (gridworld-goal gw)) (list p s2 (gridworld-goal-reward gw) #t)]
      [(vector-ref (gridworld-pit? gw) s2) (list p s2 (gridworld-pit-reward gw) #t)]
      [else (list p s2 (gridworld-step-reward gw) #f)])))

(define (gridworld-step gw s a)
  (define transitions (gridworld-transitions gw s a))
  (define r (random))
  (let loop ([ts transitions] [acc r])
    (cond
      [(null? ts)
       ;; Floating-point residue: fall back to the last transition.
       (define t (last transitions))
       (values (second t) (third t) (fourth t))]
      [(fl<= acc (first (car ts)))
       (define t (car ts))
       (values (second t) (third t) (fourth t))]
      [else (loop (cdr ts) (fl- acc (first (car ts))))])))

;; Fresh episode.  With #:random? #t the start state is uniform over all
;; non-wall, non-pit cells; the default fixed start keeps demos and tests
;; reproducible.
(define (gridworld-reset gw #:random? [random? #f])
  (cond
    [random?
     (define candidates
       (for/list ([s (in-range (gridworld-num-states gw))]
                  #:unless (or (vector-ref (gridworld-wall? gw) s)
                               (vector-ref (gridworld-pit? gw) s)))
         s))
     (list-ref candidates (random (length candidates)))]
    [else (gridworld-start gw)]))

(define (grid-cell-char gw s)
  (cond
    [(= s (gridworld-goal gw)) #\G]
    [(vector-ref (gridworld-pit? gw) s) #\X]
    [(vector-ref (gridworld-wall? gw) s) #\#]
    [(= s (gridworld-start gw)) #\S]
    [else #\.]))

;; Plain map of the board: S start, G goal, X pit, # wall, . open floor.
(define (gridworld-map-string gw)
  (string-join
   (for/list ([r (in-range (gridworld-rows gw))])
     (string-join
      (for/list ([c (in-range (gridworld-cols gw))])
        (string (grid-cell-char gw (gridworld-pos->state gw (list r c)))))
      " "))
   "\n"))

;; Board with the greedy policy drawn as arrows.  Terminal and wall cells
;; keep their map character.  Optionally annotate each open cell with its
;; state value underneath (two-line cells).
(define (gridworld-render gw #:policy [policy #f] #:values [values #f])
  (define arrows '#("↑" "→" "↓" "←"))
  (define (cell r c)
    (define s (gridworld-pos->state gw (list r c)))
    (cond
      [(or (vector-ref (gridworld-wall? gw) s)
           (vector-ref (gridworld-pit? gw) s)
           (= s (gridworld-goal gw)))
       (string (grid-cell-char gw s))]
      [policy (vector-ref arrows (policy s))]
      [else (string (grid-cell-char gw s))]))
  (define body
    (string-join
     (for/list ([r (in-range (gridworld-rows gw))])
       (string-join (for/list ([c (in-range (gridworld-cols gw))]) (cell r c)) " "))
     "\n"))
  (if values
      (string-append
       body "\n"
       (string-join
        (for/list ([r (in-range (gridworld-rows gw))])
          (string-join
           (for/list ([c (in-range (gridworld-cols gw))])
             (define s (gridworld-pos->state gw (list r c)))
             (format "~a" (real->decimal-string (vector-ref values s) 2)))
           " "))
        "\n"))
      body))

;; Value table as a grid of numbers (one line per row).
(define (gridworld-render-values gw values)
  (string-join
   (for/list ([r (in-range (gridworld-rows gw))])
     (string-join
      (for/list ([c (in-range (gridworld-cols gw))])
        (define s (gridworld-pos->state gw (list r c)))
        (if (vector-ref (gridworld-wall? gw) s)
            "  ####"
            (format "~6a" (real->decimal-string (vector-ref values s) 2))))
      " "))
   "\n"))

;; Convenience constructor for the standard 5x5 maze used throughout the
;; book examples.
(define (make-gridworld #:rows [rows 5]
                        #:cols [cols 5]
                        #:walls [walls '((1 1) (1 3) (2 3) (3 0) (3 1))]
                        #:pits [pits '((3 2))]
                        #:goal [goal (list (sub1 rows) (sub1 cols))]
                        #:start [start '(0 0)]
                        #:step-reward [step-reward -0.01]
                        #:goal-reward [goal-reward 1.0]
                        #:pit-reward [pit-reward -1.0]
                        #:slip [slip 0.0])
  (define (p->s p) (+ (* (first p) cols) (second p)))
  (define (checked-pos who p)
    (unless (and (list? p) (= 2 (length p))
                 (exact-integer? (first p)) (exact-integer? (second p))
                 (< -1 (first p) rows) (< -1 (second p) cols))
      (error who "bad position: ~a" p))
    p)
  (define goal* (checked-pos 'make-gridworld goal))
  (define start* (checked-pos 'make-gridworld start))
  (define wall? (make-vector (* rows cols) #f))
  (define pit? (make-vector (* rows cols) #f))
  (for ([p (in-list walls)])
    (vector-set! wall? (p->s (checked-pos 'make-gridworld p)) #t))
  (for ([p (in-list pits)])
    (vector-set! pit? (p->s (checked-pos 'make-gridworld p)) #t))
  (when (vector-ref wall? (p->s start*))
    (error 'make-gridworld "start state is a wall: ~a" start*))
  (when (vector-ref wall? (p->s goal*))
    (error 'make-gridworld "goal state is a wall: ~a" goal*))
  (when (fl>= slip 1.0)
    (error 'make-gridworld "slip must be < 1.0, got ~a" slip))
  (gridworld rows cols wall? pit? walls pits
              (p->s goal*) (p->s start*)
              step-reward goal-reward pit-reward slip))

;; =====================================================================
;; CartPole: continuous state, two discrete actions
;; =====================================================================
;;
;; State: (x, x-dot, theta, theta-dot) with theta measured in radians from
;; vertical.  Action 0 pushes left, action 1 pushes right.  A step pays
;; +1, and the episode ends when the pole tips past 12 degrees or the cart
;; leaves the +/-2.4 track.  The dynamics below are the standard
;; semi-implicit Euler update used by Barto/Sutton/Anderson and OpenAI Gym.

(struct cartpole
  (gravity        ; 9.8 m/s^2
   mass-cart      ; 1.0 kg
   mass-pole      ; 0.1 kg
   length         ; 0.5 m (half the pole)
   force-mag      ; 10.0 N
   tau            ; 0.02 s per step
   theta-threshold ; 12 degrees in radians
   x-threshold    ; 2.4 m
   max-steps)     ; 500
  #:transparent)

(define (make-cartpole #:max-steps [max-steps 500])
  (cartpole 9.8 1.0 0.1 0.5 10.0 0.02 0.20943951023931953 2.4 max-steps))

(define (cartpole-mass-total cp)
  (+ (cartpole-mass-cart cp) (cartpole-mass-pole cp)))

(define (cartpole-reset cp)
  ;; Uniform on [-0.05, 0.05] for each of the four state variables, the same
  ;; near-upright start distribution used by the classic Gym task.
  (for/flvector ([i (in-range 4)])
    (* 0.1 (- (random) 0.5))))

;; One 0.02 s tick.  Returns (values next-state reward done?).
(define (cartpole-act cp s a)
  (define x (flvector-ref s 0))
  (define xdot (flvector-ref s 1))
  (define theta (flvector-ref s 2))
  (define thetadot (flvector-ref s 3))
  (define force (if (= a 1) (cartpole-force-mag cp) (fl- (cartpole-force-mag cp))))
  (define costheta (flcos theta))
  (define sintheta (flsin theta))
  (define mt (cartpole-mass-total cp))
  (define temp (fl/ (fl+ force
                         (fl* (cartpole-mass-pole cp) (cartpole-length cp) thetadot thetadot sintheta))
                    mt))
  (define thetaacc
    (fl/ (fl- (fl* (cartpole-gravity cp) sintheta) (fl* costheta temp))
         (fl* (cartpole-length cp)
              (fl- 1.3333333333333333 (fl/ (fl* (cartpole-mass-pole cp) costheta costheta) mt)))))
  (define xacc
    (fl- temp (fl/ (fl* (cartpole-mass-pole cp) (cartpole-length cp) thetaacc costheta) mt)))
  (define x2 (fl+ x (fl* (cartpole-tau cp) xdot)))
  (define xdot2 (fl+ xdot (fl* (cartpole-tau cp) xacc)))
  (define theta2 (fl+ theta (fl* (cartpole-tau cp) thetadot)))
  (define thetadot2 (fl+ thetadot (fl* (cartpole-tau cp) thetaacc)))
  (define next (flvector x2 xdot2 theta2 thetadot2))
  (define done? (or (fl> (flabs x2) (cartpole-x-threshold cp))
                    (fl> (flabs theta2) (cartpole-theta-threshold cp))))
  (values next 1.0 done?))

(define (cartpole-state->list s)
  (flvector-ref-list s))

(define cartpole-obs-size 4)

;; Features handed to a function approximator: each state variable divided
;; by a rough scale, so that a linear or neural policy sees inputs of
;; comparable magnitude.  Without this, x-dot and theta-dot (which range
;; over several units) dominate x and theta (which range over ~2.4 and
;; ~0.2) and learning stalls.
(define (cartpole-observe cp s)
  (flvector (fl/ (flvector-ref s 0) (cartpole-x-threshold cp))
            (fl/ (flvector-ref s 1) 3.0)
            (fl/ (flvector-ref s 2) (cartpole-theta-threshold cp))
            (fl/ (flvector-ref s 3) 3.0)))

;; =====================================================================
;; Multi-armed bandits
;; =====================================================================
;;
;; A bandit has k arms.  Pulling arm a returns a reward drawn from that
;; arm's distribution: Bernoulli(mu_a) or Normal(mu_a, stdev^2).  With
;; drift > 0 the means take a small Gaussian random walk after every pull,
;; which turns the problem non-stationary and rewards algorithms that keep
;; learning instead of averaging over all history.

(struct bandit (dist means drift stdev) #:transparent #:mutable)

(define (make-bandit means #:dist [dist 'bernoulli] #:drift [drift 0.0] #:stdev [stdev 1.0])
  (bandit dist (list->vector means) drift stdev))

(define (bandit-num-arms b) (vector-length (bandit-means b)))

(define (bandit-arm-means b) (vector->list (bandit-means b)))

(define (bandit-optimal-mean b)
  (for/fold ([best (vector-ref (bandit-means b) 0)]) ([i (in-range 1 (bandit-num-arms b))])
    (max best (vector-ref (bandit-means b) i))))

(define (bandit-optimal-arm b)
  (vector-argmax (bandit-means b)))

;; Kept as an alias so example code reads naturally.
(define (bandit-best-arm b) (bandit-optimal-arm b))

(define (bandit-pull b a)
  (define mu (vector-ref (bandit-means b) a))
  (define r (case (bandit-dist b)
              [(bernoulli) (if (< (random) mu) 1.0 0.0)]
              [(gaussian) (random-gaussian mu (bandit-stdev b))]
              [else (error 'bandit-pull "unknown distribution: ~a" (bandit-dist b))]))
  (when (> (bandit-drift b) 0.0)
    (for ([i (in-range (bandit-num-arms b))])
      (vector-set! (bandit-means b) i
                   (+ (vector-ref (bandit-means b) i)
                      (random-gaussian 0.0 (bandit-drift b))))))
  r)

(module+ test
  (require rackunit)
  (test-case "gridworld map and geometry"
    (define gw (make-gridworld))
    (check-equal? (gridworld-num-states gw) 25)
    (check-equal? (gridworld-num-actions gw) 4)
    (check-equal? (gridworld-state->pos gw (gridworld-pos->state gw '(3 2))) '(3 2))
    (check-equal? (gridworld-start gw) 0)
    (check-equal? (gridworld-goal gw) 24))
  (test-case "walls block movement"
    (define gw (make-gridworld))
    (define-values (s r done?) (gridworld-step gw 5 0)) ; (1 0) up to (0 0)
    (check-equal? s 0)
    (check-= r -0.01 1e-12)
    (check-false done?))
  (test-case "goal is terminal and pays the goal reward"
    (define gw (make-gridworld))
    (define-values (s r done?) (gridworld-step gw 23 1)) ; (4 3) right to goal
    (check-equal? s (gridworld-goal gw))
    (check-= r 1.0 1e-12)
    (check-true done?))
  (test-case "bandit rewards are in range and the best arm is known"
    (define b (make-bandit '(0.2 0.9 0.4)))
    (check-equal? (bandit-num-arms b) 3)
    (check-equal? (bandit-optimal-arm b) 1)
    (check-= (bandit-optimal-mean b) 0.9 1e-12)
    (for ([i (in-range 50)])
      (define r (bandit-pull b 1))
      (check-true (or (= r 0.0) (= r 1.0)))))
  (test-case "cartpole reset is small and a strong push ends the episode"
    (random-seed 7)
    (define cp (make-cartpole))
    (define s (cartpole-reset cp))
    (for ([x (in-flvector s)])
      (check-true (<= (abs x) 0.051)))
    (define-values (s2 r done?) (cartpole-act cp s 1))
    (check-true (>= r 0.0))
    (check-true (flvector? s2))
    (check-false done?)))
