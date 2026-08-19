#lang racket

;; Genetic Programming and Symbolic Regression in Racket
;;
;; Demonstrates "Program as Data" - Racket S-expressions are both
;; code and data. Individuals in the population are executable program
;; trees represented as ordinary nested lists. Crossover and mutation
;; operate directly on that list structure.

(require racket/list)

;; ------------------------------------------------------------------
;; Configuration
;; ------------------------------------------------------------------

(define population-size 400)
(define max-generations 40)
(define max-initial-depth 4)
(define max-mutation-depth 3)
(define tournament-size 7)
(define crossover-rate 0.9)
(define mutation-rate 0.15)
(define elitism-count 2)

(define terminals '(x))
(define functions '(+ - * pdiv))
(define constants '(-3 -2 -1 0 1 2 3 5))

;; Training data: symbolic regression target is  x^2 + x + 1
;; Alternative targets to try:
;;   x^3 - 2*x + 1  => (lambda (x) (+ (- (* x (* x x)) (* 2 x)) 1))
;;   (x+1)^2        => (lambda (x) (* (+ x 1) (+ x 1)))

(define (target-function x)
  (+ (* x x) x 1))

(define training-data
  (for/list ([x (in-range -5 5.5 0.5)])
    (cons x (target-function x))))

;; ------------------------------------------------------------------
;; Random expression generation
;; ------------------------------------------------------------------

(define (random-terminal)
  (if (< (random) 0.5)
      'x
      (list-ref constants (random (length constants)))))

(define (random-function)
  (list-ref functions (random (length functions))))

;; Generate a random S-expression tree with bounded depth.
;; Depth 0 forces a terminal. Otherwise choose function or terminal.
(define (random-expr depth)
  (if (or (= depth 0) (< (random) 0.3))
      (random-terminal)
      (let ([op (random-function)])
        (list op (random-expr (- depth 1)) (random-expr (- depth 1))))))

;; ------------------------------------------------------------------
;; Evaluation - interpret an S-expression as arithmetic with variable x
;; ------------------------------------------------------------------

(define (protected-div a b)
  (if (< (abs b) 0.001) 1 (/ a b)))

(define (eval-expr expr x)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (if (eq? expr 'x) x 0)]
    [(list? expr)
     (match expr
       [(list '+ a b) (+ (eval-expr a x) (eval-expr b x))]
       [(list '- a b) (- (eval-expr a x) (eval-expr b x))]
       [(list '* a b) (* (eval-expr a x) (eval-expr b x))]
       [(list 'pdiv a b) (protected-div (eval-expr a x) (eval-expr b x))]
       [(list '/ a b) (protected-div (eval-expr a x) (eval-expr b x))]
       [_ 0])]
    [else 0]))

;; Safe wrapper that returns a large penalty on error.
(define (safe-eval expr x)
  (with-handlers ([exn:fail? (lambda (_) 1e6)])
    (let ([v (eval-expr expr x)])
      (cond
        [(number? v) (if (or (nan? v) (infinite? v)) 1e6 v)]
        [else 1e6]))))

;; ------------------------------------------------------------------
;; Fitness - mean squared error over training data
;; ------------------------------------------------------------------

(define (fitness expr)
  (define mse
    (/ (for/sum ([pair training-data])
         (let* ([x (car pair)]
                [y (cdr pair)]
                [pred (safe-eval expr x)]
                [err (- pred y)])
           (* err err)))
       (length training-data)))
  ;; penalize bloated trees slightly to favor parsimony
  (define size-penalty (* 0.001 (tree-size expr)))
  (+ mse size-penalty))

(define (tree-size expr)
  (cond
    [(list? expr) (+ 1 (tree-size (second expr)) (tree-size (third expr)))]
    [else 1]))

;; ------------------------------------------------------------------
;; Tree utilities - collect nodes with paths for crossover/mutation
;; Paths are lists of 1/2 steps: 1 = left child, 2 = right child.
;; The root has path '().
;; ------------------------------------------------------------------

(define (collect-nodes expr path)
  (cons (cons path expr)
        (if (list? expr)
            (append (collect-nodes (second expr) (append path '(1)))
                    (collect-nodes (third expr) (append path '(2))))
            '())))

(define (get-at-path expr path)
  (if (null? path)
      expr
      (let ([step (first path)]
            [rest (rest path)])
        (cond
          [(= step 1) (get-at-path (second expr) rest)]
          [(= step 2) (get-at-path (third expr) rest)]
          [else expr]))))

(define (replace-at-path expr path new-subtree)
  (if (null? path)
      new-subtree
      (let ([step (first path)]
            [rest (rest path)])
        (match expr
          [(list op a b)
           (cond
             [(= step 1) (list op (replace-at-path a rest new-subtree) b)]
             [(= step 2) (list op a (replace-at-path b rest new-subtree))]
             [else expr])]
          [_ new-subtree]))))

;; ------------------------------------------------------------------
;; Genetic operators operating directly on S-expressions
;; ------------------------------------------------------------------

(define (crossover parent1 parent2)
  (let* ([nodes1 (collect-nodes parent1 '())]
         [nodes2 (collect-nodes parent2 '())]
         [pick1 (list-ref nodes1 (random (length nodes1)))]
         [pick2 (list-ref nodes2 (random (length nodes2)))]
         [path1 (car pick1)]
         [subtree2 (cdr pick2)])
    (replace-at-path parent1 path1 subtree2)))

(define (mutate expr)
  (let* ([nodes (collect-nodes expr '())]
         [pick (list-ref nodes (random (length nodes)))]
         [path (car pick)]
         [new-subtree (random-expr max-mutation-depth)])
    (replace-at-path expr path new-subtree)))

;; ------------------------------------------------------------------
;; Selection
;; ------------------------------------------------------------------

(define (tournament-select population scored)
  ;; scored is list of (expr . fitness), lower is better
  (define contenders
    (for/list ([_ (in-range tournament-size)])
      (list-ref scored (random (length scored)))))
  (car (argmin cdr contenders)))

;; ------------------------------------------------------------------
;; Population helpers
;; ------------------------------------------------------------------

(define (random-population n depth)
  (for/list ([_ (in-range n)]) (random-expr depth)))

(define (score-population pop)
  (for/list ([expr pop]) (cons expr (fitness expr))))

(define (best-of scored)
  (argmin cdr scored))

;; ------------------------------------------------------------------
;; Evolution loop
;; ------------------------------------------------------------------

(define (next-generation scored-pop)
  (define sorted (sort scored-pop < #:key cdr))
  (define elites (map car (take sorted elitism-count)))
  (define (breed-one)
    (cond
      [(< (random) crossover-rate)
       (let ([p1 (tournament-select (map car scored-pop) scored-pop)]
             [p2 (tournament-select (map car scored-pop) scored-pop)])
         (let ([child (crossover p1 p2)])
           (if (< (random) mutation-rate) (mutate child) child)))]
      [else
       (mutate (tournament-select (map car scored-pop) scored-pop))]))
  (define needed (- population-size elitism-count))
  (append elites (for/list ([_ (in-range needed)]) (breed-one))))

(define (run-gp)
  (printf "=== Genetic Programming: Symbolic Regression ===~%")
  (printf "Target function: (+ (* x x) x 1)  i.e. x^2 + x + 1~%")
  (printf "Training points: ~a values from x=-5 to 5 step 0.5~%" (length training-data))
  (printf "Population: ~a  Generations: ~a  Tournament: ~a~%~%" population-size max-generations tournament-size)

  ;; Demo that programs are data
  (printf "--- Program as Data demo ---~%")
  (define demo-a '(+ (* x x) 1))
  (define demo-b '(* x (+ x 2)))
  (printf "Program A (data): ~a  eval at x=3 => ~a~%" demo-a (safe-eval demo-a 3))
  (printf "Program B (data): ~a  eval at x=3 => ~a~%" demo-b (safe-eval demo-b 3))
  (printf "Crossover A and B => ~a~%" (crossover demo-a demo-b))
  (printf "Mutate A         => ~a~%" (mutate demo-a))
  (printf "~%")

  (let loop ([gen 0]
             [pop (random-population population-size max-initial-depth)])
    (define scored (score-population pop))
    (define best (best-of scored))
    (define best-expr (car best))
    (define best-fit (cdr best))
    (printf "Gen ~a  best fitness ~a  size ~a  expr: ~a~%"
            (~a gen #:width 2) (~r best-fit #:precision 4) (tree-size best-expr) best-expr)
    ;; show predictions for a few points every 10 generations and at end
    (when (or (= (modulo gen 10) 0) (= gen max-generations))
      (printf "  sample predictions: ")
      (for ([x '(-2 0 1 3)])
        (printf "x=~a -> ~a (target ~a)  " x (~r (safe-eval best-expr x) #:precision 2) (target-function x)))
      (printf "~%"))
    (cond
      [(>= gen max-generations)
       (printf "~%=== Result ===~%")
       (printf "Best expression: ~a~%" best-expr)
       (printf "Best fitness (MSE + parsimony): ~a~%" (~r best-fit #:precision 6))
       (printf "Predictions vs target:~%")
       (for ([pair training-data])
         (let ([x (car pair)] [y (cdr pair)])
           (printf "  x=~a  predicted=~a  target=~a~%"
                   (~r x #:precision 1) (~r (safe-eval best-expr x) #:precision 3) (~r y #:precision 1))))
       (when (< best-fit 0.5)
         (printf "~%Success: evolved expression fits the data closely.~%"))
       best-expr]
      [else
       (loop (+ gen 1) (next-generation scored))])))

;; ------------------------------------------------------------------
;; Entry point
;; ------------------------------------------------------------------

(module+ main
  (run-gp))

(provide eval-expr
         safe-eval
         fitness
         random-expr
         crossover
         mutate
         tree-size
         collect-nodes
         get-at-path
         replace-at-path
         target-function
         training-data
         run-gp)
