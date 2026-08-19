# Program as Data, Data as Program: Genetic Programming and Symbolic Regression

Genetic programming treats a program as an organism. It breeds a population of programs, scores each one on how well it solves a task, and lets the fittest reproduce with random change. Over many generations the population discovers a program that solves the task without a human writing it. This is distinct from tuning numbers in a fixed model. In genetic programming the structure of the program itself evolves.

The classic demonstration is symbolic regression. You hand the system a set of points `(x, y)`$ and ask it to find a formula `y = f(x)`$ that fits the points. A neural network can predict `y`$ for a new `x`$, but it hides its knowledge in weights. Symbolic regression returns the formula itself. You get knowledge you can read, check, and reuse.

Racket makes this direct. A Racket program is an S-expression, a nested list such as `(+ (* x x) 1)`$. That list is both code you can run and data you can copy, cut, and splice. Generation, crossover, and mutation become list operations. No separate parse tree is needed. The language and the representation align.

A genetic programming run needs four pieces:

* A way to write a program as a tree.
* A way to run that tree and score it.
* A way to select parents.
* A way to breed children through crossover and mutation.

This chapter builds each piece in pure Racket and uses it to rediscover a short polynomial from data.

## The Data We Will Learn From

We choose a target function the system does not know:

```$
y = x^2 + x + 1
```

We sample it at 21 points from `x = -5`$ to `x = 5`$ in steps of `0.5`$. Each training item is a pair `(x . y)`$:

```racket
'((-5 . 21) (-4.5 . 16.75) (-4 . 13) (-3.5 . 9.75) (-3 . 7) (-2.5 . 4.75)
  (-2 . 3) (-1.5 . 1.75) (-1 . 1) (-0.5 . 0.75) (0 . 1) (0.5 . 1.75)
  (1 . 3) (1.5 . 4.75) (2 . 7) (2.5 . 9.75) (3 . 13) (3.5 . 16.75)
  (4 . 21) (4.5 . 25.75) (5 . 31))
```

In the program this appears as a list of pairs built from a function:

```racket
(define (target-function x)
  (+ (* x x) x 1))

(define training-data
  (for/list ([x (in-range -5 5.5 0.5)])
    (cons x (target-function x))))
```

Every other part of the system reads `training-data`$ through this single definition. Change `target-function`$ to `x^3 - 2x + 1`$ or `(x + 1)^2`$ and the same evolution code learns a new formula with no other edits.

## Representing Programs as S-Expressions

An individual is any Racket expression built from a small vocabulary:

* Terminals: the variable `x`$ and a set of constants `(-3 -2 -1 0 1 2 3 5)`$.
* Functions: binary arithmetic `+`$, `-`$, `*`$, and protected division `pdiv`$.

Examples of valid individuals:

```racket
'x
'(* x x)
'(+ (* x x) 1)
'(+ x (- (* x x) -1))
'(* (+ x 1) (+ x 1))
```

Each is a tree. `(+ x (- (* x x) -1))`$ has root `+`$, left child `x`$, and right child `(- (* x x) -1)`$. Its size `|T| = 7`$ nodes and its depth is 3. The whole population is just a list of such lists. Because code is data, `crossover` can take two parents, pick a random subtree from each, and splice one into the other with `cons`, `first`, `second`, `third`, and `append`.

## Building the System

The complete program lives in one file, `genetic_programming.rkt`$. It needs no outside package beyond the Racket distribution. The sections below walk through it in order. Each snippet appears as it does in the file, with a note on what it does and why it is written that way.

### 1. Configuration

```racket
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
```

Population size 400 is large enough to hold variety and small enough to run in seconds. Depth 4 keeps initial trees short. `crossover-rate`$ at 0.9 and `mutation-rate`$ at 0.15 give most children a mix of both operators. Elitism of 2 copies the two best trees unchanged so a good find is never lost.

This block also fixes the language that evolution can use. A tiny language is a strong bias. With only `+`$, `-`$, `*`$, and `pdiv`$, the system can build any polynomial and many rational forms, but it cannot cheat by calling a hidden `expt` that already encodes the answer.

### 2. Random Expression Generation

```racket
(define (random-terminal)
  (if (< (random) 0.5)
      'x
      (list-ref constants (random (length constants)))))

(define (random-function)
  (list-ref functions (random (length functions))))

(define (random-expr depth)
  (if (or (= depth 0) (< (random) 0.3))
      (random-terminal)
      (let ([op (random-function)])
        (list op (random-expr (- depth 1)) (random-expr (- depth 1))))))
```

`random-expr`$ builds a tree top down. At depth 0 it must stop and emit a terminal. Otherwise it stops early with chance 0.3, which yields a mix of shallow and deep trees rather than a set of perfect full trees. This mix improves initial coverage of the search space.

### 3. Program Evaluation

```racket
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

(define (safe-eval expr x)
  (with-handlers ([exn:fail? (lambda (_) 1e6)])
    (let ([v (eval-expr expr x)])
      (cond
        [(number? v) (if (or (nan? v) (infinite? v)) 1e6 v)]
        [else 1e6]))))
```

`eval-expr`$ is a tiny interpreter. It walks the list structure and applies the arithmetic that the list names. It needs no `eval` or `namespace` because the S-expression is data we choose to interpret. `protected-div`$ returns 1 when the divisor is near zero, so division never traps and never creates an infinite value that would poison fitness. `safe-eval`$ wraps the walk in `with-handlers` and maps `+nan.0`$ or `+inf.0`$ to a large penalty. Evolution can then grade every tree, even a broken one, without a crash.

### 4. Fitness

Fitness is mean squared error with a small parsimony term.

```$
\mathrm{MSE}(T) = \frac{1}{n}\sum_{i=1}^{n}\left( \hat{y}_i - y_i \right)^2
```

where `n = 21`$, `y_i`$ is the target value at `x_i`$, and `\hat{y}_i = \mathrm{eval}(T, x_i)`$ is the value the tree `T`$ predicts.

```$
F(T) = \mathrm{MSE}(T) + 0.001 \cdot |T|
```

The term `0.001 \cdot |T|`$ adds a cost of one thousandth per node. It does not change which tree fits best, but it breaks ties in favor of the shorter tree and curbs bloat where trees grow without improving error.

```racket
(define (fitness expr)
  (define mse
    (/ (for/sum ([pair training-data])
         (let* ([x (car pair)]
                [y (cdr pair)]
                [pred (safe-eval expr x)]
                [err (- pred y)])
           (* err err)))
       (length training-data)))
  (define size-penalty (* 0.001 (tree-size expr)))
  (+ mse size-penalty))

(define (tree-size expr)
  (cond
    [(list? expr) (+ 1 (tree-size (second expr)) (tree-size (third expr)))]
    [else 1]))
```

Lower `F(T)`$ is better. A perfect fit with 7 nodes scores `0.007`$, which is the `0.001 \cdot 7`$ penalty alone.

### 5. Tree Utilities for Crossover and Mutation

To splice trees we need paths. A path is a list of steps `1`$ for left and `2`$ for right from the root. The root has path `()`$.

```racket
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
```

`collect-nodes`$ returns every subtree paired with its path. `get-at-path`$ and `replace-at-path`$ walk a path and read or replace. All three are plain list recursion. In a Lisp this is the natural way to work with code as data.

### 6. Genetic Operators

Crossover picks a random node in each parent and grafts the second subtree into the first at that spot. Mutation picks a random node and replaces it with a new random tree.

```racket
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
```

Both operators close over the language. A child built this way is always a legal program in the same vocabulary, so no repair step is needed.

### 7. Selection and Breeding

```racket
(define (tournament-select population scored)
  (define contenders
    (for/list ([_ (in-range tournament-size)])
      (list-ref scored (random (length scored)))))
  (car (argmin cdr contenders)))

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
```

Tournament selection draws `tournament-size = 7`$ trees at random and keeps the best. This gives steady pressure toward low `F(T)`$ without letting one super tree take over in a single generation. `next-generation`$ copies the elites, then fills the rest of the slots by breeding. A child from crossover may also mutate, which mirrors how many GP systems allow both operators to act on the same child.

### Complete File Listing

For reference the full file is 276 lines. What appears above are the meaningful fragments in file order. The remainder is the evolution driver `run-gp`$ and the `module+ main`$ entry point, plus `provide` forms for import. You can copy the file as a whole from `source-code/genetic_programming/genetic_programming.rkt`$ and run it unchanged.

## Execution and Output

Run the program from the project directory:

```racket
$ racket genetic_programming.rkt
```

Because the run is stochastic, each invocation differs in early generations but converges to a tree equivalent to `y = x^2 + x + 1`$ in most runs within the 40 generations. A typical run prints a demo of program-as-data and then one line per generation, with sample predictions every ten generations and a final table over all training points.

```
=== Genetic Programming: Symbolic Regression ===
Target function: (+ (* x x) x 1)  i.e. x^2 + x + 1
Training points: 21 values from x=-5 to 5 step 0.5
Population: 400  Generations: 40  Tournament: 7

--- Program as Data demo ---
Program A (data): (+ (* x x) 1)  eval at x=3 => 10
Program B (data): (* x (+ x 2))  eval at x=3 => 15
Crossover A and B => 2
Mutate A         => (+ (* x 2) 1)

Gen 0   best fitness 3.982  size 19  expr: (* (+ (- x (+ x x)) (pdiv (pdiv x 5) x)) (- (pdiv -2 (- 0 -3)) x))
  sample predictions: x=-2 -> 2.93 (target 3)  x=0 -> -0.67 (target 1)  x=1 -> 1.33 (target 3)  x=3 -> 10.27 (target 13)
Gen 1   best fitness 1.005  size 5  expr: (+ x (* x x))
Gen 3   best fitness 0.013  size 13  expr: (- (* (+ 2 x) x) (+ (* (pdiv x x) x) -1))
Gen 5   best fitness 0.007  size 7  expr: (+ x (- (* x x) -1))
Gen 10  best fitness 0.007  size 7  expr: (+ x (- (* x x) -1))
  sample predictions: x=-2 -> 3 (target 3)  x=0 -> 1 (target 1)  x=1 -> 3 (target 3)  x=3 -> 13 (target 13)
Gen 20  best fitness 0.007  size 7  expr: (+ x (- (* x x) -1))
  sample predictions: x=-2 -> 3 (target 3)  x=0 -> 1 (target 1)  x=1 -> 3 (target 3)  x=3 -> 13 (target 13)
Gen 30  best fitness 0.007  size 7  expr: (+ x (- (* x x) -1))
  sample predictions: x=-2 -> 3 (target 3)  x=0 -> 1 (target 1)  x=1 -> 3 (target 3)  x=3 -> 13 (target 13)
Gen 40  best fitness 0.007  size 7  expr: (+ x (- (* x x) -1))
  sample predictions: x=-2 -> 3 (target 3)  x=0 -> 1 (target 1)  x=1 -> 3 (target 3)  x=3 -> 13 (target 13)

=== Result ===
Best expression: (+ x (- (* x x) -1))
Best fitness (MSE + parsimony): 0.007
Predictions vs target:
  x=-5  predicted=21  target=21
  x=-4.5  predicted=16.75  target=16.8
  x=-4  predicted=13  target=13
  x=-3.5  predicted=9.75  target=9.8
  x=-3  predicted=7  target=7
  x=-2.5  predicted=4.75  target=4.8
  x=-2  predicted=3  target=3
  x=-1.5  predicted=1.75  target=1.8
  x=-1  predicted=1  target=1
  x=-0.5  predicted=0.75  target=0.8
  x=0  predicted=1  target=1
  x=0.5  predicted=1.75  target=1.8
  x=1  predicted=3  target=3
  x=1.5  predicted=4.75  target=4.8
  x=2  predicted=7  target=7
  x=2.5  predicted=9.75  target=9.8
  x=3  predicted=13  target=13
  x=3.5  predicted=16.75  target=16.8
  x=4  predicted=21  target=21
  x=4.5  predicted=25.75  target=25.8
  x=5  predicted=31  target=31

Success: evolved expression fits the data closely.
```

The second half of an alternate run may show the algebraically identical form `(+ (+ 1 x) (* x x))`$ instead. Both denote `x^2 + x + 1`$; evolution has no notion of canonical form.

## Interpreting the Results

The log tells the story of search, not just a final score.

Generation 0 starts from random trees. Its best tree has fitness `3.982`$ and 19 nodes. It fits crudely and pays a size cost of `0.019`$. The sample predictions at `x = 0`$ are off by more than 1. This is chance.

By generation 1 the system finds `(+ x (* x x))`$, which is `x^2 + x`$. Its `MSE`$ is `1.0`$ because it misses the constant `1`$ on every point, so `F = 1.0 + 0.005 = 1.005`$. In a tight race this partial solution dominates, and tournament selection keeps it.

By generation 3 to 5 crossover and mutation add the missing constant. `(+ x (- (* x x) -1))`$ is `x + (x^2 - (-1)) = x^2 + x + 1`$. Its `MSE`$ drops to about `0`$ on the 21 training points. Fitness becomes the parsimony term alone: `F = 0.007`$. No later generation improves on this because any true change would raise `MSE`$ by more than it could save in size.

The small size of the winner matters. Without the `0.001 \cdot |T|`$ term, a tree such as `(+ (+ 1 x) (+ (* x x) (* (pdiv x x) 0)))`$ would score the same `MSE`$ and could drift to larger forms. The term breaks ties toward short forms that a reader can check.

Protected division also shows in the intermediate forms like `(- (* (+ 2 x) x) (+ (* (pdiv x x) x) -1))`$. Here `(pdiv x x)`$ tends to 1, so the tree still denotes a polynomial. That is bloat with no harm, but it costs nodes. Selection trims it in later steps.

A stochastic method carries no proof of success. A run with a different seed can stall at `x^2 + x`$ if mutation does not supply the constant `1`$ in time, or it can find a longer but equivalent form and keep it. The remedy is the standard one: run several times, keep the best tree, or widen the constant set. The result section reports predictions versus targets for all 21 `x`$ so you can verify fit point by point rather than trust a single number.

## Wrap Up

This chapter used a minimal language to make the core loop clear: represent programs as S-expressions, score them by `MSE`$ plus a size cost, and breed them with list splicing. Racket lets each of those steps stay direct. There is no compiler pass between data and code, no external tree library, and no weight vector to tune. The whole system is about 140 lines of logic plus a driver.

The same scaffold scales. Add `sin`$ and `cos`$ to `functions`$ to learn periodic forms. Add a second variable `y`$ to `terminals`$ to learn surfaces. Replace the interpreter case for `pdiv`$ with a safe `log` or `exp` and handle range with `safe-eval`$. Each change is a few lines because the representation stays a list.

Genetic programming trades compute for insight. It runs many trials where a gradient method runs one, but it returns a formula a person can read. When the goal is not just to predict but to learn a law that explains data, that tradeoff is worth the cost.

## Practice Problems

1. **Change the target.** Replace `target-function`$ with `x^3 - 2x + 1`$, that is `(+ (- (* x (* x x)) (* 2 x)) 1)`$, and with `(x + 1)^2`$, that is `(* (+ x 1) (+ x 1))`$. Run each target five times. How many runs recover the exact form up to algebra, and what depth is needed for the cubic case?

2. **Grow the language.** Add `sin`$ and `cos`$ as unary functions to `functions`$. You must extend the interpreter and `tree-size`$ to handle arity 1. Create a data set from `y = \sin(x) + x`$ on `x \in [-3, 3]`$ and test if the system can find it. What happens to `MSE`$ if you omit protected forms for large `exp`$?

3. **Test protected division.** Build an expression `(pdiv 1 (- x 2))`$ and evaluate it on `x = 2`$ and `x = 2.0005`$. Confirm it returns 1 at the singular point. Then replace `protected-div`$ with raw `/`$ and watch `safe-eval`$ turn the result into `1e6`$ via the handler. Explain why an unprotected operator stalls evolution.

4. **Control bloat.** Remove the `size-penalty`$ from `fitness`$ and run 40 generations. Record the size of the best tree per generation. Restore the penalty and run again. Plot size versus generation for both settings and explain the difference in terms of `F(T) = \mathrm{MSE}(T) + 0.001 \cdot |T|`$.

5. **Improve selection.** Implement roulette selection where a tree `T_i`$ is picked with chance proportional to `1 / (1 + F(T_i))`$. Compare its best fitness after 40 generations to tournament selection with `tournament-size`$ 3, 7, and 11. Which pressure finds `x^2 + x + 1`$ fastest and why?

6. **Two variables.** Extend `terminals`$ to `'(x y)`$ and `eval-expr`$ to take both `x`$ and `y`$. Make a data set from `z = x^2 + y^2`$ on a grid and evolve formulas over `(x, y)`$. Report the smallest tree that gets `MSE < 0.01`$.

7. **From code as data to data as code.** Write `expr->lambda`$ that turns a tree such as `'(+ (* x x) 1)`$ into a Racket procedure `(lambda (x) (+ (* x x) 1))`$ using `eval`$ and backquote. Benchmark `expr->lambda`$ plus native call versus `safe-eval`$ over 10,000 evaluations and discuss when compilation pays off.
