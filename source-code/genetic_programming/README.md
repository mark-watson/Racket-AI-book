# Genetic Programming and Symbolic Regression in Racket

Demonstrates Racket's "code is data" philosophy: programs are S-expressions (nested lists) that serve as evolvable individuals in a genetic algorithm.

## Run

```bash
racket genetic_programming.rkt
```

No extra packages required beyond Racket 8.x.

## What it does

* **Representation** - Arithmetic expressions like `(+ (* x x) 1)` are ordinary Racket lists, both executable and mutable as data.
* **Evolution** - Implements selection (tournament), crossover (swap random subtrees), and mutation (replace random subtree) directly on S-expressions using list operations.
* **Application** - Symbolic regression: given 21 data points from `x^2 + x + 1`, the system evolves a formula that fits the data without being told the target expression.

## Key functions

* `random-expr` - generate random program trees
* `eval-expr` / `safe-eval` - interpret an S-expression with variable `x`
* `fitness` - mean squared error plus parsimony penalty
* `crossover` / `mutate` - genetic operators on list structure
* `run-gp` - evolution loop with elitism

## Try other targets

Edit `target-function` in `genetic_programming.rkt`:

```racket
(define (target-function x)
  (+ (- (* x (* x x)) (* 2 x)) 1)) ; x^3 - 2x + 1
```

Then re-run.
