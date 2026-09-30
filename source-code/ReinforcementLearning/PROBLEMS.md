# Known problems / continuation notes

Status of the five runnable examples, then the bugs found while building
them and the Racket gotchas that cost time. Read this before editing the
code; most of the surprises are recorded here.

## Status

| File | Status | Notes |
|---|---|---|
| `q_learning.rkt` | Works, ~1 s | All four TD methods reach the optimal return 0.6380 on the 5x5 maze; Q-learning matches `Q*` to 0.06 on well-visited pairs. |
| `policy_gradient.rkt` | Works, ~1 s | REINFORCE with raw returns or a learned `V(s)` baseline solves GridWorld; the linear softmax policy balances CartPole for the full 500 steps. |
| `dqn.rkt` (GridWorld) | Works, ~8 s | 25-48-48-4 network, greedy return 0.6380, `mean|Q - Q*|` 0.17 over all 100 pairs. |
| `dqn.rkt --cartpole` | Works, ~30-50 s | 4-64-64-2 network, 300 episodes, epsilon decayed over 8000 steps. Greedy evaluation: mean 106.8 steps, best 112, against ~22 for random. It learns to balance, not to master; see the limitations below. |
| `bandits.rkt` | Works, ~10 s | 200 runs x 1000 pulls. Thompson sampling has the lowest regret (37.1), greedy the highest (750.6). |
| `tests.rkt` | 30 tests pass in ~8 s | `raco test .` also runs the 9 built-in module tests. |

## Bugs found while building these examples

### 1. Double Q-learning starved one of its two tables

**Symptom.** Double Q-learning reached the goal (the greedy path was
optimal), but the reported table was far from `Q*`
(`mean|dQ| ~ 0.8` instead of `~0.13`), and `Q2(s, a)` was exactly `0.0`
for the transition *into the goal* while `Q1(s, a)` was `1.0`.

**Root cause.** The update function short-circuited the terminal case
before flipping the coin that chooses which table to update:

```racket
(cond
  [done? (values r q)]          ; always Q1!
  [(eq? method 'double-q) ...]) ; coin flip happens only here
```

Terminal transitions carry the largest rewards, so they all went into
`Q1`; `Q2` never learned them, and every bootstrap from `Q2` was zero.
Because Q1 and Q2 bootstrap from each other, one starved table breaks the
whole chain.

**Fix.** Flip the coin first, then compute the target:

```racket
[(eq? method 'double-q)
 (define use-q1? (< (random) 0.5))
 (cond
   [done? (values r (if use-q1? q q2))]
   ...)]
```

**Regression test.** `tests.rkt` -> "double Q-learning learns from
terminal transitions in both tables" asserts `mean|dQ| < 0.35`, which the
buggy version failed by a wide margin.

### 2. Thompson sampling degraded after a few hundred pulls

**Symptom.** The fraction of pulls on the best arm rose to 0.97 and then
*dropped* to 0.79 in the last 100-pull block. Sampling noise could not
explain an 18-point move over 20,000 pulls.

**Root cause.** `random-gamma-int` computed `Gamma(n, 1)` as
`-log(U_1 * ... * U_n)`. After ~900 pulls the shape parameter is in the
hundreds, the product underflows to zero, the `max 1e-12` clamp turns
every arm's sample into the same 27.6, and the argmax becomes arbitrary.

**Fix.** Sum the logs instead of taking the log of the product:
`Gamma(n,1) = sum_i -log(U_i)`, which is numerically stable for any `n`.

### 3. The CartPole policy gradient learned to *minimize* the return

**Symptom.** Training episode lengths *fell* from 16 to 10, well below the
~22 steps of a random policy, while the GridWorld REINFORCE results were
fine.

**Root cause.** `adam-step!` performs gradient descent, but the episode
gradient accumulated `+ A_t * grad log pi`, which is a gradient *ascent*
direction. Handing it to Adam minimized the objective.

**Fix.** Negate the accumulated gradient before the optimizer step (see
the comment in `train-reinforce-cartpole`). The same run then reaches a
500-step episode within 600 episodes.

### 4. DQN results changed from process to process

**Symptom.** `dqn.rkt` printed `mean|Q - Q*| = 0.1112` on one run and
`0.1718` on the next with no code change.

**Root cause.** The network was constructed *before* `(random-seed seed)`
was called. Racket seeds its global PRNG from the clock at startup, so
weight initialization depended on the ambient state.

**Fix.** Call `(random-seed seed)` before allocating the network in both
DQN training functions. `racket dqn.rkt` now produces byte-identical
output on repeated runs.

### 5. A pit next to the optimal route broke the on-policy methods

**Symptom.** SARSA and expected SARSA sometimes ended training with the
greedy policy bumping into a wall forever (return -0.1988, success 0),
while Q-learning was fine.

**Root cause.** With the pit at `(3,3)` and the optimal route running
along the right edge, the cells `(3,4)` and `(4,3)` sit next to the pit.
SARSA evaluates the *epsilon-greedy* policy, so exploration into the pit
makes the goal-seeking route look worse than bumping into a wall, where
nothing bad can happen. In the limit the learner prefers safety and never
leaves the start.

**Fix.** Move the pit to `(3,2)`, off the optimal route, so the hazard
constrains the shortcut but not the goal path. The final maze is stored as
the default in `make-gridworld`.

## Racket gotchas

- **`random` does not take a float width.** `(random 1.0)` and
  `(random 0.1)` raise a contract violation in Racket 9.3. Use `(random)`
  for a flonum in `[0, 1)` and `(random n)` for an exact integer in
  `[0, n)`. A uniform on `[-0.05, 0.05)` is `(* 0.1 (- (random) 0.5))`.
- **The `fl` operators require flonums.** `(fl/ total n)` fails when `n`
  is an exact integer, and `(fl- 4/3 x)` fails on the exact rational.
  Wrap with `exact->inexact` or write `1.3333333333333333`.
- **`racket/flonum` is not `racket/math`.** It has no `fltanh` (use
  `tanh`, which is flonum-optimized), and it has no `flvector->vector`
  (build one with `for/vector` over `in-flvector`).
- **`struct-out` does not export the constructor.** A struct exported with
  `(struct-out gridworld)` still needs `make-gridworld` listed separately
  in `provide`.
- **Keyword arguments shadow function names.** `#:replay-size [replay-size
  5000]` shadowed the accessor `replay-size`, so `(replay-size buffer)`
  tried to call `5000`. The accessor is now called `replay-count`.
- **`for/fold` with several accumulators returns several values.**
  `(define x (for/fold ([a ...] [b ...]) ...))` is an arity error; use
  `define-values`.
- **`check-true` wants `#t`, not merely a true value.** A test written as
  `(check-true (member x lst))` fails when `member` returns a list; use
  `check-not-false`.
- **`(struct ... #:mutable)` generates `set-FIELD!` for every field**, which
  is how the replay buffer's `size` and `next-index` are advanced.
- **Racket's default PRNG seed comes from the clock.** Any experiment that
  must be reproducible needs its own `(random-seed n)` before the first
  random draw, including model initialization.

## Known limitations and ideas for continuation

- **DQN on CartPole learns partially and is seed sensitive.** The shipped
  run is 300 episodes with epsilon decayed over 8000 steps: the greedy
  policy averages 106.8 steps (random is ~22), but no episode reaches the
  500-step limit, and a longer 700-episode run gave greedy means anywhere
  from 55 to 308 depending on the seed. DQN needs far more experience than
  the linear policy gradient here. The obvious improvements, in order of
  expected payoff: Double DQN (decouple action selection from evaluation),
  a soft/Polyak target update instead of the hard copy every 500 steps,
  a larger batch or more frequent gradient steps, and reward scaling. The
  training return curve also stays low while epsilon is high, so the demo
  is scored on greedy evaluation rather than training episode length.
- **Tabular REINFORCE is seed sensitive.** The raw-return and
  learned-baseline variants solve the maze reliably; the episode-mean
  baseline usually does not, and that failure is the lesson. Advantage
  normalization helps on CartPole but *hurt* the tabular GridWorld runs
  (`#:normalize? #t` divides by the standard deviation of a short
  episode's returns, amplifying noise), so it defaults to off there.
- **Greedy rollouts can loop on near-ties.** If two actions have equal
  values, `argmax` picks the first, which can be a wall bump. Training from
  random starts (`#:random-start? #t`) reduces the chance of a degenerate
  greedy policy and is a single keyword away. Evaluation of a *stochastic*
  policy is available as `evaluate-policy-sampled`, which is what
  REINFORCE actually optimizes.
- **Terminal states keep value 0 by convention.** Value iteration never
  updates `V` or `Q` in the goal or pit cells; the reward of the transition
  *into* them is what matters. Comparing learned tables against `Q*` on
  terminal rows therefore says nothing.
- **`q-mean-difference` needs a visit mask to be meaningful.** Without
  `#:visits`/`#:min-visits` it averages in state-action pairs the agent
  never tried, which stay at their initial value.
- **Possible extensions:** eligibility traces (SARSA(lambda),
  Q(lambda)); n-step returns; dueling and double DQN heads; prioritized
  replay; a continuous-action bandit with Gaussian Thompson sampling; EXP3
  for adversarial bandits; function approximation for the policy gradient
  (A2C) reusing `neural_net.rkt`.
