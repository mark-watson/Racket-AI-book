# Reinforcement Learning in Racket

Reinforcement learning examples for *Practical Artificial Intelligence
Development With Racket*, covering the three topics in the book's RL
chapter:

1. **Q-learning and policy gradient methods** — `q_learning.rkt`,
   `policy_gradient.rkt`
2. **Deep Q-Networks (DQN)** — `dqn.rkt`
3. **Multi-armed bandits** — `bandits.rkt`

Everything is plain Racket. There are no package dependencies beyond the
standard library, and every experiment is driven by Racket's global PRNG
through an explicit `random-seed`, so a run is reproducible and the test
suite can pin exact numbers.

## Files

| File | What it shows |
|---|---|
| `environments.rkt` | The test beds: a 5x5 GridWorld MDP (walls, a pit, a goal), CartPole, and Bernoulli/Gaussian bandits (stationary and drifting). Also the numeric helpers used everywhere. |
| `neural_net.rkt` | A small MLP with backpropagation, He/Xavier initialization, the Adam optimizer and the Huber/clipped TD error that DQN needs. |
| `q_learning.rkt` | Tabular temporal-difference control: Q-learning, SARSA, expected SARSA and Double Q-learning, plus value iteration as the exact reference. |
| `policy_gradient.rkt` | REINFORCE on GridWorld with four baseline variants, and a linear softmax policy trained on CartPole. |
| `dqn.rkt` | Deep Q-Network with a replay buffer, a target network and Huber loss; one-hot input for GridWorld and continuous input for CartPole (`--cartpole`). |
| `bandits.rkt` | The ten-armed test bed: greedy, epsilon-greedy (sample average and constant alpha), optimistic initialization, UCB1, Thompson sampling and softmax. |
| `tests.rkt` | 30 RackUnit tests: environment mechanics, finite-difference gradient checks, and a seeded learning test for every algorithm. |

Supporting documents:

- `PROBLEMS.md` — design notes, bugs found while building the examples, and
  the Racket/maths gotchas worth knowing before editing the code.
- The book chapter lives in the manuscript: `manuscript/ReinforcementLearning.md`.
  It walks through all three topics and quotes the numbers below.

## Setup

Only Racket itself (any 8.x or 9.x release):

```
racket --version
```

No `raco pkg install` step is needed.

## Run

```
racket q_learning.rkt          # tabular TD control, ~1 s
racket policy_gradient.rkt     # REINFORCE + CartPole, ~1 s
racket dqn.rkt                 # DQN on GridWorld, ~8 s
racket dqn.rkt --cartpole      # DQN on CartPole, ~30-50 s
racket bandits.rkt             # multi-armed bandits, ~10 s
```

Run the tests with:

```
racket tests.rkt               # or: raco test .
```

## 1. Q-learning and policy gradient methods

Both files use the same 5x5 GridWorld:

```
S . . . .
. # . # .
. . . # .
# # X . .
. . . . G
```

`S` is the start, `G` the goal (+1 and the episode ends), `X` a pit (-1
and the episode ends), `#` walls. Every other step costs 0.01, the
discount is `gamma = 0.95`, and the optimal discounted return from `S` is
**0.6380** (eight moves along the top and right edge).

### Tabular TD control (`q_learning.rkt`)

Four methods share one episode loop; they differ only in the target they
regress toward:

| Method | Target |
|---|---|
| Q-learning | `r + gamma * max_a' Q(s', a')` (off-policy) |
| SARSA | `r + gamma * Q(s', a')` for the action epsilon-greedy actually picks (on-policy) |
| Expected SARSA | `r + gamma * sum_a' pi(a'|s') Q(s', a')` |
| Double Q-learning | update one table, bootstrap the greedy action of that table evaluated in the *other* table |

Training is 800 episodes with `alpha = 0.2` and epsilon annealed from 1.00
to 0.05 over the first 400 episodes. Verified output:

```
method           return     success   steps    mean|dQ|
q-learning       0.6380     1.000     8.00     0.0600
sarsa            0.6380     1.000     8.00     n/a (on-policy)
expected-sarsa   0.6380     1.000     8.00     n/a (on-policy)
double-q         0.6380     1.000     8.00     0.1291

Value iteration reference: V(start) = 0.6380 after 8 sweeps
```

All four methods find the optimal route. The `mean|dQ|` column compares the
learned table with the exact `Q*` from value iteration (averaged over
state-action pairs visited at least five times). Only the off-policy
methods are compared: SARSA and expected SARSA converge to the value of
the *epsilon-greedy behaviour policy*, which prices in the chance of
falling into the pit, so their tables are legitimately lower than `Q*`
near the hazard.

### Policy gradients (`policy_gradient.rkt`)

REINFORCE parameterizes the policy directly: a softmax over 25x4 action
scores for GridWorld, and a linear softmax over the four continuous
CartPole state variables in the second half of the file.

```
variant                  sampled ret  greedy ret   success   steps
raw-returns              0.5140       0.6380       1.000     8.00
mean-baseline            -0.2421      -0.1988      0.000     100.00
learned-value-baseline   0.4409       0.6380       1.000     8.00
value-baseline-entropy   0.3057       0.6380       1.000     8.00
```

The `mean-baseline` row is deliberately left in as a negative result, and
it is the most instructive row in the table. Subtracting the mean of an
episode's returns-to-go does not change the policy gradient *in
expectation*, but with a sparse terminal reward every return in a failed
episode is about the same (-1 for the pit), so the advantage is nearly
zero and the learner never discovers that the pit is bad. A learned,
state-dependent `V(s)` fixes it because entering a pit makes
`G_t - V(s_t)` strongly negative.

CartPole with a linear policy (600 episodes, Adam, standardized
returns-to-go):

```
Mean episode length per 100-episode block (training):
  35.9  220.7  375.4  441.0  460.6  450.9

Greedy evaluation over 20 episodes: mean 500.0 steps, best 500, 20/20 reached 500.
```

A random policy survives about 22 steps, so the policy has learned to push
the cart back under the pole and never falls inside the 500-step limit.

## 2. Deep Q-Networks (`dqn.rkt`)

DQN replaces the Q table with a network and stabilizes the Bellman target
with three tricks, all visible in `dqn.rkt`:

1. a **replay buffer** of 5000 transitions sampled in minibatches of 32,
2. a **target network** recopied from the online network every 250 steps,
3. the **Huber (clipped) TD error**, so one surprising transition cannot
   produce a huge gradient.

The GridWorld demo uses one-hot state vectors (25 inputs), which makes the
network a differentiable Q table and lets the learned values be compared
one-for-one with the tabular answer:

```
Network 25 -> 48 -> 48 -> 4 (ReLU, linear output)
Training return per 50-episode block:  -0.64  0.74  0.83  0.85  0.91

Greedy evaluation over 200 episodes: mean return 0.6380, success 1.000, mean 8.00 steps.
Value iteration optimum:                                    0.6380, success 1.000, 8.00 steps.

mean |Q - Q*| over all 100 state-action pairs:
  DQN network        0.1718
  tabular Q-learning 0.0600
```

Roughly 8 seconds on a laptop. `racket dqn.rkt --cartpole` runs the same
algorithm on four continuous inputs with a 4-64-64-2 network. That run is
honest about sample efficiency: after 300 episodes the *greedy* policy
survives 106.8 steps on average (best 112) against 22 for a random policy,
so it has learned to push the cart back under the pole, but it has not
mastered the task. The linear policy gradient in `policy_gradient.rkt`
balances for the full 500 steps in the same number of episodes because its
features already encode position and velocity. See `PROBLEMS.md` for what
would push the DQN curve higher.

## 3. Multi-armed bandits (`bandits.rkt`)

The ten-armed Bernoulli test bed from Sutton and Barto (best arm 0.85,
worst 0.10), 200 runs of 1000 pulls:

```
strategy               regret@100   regret@500  regret@1000 % best arm
greedy                 74.98        375.03      750.62      0.000
epsilon-greedy 0.1     34.26        66.10       89.47       0.740
epsilon-greedy 0.01    68.04        226.83      314.27      0.284
optimistic init 5      11.52        32.49       57.97       0.710
ucb1 c=1               26.05        73.00       98.44       0.684
thompson sampling      19.17        32.98       37.08       0.877
softmax tau=0.1        20.33        67.06       104.27      0.545
```

Greedy locks onto an early lucky arm and its regret grows linearly;
optimistic initialization explores on its own but only early; UCB1 and
Thompson sampling keep regret sub-linear, and Thompson wins here because
its Beta posterior knows exactly how uncertain each arm is.

The second half repeats the comparison on a *drifting* bandit, where every
mean takes a small Gaussian random walk after each pull. There the
constant-alpha epsilon-greedy learner (which forgets old evidence) beats
the sample-average one, which is the classic result from Chapter 2 of
*Reinforcement Learning: An Introduction*.

## Design notes

- **One environment module, three algorithms.** GridWorld exposes its full
  transition model (`gridworld-transitions`), which is what value iteration
  needs, and a sampler (`gridworld-step`) built from the same model, which
  is what the TD methods need. That is why the learned tables can be
  compared with the exact optimum.
- **Closures, not classes.** Bandit strategies are `bandit-agent` structs
  holding two closures (`choose` and `update!`), so a single benchmark loop
  can run any strategy.
- **flvectors everywhere.** State values, weights and gradients are
  flvectors indexed with `fl+`/`fl*` from `racket/flonum`. The linear
  algebra is small enough that this stays readable, and it is several times
  faster than generic arithmetic on lists.
- **Seeded at the top.** Every training function calls `(random-seed seed)`
  before it allocates anything that draws random numbers, including network
  weights.
