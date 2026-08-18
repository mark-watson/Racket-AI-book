# Known problems / continuation notes

## Status of the three examples

| File | Status |
|---|---|
| `xor.rkt` | Works. Converges exactly (seed 42, alpha 0.01, revs 4000, batch 4, 8 hidden units). |
| `two_hidden_layers.rkt` | Works. 299/300 training accuracy (re-seed 1 before `init-theta`, revs 8000, alpha 0.005, batch 16). |
| `joint_recommendation.rkt` | **FIXED.** MAE 0.09 (was 0.25), predictions differentiate products. Root cause below. |

## RESOLVED: joint_recommendation.rkt did not learn

**Root cause:** the selection-matrix builder used `(eq? j (+ i offset))`.
malt's `+` returns a value that is never `eq?` to a plain integer, so the
"selection matrices" were **all zeros**. Both towers received zero vectors;
with malt's zero-initialized biases the embeddings rectified to 0, every
weight gradient was exactly 0, and only the final bias could train — the
model collapsed to predicting the target mean (~0.72 ≈ mean rating 0.70).
Fix: `(equal? j (r+ i offset))` with base `r+`.

**How it was found (diagnostics):**
1. `(∇ ...)` of the block fn on one sample showed weight gradients exactly
   0.0 and bias gradients non-zero, identically for all inputs — structural,
   not dead ReLUs.
2. The primitives (`dot-product-2-1`, `concat`, `linear`) all pass gradients
   fine in isolation, and the two-tower architecture trains fine with literal
   selection matrices — which pointed at the matrix *construction*.
3. Printing the constructed matrix showed it was all zeros.

**Sanity check (`sanity_check.rkt`)** had already cleared the data: shapes
(400 7)/(400 1), targets match the bilinear formula within the ±0.025 noise
band, y ranges 0.16–1.61, mean 0.70.

**Note:** the ground truth is bilinear (customer-feature × product-feature
products), so a concat+linear head is structurally limited; current MAE 0.09
is acceptable for a demo, but a `dot-product` of the two embeddings
(matrix-factorization style scoring head) would match the data's inductive
bias exactly if lower error is ever needed. More revs / lower alpha did NOT
help beyond MAE ≈ 0.09.

## Malt API gotchas discovered (apply to all examples)

- `(require malt)` **shadows `+`, `-`, `*`, `/`, `abs`, `sub1`, `min`, `max`
  etc.** with differentiable tensor versions:
  - They are **binary only** — `(+ a b c)` raises an arity mismatch deep in
    `D-extend.rkt`.
  - **malt's `+` breaks `eq?`/`equal?`-style comparisons on its results** —
    `(eq? j (+ i offset))` is silently always false (this was the
    joint_recommendation bug). Use base ops for index arithmetic.
  - `sub1` on an inexact flonum raises a contract violation — use `(- x 1.0)`.
  - They can return **duals**, which break `exact->inexact`, `build-vector`,
    and base arithmetic. For scalar bookkeeping, use
    `(require (only-in racket/base [+ r+] [- r-] [/ r/] [abs r-abs]))`.
- **Model outputs contain duals** after training. Unwrap with
  `(define (realize x) (if (dual? x) (ρ x) x))` before doing plain math on
  predictions.
- **Tensors must be rectangular** — a per-sample `(tensor customer-vec
  product-vec)` with different lengths is rejected. Workaround used: one
  7-element input vector, split inside the block function with constant 0/1
  selection matrices via `dot-product-2-1` (malt has no slice op; `trefs`
  only works on the batch axis and returns vectors).
- **`build-tensor` takes a shape list**, not a count, and its result caused
  problems when fed to `dot-product-2-1` — `list->tensor` over `for/list`
  works reliably. Prefer it.
- **Dead-ReLU collapse is common**: all-zero outputs with the wrong seed.
  Symptom: every prediction identical. Fix: try other seeds / lower alpha /
  more revs. Re-seed immediately before `init-theta` so results are
  reproducible regardless of how many random draws data generation consumed.
- The `"settings="` / `'#hash((tensor-implementation . (learner)))` spam at
  startup is normal malt noise; filter with
  `racket file.rkt 2>/dev/null | grep -v -E 'hash|Tensor implementation|settings'`.
- `raco pkg install malt` was already done, but `raco setup malt` was needed
  before `(require malt)` resolved.
