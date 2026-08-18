# Malt deep learning examples (Racket)

Three standalone examples using [malt](https://docs.racket-lang.org/malt/index.html),
the deep learning library from *The Little Learner*.

## Setup

```
raco pkg install --auto malt
raco setup malt
```

## Examples

| File | What it shows |
|---|---|
| `xor.rkt` | Minimal neural net: 2→8→1 ReLU network learns XOR. |
| `two_hidden_layers.rkt` | Two hidden layers (2→8→8→1) classify whether random 2-D points fall inside a circle. Synthetic data, training-accuracy self-check. |
| `joint_recommendation.rkt` | Jointly learned recommendation model: separate "customer tower" and "product tower" embeddings, concatenated for a rating prediction. Synthetic customer/product data. MAE ≈ 0.09. |

Run any of them with:

```
racket xor.rkt
```

Startup prints some `"settings="` / hash noise from malt itself; the real
output is at the end.

## Expected output

- `xor.rkt`: predictions ≈ 0, 1, 1, 0.
- `two_hidden_layers.rkt`: training accuracy ≈ 299/300; points near the
  origin score ≈ 1, corner points ≈ 0.
- `joint_recommendation.rkt`: MAE ≈ 0.09; for a bargain-hunting
  quality-seeker, the cheap high-quality product scores highest.

## Synthetic data (joint_recommendation.rkt)

400 training samples, each a 7-element input vector plus a scalar target.

**Inputs** — every feature is an independent uniform random number in [0, 1):

- Elements 0–2, the *customer*: how much of a **bargain hunter**,
  **quality seeker**, and **novelty seeker** this customer is.
- Elements 3–6, the *product*: its **cheapness** (inverse price),
  **quality**, **popularity**, and **novelty**.

**Target (rating)** — the hidden ground truth is a "taste match":

```
rating = 0.1 + 0.8 * ( bargain_hunter * cheapness
                     + quality_seeker * quality
                     + novelty_seeker * novelty )
         + uniform noise in [-0.025, 0.025]
```

So a customer scores a product highly when the product is strong on the
attributes the customer cares about. Popularity (element 5) is deliberately
unused by the formula — a decoy feature. Ratings land roughly in [0.16, 1.61]
with mean ≈ 0.70 (verified by `sanity_check.rkt`).

## Notes for editing

malt redefines `+ - * / abs sub1` as binary differentiable tensor ops that can
return duals — use `(only-in racket/base [+ r+] ...)` for scalar bookkeeping
and unwrap predictions with `ρ` when `dual?`. Full gotcha list: PROBLEMS.md.
