# Category-Theory Deep Learning Framework (Racket)

**Reference:** Jia, Peng, Yang & Chen (2025).
*"Category-Theoretical and Topos-Theoretical Frameworks in Machine Learning: A Survey."*
Axioms 14(3):204. https://doi.org/10.3390/axioms14030204

## Overview

This framework implements all four categorical perspectives from the survey paper,
using the simple neural network in `../neural_category_theory` as its foundation.

```
racket deep_learning_category_theory.rkt
```

## Categorical Perspectives Implemented

### I. Para Category + Lens Composition (§2)

Every fully-connected layer is a **morphism in Para(Euc)**:

```
f : P × X → Y       (forward pass)
f*: Ctx × ∇Y → ∇P × ∇X  (pullback / backward pass)
```

- `forward-para` returns `(activations, pullback-closure)` — this is the **lens**
- Backpropagation = composition of pullback morphisms in reverse: `f* ∘ g* ∘ h*`
- SGD update = **endomorphism** `u_η : Model → Model`
- Networks of arbitrary depth via `make-network` + architecture spec

**Demo:** XOR problem — input(2) → hidden(4) → hidden(4) → output(1), 6000 epochs.

### II. Markov Categories — Stochastic Morphisms (§3)

A **Markov category** has symmetric monoidal structure where morphisms are
stochastic kernels (probability distributions over outputs).

- **Dropout** modelled as a stochastic lens: `X →_s X ⊗ Mask` where `Mask ~ Bernoulli(p)`.
  The same mask is reused in the backward pass (the "closed" optic requirement).
- **Bayesian layer** samples weights `W ~ N(μ, σ²)` at each forward pass —
  a stochastic morphism in the category **Stoch**.
- Monte Carlo uncertainty estimation quantifies epistemic uncertainty.

### III. Invariance & Equivariance (§4)

A layer `f` is **equivariant** w.r.t. group G if `f(g·x) = g·f(x)`.

- **Permutation-invariant pooling**: `Σ xᵢ` is the colimit over the set diagram —
  invariant to any permutation (basis of DeepSets architecture).
- **K-means as categorical colimit**: each centroid is the colimit (average) of
  its cluster; assignment morphisms are the universal maps.

### IV. Topos Framework (§5)

A topos E has a **subobject classifier** Ω and a "true" morphism `⊤ : 1 → Ω`.
Every binary classifier IS a characteristic morphism `χ : X → Ω`.

- **Subobject classifier**: the sigmoid output probability IS `χ_S(x)`.
  The decision boundary is `χ⁻¹(0.5)`.
- **Sheaf gluing**: local expert predictions are "sections"; the gluing lemma
  checks consistency (sheaf condition) before producing a global prediction.
- **Internal logic**: Ω carries a Heyting algebra — `∧`, `∨`, `¬`, `⇒` on `[0,1]`.

### V. Natural Transformations (§2)

A **natural transformation** `η : F ⇒ G` provides a coherent family of morphisms
`η_X : F(X) → G(X)`.  Demonstrated as a knowledge-distillation adapter that maps
the teacher network's 4-dim hidden representation to a 2-dim student space.

## Key Structures

| Struct | Categorical Role |
|---|---|
| `layer-params` | Object in parameter space P |
| `layer-grads` | Tangent vector at P (gradient) |
| `model` | Product of layer parameter spaces |
| `bayesian-layer` | Stochastic morphism parameters |
| `sheaf-section` | Local section F(U) of a sheaf |
| `nat-transform` | Components of η : F ⇒ G |

## Running

```bash
racket deep_learning_category_theory.rkt
```

Requires only the standard Racket installation (no additional packages).
