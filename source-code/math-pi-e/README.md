# Classical Algorithms for Computing π and e

This directory contains a Racket program that computes the mathematical constants
`\pi`$ and `e`$ using fourteen classical algorithms, ranging from the historical
(Leibniz's series, 1350s/1670s; Archimedes' polygon doubling, ~250 BCE) to the
record-setting modern methods (Chudnovsky binary splitting, 1988; the BBP
hex-digit spigot, 1995).

## Files

- **`math.rkt`** — the main program. Implements all algorithms, a method
  registry with metadata (formula, convergence rate, input defaults), automatic
  verification against trusted high-precision reference values, an interactive
  console REPL, and a command-line interface.
- **`math-tests.rkt`** — a RackUnit test suite (246 tests) that pins the fast
  algorithms against the published digits of `\pi`$ and `e`$, cross-checks the slower
  historical methods, and verifies the structural properties each algorithm
  guarantees (Archimedes brackets `\pi`$, Viète's product climbs monotonically, etc.).

## How It Works

Every algorithm (with one deliberate exception) works in **exact integer
arithmetic**. A real number `x`$ is represented as a fixed-point pair `(v, s)`$
meaning `v / 10^s`$, so results are exact to the requested number of digits with
no floating-point drift. The one exception is the BBP spigot, which is
mathematically defined in terms of floating-point fractional parts.

Each run reports not just the computed digits but also a **verification line**:
the program computes `\pi`$ or `e`$ independently with a trusted method (Chudnovsky for
`\pi`$, the exponential series for `e`$) and tells you exactly how many of your digits
are correct and where the value diverges.

## The Methods

### Computing `\pi`$ (9 methods)

| # | id           | Method                              | Convergence                          |
|---|--------------|-------------------------------------|--------------------------------------|
| 1 | `leibniz`    | Gregory-Leibniz series              | linear: error `\sim 1/(2n)`$                |
| 2 | `nilakantha` | Nilakantha series                   | cubic: ~3 digits per `10\times`$ of terms   |
| 3 | `wallis`     | Wallis product                      | linear: error `\sim \pi/(4n)`$               |
| 4 | `viete`      | Viète's nested radicals (1593)      | ~0.6 digits per factor               |
| 5 | `archimedes` | Archimedes polygon doubling         | ~0.6 digits/doubling; brackets `\pi`$       |
| 6 | `machin`     | Machin arctangent formula (1706)    | ~1.4 digits per arctan term          |
| 7 | `agm`        | Gauss-Legendre / Brent-Salamin AGM  | quadratic: digits double per step    |
| 8 | `chudnovsky` | Chudnovsky binary splitting (1988)  | ~14.18 digits per term               |
| 9 | `bbp`        | BBP hex-digit spigot (1995)         | hex digit `d`$ with no earlier digits  |

### Computing `e`$ (5 methods)

| #  | id                | Method                                | Convergence                       |
|----|-------------------|---------------------------------------|-----------------------------------|
| 10 | `e-series`        | Exponential series to `N`$ digits      | factorial: `\sim k \log_{10}(k/e)`$ digits  |
| 11 | `e-series-terms`  | Exponential series, first `n`$ terms   | error is the tail `1/n!`$          |
| 12 | `e-series-exact`  | Series kept as one exact rational     | same series, exact fraction out   |
| 13 | `e-limit`         | Limit definition `(1 + 1/n)^n`$        | linear: error `\sim e/(2n)`$        |
| 14 | `e-cf`            | Continued fraction `[2; 1,2,1, 1,4,1, \ldots]`$ | ~1 digit per convergent  |

## Running the Code

Requires [Racket](https://racket-lang.org/) (any recent version; no external
packages are needed).

### Interactive menu (REPL)

```bash
racket math.rkt
```

This prints the method table and a `choice>` prompt. Type a method number or id
to run it, or one of the commands:

```
c        compare every method with its defaults
h <id>   help for one method
s <n>    digits of output to display (default 200)
l        list methods again
q        quit
```

### Command line

```bash
racket math.rkt --list                          # show the method table
racket math.rkt --compare                       # run every method once, summarized
racket math.rkt chudnovsky digits=1000          # run one method
racket math.rkt --show 40 leibniz terms=500000  # flags must precede the method id
racket math.rkt --set terms 200000 nilakantha   # equivalent --set syntax
racket math.rkt bbp start=100 count=16          # hex digits of pi starting at position 100
```

Inputs are given as `key=value` pairs after the method id (or via `--set key
value`); each method has sensible defaults and caps on its inputs.

### Example output

```
$ racket math.rkt machin digits=20
Machin-like arctangent formula
  pi = 16*arctan(1/5) - 4*arctan(1/239)
  about 1.4 digits per arctan term; the classic hand-computation formula
inputs: digits=20
3.1415926535 8979323846
check: all 20 displayed digits match the reference (chudnovsky at 30 digits)
time: 20 us
```

## Running the Tests

```bash
racket math-tests.rkt
# or, from the directory:
raco test .
```

Expected result: `246 success(es) 0 failure(s) 0 error(s)`.

The test strategy is layered:

1. Pin the fast methods (Chudnovsky, Machin, AGM, exponential series) against
   the published 100-digit expansions of `\pi`$ and `e`$, and cross-check them against
   each other at 1000 digits.
2. Verify the slow historical methods converge at their theoretically expected
   rates (e.g., the Leibniz error is `< 1/(2n)`$).
3. Check structural guarantees: Archimedes' bounds always bracket `\pi`$, Viète's
   product increases monotonically toward `\pi`$, exact rational partial sums of `e`$
   match hand-computed fractions, and the BBP spigot's hex digits agree with
   the decimal expansion converted to hexadecimal.
