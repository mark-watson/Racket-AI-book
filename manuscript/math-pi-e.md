# Computing Pi and e: Fourteen Classical Algorithms in Exact Arithmetic

## The Oldest Calculation in Mathematics

Few numbers have occupied mathematicians as persistently as `\pi`$, the ratio of a
circle's circumference to its diameter, and `e`$, the base of the natural
logarithm. Archimedes trapped `\pi`$ between two polygons around 250 BCE,
proving `3\frac{10}{71} < \pi < 3\frac{1}{7}`$ with nothing but geometry and
patience. The Madhava school in Kerala and later Gregory and Leibniz in Europe
discovered that `\pi`$ falls out of infinitely long sums of simple fractions.
Viete wrote the first infinite product in 1593, Wallis another in 1656, Machin
found a formula in 1706 that let him compute 100 digits by hand, and Euler gave
us the continued fraction for `e`$ in 1737. The story did not end with the
computer age: the Chudnovsky brothers published the series behind every modern
record computation of `\pi`$ in 1988, and Bailey, Borwein, and Plouffe stunned
everyone in 1995 with a formula that produces a single hexadecimal digit of
`\pi`$ without computing any of the digits before it.

Why should a programmer care about this history? Because these algorithms are a
perfect laboratory for three ideas that matter far beyond recreational
mathematics.

First, **convergence rates**. Each algorithm is a process that produces better
and better approximations, and the processes differ by enormous factors. The
Leibniz series needs roughly ten times more terms for each additional correct
digit, while the Chudnovsky series buys about 14 digits per term. Running both
side by side makes the abstract idea of a convergence rate visceral.

Second, **exact arithmetic**. A 64 bit floating point number carries about 16
decimal digits of precision, so it is useless for computing the 1000th digit of
`\pi`$. The classical workaround, and the one used throughout this chapter, is
to represent a real number as an exact integer `v`$ paired with a scale `s`$,
interpreted as the value `v / 10^s`$. All arithmetic is done on integers, so
there is no rounding error to accumulate; the only error is the mathematical
error of truncating an infinite process, which we can estimate on paper.

Third, **verification**. If you compute 500 digits of `\pi`$, how do you know
they are right? The program in this chapter answers that question the way
numerical analysts do: compute the same constant twice with two mathematically
independent algorithms and count how many leading digits agree.

The example code for this chapter implements nine algorithms for `\pi`$ and
five for `e`$, wraps them in a small registry with metadata about each method,
verifies every run against a trusted reference value, and presents the whole
thing through both an interactive console menu and a command line interface. A
RackUnit test suite pins the fast algorithms against published digit tables and
checks the structural guarantees of the slow ones. The code is in the files
`math.rkt` and `math-tests.rkt`.

## A Tour of the Algorithms

Before looking at the code, it helps to have the formulas in front of you and to
know what kind of behavior to expect from each one.

### Infinite series for pi

The Gregory-Leibniz series is the simplest of all:

```$
\frac{\pi}{4} = 1 - \frac{1}{3} + \frac{1}{5} - \frac{1}{7} + \cdots
```

It is also nearly useless in practice. The error after `n`$ terms is about
`1/(2n)`$, so a million terms buys only about six digits. Nilakantha's 14th
century improvement,

```$
\pi = 3 + \frac{4}{2 \cdot 3 \cdot 4} - \frac{4}{4 \cdot 5 \cdot 6} + \frac{4}{6 \cdot 7 \cdot 8} - \cdots
```

converges much faster because the denominators grow like `n^3`$; each tenfold
increase in terms yields about three extra digits.

Machin's formula from 1706,

```$
\pi = 16 \arctan\frac{1}{5} - 4 \arctan\frac{1}{239}
```

converts the problem into evaluating the arctangent series
`\arctan(1/x) = \sum_k (-1)^k / ((2k+1) x^{2k+1})`$ at two small arguments,
where it converges rapidly. This was the standard hand computation method for
two centuries, and it is still good for about 1.4 digits per term.

### Products and iterations for pi

The Wallis product and Viete's nested radicals both express `\pi`$ as an
infinite product:

```$
\frac{\pi}{2} = \frac{2 \cdot 2}{1 \cdot 3} \cdot \frac{4 \cdot 4}{3 \cdot 5} \cdot \frac{6 \cdot 6}{5 \cdot 7} \cdots
\qquad
\frac{2}{\pi} = \sqrt{\tfrac{1}{2}} \cdot \sqrt{\tfrac{1}{2} + \tfrac{1}{2}\sqrt{\tfrac{1}{2}}} \cdots
```

Archimedes' method is the most geometric. Inscribe and circumscribe hexagons in
a circle, then repeatedly double the number of sides. The perimeters of the
inscribed polygons climb toward `\pi`$ from below while the circumscribed
perimeters descend toward it from above, so at every step `\pi`$ is trapped
inside a known bracket. In the code this appears in its modern algebraic form as
a pair of interleaved harmonic and geometric means, `a \leftarrow 2ab/(a+b)`$
and `b \leftarrow \sqrt{ab}`$, starting from `a = 2\sqrt{3}`$ and `b = 3`$.
The bracket guarantee is a property we can test directly.

The Gauss-Legendre AGM iteration replaces the geometric picture with pure
arithmetic: repeatedly replace `a`$ and `b`$ by their arithmetic and geometric
means, accumulate a correction term `t`$, and compute
`\pi \approx (a+b)^2 / (4t)`$. Its convergence is quadratic, meaning the number
of correct digits roughly doubles at every step. Four iterations already give
more than 40 digits.

The Chudnovsky series is the industrial strength method:

```$
\frac{1}{\pi} = 12 \sum_{k=0}^{\infty} \frac{(-1)^k (6k)!\, (13591409 + 545140134 k)}{(3k)!\, (k!)^3\, 640320^{3k + 3/2}}
```

Each term contributes about 14.18 digits. The code evaluates it with **binary
splitting**, a divide and conquer technique that computes the product tree of
the factorials recursively instead of term by term, which is what makes the
method fast enough to hold every modern record.

The BBP formula is the odd one out:

```$
\pi = \sum_{k=0}^{\infty} \frac{1}{16^k} \left( \frac{4}{8k+1} - \frac{2}{8k+4} - \frac{1}{8k+5} - \frac{1}{8k+6} \right)
```

Because the weights are powers of `1/16`$, the fractional part of the partial
sum up to position `d`$ determines hexadecimal digit `d`$ of `\pi`$ directly.
You can ask for the millionth hex digit without knowing any of the earlier ones.
The algorithm is genuinely defined in terms of floating point fractional parts,
so it is the single place in the chapter where exact integer arithmetic is set
aside, by design.

### Three views of e

The constant `e`$ has three classical definitions, and the program implements
all of them. The exponential series converges at a factorial rate,

```$
e = \sum_{k=0}^{\infty} \frac{1}{k!} = 1 + 1 + \frac{1}{2} + \frac{1}{6} + \frac{1}{24} + \cdots
```

the limit definition `e = \lim_{n \to \infty} (1 + 1/n)^n`$ converges only
linearly, and Euler's continued fraction

```$
e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, \ldots]
```

produces a sequence of exact rational convergents, each buying roughly one more
digit. The program also keeps the partial sums of the series as exact rationals,
so you can see `e`$ as a single enormous fraction.

## The Program at a Glance

The code lives in `math.rkt`. Rather than a collection of loose scripts, the
program is built around a **method registry**: each algorithm is described by a
`method` structure carrying its id, title, formula, convergence notes, input
specifications (with defaults and caps), and a function that runs it. The
interactive menu, the command line interface, the comparison report, and the
test suite all work off this single table, so adding a fourteenth or fifteenth
algorithm means adding one entry, not touching any user interface code.

Every method takes small integer inputs given as `key=value` pairs, for example
`digits=1000` or `terms=500000`. Here is the method table the program prints,
which is also the vocabulary for everything that follows:

```
  #  id               method                                 input
  -- ---------------- -------------------------------------- -------------
   1 leibniz          Gregory-Leibniz series                 terms
   2 nilakantha       Nilakantha series                      terms
   3 wallis           Wallis product                         terms
   4 viete            Viete's nested radicals                terms
   5 archimedes       Archimedes polygon doubling            iterations
   6 machin           Machin-like arctangent formula         digits
   7 agm              Gauss-Legendre / Brent-Salamin AGM     digits
   8 chudnovsky       Chudnovsky binary splitting            digits
   9 bbp              BBP hex-digit spigot                   start,count

  10 e-series         Exponential series to N digits         digits
  11 e-series-terms   Exponential series, first n terms      terms
  12 e-series-exact   Exponential series as an exact fraction terms
  13 e-limit          Limit definition (1 + 1/n)^n           n
  14 e-cf             Continued fraction convergents         terms
```

The rest of this section walks through the complete listing of `math.rkt` in
seven parts, following the section banners already in the file.

## Part 1: File Header and Exports

The file opens with a usage comment that doubles as the manual for the command
line interface, followed by the `provide` list. Notice that the exports include
both the raw algorithm functions (`pi-leibniz`, `pi-chudnovsky`, and friends)
and the registry machinery (`method`, `run-method`, `method-lookup`), which is
what allows the test file to exercise everything through the same interface the
user sees.

```racket
#lang racket

;; math.rkt — classical algorithms for pi and e, with a console UI.
;;
;;   racket math.rkt                                interactive menu
;;   racket math.rkt --list                         show the method table
;;   racket math.rkt --compare                      run every method once
;;   racket math.rkt chudnovsky digits=1000         run one method
;;   racket math.rkt --show 40 leibniz terms=500000 (flags precede the id)
;;
;; Every algorithm works in exact integer arithmetic.  A real number x is
;; represented as the pair (v, s) meaning v / 10^s, so results are exact to
;; the requested number of digits with no floating point drift.  The one
;; exception is the BBP spigot, which is defined in terms of flonum
;; fractional parts.

(require racket/cmdline
         racket/format)

(provide (struct-out fp)
         fp->decimal
         fp->digits
         fp-display-str
         fp->hex-frac-digits
         count-matching-digits
         reference-value
         pi-leibniz pi-nilakantha pi-wallis pi-viete pi-archimedes
         pi-machin pi-agm pi-chudnovsky pi-bbp-hex
         e-series e-series-terms e-series-exact e-limit e-continued-fraction
         (struct-out input-spec)
         (struct-out method)
         (struct-out outcome)
         format-number-lines
         show-limit
         methods
         method-lookup
         run-method)
```

## Part 2: The Fixed-Point Number Helpers

This is the foundation everything else stands on. An `fp` value packages an
exact integer (or, for the continued fraction method, an exact rational) with a
decimal scale. The central function is `fp->decimal`, which rescales a value to
a requested number of fractional digits and splits it into integer and fraction
parts. Two details are worth studying. First, digits are **truncated, not
rounded**, because the goal is to match published digit tables exactly; a
rounded last digit might differ from the table even when the computation is
correct. Second, the rescaling happens before the numerator and denominator are
taken, which matters when `v` is an exact rational: normalizing a rational
changes its denominator, so splitting first and scaling second would give wrong
answers. The comment in the code records this lesson.

`count-matching-digits` compares two digit strings and reports how many leading
characters agree; it is the workhorse of the verification reports and the test
suite. `fp->hex-frac-digits` converts the fractional part of a fixed-point value
to hexadecimal digits, which the test suite uses to cross-check the BBP spigot
against the decimal expansion.

```racket
;; =====================================================================
;;  Section 1: numeric helpers
;; =====================================================================

;; A fixed-point value: the exact number `v` scaled by 10^-`s`.
;; `v` is normally an exact integer; the continued fraction method returns
;; an exact rational with s = 0.
(struct fp (v s) #:transparent)

(define (pow10 n) (expt 10 n))

(define (log10r x)
  (if (<= x 1) 0.0 (/ (log x) (log 10))))

(define (log2r x)
  (if (<= x 1) 0.0 (/ (log x) (log 2))))

(define (left-pad str n ch)
  (if (>= (string-length str) n)
      str
      (string-append (make-string (- n (string-length str)) ch) str)))

;; Round an exact rational n/d (both positive) to the nearest integer.
(define (round-quotient n d)
  (quotient (+ (* 2 n) d) (* 2 d)))

;; Split x into integer part and `digits` fractional digits, returning
;; (values int-str frac-str digits-str).  The digits are truncated, not
;; rounded, so the output matches published digit tables of pi and e exactly.
(define (fp->decimal x digits)
  (define e (- (fp-s x) digits))
  ;; Rescale first, then split: normalising the rational changes the
  ;; denominator, so numerator/denominator must be taken after scaling.
  (define scaled (if (> e 0)
                     (/ (fp-v x) (pow10 e))
                     (* (fp-v x) (pow10 (- e)))))
  (define total (quotient (numerator scaled) (denominator scaled)))
  (define divisor (pow10 digits))
  (define int-part (quotient total divisor))
  (define frac-part (remainder total divisor))
  (define int-str (number->string int-part))
  (define frac-str (left-pad (number->string frac-part) digits #\0))
  (values int-str frac-str (string-append int-str frac-str)))

;; Just the digit string, without the integer/fraction split.
(define (fp->digits x digits)
  (define-values (_i _f all) (fp->decimal x digits))
  all)

(define (fp-display-str x digits)
  (define-values (int-str frac-str _a) (fp->decimal x digits))
  (if (zero? digits) int-str (string-append int-str "." frac-str)))

;; Number of leading digits shared by two digit strings.
(define (count-matching-digits a b)
  (define n (min (string-length a) (string-length b)))
  (let loop ([i 0])
    (cond [(= i n) n]
          [(char=? (string-ref a i) (string-ref b i)) (loop (add1 i))]
          [else i])))

;; Hexadecimal digits of the fractional part of a fixed-point value.
(define (fp->hex-frac-digits x count)
  (define den (pow10 (fp-s x)))
  (define-values (_q rem0) (quotient/remainder (fp-v x) den))
  (for/fold ([acc '()] [rem rem0] #:result (list->string (reverse acc)))
            ([_i (in-range count)])
    (define r (* rem 16))
    (define-values (d next) (quotient/remainder r den))
    (values (cons (string-ref "0123456789abcdef" d) acc) next)))
```

## Part 3: The Pi Algorithms

Here is the first remarkable thing about the code: every algorithm is a direct
transliteration of its formula into integer arithmetic. Look at `pi-leibniz`.
The accumulator is a plain integer, each term is `S / (2k + 1)` where
`S = 10^s`$, and the sign alternates with `(even? k)`. There is no floating
point anywhere, so the only error is the truncation of the series itself, the
error we predicted on paper.

Two idioms deserve attention. `pi-viete` and `pi-archimedes` need square roots
at full precision, and Racket's `integer-sqrt` gives exactly that: the floor of
the true square root of an exact integer. And `pi-archimedes` returns **two**
values, the lower and upper bounds, using Racket's multiple return values; the
caller in the method registry uses both to report how many digits the bracket
pins down.

The historical progression is visible in the function names themselves: series
from the 14th and 17th centuries, Viete's product from 1593, and Archimedes'
doubling that is over two thousand years old, all expressed in the same twenty
lines of style.

```racket
;; =====================================================================
;;  Section 2: pi
;; =====================================================================

;; pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...            (Gregory-Leibniz, 1350s/1670s)
;; Error after n terms is about 1/(2n): painfully linear convergence.
(define (pi-leibniz terms s)
  (define S (pow10 s))
  (define acc
    (for/fold ([acc 0]) ([k (in-range terms)])
      (define term (quotient S (+ (* 2 k) 1)))
      (if (even? k) (+ acc term) (- acc term))))
  (* 4 acc))

;; pi = 3 + 4/(2*3*4) - 4/(4*5*6) + 4/(6*7*8) - ...   (Nilakantha, 14th c.)
;; Error after n terms is about 1/(4n^3): three digits per tenfold of terms.
(define (pi-nilakantha terms s)
  (define S (pow10 s))
  (for/fold ([acc (* 3 S)]) ([k (in-range 1 (add1 terms))])
    (define d (* (* 2 k) (+ (* 2 k) 1) (+ (* 2 k) 2)))
    (define term (quotient (* 4 S) d))
    (if (odd? k) (+ acc term) (- acc term))))

;; pi/2 = (2*2)/(1*3) * (4*4)/(3*5) * (6*6)/(5*7) * ...   (Wallis, 1656)
(define (pi-wallis terms s)
  (define S (pow10 s))
  (define prod
    (for/fold ([p S]) ([n (in-range 1 (add1 terms))])
      (define two-n (* 2 n))
      (define factor (round-quotient (* two-n two-n S)
                                     (* (sub1 two-n) (add1 two-n))))
      (quotient (* p factor) S)))
  (* 2 prod))

;; 2/pi = sqrt(1/2) * sqrt(1/2 + 1/2*sqrt(1/2)) * ...   (Viete, 1593)
;; Each c_k = sqrt((1 + c_(k-1))/2), starting from c_0 = 0.
(define (pi-viete terms s)
  (define S (pow10 s))
  (define prod
    (for/fold ([c 0] [p S] #:result p) ([_k (in-range terms)])
      (define c2 (integer-sqrt (* S (quotient (+ S c) 2))))
      (values c2 (quotient (* p c2) S))))
  (quotient (* 2 S S) prod))

;; Archimedes' polygon doubling, written as the Pfaff/Borchardt pair of
;; means: a <- 2ab/(a+b) (harmonic, circumscribed), b <- sqrt(a*b)
;; (geometric, inscribed).  Both converge to pi and always bracket it,
;; gaining about 0.6 digits per doubling.  Returns (values lower upper).
(define (pi-archimedes iterations s)
  (define S (pow10 s))
  (define a0 (integer-sqrt (* 12 S S)))  ; 2*sqrt(3), circumscribed hexagon
  (define b0 (* 3 S))                    ; 3,           inscribed  hexagon
  (let loop ([a a0] [b b0] [k iterations])
    (if (zero? k)
        (values b a)
        (let* ([a2 (quotient (* 2 a b) (+ a b))]
               [b2 (integer-sqrt (* a2 b))])
          (loop a2 b2 (sub1 k))))))

;; arctan(1/x) = sum (-1)^k / ((2k+1) x^(2k+1)), in fixed point.
(define (arctan-recip x s)
  (define S (pow10 s))
  (define x2 (* x x))
  (let loop ([k 0] [num (quotient S x)] [acc 0])
    (if (zero? num)
        acc
        (let ([term (quotient num (+ (* 2 k) 1))])
          (loop (add1 k)
                (quotient num x2)
                (if (even? k) (+ acc term) (- acc term)))))))

;; Machin's formula (1706): pi = 16*arctan(1/5) - 4*arctan(1/239).
;; About 1.4 digits per term of the arctan series.
(define (pi-machin s)
  (- (* 16 (arctan-recip 5 s))
     (* 4 (arctan-recip 239 s))))

;; Gauss-Legendre / Brent-Salamin AGM iteration (1975-76).
;; Digits roughly double at every step.
(define (pi-agm s)
  (define S (pow10 s))
  (define iterations (+ 1 (exact-ceiling (log2r s))))
  (let loop ([a S]
             [b (integer-sqrt (quotient (* S S) 2))]  ; 1/sqrt(2)
             [t (quotient S 4)]
             [p 1]
             [k iterations])
    (if (zero? k)
        (quotient (* (+ a b) (+ a b)) (* 4 t))
        (let* ([a2 (quotient (+ a b) 2)]
               [b2 (integer-sqrt (* a b))]
               [d (- a a2)]
               [t2 (- t (quotient (* p (* d d)) S))]
               [p2 (* 2 p)])
          (loop a2 b2 t2 p2 (sub1 k))))))

(define CHUD-Q 10939058860032000)  ; 640320^3 / 24

;; Binary splitting of the Chudnovsky series (1988):
;;   1/pi = 12 * sum (-1)^k (6k)!(13591409 + 545140134k) / ((3k)!(k!)^3 640320^(3k+3/2))
;; Each term buys about 14.18 digits, which is why it holds the records.
(define (chudnovsky-split a b)
  (if (= 1 (- b a))
      (if (zero? a)
          (values 1 1 13591409)
          (let ([P (* (- (* 6 a) 5) (- (* 2 a) 1) (- (* 6 a) 1))])
            (values P
                    (* (expt a 3) CHUD-Q)
                    (* P (+ 13591409 (* 545140134 a)) (if (even? a) 1 -1)))))
      (let ([m (quotient (+ a b) 2)])
        (define-values (Pl Ql Tl) (chudnovsky-split a m))
        (define-values (Pr Qr Tr) (chudnovsky-split m b))
        (values (* Pl Pr)
                (* Ql Qr)
                (+ (* Tl Qr) (* Pl Tr))))))

(define (pi-chudnovsky s)
  (define terms (+ 2 (quotient s 14)))
  (define-values (_P Q T) (chudnovsky-split 0 terms))
  (define root (integer-sqrt (* 10005 (pow10 s) (pow10 s))))  ; sqrt(10005)*10^s
  (quotient (* Q 426880 root) T))

;; Bailey-Borwein-Plouffe (1995): hex digit `d` of pi without any of the
;; preceding digits.
;;   pi = sum 16^-k (4/(8k+1) - 2/(8k+4) - 1/(8k+5) - 1/(8k+6))
;; This one genuinely needs flonum fractional parts.
(define (modpow16 e m)
  (let loop ([e e] [b (modulo 16 m)] [acc 1])
    (cond [(zero? e) acc]
          [(even? e) (loop (quotient e 2) (modulo (* b b) m) acc)]
          [else (loop (sub1 e) b (modulo (* acc b) m))])))

(define (bbp-series j d)
  (define head
    (for/fold ([acc 0.0]) ([k (in-range d)])
      (define m (+ (* 8 k) j))
      (define term (/ (exact->inexact (modpow16 (- d k 1) m))
                      (exact->inexact m)))
      (define x (+ acc term))
      (- x (floor x))))
  (define tail
    (for/fold ([acc 0.0]) ([k (in-range d (+ (* 2 d) 10))])
      (+ acc (/ (expt 16.0 (- d k 1)) (exact->inexact (+ (* 8 k) j))))))
  (define x (+ head tail))
  (- x (floor x)))

(define (pi-bbp-hex-digit d)
  (define x (- (* 4.0 (bbp-series 1 d))
               (* 2.0 (bbp-series 4 d))
               (bbp-series 5 d)
               (bbp-series 6 d)))
  (define frac (- x (floor x)))
  (string (string-ref "0123456789ABCDEF"
                      (inexact->exact (floor (* 16.0 frac))))))

;; Hex digits of pi starting at 1-based position `start`.
(define (pi-bbp-hex start count)
  (apply string-append
         (for/list ([d (in-range start (+ start count))])
           (pi-bbp-hex-digit d))))
```

The modern algorithms show why the registry's metadata about convergence
matters. `pi-machin` loops until the current numerator is zero at the working
scale, so it stops exactly when further terms can no longer change the result.
`pi-agm` computes its iteration count from `\log_2`$ of the target scale,
because quadratic convergence means the iteration count grows only
logarithmically with the number of digits wanted. `chudnovsky-split` is the
most sophisticated recursion in the file: it returns three values `P`$, `Q`$,
and `T`$ for a range of terms and combines subranges with the identities
`P = P_l P_r`$, `Q = Q_l Q_r`$, `T = T_l Q_r + P_l T_r`$, turning an
`n`$-term sum into an `O(\log n)`$ deep product tree.

The BBP section is the one place flonums appear. `modpow16` computes
`16^e \bmod m`$ by repeated squaring, keeping numbers small, and `bbp-series`
accumulates only the fractional part of each partial sum. The trade-off is
explicit: we gave up exactness, and in return we can teleport to digit `d`$.

## Part 4: The Algorithms for e

The three classical views of `e`$ translate just as directly. `e-series`
accumulates `1/k!`$ by dividing the previous term by `k`$ and stops when the
next term is zero at the working scale. `e-series-exact` is more interesting:
it keeps the partial sum as one exact rational with denominator `(n-1)!`$,
built with pure integer arithmetic so that Racket never has to normalize the
fraction by a greatest common divisor computation. `e-limit` evaluates
`(1 + 1/n)^n`$ with binary exponentiation on fixed-point values, and
`e-continued-fraction` runs the standard convergent recurrence
`p_i = a_i p_{i-1} + p_{i-2}`$ over Euler's pattern of partial quotients
`1, 2k, 1`$.

```racket
;; =====================================================================
;;  Section 3: e
;; =====================================================================

;; e = sum 1/k!, run until the next term no longer affects the scale.
(define (e-series s)
  (define S (pow10 s))
  (let loop ([k 1] [term S] [acc S])
    (define next (quotient term k))
    (if (zero? next) acc (loop (add1 k) next (+ acc next)))))

;; The same series truncated after exactly `terms` terms (k = 0 .. terms-1).
(define (e-series-terms terms s)
  (define S (pow10 s))
  (let loop ([k 1] [term S] [acc S])
    (if (>= k terms)
        acc
        (let ([next (quotient term k)])
          (loop (add1 k) next (+ acc next))))))

;; The exact rational partial sum sum_{k=0}^{terms-1} 1/k!, built with
;; integer arithmetic only so no gcd normalisation blowup occurs.
(define (e-series-exact terms)
  (define n (max 1 terms))
  (define Q (for/fold ([f 1]) ([i (in-range 2 n)]) (* f i)))  ; (n-1)!
  (define T
    (let loop ([k 0] [m Q] [acc 0])
      (if (= k n)
          acc
          (loop (add1 k) (quotient m (add1 k)) (+ acc m)))))
  (/ T Q))

;; (1 + 1/n)^n by binary exponentiation on fixed-point values.
(define (fp-pow base exponent s)
  (define S (pow10 s))
  (let loop ([e exponent] [b base] [acc S])
    (cond [(zero? e) acc]
          [(even? e) (loop (quotient e 2) (quotient (* b b) S) acc)]
          [else (loop (sub1 e) b (quotient (* acc b) S))])))

(define (e-limit n s)
  (define S (pow10 s))
  (define base (+ S (quotient S (max 1 n))))
  (fp-pow base (max 1 n) s))

;; e = [2; 1,2,1, 1,4,1, 1,6,1, 1,8,1, ...]  (Euler, 1737)
;; Returns the exact rational of the `terms`-th convergent.
(define (e-cf-partial i)
  (cond [(= i 0) 2]
        [(= 2 (modulo i 3)) (* 2 (quotient (add1 i) 3))]
        [else 1]))

(define (e-continued-fraction terms)
  (define n (max 1 terms))
  (define-values (p q)
    (for/fold ([p-prev2 0] [p-prev1 1]
               [q-prev2 1] [q-prev1 0]
               #:result (values p-prev1 q-prev1))
              ([i (in-range n)])
      (define a (e-cf-partial i))
      (values p-prev1 (+ (* a p-prev1) p-prev2)
              q-prev1 (+ (* a q-prev1) q-prev2))))
  (/ p q))
```

Note the asymmetry between the two families: the `e` functions take the scale
`s` as an argument, exactly like the `\pi`$ functions, except
`e-series-exact` and `e-continued-fraction`, which return exact rationals
instead of scaled integers. That small difference is why the `fp` struct allows
`v` to be a rational with `s = 0`$: the formatting machinery can treat both
representations uniformly.

## Part 5: Reference Values and the Registry Scaffolding

The verification story starts here. `reference-value` computes a trusted high
precision value of either constant, using the Chudnovsky series for `\pi`$ and
the exponential series for `e`$, and caches it in a hash table keyed by
constant and digit count. Every report and every test that asks "how many
digits are correct" is really asking "how many leading digits agree with this
independently computed reference."

Below it are the three structures that define the little framework: `input-spec`
describes one named integer input with a label, default, and cap; `method`
bundles everything the user interface needs to know about an algorithm;
`outcome` is what running a method produces, either a fixed-point value plus
formatted lines, or, for the BBP spigot, a string of hex digits. The
`estimate-digits` helper implements the rule of thumb mentioned in the theory
section: if a method gains `rate` digits per tenfold increase in its input, the
suggested display precision is `offset + rate \cdot \log_{10}(\text{input})`$.

```racket
;; =====================================================================
;;  Section 4: reference values and verification
;; =====================================================================

(define reference-cache (make-hash))

;; A trusted high-precision value, used to say how many digits a method
;; actually got right.
(define (reference-value constant digits)
  (hash-ref! reference-cache
             (list constant digits)
             (lambda ()
               (define s (+ digits 10))
               (fp (case constant
                     [(pi) (pi-chudnovsky s)]
                     [(e) (e-series s)])
                   s))))

;; =====================================================================
;;  Section 5: the method table
;; =====================================================================

(struct input-spec (key label default cap) #:transparent)
(struct method (id
                title
                constant
                inputs        ; (listof input-spec)
                auto-digits   ; (-> hash? natural?) suggested display digits
                run           ; (-> hash? natural? outcome?)
                formula
                convergence
                compare-inputs)
  #:transparent)
(struct outcome (value lines hex) #:transparent)

(define GUARD 25)

;; Suggest how many digits to display, from the expected truncation error:
;; `rate` digits gained per tenfold increase in the input, plus `offset`.
(define (estimate-digits input rate offset)
  (max 1 (min 200 (inexact->exact
                   (floor (+ offset (* rate (log10r (max 2 input)))))))))

;; Build the outcome for an ordinary fixed-point result.
(define (fp-outcome v digits show)
  (define-values (int-str frac-str _all) (fp->decimal v digits))
  (outcome v (format-number-lines int-str frac-str show) #f))

(define (group10 str)
  (define n (string-length str))
  (for/list ([i (in-range 0 n 10)])
    (substring str i (min n (+ i 10)))))

(define (format-number-lines int-str frac-str show)
  (define total (string-length frac-str))
  (define shown (min total (max 0 show)))
  (define groups (group10 (substring frac-str 0 shown)))
  (define per-line 5)
  (define chunks
    (let loop ([g groups])
      (cond [(null? g) '()]
            [(<= (length g) per-line) (list g)]
            [else (cons (take g per-line) (loop (drop g per-line)))])))
  (define body
    (if (null? chunks)
        (list (string-append "  " int-str (if (zero? total) "" ".")))
        (cons (string-append "  " int-str "." (string-join (car chunks) " "))
              (for/list ([c (in-list (cdr chunks))])
                (string-append "    " (string-join c " "))))))
  (append body
          (if (> total shown)
              (list (format "    ... showing ~a of ~a digits; last 10: ~a"
                            shown total
                            (string-join (take-right (group10 frac-str) 1) " ")))
              '())))

(define (short-num str limit)
  (define n (string-length str))
  (if (<= n limit) str (string-append (substring str 0 limit) "...")))

(define (exact-rational-lines r)
  (list (format "    exact rational: ~a / ~a"
                (short-num (number->string (numerator r)) 40)
                (short-num (number->string (denominator r)) 40))
        (format "    (~a-digit numerator, ~a-digit denominator)"
                (string-length (number->string (numerator r)))
                (string-length (number->string (denominator r))))))
```

The `GUARD` constant of 25 extra digits appears in every method's `run`
function. Working at higher precision than you display is the standard defense
against truncation artifacts in the intermediate arithmetic: the guard digits
absorb the accumulated integer divisions so that the displayed digits are clean.

## Part 6: The Method Registry

Each entry pairs the raw algorithm with everything the user interface needs: the
formula as a display string, a plain language convergence note, input defaults
and caps, a suggested digit count, and the inputs used by the `--compare`
report. Read the `leibniz` entry closely and the pattern for all fourteen is
clear: its scale request adds guard digits plus `\log_{10}`$ of the term count,
because dividing `10^s`$ by numbers up to `2n`$ loses about that many digits of
headroom.

```racket
;; ---- pi methods -----------------------------------------------------

(define m-leibniz
  (method 'leibniz
          "Gregory-Leibniz series"
          'pi
          (list (input-spec 'terms "series terms" 100000 10000000))
          (lambda (in) (estimate-digits (hash-ref in 'terms) 1 -1))
          (lambda (in digits)
            (define s (+ digits GUARD (exact-ceiling (log10r (max 2 (hash-ref in 'terms))))))
            (fp-outcome (fp (pi-leibniz (hash-ref in 'terms) s) s) digits (show-limit)))
          "pi/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ..."
          "linear: error ~ 1/(2n), so one extra digit costs ten times the terms"
          (hash 'terms 200000)))

(define m-nilakantha
  (method 'nilakantha
          "Nilakantha series"
          'pi
          (list (input-spec 'terms "series terms" 1000 1000000))
          (lambda (in) (estimate-digits (hash-ref in 'terms) 3 0))
          (lambda (in digits)
            (define s (+ digits GUARD 10))
            (fp-outcome (fp (pi-nilakantha (hash-ref in 'terms) s) s) digits (show-limit)))
          "pi = 3 + 4/(2*3*4) - 4/(4*5*6) + 4/(6*7*8) - ..."
          "cubic: error ~ 1/(4n^3), so three extra digits per tenfold of terms"
          (hash 'terms 5000)))

(define m-wallis
  (method 'wallis
          "Wallis product"
          'pi
          (list (input-spec 'terms "product factors" 10000 1000000))
          (lambda (in) (estimate-digits (hash-ref in 'terms) 1 -1))
          (lambda (in digits)
            (define s (+ digits GUARD (exact-ceiling (log10r (max 2 (hash-ref in 'terms))))))
            (fp-outcome (fp (pi-wallis (hash-ref in 'terms) s) s) digits (show-limit)))
          "pi/2 = (2*2)/(1*3) * (4*4)/(3*5) * (6*6)/(5*7) * ..."
          "linear: error ~ pi/(4n), so one extra digit costs ten times the factors"
          (hash 'terms 50000)))

(define m-viete
  (method 'viete
          "Viete's nested radicals"
          'pi
          (list (input-spec 'terms "radical factors" 30 2000))
          (lambda (in) (min 200 (max 1 (inexact->exact (floor (* 0.6 (hash-ref in 'terms)))))))
          (lambda (in digits)
            (define s (+ digits GUARD))
            (fp-outcome (fp (pi-viete (hash-ref in 'terms) s) s) digits (show-limit)))
          "2/pi = sqrt(1/2) * sqrt(1/2 + 1/2*sqrt(1/2)) * ..."
          "linear: about 0.6 digits per factor (the first infinite product for pi, 1593)"
          (hash 'terms 40)))

(define m-archimedes
  (method 'archimedes
          "Archimedes polygon doubling"
          'pi
          (list (input-spec 'iterations "polygon doublings" 20 5000))
          (lambda (in) (min 200 (max 1 (inexact->exact (floor (* 0.6 (hash-ref in 'iterations)))))))
          (lambda (in digits)
            (define s (+ digits GUARD))
            (define iters (hash-ref in 'iterations))
            (define sides-desc
              (if (<= iters 40)
                  (format "~a-gon" (* 6 (expt 2 iters)))
                  (format "6*2^~a-gon" iters)))
            (define-values (lo hi) (pi-archimedes iters s))
            (define lower (fp lo s))
            (define upper (fp hi s))
            (define-values (int-str frac-str _a) (fp->decimal lower digits))
            (define-values (_i2 frac-str2 _a2) (fp->decimal upper digits))
            (define pinned
              (max 0 (sub1 (count-matching-digits (string-append int-str frac-str)
                                                  (string-append int-str frac-str2)))))
            (outcome lower
                     (append (list (format "    inscribed ~a (lower bound):" sides-desc))
                             (format-number-lines int-str frac-str 60)
                             (list (format "    circumscribed ~a (upper bound): ~a"
                                           sides-desc
                                           (fp-display-str upper digits))
                                   (format "    digits pinned by the bracket: ~a" pinned)))
                     #f))
          "a <- 2ab/(a+b); b <- sqrt(a*b), from the hexagon a=2*sqrt(3), b=3"
          "linear: about 0.6 digits per doubling, but every step brackets pi exactly"
          (hash 'iterations 30)))

(define m-machin
  (method 'machin
          "Machin-like arctangent formula"
          'pi
          (list (input-spec 'digits "decimal digits" 100 1000000))
          (lambda (in) (hash-ref in 'digits))
          (lambda (in digits)
            (define s (+ digits GUARD))
            (fp-outcome (fp (pi-machin s) s) digits (show-limit)))
          "pi = 16*arctan(1/5) - 4*arctan(1/239)"
          "about 1.4 digits per arctan term; the classic hand-computation formula"
          (hash 'digits 50)))

(define m-agm
  (method 'agm
          "Gauss-Legendre / Brent-Salamin AGM"
          'pi
          (list (input-spec 'digits "decimal digits" 100 1000000))
          (lambda (in) (hash-ref in 'digits))
          (lambda (in digits)
            (define s (+ digits GUARD))
            (fp-outcome (fp (pi-agm s) s) digits (show-limit)))
          "a,b <- (a+b)/2, sqrt(ab); t <- t - p(a-a')^2; pi ~ (a+b)^2/(4t)"
          "quadratic: the number of correct digits roughly doubles each iteration"
          (hash 'digits 50)))

(define m-chudnovsky
  (method 'chudnovsky
          "Chudnovsky binary splitting"
          'pi
          (list (input-spec 'digits "decimal digits" 1000 2000000))
          (lambda (in) (hash-ref in 'digits))
          (lambda (in digits)
            (define s (+ digits GUARD))
            (fp-outcome (fp (pi-chudnovsky s) s) digits (show-limit)))
          "1/pi = 12 * sum (-1)^k (6k)!(13591409+545140134k) / ((3k)!(k!)^3 640320^(3k+3/2))"
          "about 14.18 digits per term; used for every modern pi record"
          (hash 'digits 50)))

(define m-bbp
  (method 'bbp
          "BBP hex-digit spigot"
          'pi
          (list (input-spec 'start "starting hex digit position (1-based)" 1 200000)
                (input-spec 'count "how many hex digits" 40 1000))
          (lambda (in) 0)
          (lambda (in digits)
            (define start (hash-ref in 'start))
            (define count (hash-ref in 'count))
            (define hx (pi-bbp-hex start count))
            (outcome #f
                     (list (format "    hex digits ~a..~a of pi, after the decimal point:"
                                   start (+ start count -1))
                           (if (= 1 start)
                               (format "  3.~a" hx)
                               (format "  ...~a" hx)))
                     hx))
          "pi = sum 16^-k (4/(8k+1) - 2/(8k+4) - 1/(8k+5) - 1/(8k+6))"
          "a spigot: digit d in base 16 with none of the earlier digits computed"
          (hash 'start 1 'count 24)))

;; ---- e methods ------------------------------------------------------

(define m-e-series
  (method 'e-series
          "Exponential series to N digits"
          'e
          (list (input-spec 'digits "decimal digits" 100 1000000))
          (lambda (in) (hash-ref in 'digits))
          (lambda (in digits)
            (define s (+ digits GUARD))
            (fp-outcome (fp (e-series s) s) digits (show-limit)))
          "e = sum_{k>=0} 1/k! = 1 + 1 + 1/2 + 1/6 + 1/24 + ..."
          "factorial convergence: term k is worth about k*log10(k/e) digits"
          (hash 'digits 50)))

(define m-e-series-terms
  (method 'e-series-terms
          "Exponential series, first n terms"
          'e
          (list (input-spec 'terms "series terms" 40 200000))
          (lambda (in)
            (define n (max 2 (hash-ref in 'terms)))
            (min 200 (max 1 (inexact->exact
                             (floor (+ 0.5 (- (* n (log10r n)) (* n (log10r (exp 1))))))))))
          (lambda (in digits)
            (define n (hash-ref in 'terms))
            (define s (+ digits GUARD 10))
            (fp-outcome (fp (e-series-terms n s) s) digits (show-limit)))
          "e ~ sum_{k=0}^{n-1} 1/k!"
          "error is the tail 1/n! ~ about n*log10(n/e) digits from n terms"
          (hash 'terms 40)))

(define m-e-series-exact
  (method 'e-series-exact
          "Exponential series as an exact fraction"
          'e
          (list (input-spec 'terms "series terms" 25 20000))
          (lambda (in)
            (define n (max 2 (hash-ref in 'terms)))
            (min 200 (max 1 (inexact->exact
                             (floor (+ 0.5 (- (* n (log10r n)) (* n (log10r (exp 1))))))))))
          (lambda (in digits)
            (define r (e-series-exact (hash-ref in 'terms)))
            (define v (fp r 0))
            (define-values (int-str frac-str _a) (fp->decimal v digits))
            (outcome v
                     (append (format-number-lines int-str frac-str (show-limit))
                             (exact-rational-lines r))
                     #f))
          "e ~ sum_{k=0}^{n-1} 1/k!, kept as one exact rational"
          "same series as above, but the answer is an exact fraction rather than a decimal"
          (hash 'terms 25)))

(define m-e-limit
  (method 'e-limit
          "Limit definition (1 + 1/n)^n"
          'e
          (list (input-spec 'n "n" 100000 10000000))
          (lambda (in) (estimate-digits (hash-ref in 'n) 1 -1))
          (lambda (in digits)
            (define n (hash-ref in 'n))
            (define s (+ digits GUARD (exact-ceiling (log2r (max 2 n)))))
            (fp-outcome (fp (e-limit n s) s) digits (show-limit)))
          "e = lim (1 + 1/n)^n"
          "linear: error ~ e/(2n), one extra digit per tenfold of n"
          (hash 'n 100000)))

(define m-e-cf
  (method 'e-cf
          "Continued fraction convergents"
          'e
          (list (input-spec 'terms "partial quotients" 40 100000))
          (lambda (in) (min 200 (max 1 (quotient (* 9 (hash-ref in 'terms)) 10))))
          (lambda (in digits)
            (define r (e-continued-fraction (hash-ref in 'terms)))
            (define v (fp r 0))
            (define-values (int-str frac-str _a) (fp->decimal v digits))
            (define partials
              (for/list ([i (in-range (min 24 (hash-ref in 'terms)))])
                (number->string (e-cf-partial i))))
            (define tail (string-join (cdr partials) ", "))
            (outcome v
                     (append (format-number-lines int-str frac-str (show-limit))
                             (list (format "    convergent: [~a~a~a~a]"
                                           (car partials)
                                           (if (string=? tail "") "" "; ")
                                           tail
                                           (if (>= (hash-ref in 'terms) 24) ", ..." ""))
                                   (format "    from ~a partial quotients"
                                           (hash-ref in 'terms)))
                             (exact-rational-lines r))
                     #f))
          "e = [2; 1,2,1, 1,4,1, 1,6,1, 1,8,1, ...]"
          "the partial quotients repeat 1,2k,1; each convergent buys about one digit"
          (hash 'terms 40)))

(define methods
  (list m-leibniz m-nilakantha m-wallis m-viete m-archimedes
        m-machin m-agm m-chudnovsky m-bbp
        m-e-series m-e-series-terms m-e-series-exact m-e-limit m-e-cf))

(define (method-lookup id-or-index)
  (define n (and (string? id-or-index) (string->number id-or-index)))
  (cond [(and (exact-integer? n) (<= 1 n (length methods)))
         (list-ref methods (sub1 n))]
        [(string? id-or-index)
         (define sym (string->symbol id-or-index))
         (findf (lambda (m) (eq? (method-id m) sym)) methods)]
        [else #f]))

(define (run-method m inputs digits)
  ((method-run m) inputs digits))
```

The `archimedes` entry shows why the registry stores functions rather than
precomputed data: its `run` lambda prints the polygon size (a 30 doubling run is
a polygon with `6 \cdot 2^{30} = 6{,}442{,}450{,}944`$ sides), both bounds, and
the count of digits the two bounds agree on, which is information only
Archimedes' method can certify. The `bbp` entry is the only one whose outcome
has `#f` for the value and a string in the `hex` field, a difference the
reporting code checks for explicitly.

## Part 7: Reporting, the Console UI, and the Command Line

The final stretch of the file turns outcomes into printed reports and wires up
the two front ends. `verification-lines` is the interesting piece: for ordinary
methods it compares the result against the cached reference digit by digit and
reports either full agreement or the exact position where the digits diverge,
showing both strings around the divergence point. For the BBP outcome it
converts the reference value to hexadecimal and compares in base 16. Inputs to
the program are small and uniform: each method declares its `input-spec` list,
the command line accepts them as `key=value` pairs after the method id (or with
repeated `--set key value` flags), and the interactive loop prompts for them one
at a time with the defaults shown in brackets.

```racket
;; =====================================================================
;;  Section 6: reporting and verification
;; =====================================================================

(define show-limit (make-parameter 200))

(define (verification-lines m out digits inputs)
  (define hex-str (outcome-hex out))
  (cond
    [hex-str
     (define start (hash-ref inputs 'start 1))
     (define count (string-length hex-str))
     (cond
       [(> (+ start count) 20000)
        (list "  check: skipped (position too deep for a full-precision reference)")]
       [else
        (define ref (reference-value 'pi (exact-ceiling (+ 20 (* 1.21 (+ start count))))))
        (define ref-hex (fp->hex-frac-digits ref count))
        (define match (count-matching-digits (string-downcase hex-str) ref-hex))
        (if (= match count)
            (list (format "  check: all ~a hex digits match the reference pi" match))
            (list (format "  check: ~a of ~a hex digits match" match count)
                  (format "    got: ~a" hex-str)
                  (format "    ref: ~a" (string-upcase ref-hex))))])]
    [(outcome-value out)
     => (lambda (v)
          (define ref (reference-value (method-constant m) (+ digits 10)))
          (define got (fp->digits v digits))
          (define exp (fp->digits ref digits))
          (define match (count-matching-digits got exp))
          (define correct (max 0 (sub1 match)))
          (cond
            [(>= match (string-length exp))
             (list (format "  check: all ~a displayed digits match the reference (~a at ~a digits)"
                           correct (method-id (reference-method (method-constant m)))
                           (+ digits 10)))]
            [else
             (define at (max 0 (sub1 match)))
             (define lo (max 0 (- at 4)))
             (define hi (min (string-length got) (+ at 12)))
             (list (format "  check: ~a correct decimal digits, diverges at digit ~a"
                           correct (add1 correct))
                   (format "    got: ...~a|~a"
                           (substring got lo at)
                           (substring got at hi))
                   (format "    ref: ...~a|~a"
                           (substring exp lo at)
                           (substring exp at hi))
                   (format "    (reference: ~a at ~a digits)"
                           (method-id (reference-method (method-constant m)))
                           (+ digits 10)))]))]
    [else '()]))

(define (reference-method constant)
  (case constant
    [(pi) m-chudnovsky]
    [(e) m-e-series]))

(define (format-elapsed ms)
  (cond [(< ms 1) (format "~a us" (exact-round (* ms 1000)))]
        [(< ms 1000) (format "~a ms" (~r ms #:precision 2))]
        [else (format "~a s" (~r (/ ms 1000) #:precision 2))]))

(define (print-report m out digits elapsed-ms inputs)
  (define input-desc
    (string-join (for/list ([spec (method-inputs m)])
                   (format "~a=~a" (input-spec-key spec)
                           (hash-ref inputs (input-spec-key spec))))
                 ", "))
  (displayln (format "  ~a" (method-title m)))
  (displayln (format "    ~a" (method-formula m)))
  (displayln (format "    ~a" (method-convergence m)))
  (displayln (format "  inputs: ~a" input-desc))
  (for-each displayln (outcome-lines out))
  (for-each displayln (verification-lines m out digits inputs))
  (displayln (format "  time: ~a" (format-elapsed elapsed-ms))))

;; =====================================================================
;;  Section 7: console UI
;; =====================================================================

(define (print-banner)
  (displayln "=======================================================================")
  (displayln "  math.rkt - classical computations of pi and e")
  (displayln "=======================================================================")
  (displayln "  Exact integer / fixed-point arithmetic throughout (the BBP spigot is")
  (displayln "  the one flonum algorithm, by design).")
  (displayln ""))

(define (print-method-table)
  (displayln "  #  id               method                                 input")
  (displayln "  -- ---------------- -------------------------------------- -------------")
  (for ([(m i) (in-indexed methods)])
    (when (and (> i 0)
               (not (eq? (method-constant m) (method-constant (list-ref methods (sub1 i))))))
      (displayln ""))
    (printf "  ~a ~a ~a ~a~n"
            (left-pad (number->string (add1 i)) 2 #\space)
            (~a (method-id m) #:min-width 16)
            (~a (method-title m) #:min-width 38)
            (string-join (for/list ([spec (method-inputs m)])
                           (format "~a" (input-spec-key spec)))
                         ","))))

(define (print-help m)
  (displayln (format "  ~a  [~a]" (method-title m) (method-id m)))
  (displayln (format "    computes:   ~a" (method-constant m)))
  (displayln (format "    formula:    ~a" (method-formula m)))
  (displayln (format "    convergence: ~a" (method-convergence m)))
  (displayln "    inputs:")
  (for ([spec (method-inputs m)])
    (displayln (format "      ~a: ~a (default ~a, max ~a)"
                       (input-spec-key spec)
                       (input-spec-label spec)
                       (input-spec-default spec)
                       (input-spec-cap spec)))))

;; Read a line, or return eof.
(define (read-choice prompt-str)
  (display prompt-str)
  (flush-output)
  (define line (read-line))
  (if (eof-object? line) eof (string-downcase (string-trim line))))

(define (prompt-int label default cap)
  (let loop ()
    (define raw (read-choice (format "  ~a [~a]: " label default)))
    (cond
      [(eof-object? raw) eof]
      [(string=? raw "") default]
      [else
       (define n (string->number raw))
       (cond
         [(not (and (number? n) (exact-integer? n) (positive? n)))
          (displayln "    please enter a positive whole number (or enter for the default)")
          (loop)]
         [(> n cap)
          (displayln (format "    ~a is above the limit ~a - using ~a" n cap cap))
          cap]
         [else n])])))

(define (prompt-inputs m)
  (let loop ([specs (method-inputs m)] [acc (hash)])
    (cond [(null? specs) acc]
          [else
           (define spec (car specs))
           (define v (prompt-int (input-spec-label spec)
                                 (input-spec-default spec)
                                 (input-spec-cap spec)))
           (if (eof-object? v)
               eof
               (loop (cdr specs) (hash-set acc (input-spec-key spec) v)))])))

(define (display-digits-for m inputs)
  (define from-inputs (hash-ref inputs 'digits #f))
  (if from-inputs
      from-inputs
      (prompt-int "decimal digits to display"
                  ((method-auto-digits m) inputs)
                  1000000)))

(define (execute m inputs digits)
  (define t0 (current-inexact-milliseconds))
  (define out
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (printf "  error: ~a~n" (exn-message e))
                       #f)])
      (run-method m inputs digits)))
  (define elapsed (- (current-inexact-milliseconds) t0))
  (when out
    (print-report m out digits elapsed inputs)))

(define (run-interactive m)
  (displayln "")
  (displayln (format "-- ~a --" (method-id m)))
  (define inputs (prompt-inputs m))
  (unless (eof-object? inputs)
    (define digits (display-digits-for m inputs))
    (unless (eof-object? digits)
      (execute m inputs digits))))

(define (compare-all)
  (displayln "")
  (displayln "  Every method at its default input:")
  (displayln "")
  (printf "  ~a ~a ~a ~a ~a~n"
          (~a "method" #:min-width 16)
          (~a "input" #:min-width 18)
          (~a "time" #:min-width 12)
          (~a "correct digits" #:min-width 15)
          "value")
  (for ([m (in-list methods)])
    (define inputs (method-compare-inputs m))
    (define digits (min 20 (max 1 ((method-auto-digits m) inputs))))
    (define t0 (current-inexact-milliseconds))
    (define out
      (with-handlers ([exn:fail? (lambda (e) #f)])
        (run-method m inputs digits)))
    (define elapsed (- (current-inexact-milliseconds) t0))
    (define input-desc
      (string-join (for/list ([k (in-list (hash-keys inputs))])
                     (format "~a=~a" k (hash-ref inputs k)))
                   ","))
    (cond
      [(not out)
       (printf "  ~a ~a ~a ~a ~a~n"
               (~a (method-id m) #:min-width 16)
               (~a input-desc #:min-width 18)
               (~a (format-elapsed elapsed) #:min-width 12)
               (~a "FAILED" #:min-width 15)
               "-")]
      [(outcome-hex out)
       (printf "  ~a ~a ~a ~a ~a~n"
               (~a (method-id m) #:min-width 16)
               (~a input-desc #:min-width 18)
               (~a (format-elapsed elapsed) #:min-width 12)
               (~a (format "~a hex" (count-hex-matches out)) #:min-width 15)
               (format "3.~a" (outcome-hex out)))]
      [else
       (define v (outcome-value out))
       (define ref (reference-value (method-constant m) (+ digits 10)))
       (define match (count-matching-digits (fp->digits v digits) (fp->digits ref digits)))
       (printf "  ~a ~a ~a ~a ~a~n"
               (~a (method-id m) #:min-width 16)
               (~a input-desc #:min-width 18)
               (~a (format-elapsed elapsed) #:min-width 12)
               (~a (max 0 (sub1 match)) #:min-width 15)
               (fp-display-str v (min digits 18)))])))

(define (count-hex-matches out)
  (define hx (outcome-hex out))
  (define ref (reference-value 'pi (exact-ceiling (+ 10 (* 1.21 (string-length hx))))))
  (count-matching-digits (string-downcase hx)
                         (fp->hex-frac-digits ref (string-length hx))))

(define (start-repl)
  (print-banner)
  (print-method-table)
  (displayln "")
  (displayln "  Enter a number or id to run a method.  Other commands:")
  (displayln "    c        compare every method with its defaults")
  (displayln "    h <id>   help for one method")
  (displayln "    s <n>    digits of output to display (now 200)")
  (displayln "    l        list methods again")
  (displayln "    q        quit")
  (let loop ()
    (define cmd (read-choice "\nchoice> "))
    (cond
      [(eof-object? cmd) (displayln "\nbye")]
      [(string=? cmd "") (loop)]
      [(member cmd '("q" "quit" "exit" "bye")) (displayln "bye")]
      [(member cmd '("l" "list" "?")) (print-method-table) (loop)]
      [(member cmd '("c" "compare")) (compare-all) (loop)]
      [(string-prefix? cmd "h")
       (handle-help (string-trim (substring cmd 1)))
       (loop)]
      [(string-prefix? cmd "s")
       (handle-show (string-trim (substring cmd 1)))
       (loop)]
      [else
       (define m (method-lookup cmd))
       (if m
           (begin (run-interactive m) (loop))
           (begin (printf "  unknown choice: ~a (try 'l')~n" cmd) (loop)))])))

(define (handle-help arg)
  (cond [(string=? arg "")
         (displayln "  usage: h <method-id or number>")
         (print-method-table)]
        [else
         (define m (method-lookup arg))
         (if m (print-help m) (printf "  unknown method: ~a~n" arg))]))

(define (handle-show arg)
  (define n (string->number arg))
  (cond [(not (and (number? n) (exact-integer? n) (positive? n)))
         (displayln (format "  digits to display is currently ~a; usage: s <n>" (show-limit)))]
        [(> n 1000000)
         (displayln "  that is a lot; capped at 1000000")
         (show-limit 1000000)]
        [else
         (show-limit n)
         (displayln (format "  will display up to ~a digits" n))]))

;; =====================================================================
;;  Section 8: command line entry point
;; =====================================================================

(module+ main
  (define mode 'interactive)
  (define id-arg #f)
  (define sets '())
  (define show 200)

  (command-line
   #:program "math.rkt"
   #:once-each
   [("--list") "Print the method table and exit"
    (set! mode 'list)]
   [("--compare") "Run every method with its default inputs"
    (set! mode 'compare)]
   [("--show") n "Digits of the result to display (default 200)"
    (set! show (string->number n))]
   #:multi
   [("--set") key value "Set an input, e.g. --set digits 1000"
    (set! sets (cons (cons (string->symbol key) value) sets))]
   #:args arg-list
   ;; Flags must precede the method id; after it, inputs are key=value.
   (when (pair? arg-list)
     (set! mode 'run)
     (set! id-arg (car arg-list))
     (for ([extra (in-list (cdr arg-list))])
       (match (string-split extra "=")
         [(list "show" v) (set! show (string->number v))]
         [(list key v) (set! sets (cons (cons (string->symbol key) v) sets))]
         [_ (raise-user-error 'math.rkt "expected key=value, got: ~a" extra)]))))

  (show-limit (if (and (number? show) (positive? show)) show 200))

  ;; Inputs come from --set, falling back to each method's default.
  (define (inputs-from-sets m)
    (for/fold ([h (hash)]) ([spec (in-list (method-inputs m))])
      (define key (input-spec-key spec))
      (define raw (assv key sets))
      (define n (and raw (string->number (cdr raw))))
      (hash-set h key
                (if (and (exact-integer? n) (positive? n))
                    (min n (input-spec-cap spec))
                    (input-spec-default spec)))))

  (case mode
    [(list)
     (print-method-table)]
    [(compare)
     (compare-all)]
    [(run)
     (define m (and (string? id-arg) (method-lookup id-arg)))
     (cond
       [(not m)
        (printf "unknown method: ~a~n" id-arg)
        (print-method-table)
        (exit 1)]
       [else
        (define inputs (inputs-from-sets m))
        (define digits (max 1 ((method-auto-digits m) inputs)))
        (execute m inputs digits)])]
    [else
     (start-repl)]))
```

Two Racket idioms in this section are worth adding to your toolbox. The `cond`
clause with `=>` in `verification-lines` binds the non-false test value to a
name, avoiding a nested `if`. And `module+ main` is Racket's way of separating
"running the file" from "requiring the file": the test suite can `require`
`math.rkt` and get all the exports without triggering the command line
interface, because the `main` submodule only runs when the file is executed
directly.

## The Test Suite

Tests for numerical code need trusted data, and the most trustworthy data
available for this problem is the published digit expansion of the constants
themselves. The test file opens by embedding 100 digits of `\pi`$, 100 digits
of `e`$, and 32 hexadecimal digits of `\pi`$ as string constants. Every
subsequent check compares computed digits against these tables, or against
values produced by an independent algorithm that has itself been pinned against
the tables.

```racket
;; Published expansions of pi and e (fractional digits).
(define PI-50 "14159265358979323846264338327950288419716939937510")
(define PI-99
  (string-append PI-50 "5820974944592307816406286208998628034825342117067"))
(define E-50 "71828182845904523536028747135266249775724709369995")
(define E-99
  (string-append E-50 "9574966967627724076630353547594571382178525166427"))

;; Hex expansion of pi after the point.
(define PI-HEX-32 "243F6A8885A308D313198A2E03707344")
```

One subtlety shapes all the string comparisons. Integer division truncates, so a
computed value at scale `s`$ can sit one unit below the true value in the final
digit. The helper `frac` asks for five more digits than it returns and
discards them, which guarantees the last kept digit is a true digit of the
constant and not an artifact of truncation. With that helper in place, the test
strategy has three layers:

1. Pin the fast methods (Chudnovsky, Machin, AGM, and the exponential series)
   against the published tables, then cross-check them against each other at
   1000 digits, far beyond anything floating point could reach.
2. Verify the slow historical methods against the fast references, at the
   accuracy their theory predicts, with a digit of slack.
3. Check the structural properties each algorithm guarantees: Archimedes' bounds
   bracket `\pi`$ at every iteration, Viete's product increases monotonically,
   consecutive exact partial sums of `e`$ differ by exactly `1/k!`$, and the
   continued fraction convergents match hand computed fractions like
   `8/3`$, `11/4`$, and `19/7`$.

Here is the complete test file:

```racket
#lang racket

;; Tests for math.rkt.  Run with: racket math-tests.rkt   (or: raco test .)
;;
;; Strategy:
;;   1. pin the fast methods against the published digits of pi and e,
;;   2. cross-check the slower historical methods against those,
;;   3. check the structural properties each algorithm guarantees
;;      (Archimedes brackets pi, Viete climbs monotonically, the exact
;;      rational partial sums of e match their hand-computed fractions).
;;
;; String comparisons are made on digits that rounding cannot touch: the
;; helper `frac` asks for one more digit than it returns, so the final
;; digit of every expectation is a true digit of the constant.

(require rackunit
         rackunit/text-ui
         "math.rkt")

;; Published expansions of pi and e (fractional digits).
(define PI-50 "14159265358979323846264338327950288419716939937510")
(define PI-99
  (string-append PI-50 "5820974944592307816406286208998628034825342117067"))
(define E-50 "71828182845904523536028747135266249775724709369995")
(define E-99
  (string-append E-50 "9574966967627724076630353547594571382178525166427"))

;; Hex expansion of pi after the point.
(define PI-HEX-32 "243F6A8885A308D313198A2E03707344")

(define S 130)  ; working scale: comfortably more precision than any check needs

;; The first `n` fractional digits of a fixed-point value.  A few extra
;; digits are computed and discarded so that a value which sits one unit
;; below the truth at its working scale cannot shift the last digit shown.
(define (frac x n)
  (substring (fp->digits x (+ n 5)) 1 (add1 n)))

(define (correct-digits x ref n)
  (max 0 (sub1 (count-matching-digits (fp->digits x n) (fp->digits ref n)))))

(define PI-REF (fp (pi-chudnovsky S) S))
(define E-REF (fp (e-series S) S))

(define decimal-tests
  (test-suite
   "fixed-point formatting"
   (check-equal? (fp-display-str (fp 314159 5) 4) "3.1415")   ; truncated, not rounded
   (check-equal? (fp-display-str (fp 314159 5) 5) "3.14159")
   (check-equal? (fp-display-str (fp 314159 5) 7) "3.1415900")
   (check-equal? (fp-display-str (fp 5 1) 0) "0")              ; 0.5 with no digits left
   (check-equal? (fp-display-str (fp 35 1) 0) "3")
   (check-equal? (fp-display-str (fp 1/2 0) 3) "0.500")        ; exact rational at scale 0
   (check-equal? (fp-display-str (fp 8/3 0) 6) "2.666666")
   ;; the denominator must be taken after rescaling, not before
   (check-equal? (fp-display-str (fp (e-series-exact 40) 0) 20)
                 "2.71828182845904523536")
   (check-equal? (count-matching-digits "314159" "314159") 6)
   (check-equal? (count-matching-digits "314159" "314259") 3)
   (check-equal? (count-matching-digits "31" "314159") 2)
   (check-equal? (fp->hex-frac-digits (fp 5 1) 4) "8000")    ; 0.5 = 0.8 in hex
   (check-equal? (fp->hex-frac-digits (fp 3141592653589793238 18) 4) "243f")))

(define fast-pi-tests
  (test-suite
   "fast pi algorithms"
   (check-equal? (frac PI-REF 99) PI-99)
   (check-equal? (frac (fp (pi-machin S) S) 99) PI-99)
   (check-equal? (frac (fp (pi-agm S) S) 99) PI-99)
   (check-equal? (frac (reference-value 'pi 99) 99) PI-99)
   ;; far beyond flonum range the three independent algorithms must agree
   (check-equal? (fp->digits (fp (pi-chudnovsky 1025) 1025) 1000)
                 (fp->digits (fp (pi-machin 1025) 1025) 1000))
   (check-equal? (fp->digits (fp (pi-chudnovsky 1025) 1025) 1000)
                 (fp->digits (fp (pi-agm 1025) 1025) 1000))))

(define archimedes-tests
  (test-suite
   "Archimedes polygon iteration"
   (let-values ([(lo hi) (pi-archimedes 0 S)])
     (check-equal? (fp-display-str (fp lo S) 6) "3.000000")
     (check-equal? (fp-display-str (fp hi S) 6) "3.464101"))   ; hexagon: 3 and 2*sqrt(3)
   (for ([iters (in-list '(1 5 20 60))])
     (define-values (lo hi) (pi-archimedes iters S))
     (check-true (< lo (fp-v PI-REF) hi)
                 (format "~a doublings must bracket pi" iters)))
   ;; one doubling cuts the bracket by roughly a factor of four
   (let-values ([(lo1 hi1) (pi-archimedes 20 S)]
                [(lo2 hi2) (pi-archimedes 21 S)])
     (check-true (< (* 3 (- hi2 lo2)) (- hi1 lo1))))
   ;; and the bracket keeps tightening all the way down
   (check-true (>= (correct-digits (let-values ([(lo _h) (pi-archimedes 200 S)])
                                     (fp lo S))
                                   PI-REF 130)
                   119))))

(define viete-tests
  (test-suite
   "Viete's nested radicals"
   (check-true (< (pi-viete 1 S) (fp-v PI-REF)))
   (for/fold ([prev (pi-viete 1 S)]) ([n (in-range 2 30)])
     (define cur (pi-viete n S))
     (check-true (> cur prev) "Viete's product increases toward pi")
     (check-true (< cur (fp-v PI-REF)) "Viete's product stays below pi")
     cur)
   (check-equal? (frac (fp (pi-viete 250 S) S) 99) PI-99)))

(define slow-pi-tests
  (test-suite
   "historical pi series"
   (check-equal? (fp-display-str (fp (pi-leibniz 1 S) S) 3) "4.000")
   (check-equal? (fp-display-str (fp (pi-leibniz 2 S) S) 3) "2.666")     ; 8/3
   (check-equal? (fp-display-str (fp (pi-nilakantha 1 S) S) 3) "3.166")  ; 3 + 1/6
   ;; an alternating series errs by less than its first omitted term: 1/(2n)
   (for ([n (in-list '(10 1000 100000))])
     (define err (abs (- (pi-leibniz n S) (fp-v PI-REF))))
     (check-true (< err (quotient (* 4 (expt 10 S)) (* 2 n)))
                 (format "Leibniz error at ~a terms exceeds 1/(2n)" n)))
   ;; measured accuracy at these term counts, with a digit of slack
   (check-true (>= (correct-digits (fp (pi-leibniz 200000 S) S) PI-REF 20) 4))
   (check-true (>= (correct-digits (fp (pi-nilakantha 20000 S) S) PI-REF 20) 12))
   (check-true (>= (correct-digits (fp (pi-wallis 100000 S) S) PI-REF 20) 4))
   ;; Nilakantha beats Leibniz badly at equal term counts
   (check-true (> (correct-digits (fp (pi-nilakantha 1000 S) S) PI-REF 30)
                  (correct-digits (fp (pi-leibniz 1000 S) S) PI-REF 30)))))

(define bbp-tests
  (test-suite
   "BBP spigot"
   (check-equal? (pi-bbp-hex 1 32) PI-HEX-32)
   ;; deep digits must agree with the decimal expansion converted to hex
   (let ([ref (reference-value 'pi 400)])
     (check-equal? (string-downcase (pi-bbp-hex 100 16))
                   (substring (fp->hex-frac-digits ref 300) 99 115))
     (check-equal? (string-downcase (pi-bbp-hex 250 8))
                   (substring (fp->hex-frac-digits ref 300) 249 257)))))

(define e-tests
  (test-suite
   "e algorithms"
   (check-equal? (frac E-REF 99) E-99)
   (check-equal? (frac (reference-value 'e 99) 99) E-99)
   ;; 500 terms of the series settle 1000 digits
   (check-equal? (fp->digits (fp (e-series 1025) 1025) 1000)
                 (fp->digits (fp (e-series-terms 500 1025) 1025) 1000))
   ;; exact rational partial sums, hand-checked
   (check-equal? (e-series-exact 1) 1)
   (check-equal? (e-series-exact 2) 2)
   (check-equal? (e-series-exact 3) 5/2)
   (check-equal? (e-series-exact 4) 8/3)
   (check-equal? (e-series-exact 5) 65/24)
   (check-equal? (e-series-exact 6) 163/60)
   ;; consecutive partial sums differ by exactly the next term, 1/k!
   (check-equal? (- (e-series-exact 41) (e-series-exact 40))
                 (/ 1 (for/product ([i (in-range 1 41)]) i)))
   (check-true (< (e-series-exact 40) (e-series-exact 60)))
   ;; the decimal view of the exact sum agrees with the fixed-point one
   (check-equal? (fp->digits (fp (e-series-exact 60) 0) 50)
                 (fp->digits (fp (e-series-terms 60 80) 80) 50))
   ;; (1 + 1/n)^n climbs toward e from below and never passes it
   (for/fold ([prev 0]) ([n (in-list '(1 10 1000 100000))])
     (define cur (e-limit n S))
     (check-true (> cur prev) "the limit definition increases with n")
     (check-true (< cur (fp-v E-REF)) "the limit definition stays below e")
     cur)
   (check-true (>= (correct-digits (fp (e-limit 1000000 S) S) E-REF 20) 5))
   (check-true (> (correct-digits (fp (e-limit 10000000 S) S) E-REF 20)
                  (correct-digits (fp (e-limit 1000 S) S) E-REF 20)))
   ;; continued fraction convergents, hand-checked
   (check-equal? (e-continued-fraction 1) 2)
   (check-equal? (e-continued-fraction 2) 3)
   (check-equal? (e-continued-fraction 3) 8/3)
   (check-equal? (e-continued-fraction 4) 11/4)
   (check-equal? (e-continued-fraction 5) 19/7)
   (check-equal? (e-continued-fraction 6) 87/32)
   (check-equal? (e-continued-fraction 7) 106/39)
   (check-equal? (e-continued-fraction 8) 193/71)
   (check-equal? (fp->digits (fp (e-continued-fraction 80) 0) 50)
                 (fp->digits (reference-value 'e 50) 50))))

(define method-table-tests
  (test-suite
   "method table"
   (check-equal? (length methods) 14)
   (check-equal? (length (remove-duplicates (map method-id methods))) 14)
   (check-equal? (length (filter (lambda (m) (eq? 'pi (method-constant m))) methods)) 9)
   (check-equal? (length (filter (lambda (m) (eq? 'e (method-constant m))) methods)) 5)
   ;; lookup by index and by id agree
   (for ([i (in-range 1 (add1 (length methods)))])
     (define m (method-lookup (number->string i)))
     (check-not-false m)
     (check-eq? m (method-lookup (symbol->string (method-id m)))))
   (check-false (method-lookup "no-such-method"))
   (check-false (method-lookup "99"))
   ;; every method runs at its default inputs and reports something usable
   (for ([m (in-list methods)])
     (define inputs (method-compare-inputs m))
     (for ([spec (in-list (method-inputs m))])
       (check-true (hash-has-key? inputs (input-spec-key spec))
                   (format "~a compare-inputs is missing ~a"
                           (method-id m) (input-spec-key spec)))
       (check-true (<= (hash-ref inputs (input-spec-key spec)) (input-spec-cap spec))
                   (format "~a compare-inputs exceeds the cap on ~a"
                           (method-id m) (input-spec-key spec))))
     (define digits (max 1 ((method-auto-digits m) inputs)))
     (define out (run-method m inputs digits))
     (check-pred outcome? out (format "~a produced no outcome" (method-id m)))
     (check-pred pair? (outcome-lines out) (format "~a printed nothing" (method-id m)))
     (check-not-false (or (outcome-value out) (outcome-hex out))
                      (format "~a produced no value" (method-id m))))))

(define outcome-format-tests
  (test-suite
   "outcome formatting"
   (let*-values ([(one-seventh)
                  (values (fp (quotient (expt 10 65) 7) 65))]   ; 1/7 = 0.142857...
                 [(int-str frac-str digits-str)
                  (fp->decimal one-seventh 60)]
                 [(bbp-out)
                  (values (run-method (method-lookup "bbp") (hash 'start 1 'count 8) 1))]
                 [(arch-lines)
                  (values (outcome-lines
                           (run-method (method-lookup "archimedes")
                                       (hash 'iterations 40) 24)))])
     (check-equal? int-str "0")
     (check-equal? frac-str (string-append "1428571428" "5714285714" "2857142857"
                                           "1428571428" "5714285714" "2857142857"))
     (check-equal? digits-str (string-append "0" frac-str))
     ;; 60 digits = 6 groups of ten, five groups per line
     (check-equal? (format-number-lines int-str frac-str 200)
                   (list "  0.1428571428 5714285714 2857142857 1428571428 5714285714"
                         "    2857142857"))
     ;; a short display limit truncates and says so, showing the tail
     (check-equal? (format-number-lines int-str frac-str 25)
                   (list "  0.1428571428 5714285714 28571"
                         "    ... showing 25 of 60 digits; last 10: 2857142857"))
     ;; zero digits prints no decimal point
     (check-equal? (format-number-lines "3" "" 200) (list "  3"))
     ;; the BBP outcome carries hex digits instead of a decimal value
     (check-false (outcome-value bbp-out))
     (check-equal? (outcome-hex bbp-out) (substring PI-HEX-32 0 8))
     ;; Archimedes reports both bounds plus how many digits they pin down
     (check-true (string-contains? (car arch-lines) "inscribed 6597069766656-gon"))
     (check-true (ormap (lambda (l) (string-contains? l "circumscribed")) arch-lines))
     (check-true (ormap (lambda (l) (string-contains? l "digits pinned by the bracket: 24"))
                        arch-lines)))))

(run-tests
 (test-suite
  "math.rkt"
  decimal-tests
  fast-pi-tests
  archimedes-tests
  viete-tests
  slow-pi-tests
  bbp-tests
  e-tests
  method-table-tests
  outcome-format-tests))
```

The `method-table-tests` suite deserves a special mention because it tests the
framework rather than the mathematics: it asserts that the registry holds 14
methods with unique ids, that lookup by number and by name agree, and that every
single method runs to a usable outcome at its default inputs. If you add a
fifteenth method and forget a compare input or exceed a cap, this suite fails
immediately and tells you which method is misconfigured.

## Running the Code

The program needs only Racket itself, with no external packages. Running the
file with no arguments starts the interactive menu; the `--list`, `--compare`,
and `--show` flags and the `key=value` input syntax cover the non-interactive
uses. The following outputs were captured from real runs of the program.

Listing the method table:

```
$ racket math.rkt --list
  #  id               method                                 input
  -- ---------------- -------------------------------------- -------------
   1 leibniz          Gregory-Leibniz series                 terms
   2 nilakantha       Nilakantha series                      terms
   3 wallis           Wallis product                         terms
   4 viete            Viete's nested radicals                terms
   5 archimedes       Archimedes polygon doubling            iterations
   6 machin           Machin-like arctangent formula         digits
   7 agm              Gauss-Legendre / Brent-Salamin AGM     digits
   8 chudnovsky       Chudnovsky binary splitting            digits
   9 bbp              BBP hex-digit spigot                   start,count

  10 e-series         Exponential series to N digits         digits
  11 e-series-terms   Exponential series, first n terms      terms
  12 e-series-exact   Exponential series as an exact fraction terms
  13 e-limit          Limit definition (1 + 1/n)^n           n
  14 e-cf             Continued fraction convergents         terms
```

Running one method, Machin's formula, at 20 digits:

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

Archimedes' method shows both sides of its bracket, along with the polygon size
and the number of digits the bracket certifies:

```
$ racket math.rkt --show 60 archimedes iterations=25
  Archimedes polygon doubling
    a <- 2ab/(a+b); b <- sqrt(a*b), from the hexagon a=2*sqrt(3), b=3
    linear: about 0.6 digits per doubling, but every step brackets pi exactly
  inputs: iterations=25
    inscribed 201326592-gon (lower bound):
  3.1415926535 89793
    circumscribed 201326592-gon (upper bound): 3.141592653589793
    digits pinned by the bracket: 15
  check: all 15 displayed digits match the reference (chudnovsky at 25 digits)
  time: 49 us
```

The BBP spigot pulling 16 hexadecimal digits out of nowhere:

```
$ racket math.rkt bbp start=1 count=16
  BBP hex-digit spigot
    pi = sum 16^-k (4/(8k+1) - 2/(8k+4) - 1/(8k+5) - 1/(8k+6))
    a spigot: digit d in base 16 with none of the earlier digits computed
  inputs: start=1, count=16
    hex digits 1..16 of pi, after the decimal point:
  3.243F6A8885A308D3
  check: all 16 hex digits match the reference pi
  time: 59 us
```

The Chudnovsky method at 500 digits, with the display limited to the first 100:

```
$ racket math.rkt --show 100 chudnovsky digits=500
  Chudnovsky binary splitting
    1/pi = 12 * sum (-1)^k (6k)!(13591409+545140134k) / ((3k)!(k!)^3 640320^(3k+3/2))
    about 14.18 digits per term; used for every modern pi record
  inputs: digits=500
  3.1415926535 8979323846 2643383279 5028841971 6939937510
    5820974944 5923078164 0628620899 8628034825 3421170679
    ... showing 100 of 500 digits; last 10: 8301194912
  check: all 500 displayed digits match the reference (chudnovsky at 510 digits)
  time: 76 us
```

The comparison report runs all fourteen methods at their default inputs and
tabulates time, correct digits, and value:

```
$ racket math.rkt --compare

  Every method at its default input:

  method           input              time         correct digits  value
  leibniz          terms=200000       7.23 ms      4               3.1415
  nilakantha       terms=5000         458 us       11              3.14159265358
  wallis           terms=50000        13 ms        3               3.141
  viete            terms=40           46 us        20              3.141592653589793238
  archimedes       iterations=30      40 us        18              3.141592653589793238
  machin           digits=50          6 us         20              3.141592653589793238
  agm              digits=50          12 us        20              3.141592653589793238
  chudnovsky       digits=50          5 us         20              3.141592653589793238
  bbp              start=1,count=24   116 us       24 hex          3.243F6A8885A308D313198A2E
  e-series         digits=50          6 us         20              2.718281828459045235
  e-series-terms   terms=40           6 us         20              2.718281828459045235
  e-series-exact   terms=25           9 us         20              2.718281828459045235
  e-limit          n=100000           8 us         4               2.7182
  e-cf             terms=40           11 us        20              2.718281828459045235
```

Finally, the test suite:

```
$ racket math-tests.rkt
246 success(es) 0 failure(s) 0 error(s) 246 test(s) run
```

## Interpreting the Results

The comparison table is the payoff of the entire chapter, because it makes the
theoretical convergence rates from the opening section visible as measured
facts.

Look at the two extremes. Leibniz burns 200,000 terms and 7.23 milliseconds to
produce 4 correct digits. Chudnovsky computes 50 correct digits in 5
microseconds, using about 5 terms of its series. That is roughly a thousandfold
difference in work for more than ten times the accuracy, and it is exactly what
the error bounds predicted: `1/(2n)`$ error for Leibniz versus 14.18 digits per
term for Chudnovsky. The Nilakantha row sits between them, 11 digits from 5,000
terms, matching its cubic convergence. When theory and measurement line up this
cleanly, you know both the formulas and the implementations are right.

The `e-limit` row teaches the complementary lesson. The limit definition
`(1 + 1/n)^n`$ is how `e`$ is usually introduced, yet 100,000 exponentiations
worth of input yields only 4 digits. The continued fraction, which almost never
appears in a first calculus course, produces 20 digits from 40 partial
quotients. The choice of representation matters more than the choice of
programming language or the speed of the machine.

The Archimedes run deserves a second look. After 25 doublings the program is
tracking polygons with 201,326,592 sides, and it reports "digits pinned by the
bracket: 15." That number is different in kind from every other digit count in
the chapter. It is not a statistical statement about agreement with another
algorithm; it is a proof. The true value of `\pi`$ is guaranteed to lie between
the inscribed and circumscribed values, so those 15 digits are certain.
Archimedes had a theorem, not just an approximation, and the program preserves
that property.

The verification lines answer the question posed at the start: how do you know
the digits are right? Every `\pi`$ run is checked against the Chudnovsky series
computed at higher precision, and every `e`$ run against the exponential
series. The test suite goes further and makes the fast algorithms check each
other at 1000 digits, where the probability that two independent buggy
implementations agree by chance is effectively zero. The BBP check is the most
satisfying: the spigot works in floating point in base 16, the reference works
in exact integers in base 10, and the hex digits match anyway.

Timing is the one place to be careful with interpretation. The microsecond
timings for Machin, AGM, and Chudnovsky at 50 digits reflect that Racket's
arithmetic on small bignums is extremely fast; at 100,000 digits the picture
separates, with binary splitting pulling ahead of the AGM's repeated full
precision square roots. The compare table is a convergence demonstration, not a
benchmark.

## Wrap Up

This chapter used two of the oldest computational problems in mathematics as a
vehicle for three transferable ideas. The fixed-point representation
`v / 10^s`$ replaces floating point with exact integer arithmetic whenever
results are judged digit by digit, and the same pattern serves in financial
calculation and anywhere else rounding is the enemy. The method registry shows
how a table of structures with function fields can replace a pile of special
case user interface code; the menu, the command line, the comparison report,
and the test suite all iterate the same fourteen entries. And the layered test
strategy, pin against published data, cross-check independent algorithms, then
verify structural guarantees, is a template for testing any numerical code
where a single "expected value" cannot be typed into the test file by hand.

Along the way the chapter walked through twenty five centuries of mathematical
history, from Archimedes' bracketed polygons to a spigot formula that produces
isolated hexadecimal digits, and saw each era's algorithm run, verified, in a
few microseconds.

## Optional Practice Problems

The following exercises extend the example code. They are ordered roughly from
gentle to ambitious.

1. **Euler's arctangent identity.** Machin's formula is not the only identity of
   its kind. Euler showed that `\pi/4 = \arctan(1/2) + \arctan(1/3)`$.
   Implement `(pi-euler-arctan s)` using the existing `arctan-recip` helper,
   register it as a fifteenth method in the `methods` table, and confirm that
   `racket math-tests.rkt` still passes (remember that `method-table-tests`
   counts the methods). How many correct digits does it produce at the same
   scale where `pi-machin` produces 50? Explain the difference in terms of the
   convergence of `\arctan(1/x)`$ for larger `x`$.

2. **Measure the convergence rates yourself.** Write a small script that
   requires `math.rkt` and, for the Leibniz, Nilakantha, and Wallis methods,
   computes the number of correct digits (using `correct-digits`-style
   comparison against `reference-value`) at inputs of 100, 1000, 10000, and
   100000. Tabulate digits gained per tenfold increase and compare against the
   theoretical rates of 1 and 3 digits per tenfold stated in the registry. Why
   does the measured Nilakantha rate sometimes look better than the theory?

3. **Ramanujan's series.** In 1914 Ramanujan published the astonishing formula

```$
\frac{1}{\pi} = \frac{2\sqrt{2}}{9801} \sum_{k=0}^{\infty} \frac{(4k)!\, (1103 + 26390 k)}{(k!)^4\, 396^{4k}}
```

   Implement it in the same fixed-point style as `pi-chudnovsky` (you do not
   need binary splitting; a direct term recurrence is fine at moderate scales),
   register it as a method, and verify it against the reference at 200 digits.
   About how many digits does each term buy, and how does that compare with
   Chudnovsky's 14.18?

4. **A spigot for log 2.** The BBP idea works for any constant with a suitable
   series. The natural logarithm of 2 satisfies
   `\ln 2 = \sum_{k \geq 1} 1 / (k \, 2^k)`$, a BBP-type series in base 2.
   Adapt `modpow16` and `bbp-series` to extract binary (or hexadecimal) digits
   of `\ln 2`$ at a given position, and test them against exact rational
   partial sums of the series converted to hex with `fp->hex-frac-digits`.

5. **Continued fractions are best approximations.** A convergent `p/q`$ of the
   continued fraction for `e`$ satisfies
   `|e - p/q| < 1/q^2`$. Add a test suite that checks this inequality exactly
   for the first 40 convergents: compute the rational error using
   `e-series-exact` at high term count as the reference value of `e`$, and
   verify `|e_{\text{ref}} - p/q| \cdot q^2 < 1`$ with pure rational
   arithmetic. Then check the stronger property that each convergent is closer
   to `e`$ than any fraction with a smaller denominator, for denominators up to
   1000.
