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
