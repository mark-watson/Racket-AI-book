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
