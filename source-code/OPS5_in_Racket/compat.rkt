;; compat.rkt
;;
;; Compatibility definitions used to convert the MIT-Scheme dialect of
;; Mark Watson's 1995 "OPS5 in Scheme" source files (ops5.s, compiler.s,
;; network.s, rhs.s, temp.s, lit.s) to Racket.
;;
;; This file is loaded into the OPS5 namespace by load.rkt *before* any
;; of the converted system files, so that names the original code took
;; for granted (t, nil, mapcar, while, 1+, set-car!, ...) are available.
;;
;; Notes on the conversions:
;;   * The original code mutates pair structures in place with
;;     set-car!/set-cdr! (RETE network nodes, token memories, property
;;     lists, ...).  Racket's own pairs are immutable, but the R5RS
;;     compatibility language provides mutable pairs with consistent
;;     pair?/car/cdr/equal?/write semantics, so it is required here and
;;     the pair-based definitions below build on it.
;;   * R5RS has no eval or error; Racket's versions are re-imported
;;     under new names and re-bound.
;;   * Racket cannot define an identifier that reads as a number, so
;;     `+1` calls were rewritten to `add1` in the converted files, while
;;     `1+` and `-1+` (which read fine as identifiers) are aliased here.
;;   * `#!TRUE` / `#!FALSE` / `#!false` are MIT reader syntax; the
;;     converted files use #t / #f instead.
;;   * (->pair sym) returns a cons cell whose cdr holds the symbol's
;;     property list; implemented here with a global hash table.

;; NOTE: r5rs is required with `only-in` rather than wholesale.  The
;; full r5rs language shadows Racket's `lambda`/`define`/... syntax
;; bindings in the namespace, which makes Racket's `eval` return forms
;; unevaluated; importing just the pair/data functions keeps `eval`
;; working.  The lenient list procedures (memq, member, assq, assoc,
;; length, append) are intentionally not imported and are redefined
;; below.
(require (except-in r5rs eval lambda)
                  )
(require (only-in r5rs
                  cons car cdr set-car! set-cdr! pair? null? list list? reverse
                  caar cadr cdar cddr
                  caaar caadr cadar caddr cdaar cdadr cddar cdddr
                  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                  map for-each apply
                  = < > <= >= max min + - * / abs gcd lcm not eq? eqv? equal?
                  procedure? boolean? symbol? number? complex? real? rational?
                  integer? exact? inexact? zero? positive? negative? odd? even?
                  quotient remainder modulo floor ceiling truncate round
                  expt sqrt
                  symbol->string string->symbol number->string string->number
                  string? string string-ref string-set! string=? substring
                  string-copy string-length string-append string-fill!
                  string->list list->string
                  vector? make-vector vector vector-ref vector-set!
                  vector-length vector->list list->vector vector-fill!
                  char? char=? char<? char>? char<=? char>=?
                  char->integer integer->char char-upcase char-downcase
                  char-alphabetic? char-numeric? char-whitespace?
                  read read-char peek-char eof-object? write display newline
                  write-char flush-output
                  open-input-file open-output-file close-input-port
                  close-output-port input-port? output-port?
                  current-input-port current-output-port current-error-port
                  call-with-output-file call-with-input-file
                  with-input-from-file with-output-to-file
                  values call-with-values dynamic-wind
                  list-tail list-ref
                  ))
(require (only-in racket/base [eval rkt-eval] [error rkt-error]
                  [cons rkt-cons] [reverse rkt-reverse]
                  [car rkt-car] [cdr rkt-cdr]))
(require (for-syntax racket))

;; Racket's expander rejects forms built from r5rs mutable pairs and
;; returns them unevaluated; eval sites here build forms at runtime with
;; r5rs cons (e.g. cmp-p's RHS closure), so deep-copy the form into base
;; pairs before evaluating.
(define (to-base-pairs x)
  (cond ((pair? x)
         (rkt-cons (to-base-pairs (car x)) (to-base-pairs (cdr x))))
        (else x)))
(define (eval x . ns)
  (if (null? ns)
      (rkt-eval (to-base-pairs x))
      (rkt-eval (to-base-pairs x) (car ns))))
(define error rkt-error)

;; ---- truth values ----
(define t #t)
(define nil '())

;; ---- list predicates (MIT names) ----
(define (atom? x) (not (pair? x)))
(define proper-list? list?)
(define listp list?)
(define symbolp symbol?)

;; ---- lenient list procedures (MIT Scheme semantics) ----
;; The R5RS-derived versions require proper lists; the OPS5 code passes
;; #f and atoms around freely, so lenient versions are provided.  They
;; are defined with a separate implementation name and then aliased,
;; because a plain (define (member ...) ...) at the top level would
;; leave the recursive calls inside the body bound to the previously
;; required strict `member`, not to this definition.
(define (mapcar-impl f l)
  (cond ((null? l) '())
        ((pair? l) (cons (f (car l)) (mapcar-impl f (cdr l))))
        (else #f)))
(define mapcar mapcar-impl)

(define (mapc-impl f l)
  (cond ((null? l) #f)
        ((pair? l) (f (car l)) (mapc-impl f (cdr l)))
        (else #f)))
(define mapc mapc-impl)

(define (append-impl . lsts)
  (define (append2 l1 l2)
    (if (pair? l1) (cons (car l1) (append2 (cdr l1) l2)) l2))
  (cond ((null? lsts) '())
        ((null? (cdr lsts)) (car lsts))
        (else (append2 (car lsts) (apply append-impl (cdr lsts))))))
(define append append-impl)

(define (length-impl l)
  (cond ((pair? l) (+ 1 (length-impl (cdr l))))
        (else 0)))
(define length length-impl)

(define (memq-impl x l)
  (cond ((pair? l) (if (eq? x (car l)) l (memq-impl x (cdr l))))
        (else #f)))
(define memq memq-impl)

(define (member-impl x l)
  (cond ((pair? l) (if (equal? x (car l)) l (member-impl x (cdr l))))
        (else #f)))
(define member member-impl)

(define (assq-impl x l)
  (cond ((pair? l) (if (eq? x (caar l)) (car l) (assq-impl x (cdr l))))
        (else #f)))
(define assq assq-impl)

(define (assoc-impl x l)
  (cond ((pair? l) (if (equal? x (caar l)) (car l) (assoc-impl x (cdr l))))
        (else #f)))
(define assoc assoc-impl)

(define (remove-impl x l)
  (cond ((null? l) '())
        ((pair? l)
         (if (equal? x (car l))
             (remove-impl x (cdr l))
             (cons (car l) (remove-impl x (cdr l)))))
        (else #f)))
(define remove remove-impl)

(define (delq-impl x l)
  (cond ((null? l) '())
        ((pair? l)
         (if (eq? x (car l))
             (delq-impl x (cdr l))
             (cons (car l) (delq-impl x (cdr l)))))
        (else #f)))
(define delq delq-impl)

(define (delete-impl x l) (remove-impl x l))
(define delete delete-impl)

;; Racket's sort requires proper immutable Racket lists; the input is
;; copied into base pairs for sorting and the sorted result is copied
;; back into the (mutable) pair representation the rest of the code
;; expects, since other functions car/cdr the result.
(define racket-sort sort)
(define (sort-impl l less?)
  (define (to-base l acc)
    (if (pair? l)
        (to-base (cdr l) (rkt-cons (car l) acc))
        (rkt-reverse acc)))
  (define (to-mut l acc)
    (if (pair? l)
        (to-mut (rkt-cdr l) (cons (rkt-car l) acc))
        (rkt-reverse acc)))
  (if (pair? l)
      (to-mut (racket-sort (to-base l '()) less?) '())
      l))
(define sort sort-impl)

;; ---- numbers ----
(define 1+ add1)
(define -1+ sub1)
(define (numberp x) (number? x))
(define realp real?)
(define int truncate)

;; ---- MIT print: display the arguments and a newline ----
(define (print . args)
  (for-each display args)
  (newline))

;; ---- while loop (MIT special form) ----
;; MIT Scheme treats the empty list (nil) as false, so `(while keep-going
;; ...)` must stop when keep-going becomes '() as well as #f.  Racket
;; considers '() true, hence the explicit truthiness check.
(define (mit-true? x) (and x (not (null? x))))
(define-syntax-rule (while test body ...)
  (let loop () (when (mit-true? test) body ... (loop))))

;; ---- symbol property lists: (cdr (->pair sym)) is the plist ----
(define symbol-plists (make-hasheq))
(define (->pair var)
  (hash-ref! symbol-plists var (lambda () (cons 'plist '()))))

;; ---- globals that the original code set! without ever defining ----
(define include-source-code? #f)
(define p-list #f)
(define iport #f)
(define leftcurly "{")
(define rightcurly "}")

(define *phase* #f)
(define *halt-flag* #f)
(define *break-flag* #f)
(define *remaining-cycles* 32000)
(define *cycle-count* 0)
(define *total-token* 0)
(define *current-token* 0)
(define *max-token* 0)
(define *total-wm* 0)
(define *current-wm* 0)
(define *max-wm* 0)
(define *total-cs* 0)
(define *max-cs* 0)
(define *action-count* 0)
(define *pcount* 0)
(define *conflict-set* #f)
(define *filters* #f)
(define *record-index* 0)
(define *recording* #f)
(define *max-record-index* 0)
(define *record* #f)
(define *refracts* #f)
(define *wtrace* #f)
(define *ptrace* #f)
(define *brkpts* #f)
(define *strategy* 'lex)
(define *in-rhs* #f)
(define *wm* #f)
(define *wmpart-list* #f)
(define *vector-list* #f)
(define *buckets* #f)
(define *accept-file* #f)
(define *write-file* #f)
(define *trace-file* #f)
(define *class-list* #f)
(define *finish-lit?* #t)
(define *max-index* 0)
(define *next-index* 1)
(define *real-cnt* 0)
(define *virtual-cnt* 0)
(define *limit-token* 32000)
(define *limit-cs* 32000)
(define *critical* #f)
(define *build-trace* #f)
(define *size-result-array* 127)
(define *result-array* #f)
(define *record-array* #f)
(define *first-node* #f)
(define *p-name* #f)
(define *matrix* #f)
(define *pname* #f)
(define *feature-count* 0)
(define *ce-count* 0)
(define *vars* #f)
(define *ce-vars* #f)
(define *rhs-bound-vars* #f)
(define *rhs-bound-ce-vars* #f)
(define *last-branch* #f)
(define *last-node* #f)
(define *subnum* 0)
(define *cur-vars* #f)
(define *curcond* #f)
(define *sendtocall* #f)
(define *side* #f)
(define *flag-part* #f)
(define *data-part* #f)
(define *alpha-flag-part* #f)
(define *alpha-data-part* #f)
(define *old-wm* #f)
(define *wm-filter* #f)
(define *data-matched* #f)
(define *last* #f)
(define *variable-memory* #f)
(define *ce-variable-memory* #f)
(define *action-type* #f)
(define *ats* '())

;; ---- procedures the original code references but never defines ----
(define (reset) (error "OPS5: reset called"))

;; externalp/external: the original code declares RHS functions
;; "external"; with everything in one namespace this is a no-op.
(define (externalp . args) t)
(define (external . args) t)

(define (remove-old-wms) (process-changes #f (get-wm #f)))
