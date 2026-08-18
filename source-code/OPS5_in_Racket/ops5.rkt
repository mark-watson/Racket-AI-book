#lang racket/load
;; ops5.rkt -- the complete OPS5-in-Racket system as pure code (no embedded string).
;; racket/load gives load-like top-level semantics: redefinitions and
;; (require ...) forms behave as they did under load.rkt / the namespace hack.

;; =====================================================================
;; 1. compat.rkt (compatibility layer)
;; =====================================================================
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
;; =====================================================================
;; 2. ops5.rkt (top-level OPS5 commands)
;; =====================================================================
;; File: OPS5.RKT
;;
;; Converted from ops5.s (Mark Watson, "OPS5 in Scheme", 1995) to run
;; under Racket.  Loaded into the OPS5 namespace by load.rkt after
;; compat.rkt; see README.md.
;;
;; The MIT-Scheme `macro` special forms below are converted to Racket
;; define-syntax.  The transformers build their expansions as data and
;; quote the arguments (the original transformers returned lists that
;; MIT Scheme would then re-evaluate, which Racket's hygienic expander
;; cannot do); this preserves the intended runtime behavior, e.g.
;; (p name lhs --> rhs) still hands the whole, unevaluated production
;; to old-p.

;; SchemeOPS5 macro definitions.

(define local-eval
  (lambda (x)
    (eval x (current-namespace))))

(define-syntax p
  (lambda (stx)
    (syntax-case stx ()
      [(_ . rest)
       (datum->syntax stx
         (list 'old-p (list 'quote (syntax->datum stx))))])))

(define-syntax accept
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (list 'old-accept (list 'quote (syntax->datum #'args))))])))

(define-syntax compute
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (list '!value (list 'ari (list 'quote (syntax->datum #'args)))))])))

(define-syntax make
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (cons 'old-make
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define (old-make . l)
  (!reset)
  (eval-args l)
  (!assert))

(define-syntax modify
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (cons 'old-modify
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define-syntax ops-write
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (cons 'old-ops-write
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define-syntax literalize
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (cons 'old-literalize
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))
                 
(define (old-ops-write . z) ;; name change 5/9/86
  (let ((port #f) (max #f) (k #f) (x #f) (needspace #f))
    
    (define (loop)
     (cond ((> k max)
            #f)
           (t
              (set! x (!parameter k))
              (cond ((equal? x '"=== C R L F ===")
                     (set! needspace #f)
                     (newline))
                    ((equal? x '"=== R J U S T ===")
                     (set! k (+ 2 k))
                     (do-rjust (!parameter (sub1 k)) (!parameter k) port))
                    ((equal? x '"=== T A B T O ===")
                     (set! needspace #f)
                     (set! k (add1 k))
                     (do-tabto (!parameter k) port))
                    (t 
                     (and needspace (display " "))
                     (set! needspace t)
                     (display x)))
              (set! k (add1 k))
              (loop))))
    
    (cond ((not *in-rhs*)
           (%warn "Cannot be called at top level" 'ops-write)
           #f)
          (t  (!reset)
              (eval-args z)
              (set! k 1)
              (set! max (!parametercount))
              (cond ((< max 1)
                     (%warn "OPS-Write: nothing to print" z)
                     #f)
                    (t  (set! needspace t)
                        (loop)))))
    #f))
                 
(define-syntax ops-remove
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (cons 'old-remove
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))
      

(define-syntax strategy
  (lambda (stx)
    (syntax-case stx ()
      [(_) (datum->syntax stx '(old-strategy 'none))]
      [(_ . args)
       (datum->syntax stx
         (cons 'old-strategy
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define-syntax run
  (lambda (stx)
    (syntax-case stx ()
      [(_) (datum->syntax stx '(old-run 32000))]
      [(_ . args)
       (datum->syntax stx
         (cons 'old-run
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define-syntax watch
  (lambda (stx)
    (syntax-case stx ()
      [(_) (datum->syntax stx '(old-watch 'none))]
      [(_ . args)
       (datum->syntax stx
         (cons 'old-watch
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define-syntax pbreak
  (lambda (stx)
    (syntax-case stx ()
      [(_) (datum->syntax stx '(old-pbreak 'none))]
      [(_ . args)
       (datum->syntax stx
         (cons 'old-pbreak
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define-syntax ppwm
  (lambda (stx)
    (syntax-case stx ()
      [(_) (datum->syntax stx '(old-ppwm 'none))]
      [(_ . args)
       (datum->syntax stx
         (cons 'old-ppwm
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define-syntax matches
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (list 'mapc 'matches2 (list 'quote (syntax->datum #'args))))])))

(define-syntax wm
  (lambda (stx)
    (syntax-case stx ()
      [(_) (datum->syntax stx '(old-wm #f))]
      [(_ . args)
       (datum->syntax stx
         (cons 'old-wm
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))


;;; OPS5 Main functions

(define (main)
  (let ((instance #f) (r #f))
    
     (define (loop)
        (set! *phase* 'conflict-resolution)
	       (cond ((and #f (equal? (peek-char) 13))  ;; skip this logic because of #F clause
               (set! *halt-flag* t)
               (set!  *break-flag* t)
               (read-char)
               (newline)
               (display "Interrupted by a keystroke")
               (newline))
              (t	
                (cond (*halt-flag*
                       (set! r "End -- explicit halt")
                       (finis))
                      ((zero? *remaining-cycles*)
                       (set! r "***break***")
                       (set! *break-flag* t)
                       (finis))
                      (*break-flag*
                       (set! r "***break***")
                       (finis))
                      (t
                        (set! *remaining-cycles* (-1+ *remaining-cycles*))
                        (set! instance (conflict-resolution))
                        (cond ((not instance)
                               (set! r "End -- no production true")
                               (finis))
                              (t
                               (set! *phase* (car instance))
                               (accum-stats)
                               (eval-rhs (car instance) (cdr instance))
                               (check-limits)
                               (and
                                (broken (car instance))
                                (set! *break-flag* t))
                                (loop))))))))
  (define (finis)
     (set! *p-name* #f)
     r)
    
  (set! *halt-flag* #f)
  (set! *break-flag* #f)
  (set! instance #f)
  (loop)))




(define (process-changes adds dels)
  (let ((x #f))

       (define (process-deletes)
      (if (atom? dels)
          (process-adds)
          (begin
           (set! x (car dels))
           (set! dels (cdr dels))
           (remove-from-wm x)
           ( process-deletes))))
   (define (process-adds)
      (if (atom? adds)
          #f
          (begin
           (set! x (car adds))
           (set! adds (cdr adds))
           (add-to-wm x #f)
           (process-adds))))    
    
  (process-deletes)))

(define (do-continue wmi)
    (cond (*critical*
           (newline)
           (write "Warning: network may be inconsistent")))
    (process-changes wmi #f)
    (print-times (main)))

(define (accum-stats)
  (set! *cycle-count* (1+ *cycle-count*))
  (set! *total-token* (+ *total-token* *current-token*))
  (cond ((> *current-token* *max-token*)
         (set! *max-token* *current-token*)))
  (set! *total-wm* (+ *total-wm* *current-wm*))
  (cond ((> *current-wm* *max-wm*) (set! *max-wm* *current-wm*))))

(define (print-times mess)
  (let ((cc #f) (ac #f))
        (if  *break-flag*
             (begin (newline) mess)
             (begin
               (set! cc (if (= 0 *cycle-count*) 1 *cycle-count*))
               (set! ac  *action-count*)
               (newline)
               (display mess)
               (newline)
               (pm-size)
               (display (list *cycle-count*
		                            "firings"
		                            (list *action-count* "RHS actions")))
	              (display (list (round (/  *total-wm* cc))
		                            "Mean working memory size"
		                            (list *max-wm* "maximum")))
	              (display (list (round (/  *total-cs* cc))
		                            "mean conflict set size"
		                            (list *max-cs* "maximum")))
	              (display (list (round (/  *total-token* cc))
		                            "mean token memory size"
		                            (list *max-token* "maximum")))))))

(define (pm-size)
  (newline)
  (display (list *pcount*
                 "productions"
                 (list *real-cnt* '// *virtual-cnt* "nodes"))))

(define (check-limits)
  (cond ((> (length *conflict-set*) *limit-cs*)
         (newline)
         (newline)
         (display (list "Conflict set size exceeded the limit of"
                        *limit-cs*
                        "after"
                        *p-name*))
         (set! *halt-flag* t)))
  (cond ((> *current-token* *limit-token*)
         (newline)
         (newline)
         (display (list "Token memory size exceeded the limit of"
                         *limit-token*
                         "after"
                         *p-name*))
         (set! *halt-flag* t))))
         
         ;;; Top level routines


(define (top-level-remove z)
  (cond ((equal? z '(*)) (process-changes #f (get-wm #f)))
        (t (process-changes #f (get-wm z)))))

(define (old-excise z) 
 (mapc excise-p z))

(define (old-run z)
  (set! *remaining-cycles* z)
  (do-continue #f))

(define (old-strategy z)
  (cond ((equal? z 'none)
	        *strategy*)
	       ((equal? z 'lex)
	        (set! *strategy* 'lex))
	       ((equal? z 'mea)
	        (set! *strategy* 'mea))
	       (t "what?")))
  
(define (cs) (conflict-set))

(define (old-watch z)
  (cond ((equal? z 0)
         (set! *wtrace* #f)
         (set! *ptrace* #f)
         0)
        ((equal? z 1) (set! *wtrace* #f) (set! *ptrace* t) 1)
        ((equal? z 2) (set! *wtrace* t) (set! *ptrace* t) 2)
        ((equal? z 3)
         (set! *wtrace* t)
         (set! *ptrace* t)
         "2. -- conflict set trace not supported")
        ((and (equal? z 'none) (null? *ptrace*)) 0)
        ((and (equal? z 'none) (null? *wtrace*)) 1)
        ((equal? z 'none) 2)
        (t "what? - from old-watch")))


(define (old-pbreak p-names)
  (cond ((equal? p-names 'none) *brkpts*)
	       (t (mapc pbreak2 p-names))))

(define (pbreak2 rule)
  (cond ((not (symbolp rule)) (%warn "Illegal name" rule))
        ((not (get rule 'topnode)) (%warn "Not a production" rule))
        ((memq rule *brkpts*) (set! *brkpts* (rematm rule *brkpts*)))
        (t (set! *brkpts* (cons rule *brkpts*)))))


(define (rematm atm list)
  (cond ((atom? list) list)
        ((equal? atm (car list)) (rematm atm (cdr list)))
        (t (cons (car list) (rematm atm (cdr list))))))

(define (broken rule) (memq rule *brkpts*))

(define (old-ppwm avlist)
  (let ((next 1) (a #f))

   (define (r1)
     (if (atom? avlist)
         (print8)
         (begin
           (set! a (car avlist))
           (set! avlist (cdr avlist))
           (cond ((eq a '^)
                  (set! next (car avlist))
                  (set! avlist (cdr avlist))
                  (set! next (!litbind next))
                  (and (realp next) (set! next (int next)))
                  (cond ((or (not (numberp next))
                             (> next *size-result-array*)
                             (> 1 next))
                         (%warn "Illegal index after ^" next)
                         #f)))
                 ((!variablep a)
                  (%warn "PPWM does not take variables" a)
                  #f)
                 (t (set!
                       *filters*
		                     (cons next (cons a *filters*)))
                    (set! next (+ 1 next))
                    (r1))))))
   (define (print8)
     (mapwm ppwm2)
     #f)    
    
   (set! *filters* #f)
   (r1)))

(define (ppwm2 elm-tag)
  (cond ((filter (car elm-tag)) (ppelm (car elm-tag)))))

(define (filter elm)
  (let ((fl *filters*) (indx #f) (val #f))
    
   (define (top)
     (if (atom? fl)
         t
         (begin
           (set! indx (car fl))
           (set! val (cadr fl))
           (set! fl (cddr fl))
           (if (not (ident (nth (sub1 (int indx)) elm) val))
               #f
               (top)))))    
    
   (top)))

(define (ident x y)
  (cond ((eq? x y) t)
        ((not (number? x)) #f)
        ((not (number? y)) #f)
        ((equal? x y) t)
        (t #f)))

;;; for matching partial WMEs

(define (matches2 pp)
  (cond ((atom? pp)
         (newline)
         (newline)
         (write "For production rule ")
         (write pp)
         (matches3 (get pp 'backpointers) 2 '(1)))))

(define (matches3 nodes ce part)
  (cond ((not (null? nodes))
         (newline)
         (write " ** Matches for Condition Element number")
         (write (car part))
         (write " ** ")
         (mapc  write-elms
	               (find-left-mem (car nodes))) ;; ????
         (newline) (newline)
         (write " ** Matches for Condition Element number ")
         (write ce)
         (write " ** ")
         (mapc write-elms
	              (find-right-mem (car nodes)))
         (matches3 (cdr nodes) (1+ ce) (cons ce part)))))

(define (write-elms wme-or-count)
  (cond ((proper-list? wme-or-count)
         (newline)
         (mapc write-elms2 wme-or-count))))

(define (write-elms2 x)
  (write "  ")
  (write (creation-time x)) (write x))

(define (find-left-mem node)
  (cond ((eq? (car node) '&and) (memory-part (caddr node)))
        (t (car (caddr node)))))

(define (find-right-mem node) (memory-part (cadddr node)))

(define (ppelm elm)
   (display (list (creation-time elm) elm))
   (newline))

;;; Backing up (Undoing)

(define (record-index-plus k)
  (set! *record-index* (+ k *record-index*))
  (cond ((< *record-index* 0)
         (set! *record-index* *max-record-index*))
        ((> *record-index* *max-record-index*)
         (set! *record-index* 0))))

(define (initialize-record)
  (set! *record-index* 0)
  (set! *recording* #f)
  (set! *max-record-index* 64)
  (putvector *record-array* 0 #f))

(define (begin-record pp data)
  (set! *recording* t)
  (set! *record* (list '=>refract pp data)))

(define (end-record)
  (cond (*recording*
         (set! *record*
               (cons *cycle-count* (cons *p-name* *record*)))
         (record-index-plus 1)
         (putvector *record-array* *record-index* *record*)
         (set! *record* #f)
         (set! *recording* #f))))

(define (record-change direct time elm)
  (cond (*recording*
         (set! *record*
               (cons direct (cons time (cons elm *record*)))))))

(define (record-refract rule data)
  (and *recording*
       (set!
        *record*
        (cons '<=refract (cons rule (cons data *record*))))))

(define (refracted rule data)
  (let ((z #f))
    (if (not (null? *refracts*))
        (begin
         (set! z (cons rule data))
         (member z *refracts*))
        #f)))

(define (undo k)
  (let ((r #f))
    
   (define (loop)
     (if (< k 1)
         #f
         (begin
           (set! r (getvector *record-array* *record-index*))
           (if (null? r)
               "Nothing more stored"
               (begin
                 (putvector *record-array* *record-index* #f)
                 (record-index-plus -1)
                 (undo-record r)
                 (set! k (-1+ k))
                 (loop))))))
       
    (loop)))

(define (undo-record r)
  (let ((save *recording*) (act #f) (a #f) (b #f) (rate #f))
    
   (define (top)
     (if (atom? r)
         (fin)
         (begin
          (set! act (car r))
          (set! a (cadr r))
          (set! b (caddr r))
          (set! r (cdddr r))
          (and *wtrace* (back-print (list 'undo- act a)))
          (cond ((eq? act '<=wm) (add-to-wm b a))
                ((eq? act '=>wm) (remove-from-wm b))
                ((eq? act '<=refract)
                 (set! *refracts* (cons (cons a b) *refracts*)))
                ((and (eq? act '=>refract) (still-present b))
                 (set! *refracts* (delete (cons a b) *refracts*))
                 (set! rate (cadr (get a 'topnode)))
                 (removecs a b)
                (insertcs a b rate))
                (t (%warn "UNDO cannot undo action" (list act a))))
          (top))))
   (define (fin)
     (set! *recording* save)
     (set! *refracts* #f)
     #f)    
    
   (set! *refracts* #f)
   (set! *recording* #f)
   (and *ptrace* (back-print (list 'undo- (car r) (cadr r))))
   (set! r (cddr r))
   (top)))

(define (still-present data)
  
  (define (loop)
    (if (atom? data)
        #t
        (cond
              ((not (creation-time (car data)))
                  #f)
              (t
               (set! data (cdr data))
               (loop)))))
   
  (loop))


(define (back-print x) 
  (newline)
  (display x)
  (newline))

;;; Conflict Resolution
;
;
; each conflict set element is a list of the following form:
; ((p-name . data-part) (sorted wm-recency) special-case-number)

(define (removecs name data)
  (let ((cr-data (cons name data)) (inst #f) (cs *conflict-set*))
    (for-each
     (lambda (l)
       (if (equal? (car l) cr-data)
           (set!  *conflict-set* (delq l *conflict-set*)) #f))
     cs)
    (record-refract name data)))

(define (insertcs name data rating)
  (let ((instan #f))
    (if (refracted name data)
        #f
        (begin
          (set! instan (list (cons name data) (order-tags data) rating))
          (and (atom? *conflict-set*) (set! *conflict-set* '()))
          (set! *conflict-set* (cons instan *conflict-set*))
          *conflict-set*))))

(define (order-tags dat)
  (let ((tags #f))
    (while (and (not (atom? dat)) (not (null? dat)))
       (begin
           (set! tags (cons (creation-time (safe-car dat)) tags))
           (set! dat (cdr dat))))
    (cond ((eq? *strategy* 'mea)
           (cons (safe-car tags) (dsort (safe-cdr tags))))
          (t (dsort tags)))))

(define (dsort x) ; changed 10/24/86 to use builtin sort
  (sort x <))

(define (conflict-resolution)
  (let ((best #f) (len (length *conflict-set*)) (temp #f))
    (cond ((> len *max-cs*) (set! *max-cs* len)))
    (set! *total-cs* (+ *total-cs* len))
    (cond ((pair? *conflict-set*)
           (set! best (best-of *conflict-set*))
           (set! *conflict-set* (delq best *conflict-set*))
           (set! temp (pname-instantiation best)))
          (t temp #f))
    temp))

(define (best-of set)
  (if (null? set)
      #f
      (best-of* (car set) (cdr set))))

(define (best-of* best rem)
  ;; In MIT Scheme (not rem) is true when rem is the empty list; in
  ;; Racket '() is true, so the empty-list case is tested explicitly.
  (if (proper-list? rem)
      (cond ((or (null? rem) (not rem)) best)
            ((conflict-set-compare best (car rem))
             (best-of* best (cdr rem)))
            (t (best-of* (car rem) (cdr rem))))
      #f))

(define (remove-from-conflict-set name)
  (let ((cs #f) (entry #f) (outer t) (inner t))
    (while outer
       (begin
           (set! cs *conflict-set*)
           (while inner
              (begin
                  (cond ((atom? cs)
                         (begin (set! inner #f) (set! outer #f)))
                        (t (set! entry (car cs))
                           (set! cs (cdr cs))
                           (cond ((eq name (caar entry))
                                  (set!
                                   *conflict-set*
                                   (delq entry *conflict-set*))))))))))))

(define pname-instantiation car)      ; syntactic sugar

(define order-part cdr) ; syntactic sugar

(define (instantiation conflict-elem)
  (cdr (pname-instantiation conflict-elem)))

(define (conflict-set-compare x y) ;; ?? check this
  (let ((x-order (order-part x))
        (y-order (order-part y))
        (xl #f) (yl #f) (xv #f) (yv #f)
        (ret #f) (bailout #f))
    (define (data)
      (cond ((and (null? xl) (null? yl))
             (ps))
            ((null? yl)
             (set! ret t)
             t)
            ((null? xl)
             #f)
            (t
             (set! xv (car xl))
             (set! yv (car yl))
             (cond ((> xv yv)
                    (set! ret t))
                   ((> yv xv)
                    #f)
                   (t
                    (set! xl (cdr xl))
                    (set! xl (cdr yl))
                    (data))))))
    (define (ps)
      (set! xl (cdr x-order))
      (set! yl (cdr y-order))
      (psl))
    (define (psl)
      (cond ((null? xl)
             t)
            ((null? yl)
             #f)
            (t
             (set! xv (car xl))
             (set! yv (car yl))
             (cond ((> yv xv)
                    #f)
                   ((> yv xv)
                    #f)
                   (t
                    (set! xl (cdr xl))
                    (set! yl (cdr yl))
                    (psl))))))
                   
    (set! xl (car x-order))
    (set! yl (car y-order))
    (data)))
 

(define (conflict-set)
  (let ((cnts '()) (cs1 *conflict-set*) (p8 #f) (z #f) (best #f))
    (while (and (not (null? cs1))( proper-list? cs1))
       (begin
           (set! p8 (car (caar cs1)))
           (set! cs1 (cdr cs1))
           (set! z (assq p8 cnts))
           (cond ((not z) (set! cnts (cons (cons p8 1) cnts)))
                 (t (set-cdr! z (1+ (cdr z)))))))
    (while (and (not (null? cnts)) (proper-list? cnts))
       (begin
           (newline)
           (display (safe-car (safe-car cnts)))
           (cond ((> (safe-cdr (safe-car cnts)) 1)
                  (display "        (")
                  (display (cdr (car cnts)))
                  (display " occurrences")
                  (display ")")))
           (set! cnts (cdr cnts))))
    (set! best (best-of *conflict-set*))
    (newline)
    (list (safe-car (safe-car best)) 'dominates)))


;; Scheme OPS5 utility functions (MLW)

(define (put var property value)
   (if (null? var) (set! var #f) #f)
   (if (not (symbol? var))
       (error "Non-symbol argument to put" var) #f)
   (if (eq? property 'pname)
       (error "The pname property is inviolate" var) #f)
   (let ((entry (assq property (cdr (->pair var)))))
      (if entry
          (set-cdr! entry value)
          (set-cdr! (->pair var)
                    (cons (cons property value)
                          (cdr (->pair var)))))
      value))

(define (putprop var value property)
  (put var property value))

(define (remprop var prop)
  (put var prop #f))

(define (get var property)
   (if (null? var) (set! var #f) #f)
   (if (not (symbol? var))
       (error "Non-symbol argument to get" var) #f)
   (let ((entry (assq property (cdr (->pair var)))))
      (if entry (cdr entry) #f)))

(define (explode symbol)
   (mapcar (lambda (x) (string->symbol (list->string (list x))))
           (string->list (symbol->string symbol))))

(define (implode char-list)
   (string->symbol
      (list->string (mapcar (lambda (x)
                               (car (string->list
                                       (symbol->string x))))
                            char-list))))

(define (atomcar x)
  (car (explode x)))


(define (concat a b c)
 (let ((l (list a b c)))
   (cond ((equal? l '(t eq n)) 'teqn)
         ((equal? l '(t eq a)) 'teqa)
         ((equal? l '(t ne a)) 'tnea)
         ((equal? l '(t ne n)) 'tnen)
         ((equal? l '(t lt n)) 'tltn)
         ((equal? l '(t le n)) 'tlen)
         ((equal? l '(t gt n)) 'tgtn)
         ((equal? l '(t ge n)) 'tgen)
         ((equal? l '(t eq b)) 'teqb)
         ((equal? l '(t ne b)) 'tneb)
         ((equal? l '(t eq s)) 'teqs)
         ((equal? l '(t ne s)) 'tnes)
         ((equal? l '(t xx a)) 'txxa)
         ((equal? l '(t xx s)) 'txxs)
         ((equal? l '(t xx b)) 'txxb)
         ((equal? l '(t gt b)) 'tgtb)
         ((equal? l '(t le b)) 'tleb)
         ((equal? l '(t ge b)) 'tgeb)
         ((equal? l '(t lt b)) 'tltb)
         ((equal? l '(t gt s)) 'tgts)
         ((equal? l '(t ge s)) 'tges)
         ((equal? l '(t lt s)) 'tlts)
         ((equal? l '(t le s)) 'tles)
         (t (%warn "Concatenation error" l)))) )

(define (!litbind x)
 (let ((r #f))
   (cond ((and (symbol? x) (set! r (get x 'ops-bind)))
          r)
         (t x))))

(define getvector vector-ref)

(define putvector vector-set!)

(define (gelm x k)
  (let ((ce (truncate (/ k 1000)))
         (temp #f)
         (xsub #f)
         (temp2 #f))
    (set! xsub (1+ (- k (* ce 1000))))
    (set! temp (nth ce x))
    (set! temp2 (nth (-1+ xsub) temp))
    temp2))
  
(define (ce-gelm x k)     ;; rewriten 5/7/86
  (car (vector-ref x k)))

(define (interq x y)
  (cond ((atom? x) #f)
        ((memq (car x) y) (cons (car x) (interq (cdr x) y)))
        (t (interq (cdr x) y))))

(define (reset-ops5)
   (ops-remove *)
   (cleanup-plists)
   (remove-class-prop)
  ; (mapc excise-p p-list)
   (cleanup-vector-list)
   (remove-old-wms)
   (i-g-v))

(define *vector-list* #f)
(define *wmpart-list* #f)

(define (i-g-v)
 (let ((x #f))
    (cleanup-plists)
    (cleanup-vector-list)
    (remove-class-prop)
    (set! *vector-list* '())
    (set! *buckets* 64)
    (set! *accept-file* #f)
   	(set! leftcurly "{")
   	(set! rightcurly "}")
    (set! *write-file* #f)
    (set! *trace-file* #f)
    (set! *class-list* #f)
    (set! *finish-lit?* t)			      ;flag for finish-literalize
    (set! *max-index* 127)
    (set! *next-index* 1)
    (set! *brkpts* #f)
    (set! *strategy* 'lex)
    (set! *in-rhs* #f)
    (set! *ptrace* #f)
    (set! *wtrace* #f)
    (set! *recording* #f)
    (set! *refracts* #f)
    (set! *real-cnt* 0)
    (set! *virtual-cnt* 0)
    (set! *max-cs* 0)
    (set! *total-cs* 0)
    (set! *limit-token* 32000)
    (set! *limit-cs* 32000)
    (set! *critical* #f)
    (set! *build-trace* #f)
    (set! *wmpart-list* #f)
    (set! *size-result-array* 127)
    (set! *result-array* (make-vector 128 #f))
    (set! *record-array* (make-vector 128 #f))
    (set! *first-node* (list '&bus #f))
    (set! *pcount* 0)
    (initialize-record)
    (set! *cycle-count* 0)
    (set! *action-count* 0)
    (set! *total-token* 0)
    (set! *max-token* 0)
    (set! *current-token* 0)
    (set! *total-cs* 0)
    (set! *max-cs* 0)
    (set! *total-wm* 0)
    (set! *max-wm* 0)
    (set! *current-wm* 0)
    (set! *wm* #f)
    (set! *wmpart-list* #f)
    (set! *conflict-set* #f)
    (set! *wmpart-list* #f)
    (set! *p-name* #f)
    (set! p-list #f)
    (set! *remaining-cycles* 32000))
 (newline)
 (display "******* Beta test of OPS5 *******") (newline)
 (display "Note: the Scheme version of OPS5 requires curly brakets { and }")
 (newline)
 (display "to have surrounding double quotes.  Place spaces around the ^")
 (display "tab character.")  (newline)
 (display "Copyright 1986, Mark Watson") (newline)
)

(define (cleanup-plists)
  (mapc
   (lambda (x) (excise-p x))
   p-list)
  (mapc
   (lambda (x)
     (remprop x 'conflicts)
     (remprop x 'ops-bind)
     (remprop x 'att-list)
     (remprop x 'wmpart*)
     (remprop x 'ppdat))
   *wmpart-list*))
 
(define (cleanup-vector-list) ;added 3/18/86 to cleanup vector stuff
  (mapc
   (lambda (l)
     (remprop l 'vector-attribute))
   *vector-list*))
		  
(define (remove-class-prop) 
   (define (remove-class-prop-2 l)
       (remprop l 'ops-bind))
	(mapc remove-class-prop-2 *ats*))  ; 1/9/86


(define (%warn what where)
    (newline)
    (display '?)
    (and *p-name* (display *p-name*))
    (display "..")
    (display where)
    (display "..")
    (display what))

(define (%error what where)
    (%warn what where)
    (reset))

(define (top-levels-eq la lb)
  (let ((keep-going t) (temp #f))
    (while keep-going
     (begin
      (cond ((eq? la lb) (begin (set! keep-going #f) (set! temp t)))
            ((null? la) (set! keep-going #f))
            ((null? lb) (set! keep-going #f))
            ((not (eq? (car la) (car lb))) (set! keep-going #f)))
      (set! la (safe-cdr la))
      (set! lb (safe-cdr lb))))
    temp))

(define int truncate)

(define (safe-car l)
  (if (pair? l)
      (car l)
      #f))
 
(define (safe-cdr l)
  (if (pair? l)
      (cdr l)
      #f))

(define symbolp symbol?)

(define symeval eval)

(define eq eq?)

(define (neq? a b)
  (not (eq? a b)))

(define delq remove) ; delq should use eq? whereas remove uses equal?
  
(define (nth n l)
  ;; cddr/cdddr on short lists would raise contract errors with the
  ;; mutable-pair accessors, so the safe versions are used.
  (if (pair? l)
      (if (> n 3)
          (nth (- n 3) (safe-cdr (safe-cdr (safe-cdr l))))
          (if (= n 3)
              (safe-car (safe-cdr (safe-cdr (safe-cdr l))))
              (if (= n 2)
                  (safe-car (safe-cdr (safe-cdr l)))
                  (if (= n 1)
                      (safe-car (safe-cdr l))
                      (if (= n 0)
                          (safe-car l)
                          l)))))
      #f))

(define delete remove)

;;; do-rjust and do-tabto are referenced by old-ops-write (for the
;;; "=== R J U S T ===" and "=== T A B T O ===" markers produced by
;;; the crlf/tabto RHS functions) but were never defined in the
;;; original source files; simple implementations are provided here.

(define (do-rjust value width port)
  (define str (cond ((number? value) (number->string value))
                    ((symbol? value) (symbol->string value))
                    (else "")))
  (display (make-string (max 0 (- width (string-length str))) #\space) port)
  (display value port))

(define (do-tabto n port)
  (newline port)
  (display (make-string (max 0 (sub1 (if (number? n) n 1))) #\space) port))
;; =====================================================================
;; 3. compiler.rkt (production compiler)
;; =====================================================================
;;; File: Compiler.RKT
;;;
;;; Converted from compiler.s (Mark Watson, "OPS5 in Scheme", 1995) to
;;; run under Racket.  Loaded into the OPS5 namespace by load.rkt after
;;; ops5.rkt; see README.md.

;;; LHS Compiler for Scheme version of OPS5 (MLW)

(define (old-p z)
 ;; old-p receives the whole, unevaluated production form
 ;; (p name lhs --> rhs); the original code's first two set!s
 ;; (z = (car z) then z = (cdr z)) could not work in any Scheme
 ;; (the second cdr is applied to the symbol that car returned),
 ;; so the intended steps are written out directly here.
  (write (car z)) (newline)
  (set! z (cdr z))
  (finish-literalize) 
  (write '*)
  (let ((flag nil) (temp nil))
    (set! temp (compile-production (car z) (cdr z)))
    (set! flag t)
    (display "compiled") (display  (car z))))

(define (compile-production name matrix)
  (cond ((null? p-list)
         (set! p-list (cons name nil)))
        (t (set! p-list (cons name p-list))))
  (set! *pname* name)
  (cmp-p name matrix)
  (set! *pname* nil))

(define (peek-lex) (car *matrix*))

(define (lex)
  (let ((temp (car *matrix*)))
    (set! *matrix* (cdr *matrix*))
    temp))  

(define (prepare-lex prod) (set! *matrix* prod))

(define (sublex)
  (let ((temp (car *curcond*)))
    (set! *curcond* (cdr *curcond*))
    temp))

(define (cmp-p name matrix)
  (let ((m nil) (bakptrs nil))
        (cond ((or (null? name) (proper-list? name))
	       (%error "Illegal production name" name)))
        (prepare-lex matrix)
        (excise-p name)
        (set! bakptrs nil)
        (set! *pcount* (+ 1 *pcount*))
        (set! *feature-count* 0)
        (set! *ce-count* 0)
        (set! *vars* nil)
        (set! *ce-vars* nil)
        (set! *rhs-bound-vars* nil)
        (set! *rhs-bound-ce-vars* nil)
        (set! *last-branch* nil)
        (set! m *matrix*)
        (while (not (equal? '--> (peek-lex)))
         (begin
          (and (atom? *matrix*) (%error "No '-->' in production" m))
          (cmp-prin)
          (set! bakptrs (cons *last-branch* bakptrs))))
        (lex)
        (check-rhs *matrix*)
        (link-new-node (list '&p
                             *feature-count*
                             name
                             (encode-dope)
                             (encode-ce-dope)
                             (eval (cons 'lambda (cons nil *matrix*)))))
        (putprop name (cdr (reverse bakptrs)) 'backpointers)
        (putprop name *last-node* 'topnode)))

(define (excise-p name)
  (cond ((and (symbol? name) (get name 'topnode))
         (write name) (write " is excised")
         (set! *pcount* (sub1 *pcount*))
         (remove-from-conflict-set name)
         (kill-node (get name 'topnode))
         (remprop name 'production)
         (remprop name 'backpointers)
         (remprop name 'topnode))))


(define (kill-node node)
  (while (not (atom? node))
       (begin
         (set-car! node '&old)
         (set! node (cdr node)))))

(define (cmp-prin)
  (set! *last-node* *first-node*)
  (cond ((null? *last-branch*) (cmp-posce) (cmp-nobeta))
	       ((equal? (peek-lex) '-) (cmp-negce) (cmp-not))
	       (t (cmp-posce) (cmp-and))))

(define (cmp-negce) (lex) (cmp-ce))

(define (cmp-posce)
  (set! *ce-count* (+ 1 *ce-count*))
  (cond ((equal? (peek-lex) leftcurly) (cmp-ce+cevar))
        (t (cmp-ce))))

(define (cmp-ce+cevar)
  (let ((z nil))
        (lex)
        (cond ((and (atom? (peek-lex)) (not (null? (safe-car *curcond*)))) ; 9/20/86
                (cmp-cevar) (cmp-ce))
              (t (cmp-ce) (cmp-cevar)))
        (set! z (lex))
        (or (equal? z rightcurly)
            (%error "Missing right curly bracket" z))))

(define (new-subnum k)
  (or (number? k) (%error "Tab must be a number" k))
  (set! *subnum* (int k)))

(define (cmp-ce)
  (let ((z nil))
        (new-subnum 0)
        (set! *cur-vars* nil)
        (set! z (lex))
        (and (and (atom? z) (not (null? z))) ; 9/20/86
             (%error "Atomic conditions are not allowed" z))
        (set! *curcond* z)
        (while (and (not (atom? *curcond*)) (not (null? *curcond*))) ; 9/20/86
           (begin
               (set! *subnum* (+ *subnum* 1))
               (cmp-element)))))

(define (cmp-element)
        (and (equal? (car *curcond*) '^) (cmp-tab))
        (cond ((equal? (car *curcond*) leftcurly) (cmp-product))
              (t (cmp-atomic-or-any))))

(define (cmp-atomic-or-any)
        (cond ((equal? (car *curcond*) '<<) (cmp-any))
              (t (cmp-atomic))))

(define (cmp-any)
  (let ((a (sublex)) (z nil))
    (while (not (equal? '>> a))
       (begin
           (cond ((atom? *curcond*) (%error "Missing '>>'" a)))
           (set! a (sublex))
           (set! z (cons a z))))
    (link-new-node (list '&any nil (current-field) z))))

(define (cmp-tab)
  (let ((r nil))
    (sublex)
    (set! r (sublex))
    (set! r (litbind r))
    (new-subnum r)))

(define (litbind x)
  (let ((r nil) (temp nil))
        (cond ((and
                  (symbol? x)
                  (set! r (get x 'ops-bind)))
               (set! temp r))
              (t (set! temp x)))
        temp))

(define (get-bind x)
  (let ((r nil) (temp nil))
        (cond ((and (symbolp x) (set! r (get x 'ops-bind)))
               (set! temp r)))
        temp))
		  

(define (cmp-atomic)
  (let ((test nil) (x (car *curcond*)))
        (cond ((eq? x '=)   (set! test 'eq) (sublex))
              ((eq? x '<>)  (set! test 'ne) (sublex))
              ((eq? x '<)   (set! test 'lt) (sublex))
              ((eq? x '<=)  (set! test 'le) (sublex))
              ((eq? x '>)   (set! test 'gt) (sublex))
              ((eq? x '>=)  (set! test 'ge) (sublex))
              ((eq? x '<=>) (set! test 'xx) (sublex))
              (t (set! test 'eq)))
        (cmp-symbol test)))

(define (cmp-product)
  (let ((save *curcond*) (keep-going t))
     (sublex)
     (while (and (mit-true? keep-going) *curcond*)
        (begin
	           (cond ((and (atom? *curcond*) (not (null? *curcond*)))
                      (cond ((member rightcurly save) 
                             (%error "Wrong context for curly bracket" save))
                            (t (%error "Missing curly bracket"))))
                     ((equal? (car *curcond*) rightcurly)
                      (sublex)
                      (set! keep-going nil)))
               (if (mit-true? keep-going) (cmp-atomic-or-any) #f))))) ;; Begin OK ??

(define (!variablep x)
  (if (not (string? x))
      (if (number? x)
          nil
          (if (equal? (atomcar x) '<) t #f))
      #f))
 
(define (cmp-symbol test)
  (let ((flag t))
        (cond ((equal? (car *curcond*) '//) (sublex) (set! flag #f)))
        (cond ((and flag (!variablep (car *curcond*)))
               (cmp-var test))
              ((number? (car *curcond*)) (cmp-number test))
              ((symbol? (car *curcond*)) (cmp-constant test))
              (t (%error "Unrecognized symbol" (sublex))))))

(define (cmp-constant test)
  (or (member test '(eq ne xx))
      (%error "Non-numeric constant after numeric predicate" (sublex)))        
  (link-new-node (list (concat 't test 'a)
                       nil
                       (current-field)
                       (sublex))))

(define (cmp-number test)
  (link-new-node (list (concat 't test 'n)
                       nil
                       (current-field)
                       (sublex))))

(define (current-field) (field-name *subnum*))

(define (field-name num)
  ;; The original code assigned the field name to an unbound `temp`;
  ;; the value of the set! is returned directly here instead.
  (if (< num 20)
      (cadr (assoc num
                   '((1 *c1*) (2 *c2*) (3 *c3*) (4 *c4*) (5 *c5*)
                     (6 *c6*) (7 *c7*) (8 *c8*) (9 *c9*) (10 *c10*)
                     (11 *c11*) (12 *c12*) (13 *c13*) (14 *c14*)
                     (15 *c15*) (16 *c16*) (17 *c17*) (18 *c18)
                     (19 *c19*) (20 *c20*))))
      (%error "Condition is too long!" *curcond*)))

(define (var-dope var) (assq var *vars*))

(define (ce-var-dope var) (assq var *ce-vars*))

(define (cmp-var test)
  (let ((old nil) (name (sublex)))
        (set! old (assq name *cur-vars*))
        (cond ((and old (eq? (cadr old) 'eq))
               (cmp-old-eq-var test old))
              ((and old (eq? test 'eq)) (cmp-new-eq-var name old))
              (t (cmp-new-var name test)))))

(define (cmp-new-var name test)
  (set! *cur-vars* (cons (list name test *subnum*) *cur-vars*)))

(define (cmp-old-eq-var test old)
  (link-new-node (list (concat 't test 's)
                       nil
                       (current-field)
                       (field-name (caddr old)))))

(define (cmp-new-eq-var name old)
  (let ((pred nil) (next nil))
        (set! *cur-vars* (delq old *cur-vars*))
        (set! next (assq name *cur-vars*))
        (cond (next (cmp-new-eq-var name next))
              (t (cmp-new-var name 'eq)))
        (set! pred (cadr old))
        (link-new-node (list (concat 't pred 's)
                             nil
                             (field-name (caddr old))
                             (current-field)))))
(define (cmp-cevar)
  (let ((name nil) (old nil))
        (set! name (lex))
        (set! old (assq name *ce-vars*))
        (and old
             (%error "The condition element variable is used twice" name))   
        (set! *ce-vars* (cons (list name 0) *ce-vars*))))

(define (cmp-not) (cmp-beta '&not))

(define (cmp-nobeta) (cmp-beta nil))

(define (cmp-and) (cmp-beta '&and))

(define (cmp-beta kind)
  (let ((tlist nil) (vdope nil) (vname nil) (vpred nil) (vpos nil) (old nil))
    (while (not (atom? *cur-vars*))
      (begin
        (set! vdope (car *cur-vars*))
        (set! *cur-vars* (cdr *cur-vars*))
        (set! vname (car vdope))
        (set! vpred (cadr vdope))
        (set! vpos (caddr vdope))
        (set! old (assq vname *vars*))
        (cond (old (set! tlist (add-test tlist vdope old)))
              ((neq? kind '&not) (promote-var vdope)))))
     (and (mit-true? kind) (build-beta kind tlist))
     (or (eq? kind '&not) (fudge))
     (set! *last-branch* *last-node*)))

(define (add-test list new old)
  (let ((ttype nil) (lloc nil) (rloc nil))
        (set! *feature-count* (add1 *feature-count*))
        (set! ttype (concat 't (cadr new) 'b))
        (set! rloc (encode-singleton (caddr new)))
        (set! lloc (encode-pair (cadr old) (caddr old)))
        (cons ttype (cons lloc (cons rloc list)))))

; (define (encode-pair a b) (+ (lsh (sub1 a) 10) (sub1 b))) ;??

(define (encode-pair a b) (+ (* 1000 (-1+ a)) (-1+ b)))

(define (encode-singleton a) (sub1 a))

(define (promote-var dope)
  (let ((vname (car dope)) (vpred (cadr dope)) (vpos (caddr dope)) (new nil))
        (or (eq? 'eq vpred)
            (%error "Illegal predicate for first occurrence"
                   (list vname vpred)))
        (set! new (list vname 0 vpos))
        (set! *vars* (cons new *vars*))))

(define (fudge)
  (mapc fudge* *vars*)
  (mapc fudge* *ce-vars*))

(define (fudge* z)
  (let ((a nil))
    (set! a (cdr z))
    (set-car! a (add1 (car a)))))

(define (build-beta type tests)
  (let ((rpred *last-node*) (lpred nil) (lnode nil) (lef nil))
        (link-new-node (list '&mem nil nil (protomem)))
        ;; The rpred slot must name the right memory node just linked,
        ;; not the alpha test node: &and/&not left activations scan
        ;; (memory-part rpred) for right-side WMEs, and an alpha node
        ;; has no memory cell.  (In the original this was the alpha
        ;; node, whose missing cell made every left activation a no-op.)
        (set! rpred *last-node*)
        (cond ((eq? type '&and)
               (set! lnode (list '&mem nil nil (protomem))))
              (t (set! lnode (list '&two nil nil))))
        (set! lpred (link-to-branch lnode))
        (cond ((eq? type '&and) (set! lef lpred))
              (t (set! lef (protomem))))
        (link-new-beta-node (list type nil lef rpred tests))))

(define (protomem) (list nil))

;; memory-part reads the token memory out of a node.  Only &mem nodes
;; carry a memory cell, whose car is the current token list; the
;; original took safe-car of the cadddr to get those contents.  For
;; other node types (e.g. the alpha teqa nodes that build-beta passes
;; as rpred) the original returned #f, which not-left then fed to car;
;; return the empty list instead.
(define (memory-part mem-node)
  (let ((contents
         (safe-car (safe-car (safe-cdr (safe-cdr (safe-cdr mem-node)))))))
    (if (pair? contents) contents '())))

(define (encode-dope)
  (let ((r nil) (all *vars*) (z nil) (k nil))
    (while (not (atom? all))
        (begin
           (set! z (car all))
           (set! all (cdr all))
           (set! k (encode-pair (cadr z) (caddr z)))
           (set! r (cons (car z) (cons k r)))))
    r))


(define (encode-ce-dope)
  (let ((r nil) (all nil) (z nil) (k nil))
        (set! r nil)
        (set! all *ce-vars*)
    (while (not (atom? all))
       (begin
           (set! z (car all))
           (set! all (cdr all))
           (set! k (cadr z))
           (set! r (cons (car z) (cons k r)))))
    r))
 
;;; Linking the nodes

(define (link-new-node r)
  (cond ((and
          (proper-list? r)
          (not (member (car r) '(&p &mem &two &and &not))))
         (set! *feature-count* (+ 1 *feature-count*))))
  (set! *virtual-cnt* (+ 1 *virtual-cnt*))
  (set! *last-node* (link-left *last-node* r)))

(define (link-to-branch r)
  ;; MIT Scheme's set! returns the assigned value; Racket's returns
  ;; void, so the node is returned explicitly here.
  (set! *virtual-cnt* (+ 1 *virtual-cnt*))
  (set! *last-branch* (link-left *last-branch* r))
  *last-branch*)

(define (link-new-beta-node r)
  (set! *virtual-cnt* (add1 *virtual-cnt*))
  (set! *last-node* (link-both *last-branch* *last-node* r))
  (set! *last-branch* *last-node*))

(define (link-left pred succ)
  (let ((a (left-outs pred)) (r nil))
        (set! r (find-equiv-node succ a))
        (if r
            r
            (begin
                (set! *real-cnt* (add1 *real-cnt*))
                (attach-left pred succ)
                succ))))

(define (link-both left right succ)
  (let ((a (interq (left-outs left) (right-outs right))) (r nil))
        (set! r (find-equiv-beta-node succ a))
        (if r
            r
            (begin
               (set! *real-cnt* (add1 *real-cnt*))
               (attach-left left succ)
               (attach-right right succ)
               succ))))

(define (attach-right old new)
  (set-car! (cddr old) (cons new (caddr old))))

(define (attach-left old new)
  (set-car! (cdr old) (cons new (cadr old))))

(define (right-outs node) (safe-car (safe-cdr (safe-cdr node))))

(define (left-outs node) (safe-car (safe-cdr node)))

(define (find-equiv-node node list)
  (let ((a list) (keep-going t) (temp #f))
    (while keep-going
       (begin
           (cond ((or (atom? a) (not (null? a)))
                  (set! keep-going nil))
                 ((equiv node (safe-car a))
                  (begin (set! temp (car a)) (set! keep-going nil))))
           (set! a (safe-cdr a))))
    temp))

(define (find-equiv-beta-node node list)
  (let ((a list) (keep-going t) (temp #f))
    (while keep-going
           (cond ((atom? a) (set! keep-going nil))
                 ((beta-equiv node (car a))
                  (begin (set! keep-going nil) (set! temp (car a))))
                 (t (set! a (cdr a)))))
    temp))

(define (equiv a b)
  (and (eq? (safe-car a) (safe-car b))
       (or (eq? (safe-car a) '&mem)
           (eq? (safe-car a) '&two)
           (equal? (caddr a) (caddr b)))
       (equal? (cdddr a) (cdddr b))))

(define (beta-equiv a b)
  (and (eq? (car a) (car b))
       (equal? (cddddr a) (cddddr b))
       (or (eq? (car a) '&and) (equal? (caddr a) (caddr b)))))

;; =====================================================================
;; 4. network.rkt (Rete network)
;; =====================================================================
;; File: Network.RKT
;;
;; Converted from network.s (Mark Watson, "OPS5 in Scheme", 1995) to
;; run under Racket.  Loaded into the OPS5 namespace by load.rkt after
;; compiler.rkt; see README.md.

;;; Network interpreter

(define (match flag wme)
  (sendto flag (list wme) 'left (list *first-node*)))

(define (eval-nodelist nl)
  (while nl
     (begin
        (set! *sendtocall* #f)
        (set! *last-node* (car nl))
        (apply (local-eval (caar nl)) (cdar nl))
        (set! nl (cdr nl)))))

(define (sendto flag data side nl)
   (while nl
     (begin
        (set! *side* side)
        (set! *flag-part* flag)
        (set! *data-part* data)
        (set! *sendtocall* t)
        (set! *last-node* (car nl))
        (apply (local-eval (safe-car (safe-car nl))) (safe-cdr (safe-car nl)))
        (set! nl (safe-cdr nl)))))

(define (&bus outs)
  (let ((dp #f))
    (set! *alpha-flag-part* *flag-part*)
    (set! *alpha-data-part* *data-part*)
    (set! dp (safe-car *data-part*))
    (set! *c1* (safe-car dp))
    (set! dp (safe-cdr dp))
    (set! *c2* (safe-car dp))
    (set! dp (safe-cdr dp))
    (set! *c3* (safe-car dp))
    (set! dp (safe-cdr dp))
    (set! *c4* (safe-car dp))
    (set! dp (safe-cdr dp))
    (set! *c5* (safe-car dp))
    (set! dp (safe-cdr dp))
    (set! *c6* (safe-car dp))
    (if dp
        (begin
          (set! dp (safe-cdr dp))
          (set! *c7* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c8* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c9* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c10* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c11* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c12* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c13* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c14* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c15* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c16* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c17* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c18* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c19* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c20* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c21* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c22* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c23* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c24* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c25* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c26* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c27* (safe-car dp))
          (set! dp (safe-cdr dp))
          (set! *c28* (safe-car dp))
          (set! dp (safe-cdr dp))
          (if dp
              (begin
                (set! dp (safe-cdr dp))
                (set! *c29* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c30* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c31* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c32* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c33* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c34* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c35* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c36* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c37* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c38* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c39* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c40* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c41* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c42* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c43* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c44* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c45* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c46* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c47* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c48* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c49* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c50* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c51* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c52* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c53* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c54* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c55* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c56* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c57* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c58* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c59* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c60* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c61* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c62* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c63* (safe-car dp))
                (set! dp (safe-cdr dp))
                (set! *c64* (safe-car dp))
                (set! dp (safe-cdr dp)))
              #f))
        #f)
    (eval-nodelist outs)))


(define (&any outs register const-list)
  (let ((z (local-eval register)) (c #f))
    
    (define (sym)
      (if (null? const-list)
          #f
          (if (eq? (car const-list) z)
              (eval-nodelist outs)
              (begin
               (set! const-list (cdr const-list))
               (sym)))))
    (define (num)
      (if (null? const-list)
          #f
          (if (and
               (number? (begin (set! c (car const-list)) c))
               (equal? c z))
              (eval-nodelist outs)
              (begin
               (set! const-list (cdr const-list))
               (num)))))
    
    (if (number? z)
        (num)
        (sym))))
    
(define (teqa outs register constant)
  (and (equal? (local-eval register) constant) (eval-nodelist outs)))

(define (teqn outs register constant)
  (let ((z (local-eval register)))
        (and (number? z)
             (equal? z constant)
             (eval-nodelist outs))))

(define (tnea outs register constant)
  (and (not (equal? (local-eval register) constant)) (eval-nodelist outs)))

(define (txxa outs register constant)
  (and (symbol? (local-eval register)) (eval-nodelist outs)))

(define (tnen outs register constant)
  (let ((z (local-eval register)))
       (and (or (not (number? z))
                (not (equal? z constant)))
            (eval-nodelist outs))))

(define (tltn outs register constant)
  (let ((z (local-eval register)))
       (and (number? z)
            (> constant z)
            (eval-nodelist outs))))

(define (tgtn outs register constant)
  (let ((z (local-eval register)))
        (and (number? z)
             (> z constant)
             (eval-nodelist outs))))

(define (tgen outs register constant)
  (let ((z (local-eval register)))
        (and (number? z)
             (not (>  constant z))
             (eval-nodelist outs))))

(define (teqs outs vara varb)
  (let ((a (local-eval vara)) (b (local-eval varb)))
       (cond ((eq? a b) (eval-nodelist outs))
             ((and (number? a)
                   (number? b)
                   (equal? a b))
              (eval-nodelist outs)))))

(define (tnes outs vara varb)
  (let ((a (local-eval vara)) (b (local-eval varb)))
       (cond ((eq a b) #f)
             ((and (number? a)
                   (number? b)
                   (equal? a b))
              #f)
             (t (eval-nodelist outs)))))

(define (teqb new eqvar)
  (cond ((eq? new eqvar) t)
        ((not (number? new)) #f)
        ((not (number? eqvar)) #f)
        ((equal? new eqvar) t)
        (t #f)))

(define (tneb new eqvar)
  (cond ((eq? new eqvar) #f)
        ((not (number? new)) t)
        ((not (number? eqvar)) t)
        ((equal? new eqvar) #f)
        (t t)))

(define (tltb new eqvar)
  (cond ((not (number? new)) #f)
        ((not (number? eqvar)) #f)
        ((> eqvar new) t)
        (t #f)))

(define (tgtb new eqvar)
  (cond ((not (number? new)) #f)
        ((not (number? eqvar)) #f)
        ((> new eqvar) t)
        (t #f)))

(define (tgeb new eqvar)
  (cond ((not (number? new)) #f)
        ((not (number? eqvar)) #f)
        ((not (> eqvar new)) t)
        (t #f)))

(define (tleb new eqvar)
  (cond ((not (number? new)) #f)
        ((not (number? eqvar)) #f)
        ((not (> new eqvar)) t)
        (t #f)))

(define (tlen outs register constant)
  (let ((z (local-eval register)))
       (and (number? z)
            (not (< constant z))
            (eval-nodelist outs))))

(define (txxs outs vara varb)
  (let ((a (local-eval vara)) (b (local-eval varb)))
       (cond ((and (number? a)
		                 (number? b))
	             (eval-nodelist outs))
             ((and (not (number? a)) (not (number? b)))
               (eval-nodelist outs)))))

(define (txxb new eqvar)
  (cond ((number? new)
         (cond ((number? eqvar) t)
               (t #f)))
        (t
         (cond ((number? eqvar) #f)
               (t t)))))

(define (tlts outs vara varb)
  (let ((a (local-eval vara)) (b (local-eval varb)))
       (and (number? a)
            (number? b)
            (< a b)
            (eval-nodelist outs))))

(define (tgts outs vara varb)
  (let ((a (local-eval vara)) (b (local-eval varb)))
       (and (number? a)
            (number? b)
            (< b a)
            (eval-nodelist outs))))

(define (tges outs vara varb)
  (let ((a (local-eval vara)) (b (local-eval varb)))
        (and (number? a)
             (number? b)
             (not (< a b))
             (eval-nodelist outs))))

(define (tles outs vara varb)
  (let ((a (local-eval vara)) (b (local-eval varb)))
       (and (number? a)
            (number? b)
            (not (< b a))
            (eval-nodelist outs))))


(define (&two left-outs right-outs)
  (let ((fp #f) (dp #f))
       (cond (*sendtocall*
              (set! fp *flag-part*)
              (set! dp *data-part*))
             (t
              (set! fp *alpha-flag-part*)
              (set! dp *alpha-data-part*)))
       (sendto fp dp 'left left-outs)
       (sendto fp dp 'right right-outs)))

(define (&mem left-outs right-outs memory-list)
  (let ((fp #f) (dp #f))
       (cond (*sendtocall*
              (set! fp *flag-part*)
              (set! dp *data-part*))
             (t
              (set! fp *alpha-flag-part*)
              (set! dp *alpha-data-part*)))
       (sendto fp dp 'left left-outs)
       (add-token memory-list fp dp #f)
       (sendto fp dp 'right right-outs)))

(define (&and outs lpred rpred tests)
  (let ((mem #f))
       (cond ((eq? *side* 'right) (set! mem (memory-part lpred)))
             (t (set! mem (memory-part rpred))))
       (cond ((not mem) #f)
             ((eq? *side* 'right) (and-right outs mem tests))
             (t (and-left outs mem tests)))))

(define (and-left outs mem tests)
  (let ((fp *flag-part*) (dp *data-part*)
        (memdp #f) (tlist #f) (tst #f) (lind #f) (rind #f) (res #f))
    
   (define (fail)
     (if (not (null? mem))
         (begin
           (set! memdp (car mem))
           (set! mem (cdr mem))
           (set! tlist tests)
           (tloop)) #f))
   (define (tloop)
     (if (null? tlist)
         (succ)
         (begin
           (set! tst (car tlist))
           (set! tlist (cdr tlist))
           (set! lind (car tlist))
           (set! tlist (cdr tlist))
           (set! rind (car tlist))
           (set! tlist (cdr tlist))
           (set!
            res
            (apply (local-eval tst) (list (gelm memdp rind) (gelm dp lind))))
           (if res (tloop) (fail)))))

    (define (succ) 
        (sendto fp (cons (safe-car memdp) dp) 'left outs)
        (fail))
    (fail)))

(define (and-right outs mem tests)
  (let ((fp *flag-part*) (dp *data-part*)
        (memdp #f) (tlist #f) (tst #f) (lind #f) (rind #f) (res #f))
    
   (define (fail)
     (if (not (null? mem))
         (begin
           (set! memdp (car mem))
           (set! mem (cdr mem))
           (set! tlist tests)
           (tloop)) #f))
   (define (tloop)
     (if (null? tlist)
         (succ)
         (begin
           (set! tst (car tlist))
           (set! tlist (cdr tlist))
           (set! lind (car tlist))
           (set! tlist (cdr tlist))
           (set! rind (car tlist))
           (set! tlist (cdr tlist))
           (set!
            res
            (apply (local-eval tst) (list (gelm dp rind) (gelm memdp lind))))
           (if res (tloop) (fail)))))

    (define (succ) 
        (sendto fp (cons (safe-car dp) memdp) 'right outs)
        (fail))
    (fail)))

(define (&p rating name var-dope ce-var-dope rhs)
  (let ((fp #f) (dp #f))
        (cond (*sendtocall*
               (set! fp *flag-part*)
               (set! dp *data-part*))
              (t
               (set! fp *alpha-flag-part*)
               (set! dp *alpha-data-part*)))
        (and (memq fp '(#f old)) (removecs name dp))
        (and fp (insertcs name dp rating))))

(define (&old a b c d e) #f)

(define (&not outs lmem rpred tests)
  (cond ((and (eq *side* 'right) (eq *flag-part* 'old)) #f)
        ((eq *side* 'right) (not-right outs (car lmem) tests))
        (t (not-left outs (memory-part rpred) tests lmem))))

(define (not-left outs mem tests own-mem)
  (let ((fp *flag-part*) (dp *data-part*) (memdp #f) (tlist #f)
        (tst #f) (lind #f) (rind #f) (res #f) (c 0))
    
   (define (fail)
     (if (null? mem)
         (fin)
         (begin
           (set! memdp (car mem))
           (set! mem (cdr mem))
           (set! tlist tests)
           (tloop))))
    
   (define (tloop)
     (if (null? tlist)
         (begin
          (set! c (+ c 1))
          (fail))
         (begin
          (set! tst (car tlist))
          (set! tlist (cdr tlist))
          (set! lind (car tlist))
          (set! tlist (cdr tlist))
          (set! rind (car tlist))
          (set! tlist (cdr tlist))
          (set!
           res
           (apply (local-eval tst) (list (gelm memdp rind) (gelm dp lind))))
          (cond (res (tloop))
                (t (fail))))))
    
   (define (fin)
     (add-token own-mem fp dp c)
     (and (equal? c 0) (sendto fp dp 'left outs)))
    
   (fail)))

(define (not-right outs mem tests)
  (let ((fp *flag-part*) (dp *data-part*) (memdp #f) (tlist #f)
        (tst #f) (lind #f) (rind #f) (res #f) (newc 0)
        (newfp #f) (inc #f))
    
   (define (fail)
     (if (null? mem)
         #f
         (begin
           (set! memdp (car mem))
           (set! mem (cdr mem))
           (set! tlist tests)
           (tloop))))
    
   (define (tloop)
     (if (null? tlist)
         (succ)
         (begin
          (set! tst (car tlist))
          (set! tlist (cdr tlist))
          (set! lind (car tlist))
          (set! tlist (cdr tlist))
          (set! rind (car tlist))
          (set! tlist (cdr tlist))
          (set!
           res
           (apply (local-eval tst) (list (gelm dp rind) (gelm memdp lind))))
          (cond (res (tloop))
                (t (fail)))
          (succ))))
         
    
   (define (succ)
     ;; cdr-or-nil emulates the MIT one-armed-if semantics the original
     ;; relied on: safe-cdr of a non-pair yielded '() there, so the count
     ;; update and the two-step advance below were harmless no-ops once
     ;; mem was exhausted.  With safe-cdr returning #f they crash.
     (define (cdr-or-nil l) (if (pair? l) (cdr l) '()))
     (set! newc (+ inc newc))
     (if (null? (cdr-or-nil mem))
         (set! mem (append mem (list newc)))
         (set-car! (cdr mem) newc))
     (cond ((or (and (equal? inc -1) (equal? newc 0))
                (and (equal? inc 1) (equal? newc 1)))
            (sendto newfp memdp 'right outs)))
     (set! mem (cdr-or-nil (cdr-or-nil mem)))
     (fail))
    
  (cond ((not fp)
         (set! inc -1)
         (set! newfp 'new)
         (fail))
        ((equal? fp 'new)
         (set! inc 1)
         (set! newfp #f)
         (fail))
        (t #f))))
 

;;; Node memories

(define (add-token memlis flag data-part num)
  (let ((was-present #f))
        (cond ((eq? flag 'new)
               (set! was-present #f)
               (real-add-token memlis data-part num))
              ((not flag) 
               (set! was-present (remove-old memlis data-part num)))
              ((eq? flag 'old) (set! was-present t)))
        was-present))

(define (real-add-token lis data-part num)
  (set! *current-token* (1+ *current-token*))
  (cond (num (set-car! lis (cons num (car lis)))))
  (set-car! lis (cons data-part (car lis))))

(define (remove-old lis data num)
  (cond (num (remove-old-num lis data))
        (t (remove-old-no-num lis data))))
  
(define (remove-old-num lis data)
  (let ((m (car lis)) (next #f) (last #f))
 
   (define (loop)
     (set! last next)
     (set! next (cddr next))
     (cond ((atom? next) #f)
           ((top-levels-eq data (car next))
            (set-cdr! (cdr last) (cddr next))
            (set! *current-token* (sub1 *current-token*))
            (car next))
           (t (loop))))

    (if (atom? m)
        #f
        (if (top-levels-eq data (car m))
            (begin
              (set! *current-token* (sub1 *current-token*))
              (set-car! lis (cddr m))
              (car m))
            (begin
             (set! next m)
             (loop))))))
 
(define (remove-old-no-num lis data)
  (let ((m (car lis)) (next #f) (last #f))
  
      (define (loop)
        (set! last next)
        (set! next (cdr next))
        (cond ((atom? next) #f)
              ((top-levels-eq data (car next))
               (set-cdr! last (cdr next))
               (set! *current-token* (-1+ *current-token*))
               (car next))
              (t (loop))))
  
       (if (atom? m)
           #f
           (if (top-levels-eq data (car m))
               (begin
                 (set! *current-token* (sub1 *current-token*))
                 (set-car! lis (cdr m))
                 (car m))
               (begin
                 (set! next m)
                 (loop))))))

;;; WM functions
  
(define (add-to-wm wme override)
  (let ((fa #f) (z #f) (part #f) (timetag #f) (port #f))
    (set! *critical* t)
    (set! *current-wm* (1+ *current-wm*))
    (and (> *current-wm* *max-wm*) (set! *max-wm* *current-wm*))
    (set! *action-count* (1+ *action-count*))
    (set! fa (wm-hash wme))
    (or (memq fa *wmpart-list*)
        (set! *wmpart-list* (cons fa *wmpart-list*)))
    (set! part (get fa 'wmpart*))
    ;; override may be nil='() ("no override"); MIT treats '() as false
    (cond ((and override (not (null? override)))
             (set! timetag override))
          (t (set! timetag *action-count*)))
    (set! z (cons wme timetag))
    (putprop fa (cons z part) 'wmpart*)
    (record-change '=>wm *action-count* wme)
    (match 'new wme)
    (set! *critical* #f)
    (cond ((and *in-rhs* *wtrace*)
           (newline)
           (write "Adding to WM: ")
           (write wme)
           (newline)))))

(define (remove-from-wm wme)
  (let ((fa (wm-hash wme)) (z #f) (part #f) (timetag #f) (port #f))
    (set! part (get fa 'wmpart*))
    (set! z (assq wme part))
    (if z
        (begin
          (set! timetag (cdr z))
          (cond ((and *in-rhs* *wtrace*)
                 (newline)
                 (write "Removing from WM: ")
                 (write wme)
                 (newline)))
          (set! *action-count* (1+ *action-count*))
          (set! *critical* t)
          (set! *current-wm* (-1+ *current-wm*))
          (record-change '<=wm timetag wme)
          (match #f wme)
          (putprop fa (delq z part) 'wmpart*)
          (set! *critical* #f)) #f)))

(define (mapwm fn)
  (let ((wmpl *wmpart-list*) (part #f))
    
   (define (loop)
     (if (atom? wmpl)
         #f
         (begin
           (set! part (get (car wmpl) 'wmpart*))
           (set! wmpl (cdr wmpl))
           (mapc fn part)
           (loop))))    
    
    (loop)))


(define (old-wm a)  ; 2/11/85
  (mapc (lambda (z) (ppelm z))
        (get-wm a)))


(define (get-wm z)
  (set! *wm-filter* z)
  (set! *wm* #f)
  (mapwm get-wm2)
  (let ((temp *wm*))
    (set! *wm* #f)
    temp))

(define (get-wm2 elem)
 (cond ((or (not *wm-filter*) 
            (member (cdr elem) *wm-filter*))
        (set! *wm* (cons (car elem) *wm*)))))

(define (wm-hash x)
  (cond ((not x) '<default>)
        ((not (car x)) (wm-hash (cdr x)))
        ((symbolp (car x)) (car x)) ;; ops5 function
        (t (wm-hash (cdr x)))))

(define (creation-time wme)
  (let ((ret (safe-cdr (assq wme (get (wm-hash wme) 'wmpart*)))))
    (if ret
        ret
        (begin
         (display "Warning from creation-time: #f time tag for ")
         (display wme)
         (newline)
         0))))

(define (refresh)
  (set! *old-wm* #f)
  (mapwm refresh-collect)
  (mapc refresh-del *old-wm*)
  (mapc refresh-add *old-wm*)
  (set! *old-wm* #f))


(define (refresh-collect x) (set! *old-wm* (cons x *old-wm*)))

(define (refresh-del x) (remove-from-wm (car x)))

(define (refresh-add x) (add-to-wm (car x) (cdr x)))

;; Define glocal registers:

(define *c1* #f)
(define *c2* #f)
(define *c3* #f)
(define *c4* #f)
(define *c5* #f)
(define *c6* #f)
(define *c7* #f)
(define *c8* #f)
(define *c9* #f)
(define *c10* #f)
(define *c11* #f)
(define *c12* #f)
(define *c13* #f)
(define *c14* #f)
(define *c15* #f)
(define *c16* #f)
(define *c17* #f)
(define *c18* #f)
(define *c19* #f)
(define *c20* #f)
(define *c21* #f)
(define *c22* #f)
(define *c23* #f)
(define *c24* #f)
(define *c25* #f)
(define *c26* #f)
(define *c27* #f)
(define *c28* #f)
(define *c29* #f)
(define *c30* #f)
(define *c31* #f)
(define *c32* #f)
(define *c33* #f)
(define *c34* #f)
(define *c35* #f)
(define *c36* #f)
(define *c37* #f)
(define *c38* #f)
(define *c39* #f)
(define *c40* #f)
(define *c41* #f)
(define *c42* #f)
(define *c43* #f)
(define *c44* #f)
(define *c45* #f)
(define *c46* #f)
(define *c47* #f)
(define *c48* #f)
(define *c49* #f)
(define *c50* #f)
(define *c51* #f)
(define *c52* #f)
(define *c53* #f)
(define *c54* #f)
(define *c55* #f)
(define *c56* #f)
(define *c57* #f)
(define *c58* #f)
(define *c59* #f)
(define *c60* #f)
(define *c61* #f)
(define *c62* #f)
(define *c63* #f)
(define *c64* #f)
;; =====================================================================
;; 5. rhs.rkt (right-hand side actions)
;; =====================================================================
;; File: RHS.RKT
;;
;; Converted from rhs.s (Mark Watson, "OPS5 in Scheme", 1995) to run
;; under Racket.  Loaded into the OPS5 namespace by load.rkt after
;; network.rkt; see README.md.

;;; Check the RHSs of productions.  FILE: Check RHS

(define (check-rhs rhs)
         (mapc check-action rhs))

   (define (check-action x)
     (let ((a nil))
       (cond ((atom? x)
              (%warn "Atomic action" x)
              nil)
             (t
              (set! *action-type* (car x))
              (set! a *action-type*)
              (cond ((eq? a 'bind) (check-bind x))
                    ((eq? a 'cbind) (check-cbind x))
                    ((eq? a 'make) (check-make x))
                    ((eq? a 'modify) (check-modify x))
                    ((eq? a 'ops-remove) (check-remove x))
                    ((eq? a 'ops-write) (check-write x))
                    ((eq? a 'call) (check-call x))
                    ((eq? a 'halt) (check-halt x))
                    ((eq? a 'openfile) (check-openfile x)) ; read only
                    ((eq? a 'exec) (check-exec x))
                   ((eq? a 'closefile) (check-openfile x))
                   ((eq? a 'default)
	                    (print "No default file specification") nil)
	                   ((eq? a 'build) (print "No build function") nil)
	                   ((and (symbol? a)
                         (with-handlers ([exn:fail? (lambda (e) #f)])
                           (procedure? (eval a))))
                    t)
                    (t (%warn "Undefined RHS action" a)))))))
 
(define (check-exec z)
  (and (null? (cdr z)) (%warn "Needs arguments" z)))


(define (check-cbind z)
  (let ((v nil))
   	(or (equal? (length z) 2) (%warn "Takes only one argument" z))
	   (set! v (cadr z))
   	(or (!variablep v) (%warn "Takes a variable as argument" z))
   	(note-ce-variable v)))

(define (check-bind z)
  (let ((v nil))
	   (or (> (length z) 1) (%warn "Needs arguments" z))
	   (set! v (cadr z))
	   (or (!variablep v) (%warn "Takes a variable as argument" z))
	   (note-variable v)
	   (check-change& (cddr z))))

(define (check-openfile x) ;; only read
   (and (null? (cdr x)) (%warn "Needs arguments" x)))

(define (check-remove z) 
  (and (null? (cdr z)) (%warn "Needs arguments" z))
  (mapc check-rhs-ce-var (cdr z)))

(define (check-make z)
  (and (null? (cdr z)) (%warn "Needs arguments" z))
  (check-change& (cdr z)))

   
(define (check-modify z)
  (and (null? (cdr z)) (%warn "Needs arguments" z))
  (check-rhs-ce-var (cadr z))
  (and (null? (cddr z)) (%warn "No changes to make" z))
  (check-change& (cddr z)))

(define (check-write z)
  (and (null? (cdr z)) (%warn "Needs arguments" z))
  (check-change& (cdr z)))

(define (check-call z)
  (let ((f nil))
    (and (null? (cdr z)) (%warn "Needs arguments" z))
    (set! f (cadr z))
    (and (!variablep f)
         (%warn "Function name must be a constant" z))
    (or (symbolp f)
        (%warn "Function name must be a symbolic atom" f))
    (or (externalp f)	(procedure? f)
        (%warn "Function name not declared external" f))
    (check-change& (cddr z))))

(define (check-halt z)
  (or (null? (cdr z)) (%warn "Does not take arguments" z)))

(define (check-change& z)
  ;; tab-flag must start as Racket #f: '() is truthy in Racket, so the
  ;; first ^ in every RHS action triggered a spurious warning
  (let ((r nil) (tab-flag #f))
    (while (not (atom? z))
      (begin
        (set! r (car z))
        (set! z (cdr z))
        (cond ((eq r '^)
               (and tab-flag
                    (%warn "No value before this tab" (car z)))
               (set! tab-flag t)
               (check-tab-index (car z))
               (set! z (cdr z)))
              ((eq r '//) (set! tab-flag #f) (set! z (cdr z)))
              (t (set! tab-flag #f) (check-rhs-value r)))))))

(define (check-rhs-ce-var v)
  (cond ((and (not (number? v)) (not (ce-bound? v)))
         (%warn "Unbound element variable" v))
        ((and (number? v) (or (< v 1) (> v *ce-count*)))
         (%warn "Numeric element designator out of bounds" v))))

(define (check-rhs-value x)
  (cond ((proper-list? x) (check-rhs-function x))
	       (t (check-rhs-atomic x))))

(define (check-rhs-atomic x)
  (and (!variablep x) 
       (not (bound? x)) 
       (%warn "Unbound variable" x)))

(define (check-rhs-function x)
  (let ((a (car x)))
    (cond ((eq? a 'compute) (check-compute x))
          ((eq? a 'arith) (check-compute x))
          ((eq? a 'substr) (check-substr x))
          ((eq? a 'accept) (check-accept x))
          ((eq? a 'acceptline) (check-acceptline x))
          ((eq? a 'crlf) (check-crlf x))
          ((eq? a 'genatom) (check-genatom x))
          ((eq? a 'litval) (check-litval x))
          ((eq? a 'tabto) (check-tabto x))
          ((not (externalp a))
           (%warn "RHS function not declared external" a)))))

(define (check-litval x)
  (or (= (length x) 2) (%warn "Wrong number of arguments" x))
  (check-rhs-atomic (cadr x)))

(define (check-accept x)
  (cond ((equal? (length x) 1) nil)
        ((equal? (length x) 2) (check-rhs-atomic (cadr x)))
        (t (%warn "Too many arguments"))))

(define (check-acceptline x)
  (mapc check-rhs-atomic (cdr x)))
   
(define (check-crlf x) 
  (check-0-args x))

(define (check-genatom x) (check-0-args x))

(define (check-tabto x)
  (or (equal? (length x) 2) 
      (%warn (%warn "Wrong number of arguments" x))
  (check-print-control (cadr x))))

(define (check-rjust x)
  (or (equal? (length x) 2) 
      (%warn (%warn "Wrong number of arguments" x))
  (check-print-control (cadr x))))
   
(define (check-0-args x)
  (or (equal? (length x) 1)
      (%warn "Does not take arguments" x)))

(define (check-substr x)
  (or (equal? (length x) 4) (%warn "Wrong number of arguments" x))
  (check-rhs-ce-var (cadr x))
  (check-substr-index (caddr x))
  (check-last-substr-index (cadddr x)))
   
(define (check-compute x) (check-arithmetic (cdr x)))

(define (check-arithmetic l)
  (cond ((atom? l)
         (%warn "Bad syntax in arithmetic expression" l))
        ((atom? (cdr l)) (check-term (car l)))
        ((not (memq (cadr l) '(+ - * // ops-mod)))  ;; 2/4/86
         (%warn "Unknown operator" l))
        (t (check-term (car l)) (check-arithmetic (cddr l)))))

(define (check-term x)
  (cond ((proper-list? x) (check-arithmetic x))
        (t (check-rhs-atomic x))))

(define (check-last-substr-index x)
  (or (eq? x 'inf) (check-substr-index x)))

(define (check-substr-index x)
  (let ((v nil))
    (if (bound? x)
        #t
        (begin
         (set! v (!litbind x))
         (cond ((not (number? v))
                (%warn "Unbound symbol used as index in SUBSTR" x))
               ((or (< v 1) (> v 127))
                (%warn "Index out of bounds in tab" x)))))))

(define (check-print-control x)
  (if (bound? x)
      #t
      (cond ((or (not (number? x)) (< x 1) (> x 127))
           (%warn "Illegal value for printer control" x)))))

(define (check-tab-index x)
  (let ((v nil))
    (if (bound? x)
        #t
        (begin
         (set! v (!litbind x))
         (cond ((not (number? v))
             (%warn "Unbound symbol occurs after ^" x))
            ((or (< v 1) (> v 127))
             (%warn "Index out of bounds after ^" x)))))))

(define (note-variable var)
  (set! *rhs-bound-vars* (cons var *rhs-bound-vars*)))

(define (bound? var)
  (or (memq var *rhs-bound-vars*)
      (assq var *vars*)))

(define (note-ce-variable ce-var)
  (set! *rhs-bound-ce-vars* (cons ce-var *rhs-bound-ce-vars*)))

(define (ce-bound? ce-var)
  (or (memq ce-var *rhs-bound-ce-vars*)
      (assq ce-var *ce-vars*)))


;;; Basic functions for RHS evaluation and actions:  First file.

(define (eval-rhs pname data)
  (let ((node nil) (port nil) (eval-expression nil))
    (cond (*ptrace*
            (newline) (display *cycle-count*) (display ". ")
            (display pname) (time-tag-print data)))
    (set! *data-matched* data)
    (set! *p-name* pname)
    (set! *last* nil)
    (set! node (get pname 'topnode))
    (init-var-mem (cadddr node))
    (init-ce-var-mem (cadr (cdddr node)))
    (begin-record pname data)
    (set! *in-rhs* t)
    (set! eval-expression (caddr (cdddr node)))
    (eval-expression)
    (set! *in-rhs* nil)
    (end-record)))
	
(define (exec l) (eval (mapcar quote-!varbind l)))

(define (old-build a)
  (compile-production
    (!varbind (car a))
    (mapcar !varbind (cdr a))))

(define (time-tag-print data)
  (cond ((not (null? data))
         (time-tag-print (cdr data))
         (display " ")
         (display (creation-time (car data))))))

(define (init-var-mem vlist)
  (let ((v nil) (ind nil) (r nil))
    
   (define (top)
     (if (atom? vlist)
         nil
         (begin
          (set! v (car vlist))
          (set! ind (cadr vlist))
          (set! vlist (cddr vlist))
          (set! r (gelm *data-matched* ind))
          (set! *variable-memory* (cons (cons v r) *variable-memory*))
          (top))))
    
  (set! *variable-memory* nil)
  (top)))

(define (init-ce-var-mem vlist)
  (let ((v nil) (ind nil) (r nil))
    
   (define (top)
     (if (atom? vlist)
         nil
         (begin
          (set! v (car vlist))
          (set! ind (cadr vlist))
          (set! vlist (cddr vlist))
          (set! r (ce-gelm *data-matched* ind))
          (set! *ce-variable-memory*
                (cons (cons v r) *ce-variable-memory*))
          (top))))

    (set! *ce-variable-memory* nil)
    (top)))

(define (make-ce-var-bind var elem)
  (set! *ce-variable-memory*
        (cons (cons var elem) *ce-variable-memory*)))

(define (make-var-bind var elem)
  (set! *variable-memory* (cons (cons var elem) *variable-memory*)))

(define (!varbind x)
  (let ((r nil))
    (if (not *in-rhs*)
        x
        (begin
         (set! r (assq x *variable-memory*))
         (cond (r (cdr r))
               (t x))))))

(define (quote-!varbind l) (list 'quote (!varbind l)))

(define (get-ce-var-bind x)
  (let ((r nil))
    (if (number? x)
        (get-num-ce x)
        (begin
         (set! r (assq x *ce-variable-memory*))
         (cond (r (cdr r))
               (t nil))))))

(define (get-num-ce x)
  (let ((r *data-matched*) (l nil) (d nil))
    
   (define (loop)
     (if (null? r)
         nil
         (if (> 1 d)
             (car r)
             (begin
               (set! d (sub1 d))
               (set! r (cdr r))
               (loop)))))    
    
    (set! l (length r))
    (set! d (- l x))
    (if (> 0 d)
         nil
         (loop))))

(define (build-collect z)
  (let ((r nil))
        
    (define (loop)
     (if (atom? z)
         nil
         (begin
          (set! r (car z))
          (set! z (cdr z))
          (cond ((listp r)
                 (!value '"(")
                 (build-collect r)
                 (!value '")"))
              ((eq? r '\\) (!change (car z)) (set! z (cdr z)))
              (t (!value r)))
          (loop))))
       
    (loop)))

(define (old-accept l)
   (newline)
	  (cond ((null? l) (flat-value (read)))
	        (t (flat-value (read iport)))))

(define (flat-value x)
  (if (atom? x) (!value x) #f))

(define (unflat x)
  
   (define (unflat*)
     (let ((c nil))
        (if (atom? *rest*)
            nil
            (begin
             (set! c (car *rest*))
             (set! *rest* (cdr *rest*))
             (cond ((eq? c '"(") (cons (unflat*) (unflat*)))
                   ((eq? c ")") nil)
                   (t (cons c (unflat*))))))))
  
  (set! *rest* x) (unflat*))


(define (!change x)
  ;; '() is a placeholder value in sparse WMEs, not a function call
  (cond ((and (proper-list? x) (not (null? x))) (eval-function x)) ; used to be consp
	       (t (!value (!varbind x)))))

(define (eval-args z)
  (let ((r nil))
    
   (define (loop)
     (if (atom? z)
         nil
         (begin
          (set! r (car z))
          (set! z (cdr z))
          (cond ((eq? r '^)
                 (rhs-tab (car z))
                 (set! r (cadr z))
                 (set! z (cddr z))))
          (cond ((eq? r '//) (!value (car z)) (set! z (cdr z)))
                (t (!change r)))
          (loop))))
     
    (rhs-tab 1)
    (loop)))

(define (eval-function form)
  (cond ((not *in-rhs*)
         (%warn "Functions cannot be used at top level" (car form)))
        (t (eval form))))

(define (!reset)
  (set! *max-index* 0)
  (set! *next-index* 1))

(define (rhs-tab z) (!tab (!varbind z)))

(define (!tab z)
  (let ((edge nil) (next (!litbind z)))
    
   (define (clear)
     (cond ((equal? *max-index* edge) (ok))
           (t  (putvector *result-array* edge nil)
               (set! edge (-1+ edge))
               (clear))))
   (define (ok)
     (set! *next-index* next)
     next)

  (and (real? next) (set! next (truncate next))) ; 2/8/85 change
  (cond ((or (not (number? next)) 
             (> next *size-result-array*)
             (> 1 next))
         (%warn "Illegal index after ^" next)
         *next-index*)
        (t (set! edge (- next 1))
           (cond ((> *max-index* edge) (ok))
                 (t (clear)))))))

(define (!value v)
  (cond ((> *next-index* *size-result-array*)
         (%warn "Index too large" *next-index*))
        (t
         (and (> *next-index* *max-index*)
              (set! *max-index* *next-index*))
         (putvector *result-array* *next-index* v)
         (set! *next-index* (add1 *next-index*)))))

(define (use-result-array)
  (let ((k *max-index*) (r nil))
    
    (define (top)
      (if (equal? k 0)
          r
          (if (<= k 0)
              r
              (begin
               (set! r (cons (getvector *result-array* k) r))
               (set! k (-1+ k))
               (top)))))
   (top)))

(define (!assert)
  (set! *last* (use-result-array))
  (add-to-wm *last* nil))

(define (!parametercount) *max-index*)

(define (!parameter k)
  (cond ((or (not (number? k))
	            (< *size-result-array* k)
	            (< k 1))
          (%warn "Illegal parameter number " k)
	         nil)
        ((< *max-index* k) nil)
        (t (getvector *result-array* k))))


;;; RHS actions

(define (old-modify . z)
  (let ((old nil))
    
      (define (copy)
        (while old
               (begin
                  (!change (safe-car old))
                  (set! old (cdr old))))
        (fin))
    
      (define (fin)
	       (eval-args z)
	       (!assert))
  
	  (cond ((not *in-rhs*)
          (%warn "Cannot be called at top level" 'modify)
          nil)
         (t
	          (set! old (get-ce-var-bind (car z)))
	          (cond ((null? old)
		                (%warn
                   "Modify: first argument must be an element variable"
                   (car z))
		                nil )
	                (t
                  (remove-from-wm old)
	                 (set! z (cdr z))
	                 (!reset)
                  (copy)))))))

(define (old-remove . z)
  (let ((old nil))
   (define (loop)
     (if (and (atom? z) (not (number? z)))
         nil
         (begin
          (set! old (get-ce-var-bind (car z)))
          (cond ((null? old)
                 (%warn
                  "Remove: argument not an element variable"
                  (car z))
                 nil)
                (t
                  (remove-from-wm old)
                  (if (proper-list? z)
                      (begin
                        (set! z (cdr z))
                        (loop)) #f))))))
  
   (if (not *in-rhs*)
       (top-level-remove z)
       (loop))))

(define (old-call z)
  (let ((f (car z)))
    (!reset)
    (eval-args (cdr z))
    (eval (list f))))

(define (halt)
  (cond ((not *in-rhs*)
         (%warn "Cannot be called at top level" 'halt))
        (t (set! *halt-flag* t))))

;;; RHS Functions

(define (old-substr l)
  (let ((k nil) (elm nil) (start nil) (end nil))

   (define (loop)
     (if (> k end)
         nil
         (begin
           (if (not (< k start))
               (!value (car elm))
               (begin
                (set! elm (cdr elm))
                (set! k (add1 k))
                (loop))))))

    (cond ((not (equal? (length l) 3))
           (%warn "Substr: wrong number of arguments" l)
           nil)
          (t
            (set! elm (get-ce-var-bind (car l)))
            (cond ((null? elm)
                   (%warn "First argument to SUBSTR must be a CE var" l)
                   nil)
                  (t
                   (set! start (!varbind (cadr l)))
                   (set! start (!litbind start))
                   (cond ((not (numberp start))
                          (%warn
                           "Second argument to SUBSTR must be a number"
                           l)
                          nil)
                         (t
                          (set! end (!varbind (caddr l)))
                          (cond ((eq? end 'inf) (set! end (length elm))))
                          (set! end (!litbind end))
                          (cond ((not (number? end))
                                 (%warn
                                  "Third arg to SUBSTR must be a number"
                                  l)
                                 nil)
                                (t
                                  (set! k 1)
                                  (loop)))))))))))


(define (mod i j) (truncate (- i (* (truncate (/ i j)) j))))

(define (ari x)
  (cond ((atom? x)
         (%warn "Bad syntax in arithmetic expression" x)
         0)
        ((atom? (cdr x)) (ari-unit (car x)))
        ((equal? (cadr x) '+)
         (+ (ari-unit (car x)) (ari (cddr x))))
        ((equal? (cadr x) '-)
         (- (ari-unit (car x)) (ari (cddr x))))
        ((equal? (cadr x) '*)
         (* (ari-unit (car x)) (ari (cddr x))))
        ((equal? (cadr x) '//)
         (quotient (ari-unit (car x)) (ari (cddr x))))
        ((equal? (cadr x) '/)
         (quotient (ari-unit (car x)) (ari (cddr x))))
        ((equal? (cadr x) 'ops-mod) ;; 2/4/86
         (mod (int (ari-unit (car x))) (int (ari (cddr x)))))
        (t (%warn "Undefined operator" x) 0)))

(define (ari-unit a)
  (let ((r nil))
    (cond ((proper-list? a) (set! r (ari a)))
          (t (set! r (!varbind a))))
    (cond ((not (number? r))
           (%warn "Bad value in arithmetic expression" a)
           0)
          (t r))))

(define (genatom) (!value (gensym)))

(define (old-litval z)
  (let ((r nil))
        (cond ((not (equal? (length z) 1))
               (%warn "LITVAL: wrong number of arguments" z)
               (!value 0) 
               nil)
              ((number? (car z))
               (!value (car z))
               nil)
              (t
               (set! r (!litbind (!varbind (car z))))
               (cond ((number? r)
                      (!value r)
                      nil)
              (t
               (%warn "LITVAL: argument has no literal binding" (car z))
               (!value 0)))))))

(define (crlf)  (!value '"=== C R L F ==="))

(define (old-tabto z)
  (let ((val nil))
        (cond ((not (equal? (length z) 1))
               (%warn "TABTO: wrong number of arguments" Z)
               nil)
              (t
               (set! val (!varbind (car z)))
               (cond ((or (not (numberp val)) (< val 1) (> val 127))
                      (%warn "TABTO: illegal column number" z)
                      nil)
                     (t
                      (!value '"=== T A B T O ===")
                      (!value val)))))))

(define (old-openfile x) ;; only for reading
   (set! iport (open-input-file (car x))))

(define (closefile)
   (close-input-port iport) (set! iport #f))

(define (old-bind z)
  (let ((val nil))
        (cond ((not *in-rhs*)
               (%warn "Cannot be called at top level" 'bind)
               nil)
              (t
               (cond ((< (length z) 1)
                      (%warn "BIND: wrong number of arguments to" z)
                      nil)
                     ((not (symbolp (car z))) ; symbolp is ops5 function
                      (%warn "BIND: illegal argument" (car z))
                      nil)
                     ((equal? (length z) 1)
                      (set! val (gensym)))
                     (t (!reset)
                        (eval-args (cdr z))
                        (set! val (!parameter 1))))
                (make-var-bind (car z) val)))))

(define (old-cbind z)
  (cond ((not *in-rhs*)
         (%warn "Cannot be called at top level" 'cbind))
        ((not (equal? (length z) 1))
         (%warn "CBIND: wrong number of arguments" z))
        ((not (symbolp (car z))) ; symbolp is a ops5 function
         (%warn "CBIND: illegal argument" (car z)))
        ((null? *last*)
         (%warn "CBIND: nothing added yet" (car z)))
        (t (make-ce-var-bind (car z) *last*))))

(define (externalp . args) t) ; removing the need for external

(define (external . args) t)

(define (lisp-eval l)
  (mapcar !value (eval (mapcar quote-!varbind l))))

;; =====================================================================
;; 6. lit.rkt (literalize support)
;; =====================================================================
;; File: OPS5Literalize.RKT
;;
;; Converted from lit.s (Mark Watson, "OPS5 in Scheme", 1995) to run
;; under Racket.  Loaded into the OPS5 namespace by load.rkt after
;; rhs.rkt; see README.md.  (The original load.s referred to this file
;; as "literalize.s".)

(define *ats* #f)

(define (old-literalize . l) 
 (let  ((class-name #f) (atts #f))
       	(set! class-name (car l))
       	(cond ((not (zero? *pcount*))
               (%warn "Literalize called after p" class-name)
               #f)
              ((get class-name 'att-list)
               (%warn "Attempt to redefine class" class-name) 
               #f)
              (t    
		       			   (set! *class-list* (cons class-name *class-list*))
		       	     (set! atts (ops5-remove-duplicates (cdr l)))
		             (set! *ats* (ops5-remove-duplicates (append atts *ats*)))
		             (test-attribute-names atts)
		             (mark-conflicts atts atts)
		             (put class-name 'att-list atts)))))

(define (old-vector-attribute l)
   (cond ((not (zero? *pcount*))
          (%warn "Vector attribute called after trying to compile a production"))

         (t (test-attribute-names l)
            (mapc vector-attribute2 l))))

(define (vector-attribute2 att) ; modified 3/18/86 to save names on *vector-list*
  (set! *vector-list* (cons att *vector-list*))
  (put att 'vector-attribute t))

(define (is-vector-attribute att) (get att 'vector-attribute))

(define (test-attribute-names l)
  (mapc test-attribute-names2 l))

(define (test-attribute-names2 atm)
  (cond ((or (not (symbol? atm))  (!variablep atm) )	; 2/11/85
         (%warn "Can bind only constant atoms" atm))))

(define (finish-literalize)
  (cond (*finish-lit?*
         (mapc assign-scalars *class-list*)
         (mapc assign-vectors *class-list*)
         (mapc put-ppdat *class-list*)
         (mapc erase-literal-info *class-list*) 
         (set! *finish-lit?* #f)
         (set! *buckets* #f))))

(define (put-ppdat class)
  (let ((al #f) (att #f) (ppdat #f))
    (define (loop)
       (cond ((not (or (atom? al) (null? al)))
	   		  (set! att (car al))
	          (set! al (cdr al))
	          (set! ppdat (cons (cons (get att 'ops-bind) att)  ppdat))
	          (loop))))
	 (set! ppdat #f)
  (set! al (get class 'att-list))
  (loop)         
  (put class 'ppdat ppdat)))

(define (assign-scalars class)
  (mapc assign-scalars2 (get class 'att-list)))

(define (assign-scalars2 att)
  (let ((tlist #f) (num #f) (bucket #f) (conf #f))
  
   (define (top)
     (cond ((or (atom? tlist) (null? tlist))
		          (%warn "Could not generate a binding" att)
		          (store-binding att -1)
		          #f)
		         (t
           	(set! num (caar tlist))
           	(set! bucket (cdar tlist))
          		(set! tlist (cdr tlist))
           	(cond ((not (find-common-atom bucket conf))
        	          (store-binding att num))
	                  (t (top))))))
	               
    (if (not (get att 'ops-bind))
        (if (not (is-vector-attribute att))
            (begin
              (set! tlist (buckets))
              (set! conf (get att 'conflicts))
              (top))
            #f)
        #f)))

(define (assign-vectors class)
  (mapc assign-vectors2 (get class 'att-list)))

(define (assign-vectors2 att)
  (let ((big #f) (conf #f) (new #f) (old #f) (need #f))
  
   (define (top)
     (cond ((not (or (atom? conf) (null? conf)))
		          (set! new (car conf))
		          (set! conf (cdr conf))
		          (cond ((is-vector-attribute new)
			                (%warn "Class has two vector attributes"
			                (list att new)))
		                (t (set! big (max (get new 'ops-bind) big))
		                   (top))))
		         (t
           	(set! need (1+ big))
        	   (set! old (get att 'ops-bind))
        	   (if (not old)
               	(store-binding att need) #f))))
        
   (if (not (is-vector-attribute att))
       	(begin
          (set! big 1)
          (set! conf (get att 'conflicts))
          (top)) #f)))
		
;; Renamed ops5-remove-duplicates: under #lang racket/load, free
;; references are bound at expansion time and old-literalize above is
;; compiled before this definition exists -- under the original name its
;; calls would capture racket/base's strict remove-duplicates (which
;; rejects mutable pairs and #f).  A unique name makes those calls
;; forward references resolved at run time.
(define (ops5-remove-duplicates lst)
  ;; the atom base case must return '() (MIT nil doubles as empty list
  ;; and false); returning #f would make appended lists improper
  (cond ((null? lst) '())
        ((atom? lst) #f)
        ((member (car lst) (cdr lst)) (ops5-remove-duplicates (cdr lst)))
        (t (cons (car lst) (ops5-remove-duplicates (cdr lst))))))

(define (store-binding name lit)
  (put name 'ops-bind lit)
  (add-bucket name lit))

(define (add-bucket name num)
  (let ((buc #f))
	   (set! buc (assoc num (buckets)))
	   (if (not (memq name buc))
        (if buc
 	          (set-cdr! buc (cons name (safe-cdr buc)))
            (set! buc (cons name '()))) #f)
 	buc))

(define (buckets)
  (and (atom? *buckets*) (set! *buckets* (make-nums *buckets*)))
  *buckets*)

(define (make-nums k)
  ;; MIT #f doubles as the empty list; use '() so the bucket pairs are
  ;; proper lists (their cdr is scanned with null?/memq)
  (let ((nums #f))
    (set! nums '())
    (while (> k 1)
           (set! nums (cons (cons k '()) nums))
           (set! k (-1+ k)))
    nums))

(define (erase-literal-info class)
  (mapc erase-literal-info2 (get class 'att-list))
  (remprop class 'att-list))

(define (erase-literal-info2 att) (remprop att 'conflicts))


(define (mark-conflicts rem all)
  (cond ((not (null? rem))
         (mark-conflicts2 (car rem) all)
         (mark-conflicts (cdr rem) all))))

(define (mark-conflicts2 atm lst)
  (while (pair? lst)
         (!conflict atm (car lst))
         (set! lst (cdr lst))))

(define (!conflict a b)
  (let ((old #f)) 
	   (set! old (get a 'conflicts))
	   (and (not (equal? a b))
	        (not (memq b old))
	        (put a 'conflicts (cons b old)))))

(define (find-common-atom la lb)
   (define (top)
      (cond ((null? la) #f)
		          ((memq (car la) lb) (car la))
	          	(t (set! la (cdr la)) (top))))
  (top))
	

;; ---------------------------------------------------------------------
;; Driver / REPL (from load.rkt)
;; ---------------------------------------------------------------------

(define (repl)
  (displayln "")
  (displayln "OPS5 Scheme interpreter (Racket conversion)")
  (displayln "Type OPS5 expressions, e.g.:")
  (displayln "  (load \"draw.ops\")   load a program file")
  (displayln "  (i-g-v)              initialize (or reset) OPS5")
  (displayln "  (p name lhs --> rhs) define a production")
  (displayln "  (make class ...)     add a working-memory element")
  (displayln "  (run)                run the productions")
  (displayln "  (wm)                 print working memory")
  (displayln "  (exit)               leave the REPL")
  (displayln "")
  (let loop ()
    (display "OPS5> ")
    (flush-output)
    (let ([form (read)])
      (cond
        [(eof-object? form)
         (displayln "bye")]
        [else
         (let ([v (with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
                    (eval form))])
           (unless (void? v)
             (write v)
             (newline)))
         (loop)]))))

;; Any .ops files given on the command line are loaded before the REPL starts.
(define ops-files (vector->list (current-command-line-arguments)))

(for-each load ops-files)
(repl)
