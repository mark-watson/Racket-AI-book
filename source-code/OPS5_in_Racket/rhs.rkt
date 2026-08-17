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

