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
