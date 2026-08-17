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

