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
		       	     (set! atts (remove-duplicates (cdr l)))
		             (set! *ats* (remove-duplicates (append atts *ats*)))
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
		
(define (remove-duplicates lst)
  ;; the atom base case must return '() (MIT nil doubles as empty list
  ;; and false); returning #f would make appended lists improper
  (cond ((null? lst) '())
        ((atom? lst) #f)
        ((member (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
        (t (cons (car lst) (remove-duplicates (cdr lst))))))

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
	
