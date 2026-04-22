#lang racket

;;; microgpt.rkt — Karpathy's microGPT in dependency-free Racket

(require racket/math)

(provide run-microgpt)

;; ============================================================
;; UTILITIES
;; ============================================================

(define (random-gauss mean std)
  (+ mean (* std (sqrt (* -2 (log (max (random) 1e-15)))) (cos (* 2 pi (random))))))

(define (shuffle-list lst)
  (sort lst (lambda (a b) (< (random) 0.5))))

(define (read-lines file-path)
  (if (file-exists? file-path)
      (with-input-from-file file-path
        (lambda ()
          (let loop ([lines '()])
            (let ([line (read-line)])
              (if (eof-object? line)
                  (reverse lines)
                  (let ([tr (string-trim line)])
                    (if (positive? (string-length tr))
                        (loop (cons tr lines))
                        (loop lines))))))))
      '()))

(define *docs* '())
(define *uchars* "")
(define *bos* 0)
(define *vocab-size* 0)
(define *n-layer* 1)
(define *n-embd* 16)
(define *block-size* 16)
(define *n-head* 4)
(define *head-dim* 4)
(define *state-dict* (make-hash))
(define *params* '())

;; ============================================================
;; AUTOGRAD
;; ============================================================

(struct val (data grad children local-grads) #:mutable #:transparent)

(define (make-val d [ch '()] [lg '()])
  (val (if (val? d) (val-data d) (real->double-flonum d))
       0.0 ch lg))

(define (ens v)
  (if (val? v) v (make-val v)))

(define (u+ a b)
  (let ([a (ens a)] [b (ens b)])
    (make-val (+ (val-data a) (val-data b)) (list a b) '(1.0 1.0))))

(define (u* a b)
  (let ([a (ens a)] [b (ens b)])
    (make-val (* (val-data a) (val-data b)) (list a b) (list (val-data b) (val-data a)))))

(define (upow v p)
  (let ([v (ens v)])
    (make-val (expt (val-data v) p) (list v)
                (list (real->double-flonum (* p (expt (val-data v) (- p 1))))))))

(define (ulog v)
  (let ([v (ens v)])
    (make-val (log (val-data v)) (list v) (list (/ 1.0 (val-data v))))))

(define (uexp v)
  (let ([v (ens v)])
    (make-val (exp (val-data v)) (list v) (list (exp (val-data v))))))

(define (urelu v)
  (let ([v (ens v)])
    (make-val (max 0.0 (val-data v)) (list v) (list (if (positive? (val-data v)) 1.0 0.0)))))

(define (uneg v) (u* v -1.0))
(define (u- a b) (u+ a (uneg b)))
(define (u/ a b) (u* a (upow b -1.0)))

(define (back v)
  (define topo '())
  (define seen (make-hasheq))
  (define (build n)
    (unless (hash-has-key? seen n)
      (hash-set! seen n #t)
      (for-each build (val-children n))
      (set! topo (cons n topo))))
  (build v)
  (set-val-grad! v 1.0)
  (for ([n topo])
    (let ([grad (val-grad n)])
      (for-each (lambda (c g)
                  (set-val-grad! c (+ (val-grad c) (* g grad))))
                (val-children n)
                (val-local-grads n)))))

;; ============================================================
;; MODEL PARAMETERS
;; ============================================================

(define (make-matr nout nin [std 0.08])
  (for/vector ([i (in-range nout)])
    (for/vector ([j (in-range nin)])
      (let ([v (make-val (random-gauss 0 std))])
        (set! *params* (cons v *params*))
        v))))

(define (init-mod)
  (set! *state-dict* (make-hash))
  (set! *params* '())
  (define (mat k r c) (hash-set! *state-dict* k (make-matr r c)))
  (mat "wte" *vocab-size* *n-embd*)
  (mat "wpe" *block-size* *n-embd*)
  (mat "lm_head" *vocab-size* *n-embd*)
  (for ([i (in-range *n-layer*)])
    (define (lmat s r c) (hash-set! *state-dict* (format "layer~a.~a" i s) (make-matr r c)))
    (lmat "attn_wq" *n-embd* *n-embd*)
    (lmat "attn_wk" *n-embd* *n-embd*)
    (lmat "attn_wv" *n-embd* *n-embd*)
    (lmat "attn_wo" *n-embd* *n-embd*)
    (lmat "mlp_fc1" (* 4 *n-embd*) *n-embd*)
    (lmat "mlp_fc2" *n-embd* (* 4 *n-embd*)))
  (set! *params* (reverse *params*)))

;; ============================================================
;; ARCHITECTURE
;; ============================================================

(define (usum vs) (foldl u+ (make-val 0.0) vs))

(define (ulinear x w)
  (for/list ([row (in-vector w)])
    (foldl u+ (make-val 0.0) (map u* (vector->list row) x))))

(define (usoftmax logits)
  (let* ([mx (apply max (map val-data logits))]
         [exps (map (lambda (v) (uexp (u- v mx))) logits)]
         [sum (usum exps)])
    (map (lambda (e) (u/ e sum)) exps)))

(define (urmsnorm x)
  (let* ([x2 (map (lambda (xi) (u* xi xi)) x)]
         [mean-x2 (u/ (usum x2) (length x))]
         [sc (upow (u+ mean-x2 1e-5) -0.5)])
    (map (lambda (xi) (u* xi sc)) x)))

(define (gp tok-id pos-id keys-vec vals-vec)
  (let* ([wte (hash-ref *state-dict* "wte")]
         [wpe (hash-ref *state-dict* "wpe")]
         [x (map u+ (vector->list (vector-ref wte tok-id))
                    (vector->list (vector-ref wpe pos-id)))]
         [x (urmsnorm x)])
    (for ([li (in-range *n-layer*)])
      (let* ([xr x]
             [x-norm (urmsnorm x)]
             [wq (hash-ref *state-dict* (format "layer~a.attn_wq" li))]
             [wk (hash-ref *state-dict* (format "layer~a.attn_wk" li))]
             [wv (hash-ref *state-dict* (format "layer~a.attn_wv" li))]
             [wo (hash-ref *state-dict* (format "layer~a.attn_wo" li))]
             [q (ulinear x-norm wq)]
             [k (ulinear x-norm wk)]
             [v (ulinear x-norm wv)])
        
        (vector-set! keys-vec li (append (vector-ref keys-vec li) (list k)))
        (vector-set! vals-vec li (append (vector-ref vals-vec li) (list v)))
        
        (let* ([heads 
                (for/list ([h (in-range *n-head*)])
                  (let* ([hs (* h *head-dim*)]
                         [he (+ hs *head-dim*)]
                         [q-h (take (drop q hs) *head-dim*)]
                         [k-h (map (lambda (ki) (take (drop ki hs) *head-dim*)) (vector-ref keys-vec li))]
                         [v-h (map (lambda (vi) (take (drop vi hs) *head-dim*)) (vector-ref vals-vec li))]
                         [scores (for/list ([kt k-h])
                                   (u/ (usum (map u* q-h kt)) (sqrt *head-dim*)))]
                         [aw (usoftmax scores)])
                    (for/list ([j (in-range *head-dim*)])
                      (usum (map (lambda (w vt) (u* w (list-ref vt j))) aw v-h)))))])
          (set! x (ulinear (apply append heads) wo))
          (set! x (map u+ x xr)))
        
        (let ([xr x]
              [x-norm (urmsnorm x)]
              [fc1 (hash-ref *state-dict* (format "layer~a.mlp_fc1" li))]
              [fc2 (hash-ref *state-dict* (format "layer~a.mlp_fc2" li))])
          (set! x (map urelu (ulinear x-norm fc1)))
          (set! x (ulinear x fc2))
          (set! x (map u+ x xr)))))
    (ulinear x (hash-ref *state-dict* "lm_head"))))

;; ============================================================
;; TRAINING
;; ============================================================

(define (string-index str ch)
  (for/first ([i (in-range (string-length str))]
              #:when (char=? (string-ref str i) ch))
    i))

(define (run-train)
  (let* ([lr 0.01] [b1 0.85] [b2 0.99] [eps 1e-8] [steps 1000]
         [np (length *params*)]
         [m (make-vector np 0.0)]
         [mv (make-vector np 0.0)])
    (for ([step (in-range steps)])
      (let* ([doc (list-ref *docs* (modulo step (length *docs*)))]
             [tokens (append (list *bos*)
                             (for/list ([ch (in-string doc)])
                               (string-index *uchars* ch))
                             (list *bos*))]
             [n (min *block-size* (- (length tokens) 1))]
             [keys (for/vector ([i (in-range *n-layer*)]) '())]
             [vals (for/vector ([i (in-range *n-layer*)]) '())]
             [losses '()])
        
        (for ([pos (in-range n)])
          (let* ([logits (gp (list-ref tokens pos) pos keys vals)]
                 [probs (usoftmax logits)])
            (set! losses (cons (uneg (ulog (list-ref probs (list-ref tokens (+ pos 1))))) losses))))
        
        (let* ([loss (u/ (usum losses) n)]
               [t+1 (+ step 1)])
          ;; Reset grads
          (for ([p *params*]) (set-val-grad! p 0.0))
          (back loss)
          
          (for ([i (in-range np)]
                [p *params*])
            (let* ([grad (val-grad p)]
                   [m-val (+ (* b1 (vector-ref m i)) (* (- 1 b1) grad))]
                   [mv-val (+ (* b2 (vector-ref mv i)) (* (- 1 b2) (expt grad 2)))])
              (vector-set! m i m-val)
              (vector-set! mv i mv-val)
              (let* ([mh (/ m-val (- 1 (expt b1 t+1)))]
                     [vh (/ mv-val (- 1 (expt b2 t+1)))])
                (set-val-data! p (- (val-data p) (/ (* lr (- 1 (/ step steps)) mh) (+ (sqrt vh) eps)))))))
          
          (when (or (zero? (modulo t+1 10)) (= t+1 1))
            (printf "step ~a/~a | loss ~a\n" t+1 steps (val-data loss))
            (flush-output)))))))

;; ============================================================
;; INFERENCE
;; ============================================================

(define (random-choice weights)
  (let* ([total (apply + weights)]
         [r (* (random) total)])
    (let loop ([i 0] [acc 0.0])
      (if (>= i (- (length weights) 1))
          i
          (let ([new-acc (+ acc (list-ref weights i))])
            (if (< r new-acc)
                i
                (loop (+ i 1) new-acc)))))))

(define (run-inference)
  (printf "\n--- inference (new, hallucinated names) ---\n")
  (for ([i (in-range 20)])
    (let ([keys (for/vector ([l (in-range *n-layer*)]) '())]
          [vals (for/vector ([l (in-range *n-layer*)]) '())]
          [tok *bos*]
          [sample '()])
      (let loop ([pos 0])
        (when (< pos *block-size*)
          (let* ([logits (gp tok pos keys vals)]
                 [lt (map (lambda (l) (/ (val-data l) 0.5)) logits)]
                 [mx (apply max lt)]
                 [exps (map (lambda (v) (exp (- v mx))) lt)]
                 [sum-exps (apply + exps)]
                 [probs (map (lambda (e) (/ e sum-exps)) exps)])
            (set! tok (random-choice probs))
            (unless (= tok *bos*)
              (set! sample (cons (string-ref *uchars* tok) sample))
              (loop (+ pos 1))))))
      (printf "sample ~a: ~a\n" (+ i 1) (list->string (reverse sample))))))

;; ============================================================
;; MAIN
;; ============================================================

(define (run-microgpt)
  (set! *docs* (shuffle-list (read-lines "names.txt")))
  (printf "num docs: ~a\n" (length *docs*))
  (let ([chars '()])
    (for ([doc *docs*])
      (for ([ch (in-string doc)])
        (unless (member ch chars)
          (set! chars (cons ch chars)))))
    (set! *uchars* (list->string (sort chars char<?)))
    (set! *bos* (string-length *uchars*))
    (set! *vocab-size* (+ *bos* 1)))
  (printf "vocab size: ~a\n" *vocab-size*)
  (init-mod)
  (printf "num params: ~a\n" (length *params*))
  (run-train)
  (run-inference))

(module+ main
  (run-microgpt))
