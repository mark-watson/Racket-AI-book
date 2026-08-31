#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Extensions to the embeddingsdb example that run with no network and no
;;; API key:
;;;
;;;   - cosine-similarity: the right metric for non-normalized embeddings
;;;   - split-sentences / chunk-by-sentences: sentence-aware chunking with
;;;     configurable overlap
;;;   - a tiny deterministic hashing embedder: stands in for a real
;;;     embedding model so retrieval can be demonstrated and tested offline
;;;   - rank-chunks: scored, sorted retrieval instead of threshold filtering
;;;   - assemble-prompt: the context+question string sent to the LLM
;;;
;;; Run the demo:  racket rag_extensions.rkt
;;; Run tests:     raco test tests.rkt

(require racket/string)

(provide cosine-similarity
         magnitude
         split-sentences
         chunk-by-sentences
         hash-embed
         rank-chunks
         assemble-prompt)

;;; -----------------------------------------------------------------------------
;;; Cosine similarity
;;;
;;; Dot product answers "how much do these vectors add up together", which
;;; depends on their lengths. Cosine similarity divides by both magnitudes,
;;; so it measures only the angle between the vectors. OpenAI's
;;; text-embedding-ada-002 returns unit-length vectors, so dot product and
;;; cosine similarity give the same ranking there; with any other model you
;;; should use cosine.

(define (magnitude v)
  (sqrt (for/sum ([x v]) (* x x))))

(define (cosine-similarity a b)
  (let ([ma (magnitude a)]
        [mb (magnitude b)])
    (if (or (zero? ma) (zero? mb))
        0.0
        (/ (for/sum ([x a] [y b]) (* x y))
           (* ma mb)))))

;;; -----------------------------------------------------------------------------
;;; Sentence-aware chunking
;;;
;;; break-into-chunks in embeddingsdb.rkt cuts text every 200 characters,
;;; which often slices a sentence in half. Embedding half a sentence gives a
;;; vector for a thought that was never actually written, and the chunk the
;;; LLM receives starts mid-word. chunk-by-sentences first splits on sentence
;;; boundaries, then greedily packs whole sentences into chunks of at most
;;; CHUNK-SIZE characters, and finally appends the trailing
;;; OVERLAP characters of the previous chunk to the next one so local
;;; context is preserved across boundaries.

(define sentence-end (pregexp "[.!?]+[\"'\\)]*\\s+"))

(define (split-sentences text)
  (let loop ([rest (string-trim text)] [acc '()])
    (if (zero? (string-length rest))
        (reverse acc)
        (let ([m (regexp-match-positions sentence-end rest)])
          (if (not m)
              (reverse (cons rest acc))
              (let* ([end (cdar m)]
                     [sentence (string-trim (substring rest 0 end))])
                (loop (string-trim (substring rest end))
                      (cons sentence acc))))))))

(define (chunk-by-sentences text
                            #:chunk-size [chunk-size 500]
                            #:overlap [overlap 40])
  (define sentences (split-sentences text))
  (define (with-overlap chunk prev-chunk)
    (if (and prev-chunk (> overlap 0))
        (let ([tail (substring prev-chunk
                               (max 0 (- (string-length prev-chunk) overlap)))])
          (string-trim (string-append tail " " chunk)))
        chunk))
  ;; First pass: pack whole sentences into raw chunks of at most
  ;; CHUNK-SIZE characters (a sentence longer than CHUNK-SIZE stands alone).
  (define raw-chunks
    (let loop ([todo sentences] [current ""] [chunks '()])
      (cond
        [(null? todo)
         (if (zero? (string-length current))
             (reverse chunks)
             (reverse (cons current chunks)))]
        [else
         (define sentence (car todo))
         (define candidate
           (if (zero? (string-length current))
               sentence
               (string-append current " " sentence)))
         (cond
           [(not (> (string-length candidate) chunk-size))
            (loop (cdr todo) candidate chunks)]
           [(zero? (string-length current))
            ;; Single oversized sentence gets its own chunk.
            (loop (cdr todo) "" (cons sentence chunks))]
           [else
            (loop todo "" (cons current chunks))])])))
  ;; Second pass: prepend the trailing OVERLAP characters of each chunk to
  ;; the next, so context survives chunk boundaries.
  (if (null? raw-chunks)
      '()
      (cons (car raw-chunks)
            (for/list ([prev raw-chunks] [cur (cdr raw-chunks)])
              (with-overlap cur prev)))))

;;; -----------------------------------------------------------------------------
;;; A deterministic hashing embedder for offline demos and tests
;;;
;;; Real embedders map text to 1536 floats by running a trained neural
;;; network. Here we map text to K floats by hashing each word into one of K
;;; buckets and normalizing to unit length. Texts that share words score
;;; well, which is enough to demonstrate and test the retrieval machinery.
;;; Never use this in production: it is a stand-in with the same shape as
;;; the real thing.

(define vocab-dim 256)

(define (tokenize text)
  (regexp-split #px"[^a-z0-9]+" (string-downcase text)))

(define (hash-embed text)
  "Deterministic unit-length embedding of TEXT as a list of VOCAB-DIM floats."
  (define v (make-vector vocab-dim 0.0))
  (for ([tok (tokenize text)])
    (when (> (string-length tok) 0)
      (define h (modulo (equal-hash-code tok) vocab-dim))
      (vector-set! v h (+ 1.0 (vector-ref v h)))))
  (define m (magnitude (vector->list v)))
  (if (zero? m)
      (vector->list v)
      (map (lambda (x) (/ x m)) (vector->list v))))

;;; -----------------------------------------------------------------------------
;;; Retrieval and prompt assembly

(define (rank-chunks query chunks
                     #:embed [embed hash-embed]
                     #:top-k [top-k (length chunks)])
  "Rank CHUNKS (list of strings) against QUERY by embedding similarity.
   Returns a list of (score . chunk) pairs, best first, at most TOP-K."
  (define q-emb (embed query))
  (define scored
    (for/list ([chunk chunks])
      (cons (cosine-similarity q-emb (embed chunk)) chunk)))
  (take (sort scored > #:key car) (min top-k (length scored))))

(define (assemble-prompt contexts custom-context query)
  "Build the exact string sent to the LLM: retrieved context, any extra
   context the caller supplies, then the question."
  (string-join (list (string-join contexts " . ")
                     custom-context
                     "Question:" query)
               " "))

;;; -----------------------------------------------------------------------------
;;; Demo
;;;
;;; Uses hash-embed against small inline documents so the whole pipeline
;;; runs offline.

(module+ main
  (define docs
    '("Amyl alcohol is an organic compound with the formula C 5 H 12 O. \
ZorroOnian Alcohol is another organic compound with the formula C 6 H 10 O. \
All eight isomers of amyl alcohol are known."
      "Robert Boyle is known as one of the pioneers of modern chemistry. \
He is famous for Boyle's Law, which describes the inverse relationship \
between the pressure and volume of a gas."
      "Playing sports improves cardiovascular health, builds muscle, and \
teaches teamwork. Regular exercise also reduces stress and improves sleep."
      "Dmitri Mendeleev published the periodic table in 1869, organizing the \
known elements by atomic weight and chemical properties."))

  (define (demo query)
    (define ranked (rank-chunks query docs #:top-k 2))
    (printf "Query: ~a\n" query)
    (for ([pair ranked])
      (printf "  score ~a  ~a\n"
              (~r (car pair) #:precision 4)
              (truncate (cdr pair) 70)))
    (newline))

  (define (truncate s n)
    (if (> (string-length s) n)
        (string-append (substring s 0 n) "...")
        s))

  (printf "\n== Retrieval over ~a tiny documents (hash embedder) ==\n\n"
          (length docs))
  (demo "what is the formula for ZorroOnian Alcohol?")
  (demo "who is Robert Boyle?")
  (demo "tell me about team sports and exercise")

  (printf "== Assembled RAG prompt for the top match ==\n\n")
  (define q "who is Robert Boyle?")
  (define best (cdr (car (rank-chunks q docs #:top-k 1))))
  (displayln (assemble-prompt (list best) "" q)))
