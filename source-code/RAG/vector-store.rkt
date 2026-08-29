#lang racket

;;; vector-store.rkt — In-memory vector store for document chunks
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

;;; Simple in-memory vector store. Documents are chunked, embedded,
;;; and stored as vectors with precomputed norms.
;;; Retrieval uses brute-force cosine similarity. This is intentionally
;;; simple for clarity; production systems would use a dedicated
;;; vector database.

(require "embeddings.rkt")

(provide (struct-out document-chunk)
         make-corpus
         corpus?
         corpus-name
         corpus-description
         corpus-chunks
         set-corpus-chunks!
         make-document-chunk/embedded
         document-chunk-key
         corpus-chunk-count
         *default-chunk-size*
         *chunk-overlap*
         split-into-chunks
         add-document
         save-corpus
         load-corpus
         score-chunks
         search-corpus
         search-corpora
         format-retrieved-chunks)

;;; ---- Custom printers ----

(define (print-embedding-preview vec out)
  ; Print up to 10 leading values of VEC plus a dimension count.
  (define n (vector-length vec))
  (display "#(" out)
  (for ([i (in-range (min 10 n))])
    (when (> i 0) (display " " out))
    (display (vector-ref vec i) out))
  (when (> n 10) (display " ..." out))
  (display ") [" out)
  (display n out)
  (display " dimensions]" out))

(define (print-document-chunk chunk out mode)
  ; Custom printer for document-chunk: text and source in full, but only
  ; the first 10 embedding values, so REPL inspection of corpora does not
  ; dump thousands of floats.
  (display "#<document-chunk :source " out)
  (write (document-chunk-source chunk) out)
  (display " :text " out)
  (write (document-chunk-text chunk) out)
  (display " :embedding " out)
  (print-embedding-preview (document-chunk-embedding chunk) out)
  (display ">" out))

(define (print-corpus c out mode)
  ; Custom printer for corpus: name, description, and chunk count instead
  ; of every chunk. The chunks are reachable with (corpus-chunks c).
  (display "#<corpus :name " out)
  (write (corpus-name c) out)
  (display " :description " out)
  (write (corpus-description c) out)
  (display " :chunks " out)
  (display (length (corpus-chunks c)) out)
  (display ">" out))

;;; ---- Structs ----

(struct document-chunk (text source embedding norm)
  #:methods gen:custom-write
  [(define write-proc print-document-chunk)]
  ; A chunk of text with its source file, embedding vector (a normalized
  ; flonum vector), and precomputed L2 norm (1.0 for normalized chunks).
  )

(struct corpus (name description [chunks #:mutable])
  #:methods gen:custom-write
  [(define write-proc print-corpus)]
  ; A named collection of document chunks for retrieval.
  )

(define (make-corpus #:name name #:description description)
  (corpus name description '()))

(define (document-chunk-key chunk)
  ; Stable identity for a chunk across corpora: (source . text). The
  ; same text in different source files is a different chunk.
  (cons (document-chunk-source chunk) (document-chunk-text chunk)))

(define (corpus-chunk-count c)
  ; Number of document chunks currently stored in C.
  (length (corpus-chunks c)))

;;; ---- Chunking ----

(define *default-chunk-size* 500)
; Default size in characters for splitting documents into chunks.

(define *chunk-overlap* 50)
; Number of characters to overlap between adjacent chunks.

(define (last-index-of-char str ch)
  ; Return the last index of CH in STR, or #f.
  (let loop ([i (- (string-length str) 1)])
    (cond
      [(< i 0) #f]
      [(char=? (string-ref str i) ch) i]
      [else (loop (- i 1))])))

(define (split-into-chunks text
                           #:chunk-size [chunk-size *default-chunk-size*]
                           #:overlap [overlap *chunk-overlap*])
  ; Split TEXT into overlapping chunks of approximately CHUNK-SIZE characters.
  ; Tries to break at sentence boundaries when possible.
  (define len (string-length text))
  (let loop ([start 0] [chunks '()])
    (if (>= start len)
        (reverse chunks)
        (let* ([end (min (+ start chunk-size) len)]
               ; Try to find a sentence boundary at or before END
               ; (searching the window up to 80 chars back from END)
               [break-pos
                (if (>= end len)
                    end
                    (let* ([search-from (max start (- end 80))]
                           [window (substring text search-from end)]
                           [period-idx (last-index-of-char window #\.)]
                           [newline-idx (last-index-of-char window #\newline)]
                           [idx (max (or period-idx -1) (or newline-idx -1))])
                      (if (>= idx 0)
                          (+ search-from idx)
                          end)))]
               ; Advance past the break character
               [actual-end* (if (< break-pos end) (+ break-pos 1) end)])
          ; Guarantee forward progress: if the break search left us
          ; at or before START, fall back to a hard cut at CHUNK-SIZE.
          ; Without this guard a chunk could be empty or START could
          ; fail to advance (looping forever or dropping text).
          (define actual-end
            (if (<= actual-end* start)
                (min (+ start chunk-size) len)
                actual-end*))
          (define chunk
            (string-trim (substring text start actual-end)))
          (define new-chunks
            (if (> (string-length chunk) 0)
                (cons chunk chunks)
                chunks))
          ; Never move START backwards: the overlap backtrack must
          ; not undo progress made by the forward-progress guard.
          (define new-start
            (if (>= actual-end len)
                len
                (max (+ start 1) (- actual-end overlap))))
          (loop new-start new-chunks)))))

;;; ---- Chunk construction ----

(define (make-document-chunk/embedded text source raw-embedding)
  ; Build a document-chunk with a normalized flonum-vector embedding and
  ; its precomputed norm. All chunks in the store are normalized, so
  ; search is a dot product (see search-corpus).
  (define vec (let ([v (if (vector? raw-embedding)
                           (list->vector (map exact->inexact (vector->list raw-embedding)))
                           (list->vector (map exact->inexact raw-embedding)))])
                (normalize-vector v)))
  (define norm (vector-magnitude vec))
  (when (zero? norm)
    (error "Zero-magnitude embedding for chunk from ~a; cannot normalize" source))
  (document-chunk text source vec norm))

;;; ---- Document loading ----

(define (add-document c filepath #:chunk-size [chunk-size *default-chunk-size*])
  ; Read a text file, split it into chunks, compute embeddings (in
  ; batched API calls when the default embedding function is used),
  ; and add the chunks to C. Returns the number of chunks added.
  (debug-log "~%DEBUG add-document: loading ~a~%" filepath)
  (define text (file->string filepath))
  (define chunks (split-into-chunks text #:chunk-size chunk-size))
  (define source (path->string (file-name-from-path filepath)))
  (debug-log "DEBUG add-document: split into ~a chunks~%" (length chunks))
  (define embeddings (get-embeddings chunks))
  (set-corpus-chunks!
   c
   (append (corpus-chunks c)
           (map (lambda (chunk-text embedding)
                  (make-document-chunk/embedded chunk-text source embedding))
                chunks embeddings)))
  (debug-log "DEBUG add-document: added ~a chunks from ~a~%"
             (length chunks) source)
  (length chunks))

;;; ---- Corpus persistence ----

(define (save-corpus c pathname)
  ; Write C (name, description, chunks with embeddings) to PATHNAME
  ; as a single s-expression. Load it back with load-corpus. Saving lets
  ; you avoid re-embedding documents (and re-paying API calls) each run.
  (call-with-output-file pathname
    #:exists 'replace
    (lambda (out)
      (write (list (cons 'name (corpus-name c))
                   (cons 'description (corpus-description c))
                   (cons 'chunks
                         (map (lambda (chunk)
                                (list (cons 'text (document-chunk-text chunk))
                                      (cons 'source (document-chunk-source chunk))
                                      (cons 'embedding (vector->list (document-chunk-embedding chunk)))))
                              (corpus-chunks c))))
             out)))
  pathname)

(define (%valid-chunk-data? chunk-data)
  ; True when one saved chunk alist has non-empty TEXT and SOURCE and an
  ; EMBEDDING that is a non-empty list of numbers.
  (and (list? chunk-data)
       (let ([text (let ([p (assq 'text chunk-data)]) (and p (cdr p)))])
         (and (string? text) (> (string-length text) 0)))
       (let ([source (let ([p (assq 'source chunk-data)]) (and p (cdr p)))])
         (and (string? source) (> (string-length source) 0)))
       (let ([emb (let ([p (assq 'embedding chunk-data)]) (and p (cdr p)))])
         (and (list? emb) (> (length emb) 0) (andmap number? emb)))))

(define (load-corpus pathname)
  ; Load a corpus previously written by save-corpus. Returns a corpus struct.
  ; Signals an error when the file is truncated, corrupt, or contains a
  ; chunk missing its text, source, or embedding.
  (define data (call-with-input-file pathname read))
  (unless (and (list? data)
               (assq 'name data)
               (let ([p (assq 'chunks data)])
                 (and p (list? (cdr p)) (> (length (cdr p)) 0))))
    (error "Corrupt corpus file ~a: expected ((name . ...) (chunks . ...))" pathname))
  (define c (corpus (cdr (assq 'name data))
                    (let ([p (assq 'description data)]) (and p (cdr p)))
                    '()))
  (set-corpus-chunks!
   c
   (map (lambda (chunk-data)
          (unless (%valid-chunk-data? chunk-data)
            (error "Corrupt chunk in corpus file ~a: ~s" pathname chunk-data))
          ; Re-normalize on load: files saved by older
          ; versions may hold un-normalized vectors.
          (make-document-chunk/embedded
           (cdr (assq 'text chunk-data))
           (cdr (assq 'source chunk-data))
           (cdr (assq 'embedding chunk-data))))
        (cdr (assq 'chunks data))))
  c)

;;; ---- Retrieval ----

(define (score-chunks chunks query-embedding #:query-norm [query-norm 1.0])
  ; Score CHUNKS against QUERY-EMBEDDING. Chunks are stored normalized,
  ; so cosine similarity is the dot product divided by the query norm;
  ; each chunk's norm is not recomputed.
  (map (lambda (chunk)
         (cons (/ (dot-product query-embedding (document-chunk-embedding chunk))
                  query-norm)
               chunk))
       chunks))

(define (%top-k-by-score scored-chunks top-k)
  ; Return the TOP-K entries of SCORED-CHUNKS (sorted by descending car)
  ; using a single O(n) selection pass instead of a full O(n log n) sort.
  (define k (min top-k (length scored-chunks)))
  (if (zero? k)
      '()
      ; Repeatedly extract the max: k passes, each O(n). Worst case
      ; k = n is O(n^2), but k is small (3 by default), so this beats
      ; sorting at demo scale and stays O(n) for constant k.
      (let loop ([remaining scored-chunks] [result '()] [i 0])
        (if (>= i k)
            (reverse result)
            (let* ([best-score (apply max (map car remaining))]
                   [winner (findf (lambda (entry) (= (car entry) best-score)) remaining)])
              (loop (remq winner remaining)
                    (cons winner result)
                    (+ i 1)))))))

(define (search-corpus c query-embedding #:top-k [top-k 3])
  ; Search C for the TOP-K chunks most similar to QUERY-EMBEDDING.
  ; Returns a list of (score . document-chunk) pairs, sorted by
  ; descending similarity. The query embedding may be normalized or raw;
  ; its norm is computed once here.
  (define q-vec (if (vector? query-embedding)
                    query-embedding
                    (list->vector (map exact->inexact query-embedding))))
  (%top-k-by-score
   (score-chunks (corpus-chunks c) q-vec
                 #:query-norm (vector-magnitude q-vec))
   top-k))

(define (search-corpora corpora query-embedding #:top-k [top-k 3])
  ; Search multiple CORPORA for the TOP-K most similar chunks overall.
  ; Returns a list of (score . document-chunk) pairs.
  (define q-vec (if (vector? query-embedding)
                    query-embedding
                    (list->vector (map exact->inexact query-embedding))))
  (define q-norm (vector-magnitude q-vec))
  (define all-results
    (append-map
     (lambda (c)
       (%top-k-by-score
        (score-chunks (corpus-chunks c) q-vec #:query-norm q-norm)
        top-k))
     corpora))
  (%top-k-by-score all-results top-k))

(define (format-retrieved-chunks scored-chunks)
  ; Format scored chunks into a text string for use as LLM context.
  ; Each chunk is labeled with its source and similarity score.
  (with-output-to-string
   (lambda ()
     (for ([pair scored-chunks]
            [i (in-naturals 1)])
       (define score (car pair))
       (define chunk (cdr pair))
       (define score-str (~r score #:precision 2))
       (printf "~%--- Retrieved Passage ~a (source: ~a, relevance: ~a) ---~%~a~%"
               i
               (document-chunk-source chunk)
               score-str
               (document-chunk-text chunk))))))
