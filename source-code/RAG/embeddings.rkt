#lang racket

;;; embeddings.rkt — Gemini embedding integration
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

;;; Uses the Gemini gemini-embedding-001 model for computing document
;;; and query embeddings (text-embedding-004 was retired from the
;;; v1beta API). This model is inexpensive and available on the free tier.
;;; The API key is sent in the x-goog-api-key header, never in the URL.

(require net/http-easy)
(require json)

(provide *rag-verbose*
         debug-log
         (struct-out exn:fail:http)
         call-with-retries
         *retry-sleep-fn*
         *embedding-model*
         *embedding-dimension*
         *embedding-batch-limit*
         *embedding-cache-cap*
         *embedding-cache*
         clear-embedding-cache
         embedding-cache-key
         %fetch-embedding
         *embedding-fn*
         *batch-request-fn*
         get-embedding
         get-embeddings
         dot-product
         vector-magnitude
         cosine-similarity
         normalize-vector)

;;; ---- Verbosity control ----

(define *rag-verbose* (make-parameter #t))
; When true (the default), the pipeline prints DEBUG tracing showing
; each agent's decisions. Set to #f for quiet library use.

(define-syntax-rule (debug-log control arg ...)
  ; Like printf but suppressed when *rag-verbose* is #f.
  (when (*rag-verbose*)
    (printf control arg ...)
    (flush-output)))

;;; ---- Exn type for HTTP errors (transient retry logic) ----

(struct exn:fail:http exn:fail (status) #:transparent)

;;; ---- Retry helper ----

(define (%transient? e)
  ; True when E is worth retrying: HTTP 429/5xx, or a connection-level
  ; failure. Permanent 4xx client errors (bad request, bad API key,
  ; wrong model name) signal immediately.
  (cond
    [(exn:fail:http? e)
     (let ([status (exn:fail:http-status e)])
       (or (= status 429) (>= status 500)))]
    [(exn:fail:network? e) #t]
    [else #f]))

(define *retry-sleep-fn* (make-parameter sleep))
; Function called to pause between retries. Rebind to a no-op in
; tests so retry backoff does not slow the suite down.

(define (call-with-retries thunk #:attempts [attempts 3] #:initial-delay [initial-delay 1.0])
  ; Call THUNK, retrying transient failures (HTTP 429/5xx, connection
  ; errors) with exponential backoff (1s, 2s, 4s by default). Permanent
  ; HTTP 4xx errors signal immediately; after ATTEMPTS transient
  ; failures an error is signaled.
  (let loop ([attempt 1]
             [delay initial-delay])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (cond
                         [(not (%transient? e))
                          (error "Non-transient API error: ~a" (exn-message e))]
                         [(>= attempt attempts)
                          (error "API request failed after ~a attempts: ~a"
                                 attempts (exn-message e))]
                         [else
                          (debug-log "~%DEBUG call-with-retries: attempt ~a/~a failed (~a), retrying in ~a seconds~%"
                                     attempt attempts (exn-message e) delay)
                          ((*retry-sleep-fn*) delay)
                          (loop (+ attempt 1) (* delay 2))]))])
      (thunk))))

;;; ---- Configuration ----

(define *embedding-model* (make-parameter "gemini-embedding-001"))
; Gemini embedding model name. If you change this you must re-embed
; existing corpora: saved corpus files hold vectors from the old
; model/dimension and search signals a dimension mismatch.

(define *embedding-dimension* (make-parameter #f))
; Output embedding dimension, or #f for the model default (3072 for
; gemini-embedding-001). The API accepts 768, 1536, or 3072 for this
; model; 768 saves 4x memory and search time with little quality
; loss. Set before building or loading a corpus.

(define *embedding-batch-limit* (make-parameter 100))
; Maximum texts per batchEmbedContents request; the API rejects more.

(define *embedding-api-url*
  "https://generativelanguage.googleapis.com/v1beta/models/")

(define (get-google-api-key)
  (or (getenv "GOOGLE_API_KEY")
      (error "GOOGLE_API_KEY environment variable is not set")))

;;; ---- Embedding cache ----

(define *embedding-cache-cap* (make-parameter 50000))
; Maximum entries kept in *embedding-cache*. When the cap is reached
; the cache is cleared and refilled. #f means never evict.

(define *embedding-cache* (make-parameter (make-hash)))
; Memoizes embeddings so re-running demos or tests does not re-embed
; previously seen text. Keyed on (model . text). See
; *embedding-cache-cap* for eviction.

(define (embedding-cache-key text)
  (cons (*embedding-model*) text))

(define (clear-embedding-cache)
  (hash-clear! (*embedding-cache*)))

(define (%cache-put text vec)
  ; Store VEC for TEXT, evicting the whole cache when the cap is reached
  ; (simple and safe; a full rebuild costs a few batched API calls).
  (when (and (*embedding-cache-cap*)
             (>= (hash-count (*embedding-cache*)) (*embedding-cache-cap*)))
    (debug-log "~%DEBUG embedding cache reached ~a entries; clearing~%"
               (*embedding-cache-cap*))
    (hash-clear! (*embedding-cache*)))
  (hash-set! (*embedding-cache*) (embedding-cache-key text) vec))

;;; ---- HTTP helpers ----

(define (%post-json url payload)
  ; POST PAYLOAD (a jsexpr) to URL with the API key header, raising
  ; exn:fail:http on non-2xx responses so call-with-retries can
  ; classify transient vs permanent failures.
  (define resp
    (post url
          #:headers (hash 'Content-Type "application/json"
                          'x-goog-api-key (get-google-api-key))
          #:json payload))
  (define code (response-status-code resp))
  (when (>= code 400)
    (raise (exn:fail:http
            (format "HTTP ~a from embedding API: ~a" code (response-body resp))
            (current-continuation-marks)
            code)))
  (bytes->string/utf-8 (response-body resp)))

;;; ---- Low-level API calls (with error checking and retries) ----

(define (%make-embedding-request text)
  ; Build one EmbedContentRequest for TEXT as a jsexpr.
  (define req
    (hash 'model (string-append "models/" (*embedding-model*))
          'content (hash 'parts (list (hash 'text text)))))
  ; gemini-embedding-001 honors the deprecated top-level
  ; outputDimensionality field; newer models honor embedContentConfig.
  ; Send both so either model works.
  (if (*embedding-dimension*)
      (hash-set* req
                 'outputDimensionality (*embedding-dimension*)
                 'embedContentConfig (hash 'outputDimensionality (*embedding-dimension*)))
      req))

(define (%decode-embedding-response response-string)
  ; Decode an embedContent response, checking for API errors.
  (define decoded (string->jsexpr response-string))
  (when (hash-has-key? decoded 'error)
    (error "Gemini embedding API error: ~a" response-string))
  (define embedding-obj (hash-ref decoded 'embedding (hash)))
  (define values-list (hash-ref embedding-obj 'values #f))
  (unless values-list
    (error "Gemini embedding response contained no embedding vector: ~a"
           response-string))
  values-list)

(define (%fetch-embedding text)
  ; Compute an embedding vector for TEXT via the embedContent endpoint.
  ; Returns a vector of flonums. Retries transient failures.
  (define api-url
    (string-append *embedding-api-url* (*embedding-model*) ":embedContent"))
  (define req (%make-embedding-request text))
  (define payload
    (hash 'content (hash-ref req 'content)
          'model (hash-ref req 'model)))
  (define payload*
    (if (*embedding-dimension*)
        (hash-set* payload
                   'outputDimensionality (*embedding-dimension*)
                   'embedContentConfig (hash 'outputDimensionality (*embedding-dimension*)))
        payload))
  (list->vector
   (map exact->inexact
        (%decode-embedding-response
         (call-with-retries (lambda () (%post-json api-url payload*)))))))

(define (%post-batch-request texts)
  ; POST one batchEmbedContents request for TEXTS (at most
  ; *embedding-batch-limit* of them) and return the decoded list of
  ; embedding vectors in the same order.
  (define api-url
    (string-append *embedding-api-url* (*embedding-model*) ":batchEmbedContents"))
  (define payload
    (hash 'requests (map %make-embedding-request texts)))
  (define response-string
    (call-with-retries (lambda () (%post-json api-url payload))))
  (define decoded (string->jsexpr response-string))
  (when (hash-has-key? decoded 'error)
    (error "Gemini batch embedding API error: ~a" response-string))
  (define embeddings (hash-ref decoded 'embeddings '()))
  (unless (= (length embeddings) (length texts))
    (error "batchEmbedContents returned ~a embeddings for ~a texts: ~a"
           (length embeddings) (length texts) response-string))
  (for/list ([embedding-obj embeddings])
    (define values-list (hash-ref embedding-obj 'values #f))
    (unless values-list
      (error "Embedding without values: ~a" response-string))
    values-list))

(define *batch-request-fn* (make-parameter %post-batch-request))
; Function of one argument (a list of texts, at most
; *embedding-batch-limit* long) returning a list of embedding vectors
; in the same order. Rebind in tests to stub the HTTP layer.

(define (%fetch-embeddings-batch texts)
  ; Compute embeddings for all TEXTS, splitting into batches of at most
  ; *embedding-batch-limit* texts per batchEmbedContents request (the
  ; API cap). Returns a list of vectors in the same order as TEXTS.
  (let loop ([remaining texts])
    (if (null? remaining)
        '()
        (let ([batch (take remaining (min (*embedding-batch-limit*) (length remaining)))]
              [rest (drop remaining (min (*embedding-batch-limit*) (length remaining)))])
          (append ((*batch-request-fn*) batch) (loop rest))))))

;;; ---- Public embedding interface (injectable for tests) ----

(define *embedding-fn* (make-parameter %fetch-embedding))
; Function of one argument (a string) returning an embedding vector.
; Rebind this in tests to run the pipeline without network access.
; When left at its default, get-embeddings batches all cache misses
; through *batch-request-fn* instead.

(define (get-embedding text)
  ; Compute (or retrieve from cache) an embedding vector for TEXT.
  ; Returns a vector of flonums.
  (define key (embedding-cache-key text))
  (define cached (hash-ref (*embedding-cache*) key #f))
  (if cached
      (begin
        (debug-log "~%DEBUG get-embedding: cache hit for ~s~%"
                   (substring text 0 (min 60 (string-length text))))
        cached)
      (let ([vec (list->vector (map exact->inexact ((*embedding-fn*) text)))])
        (%cache-put text vec)
        (debug-log "~%DEBUG get-embedding: got ~a-dimensional vector for ~s~%"
                   (vector-length vec)
                   (substring text 0 (min 60 (string-length text))))
        vec)))

(define (get-embeddings texts)
  ; Compute embeddings for a list of TEXTS. When the default embedding
  ; function is in use, all cache misses are fetched with batched
  ; batchEmbedContents API calls (at most *embedding-batch-limit* texts
  ; per request) instead of one HTTP round trip per text.
  (when (eq? (*embedding-fn*) %fetch-embedding)
    (define misses
      (remove-duplicates
       (filter (lambda (text)
                 (not (hash-has-key? (*embedding-cache*) (embedding-cache-key text))))
               texts)))
    (unless (null? misses)
      (debug-log "~%DEBUG get-embeddings: batch-fetching ~a embeddings~%"
                 (length misses))
      (for ([text misses]
            [vec (%fetch-embeddings-batch misses)])
        (%cache-put text
                    (if (vector? vec)
                        vec
                        (list->vector (map exact->inexact vec)))))))
  (map get-embedding texts))

;;; ---- Vector math ----

(define (->float-vector vec)
  ; Coerce a list or vector of numbers to a flonum vector.
  (if (vector? vec)
      (list->vector (map exact->inexact (vector->list vec)))
      (list->vector (map exact->inexact vec))))

(define (dot-product vec-a vec-b)
  ; Compute the dot product of two equal-length vectors of floats.
  ; Signals an error on length mismatch: silently truncating would hide
  ; a model/dimension change and corrupt similarity scores.
  (define a (->float-vector vec-a))
  (define b (->float-vector vec-b))
  (unless (= (vector-length a) (vector-length b))
    (error "Embedding dimension mismatch: ~a vs ~a (did *embedding-model* or *embedding-dimension* change after the corpus was built?)"
           (vector-length a) (vector-length b)))
  (for/sum ([x (in-vector a)]
            [y (in-vector b)])
    (* x y)))

(define (vector-magnitude vec)
  ; Compute the magnitude (L2 norm) of a vector of floats.
  (define v (->float-vector vec))
  (sqrt (for/sum ([x (in-vector v)]) (* x x))))

(define (cosine-similarity vec-a vec-b)
  ; Compute cosine similarity between two embedding vectors.
  ; Returns a value between -1 and 1.
  (define mag-a (vector-magnitude vec-a))
  (define mag-b (vector-magnitude vec-b))
  (if (or (zero? mag-a) (zero? mag-b))
      0.0
      (/ (dot-product vec-a vec-b) (* mag-a mag-b))))

(define (normalize-vector vec)
  ; Return VEC scaled to unit length (as a flonum vector). Normalized
  ; vectors make cosine similarity a plain dot product, so chunk norms
  ; are computed once at add time instead of per query. Vectors whose
  ; magnitude is already 1 within single-float precision are returned
  ; unchanged so normalization stays idempotent.
  (define v (->float-vector vec))
  (define mag (vector-magnitude v))
  (cond
    [(or (zero? mag) (<= (abs (- 1.0 mag)) 1e-6)) v]
    [else
     (list->vector
      (map (lambda (x) (/ x mag)) (vector->list v)))]))
