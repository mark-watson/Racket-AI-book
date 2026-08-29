#lang racket

;;; tests.rkt — Offline unit tests for the RAG system
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; These tests require no network access: *embedding-fn*,
;;; *batch-request-fn*, and *generate-fn* are rebound to deterministic
;;; stubs. Run with:
;;;   racket tests.rkt

(require rackunit)
(require rackunit/text-ui)
(require "embeddings.rkt")
(require "vector-store.rkt")
(require "agents.rkt")
(require "main.rkt")

;;; ---- Chunking ----

(define (test-split-into-chunks)
  ; Empty text
  (check-equal? (split-into-chunks "") '())
  ; Text shorter than chunk-size -> single chunk
  (check-equal? (split-into-chunks "Short text." #:chunk-size 500)
                '("Short text."))
  ; Prose with sentence boundaries: chunks non-empty and bounded in size
  (let* ([sentence "The quick brown fox jumps over the lazy dog. "]
         [text (apply string-append (make-list 40 sentence))]
         [chunks (split-into-chunks text #:chunk-size 100 #:overlap 10)])
    (check-true (> (length chunks) 3))
    (check-true (andmap (lambda (c) (> (string-length c) 0)) chunks))
    (check-true (andmap (lambda (c) (<= (string-length c) 101)) chunks)))
  ; Regression: text with a sentence break only at position 0 and
  ; no later breaks. The old loop moved START backwards (never
  ; terminating or erroring); the guard must guarantee strict forward
  ; progress.
  (let* ([text (string-append "." (make-string 300 #\x))]
         [chunks (split-into-chunks text #:chunk-size 30 #:overlap 10)])
    (check-true (> (length chunks) 5))
    (check-true (andmap (lambda (c) (> (string-length c) 0)) chunks))
    ; Coverage: the final chunk must contain the end of the text
    (check-true (regexp-match? "xxxx" (last chunks))))
  ; Chunks must appear in document order
  (let* ([text "AAAA. BBBB. CCCC. DDDD. EEEE. FFFF. GGGG. HHHH."]
         [chunks (split-into-chunks text #:chunk-size 15 #:overlap 3)])
    (check-true (string-contains? (first chunks) "AAAA"))))

;;; ---- Query line parsing (regression: digit-trimming bug) ----

(define (test-parse-query-lines)
  ; Plain queries pass through untouched, including digits
  (check-equal? (parse-query-lines "2024 lithium battery prices")
                '("2024 lithium battery prices"))
  (check-equal? (parse-query-lines "1.5 MW turbine output")
                '("1.5 MW turbine output"))
  (check-equal? (parse-query-lines "75-100 kg CO2 per kWh")
                '("75-100 kg CO2 per kWh"))
  (check-equal? (parse-query-lines "CO2 emissions 2024 targets")
                '("CO2 emissions 2024 targets"))
  ; Numbered list prefixes are stripped for ANY number, not just 1-3
  (check-equal? (parse-query-lines "4. fourth query\n5. fifth query")
                '("fourth query" "fifth query"))
  ; Bullet and dash prefixes are stripped
  (check-equal? (parse-query-lines "- a query\n* another query")
                '("a query" "another query"))
  ; "1.5" with no space after the dot is query text, not a list prefix
  (check-equal? (parse-query-lines "1.5 MW output") '("1.5 MW output"))
  ; Empty lines dropped; empty/nil response handled
  (check-equal? (parse-query-lines "\n\nonly one\n\n")
                '("only one"))
  (check-equal? (parse-query-lines #f) '())
  (check-equal? (parse-query-lines "") '()))

;;; ---- Vector math ----

(define (test-vector-math)
  (check-= (dot-product '(1 2 3) '(4 5 6)) 32 1e-9)
  (check-equal? (vector-magnitude '(3 4)) 5.0)
  (check-= (cosine-similarity '(1 0 0) '(1 0 0)) 1.0 1e-9)
  (check-= (cosine-similarity '(1 0 0) '(0 1 0)) 0.0 1e-6)
  (check-= (cosine-similarity '(0 0 0) '(1 1 1)) 0.0 1e-9)
  ; Regression: mismatched dimensions must signal, not truncate
  (check-exn exn:fail? (lambda () (dot-product '(1 2 3) '(4 5 6 7 8))))
  (check-exn exn:fail? (lambda () (cosine-similarity '(1 0 0) '(1 0 0 0 0))))
  ; Works with vectors as well as lists
  (check-= (dot-product #(1 2 3) #(4 5 6)) 32 1e-9)
  ; normalize-vector: unit output, zero-safe, idempotent
  (let ([normed (normalize-vector '(3 4))])
    (check-true (< (abs (- 1.0 (vector-magnitude normed))) 1e-9))
    (check-true (< (abs (- (dot-product '(3 4) normed) 5.0)) 1e-9)))
  (check-equal? (normalize-vector #(0 0 0)) #(0.0 0.0 0.0))
  ; cosine-similarity of normalized vectors equals the dot product
  ; (single-float rounding allows ~1e-6, not 1e-9)
  (let ([a (normalize-vector '(1 2 3))]
        [b (normalize-vector '(2 1 1))])
    (check-true (< (abs (- (cosine-similarity a b)
                           (dot-product a b)))
                   1.0e-6))
    ; normalization is idempotent within single-float precision
    (check-equal? a (normalize-vector a))))

;;; ---- Retrieval ----

(define (make-test-chunk text embedding [source "test.txt"])
  (make-document-chunk/embedded text source embedding))

(define (make-test-corpus)
  (define c (make-corpus #:name "test" #:description "test corpus"))
  (set-corpus-chunks!
   c
   (list (make-test-chunk "alpha" '(1.0 0.0 0.0))
         (make-test-chunk "beta"  '(0.9 0.1 0.0))
         (make-test-chunk "gamma" '(0.0 1.0 0.0))
         (make-test-chunk "delta" '(0.0 0.0 1.0))))
  c)

(define (test-search-corpus)
  (let ([corpus (make-test-corpus)])
    ; Ranking: most similar first
    (let ([results (search-corpus corpus '(1.0 0.0 0.0) #:top-k 2)])
      (check-equal? 2 (length results))
      (check-equal? "alpha" (document-chunk-text (cdr (first results))))
      (check-equal? "beta"  (document-chunk-text (cdr (second results))))
      (check-true (> (car (first results)) (car (second results)))))
    ; top-k larger than the corpus is clamped
    (check-equal? 4 (length (search-corpus corpus '(1.0 0.0 0.0) #:top-k 99))))
  ; Raw (un-normalized) query embeddings get the same scores as
  ; normalized ones: search must normalize by the query norm
  (let ([corpus (make-test-corpus)])
    (check-true
     (< (abs (- (car (first (search-corpus corpus '(2.0 0.0 0.0) #:top-k 1)))
                (car (first (search-corpus corpus '(1.0 0.0 0.0) #:top-k 1)))))
        1e-9))))

(define (test-search-corpora)
  (let* ([corpus-a (make-test-corpus)]
         [corpus-b (make-corpus #:name "test-b" #:description "second")])
    (set-corpus-chunks!
     corpus-b
     (list (make-test-chunk "best" '(0.99 0.01 0.0) "b.txt")))
    (let ([results (search-corpora (list corpus-a corpus-b)
                                   '(1.0 0.0 0.0) #:top-k 2)])
      (check-equal? 2 (length results))
      ; Cross-corpus re-sort: alpha (1.0) still beats best (~0.99)
      (check-equal? "alpha" (document-chunk-text (cdr (first results))))
      (check-equal? "best"  (document-chunk-text (cdr (second results)))))))

(define (test-search-fanout-dedup)
  ; Two sub-queries with identical embeddings retrieve the same chunks;
  ; fanout must deduplicate by (source . text).
  (let ([corpus (make-test-corpus)])
    (parameterize ([*embedding-fn* (lambda (text) '(1.0 0.0 0.0))]
                   [*embedding-cache* (make-hash)]
                   [*rag-verbose* #f])
      (let ([results (search-fanout (list corpus) '("q1" "q2") #:top-k 4)])
        (check-equal? 4 (length results))
        (check-equal? 4 (length (remove-duplicates
                                 results
                                 (lambda (a b)
                                   (equal? (document-chunk-text (cdr a))
                                           (document-chunk-text (cdr b))))))))))
  ; Same text in different source files is NOT deduplicated
  (let* ([chunk-a (make-test-chunk "same text" '(1.0 0.0 0.0) "a.txt")]
         [chunk-b (make-test-chunk "same text" '(1.0 0.0 0.0) "b.txt")]
         [corpus (make-corpus #:name "two" #:description "sources")])
    (set-corpus-chunks! corpus (list chunk-a chunk-b))
    (parameterize ([*embedding-fn* (lambda (text) '(1.0 0.0 0.0))]
                   [*embedding-cache* (make-hash)]
                   [*rag-verbose* #f])
      (let ([results (search-fanout (list corpus) '("q") #:top-k 5)])
        (check-equal? 2 (length results))))))

(define (test-search-fanout-batches-queries)
  ; search-fanout must embed all sub-queries in one batch call
  ; (get-embeddings), not one call per query. *batch-request-fn* is
  ; stubbed (not *embedding-fn*) so get-embeddings takes its batch path.
  (define corpus (make-test-corpus))
  (define batch-calls 0)
  (parameterize ([*rag-verbose* #f]
                 [*embedding-fn* %fetch-embedding]
                 [*batch-request-fn*
                  (lambda (texts)
                    (set! batch-calls (+ batch-calls 1))
                    (map (lambda (t) '(1.0 0.0 0.0)) texts))]
                 [*embedding-cache* (make-hash)])
    (let ([results (search-fanout (list corpus)
                                  '("q1" "q2" "q3" "q4")
                                  #:top-k 4)])
      (check-equal? 4 (length results))
      (check-equal? 1 batch-calls))))

;;; ---- Embedding batching ----

(define (test-batch-splitting)
  ; More than *embedding-batch-limit* texts must split into multiple
  ; batch requests, preserving order. All texts are cache misses, so
  ; only *batch-request-fn* is ever called.
  (define sizes '())
  (define all-texts (for/list ([i (in-range 1 206)]) (format "text ~a" i)))
  (parameterize ([*embedding-cache* (make-hash)]
                 [*rag-verbose* #f]
                 [*embedding-fn* %fetch-embedding]
                 [*batch-request-fn*
                  (lambda (texts)
                    (set! sizes (cons (length texts) sizes))
                    (map (lambda (t) '(1.0)) texts))])
    (check-equal? 205 (length (get-embeddings all-texts)))
    ; batches of 100, 100, 5 in call order
    (check-equal? '(100 100 5) (reverse sizes))
    ; every text now cached; a second call makes no HTTP at all
    (define second-sizes '())
    (parameterize ([*batch-request-fn*
                    (lambda (texts)
                      (set! second-sizes (cons (length texts) second-sizes))
                      (map (lambda (t) '(1.0)) texts))])
      (get-embeddings all-texts)
      (check-equal? '() second-sizes))))

(define (test-cache-eviction)
  ; Cache clears itself at the cap
  (parameterize ([*embedding-cache* (make-hash)]
                 [*embedding-cache-cap* 3]
                 [*embedding-fn* (lambda (text) '(1.0))]
                 [*rag-verbose* #f])
    (for-each get-embedding '("a" "b" "c" "d"))
    (check-equal? 1 (hash-count (*embedding-cache*)))
    (check-true (hash-has-key? (*embedding-cache*) (embedding-cache-key "d")))
    ; "a" was evicted with the rest of the old cache
    (check-false (hash-has-key? (*embedding-cache*) (embedding-cache-key "a")))))

;;; ---- Retry logic ----

(define (test-call-with-retries)
  ; Backoff sleep is stubbed so the suite stays fast.
  (parameterize ([*rag-verbose* #f]
                 [*retry-sleep-fn* (lambda (s) (void))])
    ; transient 503 twice, then success
    (define attempts 0)
    (check-equal?
     'success
     (call-with-retries
      (lambda ()
        (set! attempts (+ attempts 1))
        (if (< attempts 3)
            (raise (exn:fail:http "down" (current-continuation-marks) 503))
            'success))))
    (check-equal? 3 attempts)
    ; permanent 400 signals immediately, no retries
    (set! attempts 0)
    (check-exn exn:fail?
               (lambda ()
                 (call-with-retries
                  (lambda ()
                    (set! attempts (+ attempts 1))
                    (raise (exn:fail:http "bad" (current-continuation-marks) 400))))))
    (check-equal? 1 attempts)
    ; 429 is transient
    (set! attempts 0)
    (check-equal?
     'ok
     (call-with-retries
      (lambda ()
        (set! attempts (+ attempts 1))
        (if (= attempts 1)
            (raise (exn:fail:http "slow" (current-continuation-marks) 429))
            'ok))))
    (check-equal? 2 attempts)
    ; usocket connection errors are transient
    (set! attempts 0)
    (check-equal?
     'ok
     (call-with-retries
      (lambda ()
        (set! attempts (+ attempts 1))
        (if (= attempts 1)
            (raise (exn:fail:network "refused" (current-continuation-marks)))
            'ok))))
    (check-equal? 2 attempts)
    ; exhausted retries signal with the underlying condition
    (set! attempts 0)
    (check-exn exn:fail?
               (lambda ()
                 (call-with-retries
                  (lambda ()
                    (set! attempts (+ attempts 1))
                    (raise (exn:fail:http "down" (current-continuation-marks) 503)))
                  #:attempts 2)))
    (check-equal? 2 attempts)
    ; attempts=1 means no retry at all
    (set! attempts 0)
    (check-exn exn:fail?
               (lambda ()
                 (call-with-retries
                  (lambda ()
                    (set! attempts (+ attempts 1))
                    (raise (exn:fail:network "refused" (current-continuation-marks))))
                  #:attempts 1)))
    (check-equal? 1 attempts)))

;;; ---- Sufficiency verdict parsing ----

(define (test-parse-verdict)
  (define-values (ok1 fb1)
    (parse-verdict-response
     "VERDICT: SUFFICIENT\nREASON: all facts present\nMISSING: NONE"))
  (check-true ok1)
  (check-equal? "NONE" fb1)
  (define-values (ok2 fb2)
    (parse-verdict-response
     "VERDICT: INSUFFICIENT\nREASON: no pricing data\nMISSING: current lithium-ion battery prices"))
  (check-false ok2)
  (check-equal? "current lithium-ion battery prices" fb2)
  ; Case-insensitive
  (define-values (ok3 fb3)
    (parse-verdict-response "verdict: sufficient"))
  (check-true ok3)
  ; Unparseable verdict -> documented fallback is SUFFICIENT (bounds cost)
  (define-values (ok4 fb4)
    (parameterize ([*rag-verbose* #f])
      (parse-verdict-response "I am not sure.")))
  (check-true ok4)
  ; NIL/empty response must not crash
  (define-values (ok5 fb5)
    (parameterize ([*rag-verbose* #f])
      (parse-verdict-response #f)))
  (check-true ok5))

;;; ---- Corpus persistence ----

(define (test-corpus-persistence)
  (define path (make-temporary-file "rag-test-corpus-~a.sexp"))
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     (let* ([corpus (make-test-corpus)]
            [loaded (begin (save-corpus corpus path)
                           (load-corpus path))])
       (check-equal? "test" (corpus-name loaded))
       (check-equal? (corpus-chunk-count corpus)
                     (corpus-chunk-count loaded))
       (check-equal? (map document-chunk-text (corpus-chunks corpus))
                     (map document-chunk-text (corpus-chunks loaded)))
       ; embeddings round-trip exactly: chunks are stored normalized
       ; and load-corpus re-normalization is a no-op
       (check-equal? (map document-chunk-embedding (corpus-chunks corpus))
                     (map document-chunk-embedding (corpus-chunks loaded)))
       ; round trip keeps search working
       (let ([results (search-corpus loaded '(1.0 0.0 0.0) #:top-k 1)])
         (check-equal? "alpha" (document-chunk-text (cdr (first results)))))))
   (lambda () (delete-file path))))

(define (test-load-corpus-validation)
  ; corrupt files must signal, not produce #f embeddings
  (define path (make-temporary-file "rag-bad-corpus-~a.sexp"))
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     ; missing embedding
     (call-with-output-file path #:exists 'replace
       (lambda (out)
         (write (list (cons 'name "bad")
                      (cons 'chunks (list (list (cons 'text "t") (cons 'source "s")))))
                out)))
     (check-exn exn:fail? (lambda () (load-corpus path)))
     ; truncated structure
     (call-with-output-file path #:exists 'replace
       (lambda (out)
         (write (list (cons 'name "bad")) out)))
     (check-exn exn:fail? (lambda () (load-corpus path)))
     ; garbage
     (call-with-output-file path #:exists 'replace
       (lambda (out)
         (write 42 out)))
     (check-exn exn:fail? (lambda () (load-corpus path))))
   (lambda () (delete-file path))))

;;; ---- Full pipeline with stubbed LLM and embeddings ----

(define (test-agentic-rag-pipeline)
  (define corpus (make-test-corpus))
  (define assess-calls 0)
  (define generate-calls 0)
  (parameterize ([*embedding-fn* (lambda (text) '(1.0 0.0 0.0))]
                 [*embedding-cache* (make-hash)]
                 [*rag-verbose* #f]
                 [*retry-sleep-fn* (lambda (s) (void))])
    ; Sufficient on the first assessment
    (set! assess-calls 0)
    (parameterize ([*generate-fn*
                    (lambda (prompt #:model model)
                      (set! generate-calls (+ generate-calls 1))
                      (cond
                        [(string-contains? prompt "Sufficient Context Agent")
                         (set! assess-calls (+ assess-calls 1))
                         "VERDICT: SUFFICIENT\nREASON: context covers it\nMISSING: NONE"]
                        [(string-contains? prompt "Synthesis Agent")
                         "THE ANSWER"]
                        [else "sub-query one"]))])
      (check-equal? "THE ANSWER"
                    (agentic-rag (list corpus) "test question"))
      (check-equal? 1 assess-calls))
    ; Insufficient once, then sufficient: refine loop must run
    (set! assess-calls 0)
    (parameterize ([*generate-fn*
                    (lambda (prompt #:model model)
                      (cond
                        [(string-contains? prompt "Sufficient Context Agent")
                         (set! assess-calls (+ assess-calls 1))
                         (if (= assess-calls 1)
                             "VERDICT: INSUFFICIENT\nREASON: missing facts\nMISSING: more facts"
                             "VERDICT: SUFFICIENT\nREASON: now complete\nMISSING: NONE")]
                        [(string-contains? prompt "Synthesis Agent")
                         "REFINED ANSWER"]
                        [else "refined query"]))])
      (check-equal? "REFINED ANSWER"
                    (agentic-rag (list corpus) "test question"))
      (check-equal? 2 assess-calls))
    ; cap-context caps the passages sent to the LLM
    (check-equal? 2 (length (cap-context
                             (list (cons 0.9 'a) (cons 0.8 'b) (cons 0.7 'c)) 2)))
    ; Verbose off means no pipeline banner or iteration headers
    (set! assess-calls 0)
    (define output
      (with-output-to-string
       (lambda ()
         (parameterize ([*generate-fn*
                         (lambda (prompt #:model model)
                           (cond
                             [(string-contains? prompt "Sufficient Context Agent")
                              (set! assess-calls (+ assess-calls 1))
                              "VERDICT: SUFFICIENT\nREASON: ok\nMISSING: NONE"]
                             [(string-contains? prompt "Synthesis Agent")
                              "A"]
                             [else "q"]))])
           (agentic-rag (list corpus) "quiet question")))))
    (check-equal? 0 (string-length output))
    ; Insufficient all the way: last iteration must skip the sufficiency
    ; call (it cannot change the outcome), so with max-iterations 3 only
    ; 2 assess calls happen.
    (set! assess-calls 0)
    (parameterize ([*generate-fn*
                    (lambda (prompt #:model model)
                      (cond
                        [(string-contains? prompt "Sufficient Context Agent")
                         (set! assess-calls (+ assess-calls 1))
                         "VERDICT: INSUFFICIENT\nREASON: never enough\nMISSING: more"]
                        [(string-contains? prompt "Synthesis Agent")
                         "BEST EFFORT"]
                        [else "refined"]))])
      (check-equal? "BEST EFFORT"
                    (agentic-rag (list corpus) "test question"
                                 #:max-iterations 3))
      (check-equal? 2 assess-calls))
    ; rewrite-queries always includes the original query
    (parameterize ([*generate-fn*
                    (lambda (prompt #:model model)
                      "generated query")])
      (check-equal? '("generated query" "orig")
                    (rewrite-queries "orig")))))

(define (test-rag-generate-retries)
  ; rag-generate retries transient failures through *generate-fn*
  (define attempts 0)
  (parameterize ([*rag-verbose* #f]
                 [*retry-sleep-fn* (lambda (s) (void))])
    (parameterize ([*generate-fn*
                    (lambda (prompt #:model model)
                      (set! attempts (+ attempts 1))
                      (if (= attempts 1)
                          (raise (exn:fail:http "oops" (current-continuation-marks) 500))
                          "recovered text"))])
      (check-equal? "recovered text" (rag-generate "p")))))

;;; ---- Runner ----

(define all-tests
  (test-suite
   "RAG tests"
   (test-case "split-into-chunks" (test-split-into-chunks))
   (test-case "parse-query-lines" (test-parse-query-lines))
   (test-case "vector-math" (test-vector-math))
   (test-case "search-corpus" (test-search-corpus))
   (test-case "search-corpora" (test-search-corpora))
   (test-case "search-fanout-dedup" (test-search-fanout-dedup))
   (test-case "search-fanout-batches-queries" (test-search-fanout-batches-queries))
   (test-case "batch-splitting" (test-batch-splitting))
   (test-case "cache-eviction" (test-cache-eviction))
   (test-case "call-with-retries" (test-call-with-retries))
   (test-case "parse-verdict" (test-parse-verdict))
   (test-case "corpus-persistence" (test-corpus-persistence))
   (test-case "load-corpus-validation" (test-load-corpus-validation))
   (test-case "agentic-rag-pipeline" (test-agentic-rag-pipeline))
   (test-case "rag-generate-retries" (test-rag-generate-retries))))

(module+ main
  (run-tests all-tests))
