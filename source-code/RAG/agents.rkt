#lang racket

;;; agents.rkt — Multi-agent pipeline for Agentic RAG
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Inspired by Google's "Unlocking Dependable Responses with Agentic RAG"
;;; https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/
;;;
;;; Architecture:
;;;   1. Query Rewriter   — decomposes complex queries into focused sub-queries
;;;   2. Search Fanout    — searches corpora with embeddings for each sub-query
;;;   3. Sufficient Context Agent — evaluates if retrieved context is adequate
;;;   4. Synthesis Agent  — generates grounded answer from accumulated context

(require "embeddings.rkt")
(require "vector-store.rkt")
(require net/http-easy)
(require json)

(provide *rag-model*
         *generate-fn*
         rag-generate
         parse-query-lines
         rewrite-queries
         search-fanout
         parse-verdict-response
         assess-sufficiency
         synthesize-answer
         refine-queries
         cap-context
         agentic-rag)

(define *rag-model* (make-parameter "gemini-3-flash-preview"))
; Gemini model used for all agent LLM calls. Override per call with
; the #:model keyword argument to agentic-rag.

;;; ---- LLM generation ----

(define (gemini-generate prompt #:model [model (*rag-model*)])
  ; Call the Gemini generateContent endpoint; returns text.
  ; Raises exn:fail:http on 4xx/5xx so call-with-retries can retry.
  (define api-url
    (string-append "https://generativelanguage.googleapis.com/v1beta/models/"
                   model ":generateContent"))
  (define resp
    (post api-url
          #:headers (hash 'Content-Type "application/json"
                          'x-goog-api-key (google-api-key))
          #:json (hash 'contents (list (hash 'parts (list (hash 'text prompt)))))))
  (define code (response-status-code resp))
  (when (>= code 400)
    (raise (exn:fail:http
            (format "HTTP ~a from Gemini API: ~a" code (response-body resp))
            (current-continuation-marks)
            code)))
  (define body (bytes->string/utf-8 (response-body resp)))
  (define decoded (string->jsexpr body))
  (when (hash-has-key? decoded 'error)
    (error "Gemini API error: ~a" body))
  (define candidates (hash-ref decoded 'candidates '()))
  (if (null? candidates)
      "No response"
      (let* ([first-cand (car candidates)]
             [content (hash-ref first-cand 'content (hash))]
             [parts (hash-ref content 'parts '())]
             [first-part (if (null? parts) (hash) (car parts))])
        (hash-ref first-part 'text "No response"))))

(define (google-api-key)
  (or (getenv "GOOGLE_API_KEY")
      (error "GOOGLE_API_KEY environment variable is not set")))

(define *generate-fn* (make-parameter gemini-generate))
; Function of (prompt #:model model) returning generated text. Defaults
; to the Gemini generateContent endpoint. Rebind in tests to run the
; pipeline without network access.

(define (rag-generate prompt #:model [model (*rag-model*)])
  ; Call the LLM through *generate-fn* with retries on transient
  ; failures (HTTP 429/5xx, connection errors), so one flaky request
  ; does not throw away the whole pipeline's work. Signals an error if
  ; the model returns no text.
  (define result
    (call-with-retries (lambda () ((*generate-fn*) prompt #:model model))))
  (or result
      (error "LLM returned no text for prompt: ~a"
             (substring prompt 0 (min 80 (string-length prompt))))))

(define (cap-context scored-chunks max-context-chunks)
  ; Keep at most MAX-CONTEXT-CHUNKS highest-scoring chunks so iterative
  ; retrieval cannot grow the LLM prompt without bound. SCORED-CHUNKS
  ; must already be sorted by descending score.
  (if (and max-context-chunks (> (length scored-chunks) max-context-chunks))
      (take scored-chunks max-context-chunks)
      scored-chunks))

;;; ---- Agent 1: Query Rewriter ----

(define (ws-char? c)
  (member c '(#\space #\tab #\return #\newline)))

(define (%strip-list-prefix line)
  ; Remove an optional markdown/numbered list prefix from LINE and the
  ; surrounding whitespace. Only leading list syntax is stripped:
  ; interior and trailing digits are part of the query (so
  ; "2024 lithium prices", "1.5 MW output", and "75-100 kg" survive
  ; intact, while "4. fourth query" and "- bullet" are cleaned).
  (let* ([len (string-length line)]
         [i 0])
    ; skip leading whitespace
    (let loop ()
      (when (and (< i len) (ws-char? (string-ref line i)))
        (set! i (+ i 1))
        (loop)))
    ; skip an optional bullet character
    (when (and (< i len) (member (string-ref line i) '(#\- #\* #\+)))
      (set! i (+ i 1))
      (let loop ()
        (when (and (< i len) (ws-char? (string-ref line i)))
          (set! i (+ i 1))
          (loop))))
    ; skip an optional numbering: digits followed by . or ) followed
    ; by whitespace. "1.5 MW" fails the whitespace test, so it stays.
    (let ([j i])
      (let loop ()
        (when (and (< j len) (char-numeric? (string-ref line j)))
          (set! j (+ j 1))
          (loop)))
      (when (and (> j i) (< j len)
                 (member (string-ref line j) '(#\. #\)))
                 (< (+ j 1) len)
                 (ws-char? (string-ref line (+ j 1))))
        (set! i (+ j 1))
        (let loop ()
          (when (and (< i len) (ws-char? (string-ref line i)))
            (set! i (+ i 1))
            (loop)))))
    (string-trim (substring line i))))

(define (parse-query-lines response)
  ; Extract one query per line from a rewriter agent RESPONSE, dropping
  ; empty lines and list prefixes. Query text itself is untouched.
  (filter (lambda (s) (> (string-length s) 0))
          (map %strip-list-prefix
               (regexp-split #rx"\n" (or response "")))))

(define (rewrite-queries user-query #:model [model (*rag-model*)])
  ; Decompose USER-QUERY into 1-3 focused sub-queries for retrieval.
  ; Returns a list of query strings. The original query is always
  ; appended as a fallback so the fanout always searches for what the
  ; user actually asked.
  (debug-log "~%DEBUG rewrite-queries: decomposing query...~%")
  (define prompt
    (format (string-append
             "You are a search query rewriter for a RAG system. "
             "Your job is to break a complex user question into "
             "1-3 simple, focused search queries that will help "
             "retrieve relevant information from a document collection.\n"
             "\nRules:\n"
             "- Output ONLY the queries, one per line\n"
             "- No numbering, bullets, or extra text\n"
             "- Each query should target a specific fact or concept\n"
             "- Keep queries concise (under 15 words each)\n"
             "\nUser question: ~a")
            user-query))
  (define queries (parse-query-lines (rag-generate prompt #:model model)))
  (debug-log "DEBUG rewrite-queries: generated ~a sub-queries:~%~a"
             (length queries)
             (apply string-append
                    (map (lambda (q) (format "  - ~a~%" q)) queries)))
  (remove-duplicates (append queries (list user-query))))

;;; ---- Agent 2: Search Fanout ----

(define (search-fanout corpora sub-queries #:top-k [top-k 3])
  ; Execute embedding search across CORPORA for each sub-query.
  ; All sub-query embeddings are fetched with one batched API call.
  ; Returns a deduplicated list of (score . document-chunk) pairs,
  ; sorted by descending score. Chunks are deduplicated by
  ; (source . text) so identical text in different files stays distinct.
  (debug-log "~%DEBUG search-fanout: searching ~a corpora with ~a queries~%"
             (length corpora) (length sub-queries))
  ; One batched embedding call for all sub-queries instead of N round trips
  (define query-embeddings (get-embeddings sub-queries))
  (define seen-keys (make-hash))
  (define all-results '())
  (for ([query sub-queries]
        [query-embedding query-embeddings])
    (debug-log "DEBUG search-fanout: searching with: ~s~%" query)
    (for ([result (search-corpora corpora query-embedding #:top-k top-k)])
      (define key (document-chunk-key (cdr result)))
      (unless (hash-ref seen-keys key #f)
        (hash-set! seen-keys key #t)
        (set! all-results (cons result all-results)))))
  ; Sort by score descending
  (define sorted (sort all-results > #:key car))
  (debug-log "DEBUG search-fanout: found ~a unique chunks~%" (length sorted))
  sorted)

;;; ---- Agent 3: Sufficient Context Agent ----

(define (string-contains-ci? haystack needle)
  ; Case-insensitive substring search; returns the starting index or #f.
  (define h (string-downcase haystack))
  (define n (string-downcase needle))
  (define n-len (string-length n))
  (define h-len (string-length h))
  (let loop ([i 0])
    (cond
      [(> (+ i n-len) h-len) #f]
      [(string=? (substring h i (+ i n-len)) n) i]
      [else (loop (+ i 1))])))

(define (parse-verdict-response response)
  ; Parse a Sufficient Context Agent RESPONSE of the form:
  ;     VERDICT: SUFFICIENT | INSUFFICIENT
  ;     REASON: ...
  ;     MISSING: ...
  ; Returns two values: sufficient? and feedback (the MISSING text).
  ; An unparseable verdict is treated as SUFFICIENT; this bounds API
  ; cost because the iteration limit is the only other safeguard.
  (define lines (regexp-split #rx"\n" (or response "")))
  (define verdict-line
    (findf (lambda (line) (string-contains-ci? line "VERDICT:")) lines))
  (define missing-line
    (findf (lambda (line) (string-contains-ci? line "MISSING:")) lines))
  (define verdict-word
    (and verdict-line
         (string-trim
          (string-replace (substring verdict-line
                                     (+ (string-contains-ci? verdict-line "VERDICT:") 8))
                          "." ""))))
  (define feedback
    (if missing-line
        (string-trim (substring missing-line
                                (+ (string-contains-ci? missing-line "MISSING:") 8)))
        "No specific feedback available"))
  (cond
    [(and verdict-word (string-contains-ci? verdict-word "INSUFFICIENT"))
     (values #f feedback)]
    [(and verdict-word (string-contains-ci? verdict-word "SUFFICIENT"))
     (values #t feedback)]
    [else
     (debug-log "WARNING parse-verdict-response: unparseable verdict ~s; treating as SUFFICIENT~%"
                verdict-word)
     (values #t feedback)]))

(define (assess-sufficiency user-query retrieved-chunks #:model [model (*rag-model*)])
  ; Evaluate whether RETRIEVED-CHUNKS provide sufficient context
  ; to answer USER-QUERY. Returns two values:
  ;   1. sufficient? — #t if context is sufficient, #f otherwise
  ;   2. feedback — string describing what information is missing.
  (debug-log "~%DEBUG assess-sufficiency: evaluating ~a chunks~%"
             (length retrieved-chunks))
  (define context (format-retrieved-chunks retrieved-chunks))
  (define prompt
    (format (string-append
             "You are a Sufficient Context Agent in an agentic RAG system. "
             "Your role is to evaluate whether the retrieved passages "
             "contain enough information to fully answer the user's question.\n"
             "\nUser Question: ~a\n"
             "\nRetrieved Passages:~a\n"
             "\nEvaluate carefully:\n"
             "1. Does the context contain ALL the specific facts needed?\n"
             "2. Are there any parts of the question left unanswered?\n"
             "3. Is any critical information missing?\n"
             "\nRespond in EXACTLY this format:\n"
             "VERDICT: SUFFICIENT or INSUFFICIENT\n"
             "REASON: (one sentence explaining your assessment)\n"
             "MISSING: (if insufficient, describe what specific "
             "information to search for next; if sufficient, write NONE)")
            user-query context))
  (define response (rag-generate prompt #:model model))
  (debug-log "DEBUG assess-sufficiency response:~%~a~%" response)
  (define-values (sufficient? feedback)
    (parse-verdict-response response))
  (debug-log "DEBUG assess-sufficiency: verdict=~a~%"
             (if sufficient? "SUFFICIENT" "INSUFFICIENT"))
  (values sufficient? feedback))

;;; ---- Agent 4: Synthesis Agent ----

(define (synthesize-answer user-query retrieved-chunks #:model [model (*rag-model*)])
  ; Generate a grounded answer to USER-QUERY using RETRIEVED-CHUNKS.
  ; The answer cites source documents.
  (debug-log "~%DEBUG synthesize-answer: generating answer from ~a chunks~%"
             (length retrieved-chunks))
  (define context (format-retrieved-chunks retrieved-chunks))
  (define prompt
    (format (string-append
             "You are a Synthesis Agent in a RAG system. Generate a "
             "clear, accurate answer to the user's question using ONLY "
             "the information in the retrieved passages below. "
             "\n\nRules:\n"
             "- Base your answer strictly on the retrieved passages\n"
             "- Cite sources by mentioning the source filename\n"
             "- If the passages don't fully answer the question, "
             "say what you can answer and note what's missing\n"
             "- Be concise but thorough\n"
             "\nUser Question: ~a\n"
             "\nRetrieved Passages:~a")
            user-query context))
  (define response (rag-generate prompt #:model model))
  (debug-log "DEBUG synthesize-answer: generated response (~a chars)~%"
             (string-length response))
  response)

;;; ---- Orchestrator: Agentic RAG Pipeline ----

(define (refine-queries user-query feedback #:model [model (*rag-model*)])
  ; Generate refined search queries based on sufficiency FEEDBACK.
  ; Used when the initial retrieval was insufficient.
  (debug-log "~%DEBUG refine-queries: generating refined queries from feedback~%")
  (define prompt
    (format (string-append
             "You are a search query rewriter. The previous search "
             "did not find enough information. Based on the feedback "
             "below, generate 1-2 NEW, DIFFERENT search queries to "
             "find the missing information.\n"
             "\nOriginal question: ~a\n"
             "Missing information: ~a\n"
             "\nOutput ONLY the new queries, one per line. "
             "No numbering or extra text.")
            user-query feedback))
  (define queries (parse-query-lines (rag-generate prompt #:model model)))
  (debug-log "DEBUG refine-queries: generated ~a refined queries:~%~a"
             (length queries)
             (apply string-append
                    (map (lambda (q) (format "  - ~a~%" q)) queries)))
  (if (null? queries) (list feedback) queries))

(define (agentic-rag corpora user-query
                     #:max-iterations [max-iterations 3]
                     #:top-k [top-k 3]
                     #:model [model (*rag-model*)]
                     #:max-context-chunks [max-context-chunks 8])
  ; Run the full agentic RAG pipeline:
  ;     1. Rewrite the user query into sub-queries
  ;     2. Search corpora for relevant chunks
  ;     3. Check if context is sufficient (loop if not)
  ;     4. Synthesize a grounded answer
  ;
  ; CORPORA is a list of corpus structs.
  ; MODEL is the Gemini model id used for every agent call.
  ; MAX-CONTEXT-CHUNKS caps how many retrieved passages are sent to the
  ; LLM (highest-scoring first) no matter how many iterations ran.
  ; Returns the synthesized answer string.
  (debug-log "~%~%========================================~%")
  (debug-log "  AGENTIC RAG PIPELINE~%")
  (debug-log "  Query: ~a~%" user-query)
  (debug-log "========================================~%")

  ; Phase 1: Rewrite queries
  (define sub-queries (rewrite-queries user-query #:model model))
  ; Phase 2: Initial search
  (define initial-chunks (search-fanout corpora sub-queries #:top-k top-k))

  ; Phase 3: Iterative sufficiency check
  ; We use an early-return pattern (call/ec) because the pipeline can
  ; exit from three different points inside the loop body.
  (call/ec
   (lambda (return)
     (let loop ([iteration 1] [all-chunks initial-chunks])
       (debug-log "~%--- Iteration ~a/~a ---~%" iteration max-iterations)

       (when (null? all-chunks)
         (debug-log "DEBUG agentic-rag: no chunks found, returning empty answer~%")
         (return "I could not find any relevant information in the available documents."))

       ; Cap prompt size regardless of how many iterations accumulated chunks
       (define context-chunks (cap-context all-chunks max-context-chunks))

       ; At the last allowed iteration both branches end in "synthesize
       ; with what we have", so skip the sufficiency LLM call entirely.
       (when (>= iteration max-iterations)
         (debug-log "~%DEBUG agentic-rag: max iterations reached, synthesizing with available context~%")
         (return (synthesize-answer user-query context-chunks #:model model)))

       (define-values (sufficient? feedback)
         (assess-sufficiency user-query context-chunks #:model model))

       (when sufficient?
         (debug-log "~%DEBUG agentic-rag: context is SUFFICIENT at iteration ~a~%" iteration)
         ; Phase 5: Synthesize answer
         (return (synthesize-answer user-query context-chunks #:model model)))

       ; Phase 4: Refine and search again
       (debug-log "~%DEBUG agentic-rag: context INSUFFICIENT, refining...~%")
       (debug-log "DEBUG agentic-rag: feedback: ~a~%" feedback)
       (define refined-queries (refine-queries user-query feedback #:model model))
       (define new-chunks (search-fanout corpora refined-queries #:top-k top-k))
       ; Accumulate new chunks with existing ones (deduplicate by
       ; (source . text) so identical text from different files stays distinct)
       (define seen (make-hash))
       (for ([scored-chunk all-chunks])
         (hash-set! seen (document-chunk-key (cdr scored-chunk)) #t))
       (define accumulated all-chunks)
       (for ([scored-chunk new-chunks])
         (define key (document-chunk-key (cdr scored-chunk)))
         (unless (hash-ref seen key #f)
           (hash-set! seen key #t)
           (set! accumulated (cons scored-chunk accumulated))))
       ; Re-sort by score
       (loop (+ iteration 1)
             (sort accumulated > #:key car))))))
