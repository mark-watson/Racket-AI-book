#lang racket

;;; main.rkt — Public API for the Agentic RAG system
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

;;; Re-exports the full public API and provides the top-level functions
;;; (query, interactive-demo, test). Use as:
;;;   (require "main.rkt")
;;; or install as a package and use:
;;;   (require rag)

(require "embeddings.rkt")
(require "vector-store.rkt")
(require "agents.rkt")
(require racket/runtime-path)

(provide make-corpus
         add-document
         save-corpus
         load-corpus
         corpus-chunk-count
         query
         agentic-rag
         interactive-demo
         test
         *rag-verbose*
         *rag-model*
         *embedding-model*
         *embedding-dimension*
         *embedding-batch-limit*
         *embedding-cache-cap*
         *embedding-fn*
         *batch-request-fn*
         *generate-fn*
         *retry-sleep-fn*
         *embedding-cache*
         embedding-cache-key
         clear-embedding-cache
         get-embedding
         get-embeddings
         %fetch-embedding
         exn:fail:http
         cosine-similarity
         dot-product
         vector-magnitude
         normalize-vector
         document-chunk
         document-chunk-text
         document-chunk-source
         document-chunk-embedding
         document-chunk-norm
         corpus-name
         corpus-description
         corpus-chunks
         corpus?
         set-corpus-chunks!
         split-into-chunks
         search-corpus
         search-corpora
         format-retrieved-chunks
         parse-query-lines
         rewrite-queries
         parse-verdict-response
         assess-sufficiency
         synthesize-answer
         refine-queries
         cap-context)

;;; ---- Public API ----

(define (query corpora question
               #:max-iterations [max-iterations 3]
               #:top-k [top-k 3]
               #:model [model (*rag-model*)]
               #:max-context-chunks [max-context-chunks 8])
  ; Ask QUESTION against the given CORPORA using agentic RAG.
  ; CORPORA is a list of corpus structs (or a single corpus).
  ; Keyword arguments are passed through to agentic-rag.
  ; Returns the answer string.
  (define corpus-list (if (list? corpora) corpora (list corpora)))
  (agentic-rag corpus-list question
               #:max-iterations max-iterations
               #:top-k top-k
               #:model model
               #:max-context-chunks max-context-chunks))

;;; ---- Interactive Demo ----

(define (interactive-demo corpora)
  ; Start an interactive REPL for querying CORPORA.
  ; Type 'quit' or 'exit' to stop.
  (define corpus-list (if (list? corpora) corpora (list corpora)))
  (printf "~%~%============================~%")
  (printf "  Agentic RAG Interactive Demo~%")
  (printf "============================~%")
  (printf "~%Loaded ~a corpora with ~a total chunks.~%"
          (length corpus-list)
          (for/sum ([c corpus-list]) (corpus-chunk-count c)))
  (printf "Type your question (or 'quit' to exit):~%")
  (let loop ()
    (printf "~%RAG> ")
    (flush-output)
    (define input (read-line))
    (cond
      [(or (eof-object? input)
           (member (string-downcase input) '("quit" "exit" "q")))
       (printf "~%Goodbye!~%")]
      [(zero? (string-length (string-trim input)))
       (loop)]
      [else
       (define answer (agentic-rag corpus-list input))
       (printf "~%~%===== ANSWER =====~%~a~%==================~%" answer)
       (loop)])))

;;; ---- Test / Demo ----

(define-runtime-path data-dir "data")

(define (data-path filename)
  ; Resolve a filename relative to the data/ subdirectory.
  (path->string (build-path data-dir filename)))

(define (test)
  ; Run a demo of the Agentic RAG system with sample documents.
  ; Creates three corpora (energy, vehicles, climate) and runs
  ; multi-hop queries that require cross-corpus retrieval.
  (printf "~%~%============================================~%")
  (printf "  Agentic RAG Demo -- Loading Documents~%")
  (printf "============================================~%")

  ; Create three separate corpora to demonstrate cross-corpus retrieval
  (define energy-corpus (make-corpus #:name "renewable-energy"
                                      #:description "Renewable energy sources and technologies"))
  (define ev-corpus (make-corpus #:name "electric-vehicles"
                                  #:description "Electric vehicle technology and infrastructure"))
  (define climate-corpus (make-corpus #:name "climate-science"
                                       #:description "Climate science and carbon emissions"))

  ; Load documents into their respective corpora
  (add-document energy-corpus (data-path "renewable-energy.txt"))
  (add-document ev-corpus (data-path "electric-vehicles.txt"))
  (add-document climate-corpus (data-path "climate-science.txt"))

  (define all-corpora (list energy-corpus ev-corpus climate-corpus))
  (printf "~%~%Loaded ~a total chunks across ~a corpora.~%"
          (for/sum ([c all-corpora]) (corpus-chunk-count c))
          (length all-corpora))

  ; Query 1: Single-corpus question (should find answer easily)
  (printf "~%~%===== TEST QUERY 1 (single topic) =====~%")
  (define answer1
    (query all-corpora
           "What is the current cost of lithium-ion battery storage per kilowatt-hour?"))
  (printf "~%~%ANSWER 1:~%~a~%~%" answer1)

  ; Query 2: Multi-hop question requiring cross-corpus retrieval
  (printf "~%~%===== TEST QUERY 2 (multi-hop, cross-corpus) =====~%")
  (define answer2
    (query all-corpora
           "How does the carbon footprint of manufacturing EV batteries compare to the emissions saved by charging EVs from renewable energy sources?"))
  (printf "~%~%ANSWER 2:~%~a~%~%" answer2)

  ; Query 3: Complex question that may need iterative retrieval
  (printf "~%~%===== TEST QUERY 3 (complex, iterative) =====~%")
  (define answer3
    (query all-corpora
           "What role could solid-state batteries and pumped-storage hydroelectricity play together in solving the intermittency problem of wind and solar energy?"))
  (printf "~%~%ANSWER 3:~%~a~%~%" answer3)

  (printf "~%~%============================================~%")
  (printf "  Demo Complete~%")
  (printf "============================================~%")

  ; Return corpora for interactive use
  all-corpora)

(module+ main
  (displayln "RAG package loaded. Use (require \"main.rkt\") or (require rag) to access the API.")
  (displayln "Run (test) for the interactive demo, or (interactive-demo corpora) to query interactively."))
