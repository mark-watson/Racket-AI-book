#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Tests for rag_extensions.rkt. Everything here runs offline.

(require rackunit)
(require "rag_extensions.rkt")

;;; -----------------------------------------------------------------------------
;;; Cosine similarity

(test-case "cosine similarity basics"
  (check-= (cosine-similarity '(1 0) '(1 0)) 1.0 1e-9)
  (check-= (cosine-similarity '(1 0) '(0 1)) 0.0 1e-9)
  (check-= (cosine-similarity '(1 0) '(-1 0)) -1.0 1e-9)
  ;; scaling either vector does not change the angle
  (check-= (cosine-similarity '(1 2 3) '(2 4 6)) 1.0 1e-9))

(test-case "cosine never throws on a zero vector"
  (check-equal? (cosine-similarity '(0 0 0) '(1 2 3)) 0.0))

(test-case "dot product is length-sensitive, cosine is not"
  ;; The raw dot product of (1 0) with (10 0) is 10x that with (1 0);
  ;; cosine says both point the same direction. This is why unnormalized
  ;; embeddings need cosine.
  (define dot (lambda (a b) (for/sum ([x a] [y b]) (* x y))))
  (check-= (dot '(1 0) '(10 0)) 10.0 1e-9)
  (check-= (cosine-similarity '(1 0) '(10 0)) 1.0 1e-9))

;;; -----------------------------------------------------------------------------
;;; Sentence splitting and chunking

(test-case "split-sentences"
  (check-equal? (split-sentences "One. Two! Three?")
                '("One." "Two!" "Three?"))
  (check-equal? (split-sentences "Single sentence with no terminator")
                '("Single sentence with no terminator")))

(test-case "chunk-by-sentences keeps whole sentences"
  (define text
    "Alpha beta gamma delta. Epsilon zeta eta theta. Iota kappa lambda mu.")
  (define chunks (chunk-by-sentences text #:chunk-size 30 #:overlap 0))
  (check-true (> (length chunks) 1))
  ;; no chunk, ignoring trailing overlap text, ends mid-word
  (for ([c chunks])
    (check-true (regexp-match? #px"[.!?]\\s*$" (string-trim c)))))

(test-case "overlap carries context across the boundary"
  (define text
    "First statement here now. Second statement follows after that one please. Third and final statement ends things.")
  (define chunks (chunk-by-sentences text #:chunk-size 35 #:overlap 15))
  (check-true (>= (length chunks) 2))
  ;; the start of every later chunk repeats the tail of the previous chunk
  (define first-tail
    (substring (car chunks)
               (max 0 (- (string-length (car chunks)) 15))))
  (check-true (string-contains? (second chunks)
                                (string-trim first-tail))))

(test-case "a sentence longer than chunk-size still becomes one chunk"
  (define long "ThisSentenceIsFarTooLongToFitInsideTheRequestedChunkSize. Short.")
  (define chunks (chunk-by-sentences long #:chunk-size 10 #:overlap 0))
  (check-equal? (length chunks) 2))

;;; -----------------------------------------------------------------------------
;;; Hash embedder

(test-case "hash-embed is deterministic and unit length"
  (define e1 (hash-embed "Robert Boyle and his law"))
  (define e2 (hash-embed "Robert Boyle and his law"))
  (check-equal? e1 e2)
  (check-= (magnitude e1) 1.0 1e-9)
  (check-equal? (length e1) 256))

(test-case "hash-embed distinguishes related and unrelated text"
  (define chem (hash-embed "chemistry atoms molecules elements"))
  (define chem2 (hash-embed "the chemistry of molecules and atoms"))
  (define sports (hash-embed "sports players teams and goals"))
  (check-true (> (cosine-similarity chem chem2)
                 (cosine-similarity chem sports))))

;;; -----------------------------------------------------------------------------
;;; Retrieval and prompt assembly

(define test-docs
  '("The periodic table organizes elements by atomic weight."
    "Boyle's Law relates gas pressure and volume."
    "Sports improve cardiovascular health."))

(test-case "rank-chunks orders best first and honors top-k"
  (define ranked (rank-chunks "gas pressure law" test-docs #:top-k 2))
  (check-equal? (length ranked) 2)
  (check-true (> (car (first ranked)) (car (second ranked))))
  (check-true (string-contains? (cdr (first ranked)) "Boyle")))

(test-case "assemble-prompt concatenates context and question"
  (define prompt (assemble-prompt '("ctx text") "extra" "the question"))
  (check-true (string-contains? prompt "ctx text"))
  (check-true (string-contains? prompt "extra"))
  (check-true (string-suffix? prompt "Question: the question")))

(displayln "\nAll tests passed.")
