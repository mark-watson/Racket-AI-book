#lang racket

;;; make-index.rkt — Regenerate manuscript/TOC.md and SOURCE_CODE_MAP.md
;;;
;;; Both files are derived from manuscript/Book.txt (the chapter order that
;;; Leanpub uses) and from the first heading of each chapter file, so they
;;; cannot drift from the book. Run from anywhere:
;;;
;;;     racket tools/make-index.rkt
;;;
;;; When you add a chapter, add it to Book.txt and to chapter-sources below.

(require racket/runtime-path)

(define-runtime-path repo-root "..")
(define manuscript (build-path repo-root "manuscript"))

;; chapter file -> source-code directories (in the order to list them)
(define chapter-sources
  (hash "tutorial.md"                            '("misc_code")
        "datastores.md"                          '("sparql" "misc_code")
        "RDF_Datastore_SPARQL_Implementation.md" '("simple_RDF_SPARQL")
        "webscrape.md"                           '("webscrape" "misc_code")
        "lightpanda.md"                          '("lightpanda")
        "karpathy.md"                            '("Karpathy_MicroGPT")
        "llm.md"                                 '("llmapis" "racket_llm_language")
        "Ollama_Tools.md"                        '("ollama_tools")
        "coding-agent-harness.md"                '("coding-agent-harness")
        "embeddings.md"                          '("embeddingsdb")
        "RAG.md"                                 '("RAG")
        "Malt_deep_learning.md"                  '("Malt_deep_learning")
        "deep_learning_category_theory.md"       '("deep_learning_category_theory")
        "ReinforcementLearning.md"               '("ReinforcementLearning")
        "nlp.md"                                 '("nlp")
        "kgn.md"                                 '("kgn" "sparql" "nlp")
        "OPS5_in_Racket.md"                      '("OPS5_in_Racket")
        "Racklog_expert_systems_logic.md"        '("Racklog_expert_systems_logic")
        "math-pi-e.md"                           '("math-pi-e")))

(define book-files
  (filter (lambda (l) (not (string=? l "")))
          (map string-trim (file->lines (build-path manuscript "Book.txt")))))

;; The title is the first "# " heading, without a trailing {#anchor}
(define (chapter-title file)
  (define heading
    (for/first ([line (file->lines (build-path manuscript file))]
                #:when (string-prefix? line "# "))
      line))
  (unless heading
    (error 'make-index "no top-level heading in ~a" file))
  (string-trim (regexp-replace #rx" *{#[^}]*}$" (substring heading 2) "")))

(define (part? file) (regexp-match? #rx"^PART[0-9]+\\.md$" file))
(define (unnumbered? file) (member file '("preface.md")))

(define (write-toc)
  (with-output-to-file (build-path manuscript "TOC.md") #:exists 'truncate
    (lambda ()
      (printf "# Table of contents\n\n")
      (printf "Generated from `Book.txt` by `tools/make-index.rkt`; do not edit by hand.\n\n")
      (for/fold ([n 1]) ([file book-files])
        (cond
          [(part? file)
           (printf "\n## ~a\n\n" (chapter-title file))
           n]
          [(unnumbered? file)
           (printf "- [~a](~a)\n" (chapter-title file) file)
           n]
          [else
           (printf "- [Chapter ~a: ~a](~a)\n" n (chapter-title file) file)
           (add1 n)]))
      (void))))

(define (write-source-map)
  (with-output-to-file (build-path repo-root "SOURCE_CODE_MAP.md")
    #:exists 'truncate
    (lambda ()
      (printf "# Source code map\n\n")
      (printf "Generated from `manuscript/Book.txt` by `tools/make-index.rkt`; do not edit by hand.\n\n")
      (printf "This table maps each chapter file in `./manuscript` to its example directories in `./source-code`, in book order. Chapters with no code show `(none)`.\n\n")
      (printf "| Chapter file | Source code directory |\n| --- | --- |\n")
      (for ([file book-files] #:unless (part? file))
        (define dirs (hash-ref chapter-sources file '()))
        (for ([d dirs])
          (unless (directory-exists? (build-path repo-root "source-code" d))
            (error 'make-index "~a lists missing directory source-code/~a" file d)))
        (printf "| `manuscript/~a` | ~a |\n"
                file
                (if (null? dirs)
                    "(none)"
                    (string-join
                     (for/list ([d dirs]) (format "`source-code/~a`" d))
                     "<br>")))))))

(module+ main
  (write-toc)
  (write-source-map)
  (printf "wrote manuscript/TOC.md and SOURCE_CODE_MAP.md (~a entries)\n"
          (length book-files)))
