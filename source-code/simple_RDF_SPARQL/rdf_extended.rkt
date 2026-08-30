#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Extensions to the simple RDF datastore in rdf_sparql.rkt:
;;;
;;;   - N-Triples persistence: save and load a store as plain text
;;;   - FILTER support: filter query results with Racket predicates
;;;   - A larger example dataset: a small family/food knowledge graph
;;;
;;; Run the demo:  racket rdf_extended.rkt
;;; Run tests:     raco test tests.rkt

(require "rdf_sparql.rkt")

(provide triples->ntriples-string
         save-store
         load-store
         execute-sparql-query-filtered
         comparison->predicate
         populate-demo-store
         print-bindings)

;;; -----------------------------------------------------------------------------
;;; N-Triples Persistence
;;;
;;; N-Triples is the simplest RDF serialization: one triple per line,
;;; subject predicate object, ending with a period. Our version keeps
;;; every node as a quoted string, which is enough for this datastore.

(define (triple->ntriple-line t)
  (format "\"~a\" \"~a\" \"~a\" ."
          (triple-subject t)
          (triple-predicate t)
          (triple-object t)))

(define (triples->ntriples-string triples)
  (string-join (map triple->ntriple-line triples) "\n" #:after-last "\n"))

(define (save-store [path "store.nt"])
  "Save the current rdf-store to PATH in N-Triples format."
  (call-with-output-file path
    (lambda (out) (display (triples->ntriples-string rdf-store) out))
    #:exists 'replace))

(define ntriple-line-rx
  (pregexp "^\"([^\"]*)\"\\s+\"([^\"]*)\"\\s+\"([^\"]*)\"\\s*\\.\\s*$"))

(define (parse-ntriple-line line)
  "Parse one N-Triples line into a triple, or #f for blank/comment lines."
  (let ([m (regexp-match ntriple-line-rx line)])
    (and m (triple (second m) (third m) (fourth m)))))

(define (load-store path)
  "Load triples from PATH (N-Triples format) into rdf-store.
   Returns the number of triples loaded."
  (set-rdf-store! '())
  (for ([line (file->lines path)])
    (let ([t (parse-ntriple-line line)])
      (when t
        (set-rdf-store! (cons t rdf-store)))))
  (length rdf-store))

;;; -----------------------------------------------------------------------------
;;; FILTER Support
;;;
;;; Real SPARQL has FILTER(expr) inside WHERE. We add a simple post-query
;;; form: execute the query as usual, then keep only result rows where the
;;; predicate applied to the variable bindings returns true.

(define (comparison->predicate op column threshold)
  "Build a predicate on bindings comparing the value bound to COLUMN
   (parsed as a number) with THRESHOLD using OP (<, >, <=, >=, =)."
  (define cmp
    (match op
      ["<" <] [">" >] ["<=" <=] [">=" >=] ["=" =]
      [_ (error (format "unknown comparison operator: ~a" op))]))
  (lambda (bindings)
    (let ([raw (dict-ref bindings column #f)])
      (and raw
           (let ([n (string->number raw)])
             (and n (cmp n threshold)))))))

(define (execute-sparql-query-filtered query-string keep?)
  "Execute QUERY-STRING and keep only result rows for which KEEP?,
   a predicate on a binding list, returns true."
  (filter keep? (execute-sparql-query query-string)))

;;; -----------------------------------------------------------------------------
;;; Demo Dataset
;;;
;;; A slightly larger knowledge graph: people, their ages, the foods they
;;; like, and who knows whom. Enough rows to make joins interesting.

(define demo-triples
  '(("John" "age" "30")
    ("John" "likes" "pizza")
    ("John" "knows" "Mary")
    ("John" "knows" "Bob")
    ("Mary" "age" "25")
    ("Mary" "likes" "sushi")
    ("Mary" "knows" "Alice")
    ("Bob" "age" "35")
    ("Bob" "likes" "burger")
    ("Bob" "knows" "Mary")
    ("Alice" "age" "41")
    ("Alice" "likes" "sushi")
    ("Alice" "knows" "John")
    ("Carol" "age" "17")
    ("Carol" "likes" "pizza")
    ("Carol" "knows" "John")))

(define (populate-demo-store)
  (set-rdf-store! '())
  (for ([row demo-triples])
    (apply add-triple row)))

;;; -----------------------------------------------------------------------------
;;; Display Helper

(define (print-bindings title results)
  (printf "~a\n" title)
  (if (null? results)
      (printf "  No results\n\n")
      (begin
        (for ([result results])
          (printf "  ~a\n"
                  (string-join
                   (map (lambda (pair)
                          (format "~a: ~a" (car pair) (cdr pair)))
                        result)
                   ", ")))
        (printf "\n"))))

;;; -----------------------------------------------------------------------------
;;; Demo

(module+ main
  (populate-demo-store)
  (print-all-triples)

  ;; A two-hop join: friends of friends.
  (print-bindings
   "Query: people and the foods liked by someone they know\n       (two-hop join over knows and likes)"
   (execute-sparql-query
    "select ?person ?friend ?food where { ?person knows ?friend . ?friend likes ?food }"))

  ;; Same join plus an age filter on the friend.
  (print-bindings
   "Query with FILTER: friends older than 30 and what they like"
   (execute-sparql-query-filtered
    "select ?person ?friend ?age ?food where { ?person knows ?friend . ?friend age ?age . ?friend likes ?food }"
    (comparison->predicate ">" "?age" 30)))

  ;; Persist and reload the store.
  (save-store "store.nt")
  (set-rdf-store! '())
  (printf "Saved and cleared the store; it now holds ~a triples.\n"
          (length rdf-store))
  (define loaded (load-store "store.nt"))
  (printf "Reloaded ~a triples from store.nt.\n\n" loaded)
  (print-bindings
   "Query after reload: everyone who likes sushi"
   (execute-sparql-query "select ?name where { ?name likes sushi }")))
