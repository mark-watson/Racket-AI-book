#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Tests for rdf_sparql.rkt and rdf_extended.rkt

(require rackunit)
(require "rdf_sparql.rkt")
(require "rdf_extended.rkt")

;;; -----------------------------------------------------------------------------
;;; Store operations

(test-case "add and remove triples"
  (set-rdf-store! '())
  (add-triple "John" "likes" "pizza")
  (add-triple "Mary" "likes" "sushi")
  (check-equal? (length rdf-store) 2)
  (remove-triple "John" "likes" "pizza")
  (check-equal? (length rdf-store) 1)
  (check-equal? (triple-object (car rdf-store)) "sushi"))

;;; -----------------------------------------------------------------------------
;;; Parser

(test-case "variables are detected"
  (check-true (variable? "?name"))
  (check-false (variable? "name"))
  (check-false (variable? ""))
  (check-false (variable? 42)))

(test-case "where patterns split on periods"
  (check-equal?
   (parse-where-patterns '("?name" "age" "?age" "." "?name" "likes" "?food"))
   '(("?name" "age" "?age") ("?name" "likes" "?food")))
  ;; trailing period and empty patterns are both fine
  (check-equal?
   (parse-where-patterns '("?s" "likes" "?o" "."))
   '(("?s" "likes" "?o"))))

(test-case "parse a full query"
  (define q (parse-sparql-query "select ?s ?o where { ?s likes ?o }"))
  (check-equal? (sparql-query-select-vars q) '("?s" "?o"))
  (check-equal? (sparql-query-where-patterns q) '(("?s" "likes" "?o")))
  ;; keywords are case-insensitive
  (define q2 (parse-sparql-query "SELECT * WHERE { ?s likes ?o }"))
  (check-equal? (sparql-query-select-vars q2) '("*")))

;;; -----------------------------------------------------------------------------
;;; Query execution

(define (setup-food-store)
  (set-rdf-store! '())
  (add-triple "John" "age" "30")
  (add-triple "John" "likes" "pizza")
  (add-triple "Mary" "age" "25")
  (add-triple "Mary" "likes" "sushi")
  (add-triple "Bob" "age" "35")
  (add-triple "Bob" "likes" "burger"))

(test-case "single pattern query"
  (setup-food-store)
  (define results (execute-sparql-query "select ?s ?o where { ?s likes ?o }"))
  (check-equal? (length results) 3))

(test-case "join on shared variable"
  (setup-food-store)
  (define results
    (execute-sparql-query
     "select ?name where { ?name age ?age . ?name likes pizza }"))
  (check-equal? (length results) 1)
  (check-equal? (dict-ref (car results) "?name") "John"))

(test-case "query with no matches returns empty list, not an error"
  (setup-food-store)
  (check-equal? (execute-sparql-query "select ?s where { ?s dislikes ?o }")
                '()))

(test-case "literal in subject position"
  (setup-food-store)
  (define results
    (execute-sparql-query "select ?p ?o where { Bob ?p ?o }"))
  (check-equal? (length results) 2))

(test-case "select * keeps all bound variables"
  (setup-food-store)
  (define results
    (execute-sparql-query "select * where { ?name age ?age }"))
  (check-equal? (length results) 3)
  (for ([row results])
    (check-equal? (length row) 2)))

;;; -----------------------------------------------------------------------------
;;; Bindings helpers

(test-case "apply-bindings substitutes known variables"
  (check-equal? (apply-bindings '("?s" "likes" "?o")
                                '(("?s" . "John")))
                '("John" "likes" "?o")))

(test-case "triple-to-binding only binds variables"
  (define t (triple "John" "likes" "pizza"))
  (check-equal? (triple-to-binding t '("?s" "likes" "?o"))
                '(("?o" . "pizza") ("?s" . "John")))
  (check-equal? (triple-to-binding t '("?s" "likes" "pizza"))
                '(("?s" . "John"))))

;;; -----------------------------------------------------------------------------
;;; N-Triples persistence

(define test-file "test-store.nt")

(test-case "N-Triples round trip"
  (setup-food-store)
  (save-store test-file)
  (define saved-lines (file->lines test-file))
  (check-equal? (length saved-lines) 6)
  ;; every line is subject predicate object period
  (check-true
   (andmap (lambda (line)
             (regexp-match? #px"^\"[^\"]*\" \"[^\"]*\" \"[^\"]*\" \\.$" line))
           saved-lines))
  (set-rdf-store! '())
  (check-equal? (length rdf-store) 0)
  (check-equal? (load-store test-file) 6)
  ;; same query as before the save gives the same answer
  (define results
    (execute-sparql-query "select ?name where { ?name likes sushi }"))
  (check-equal? (length results) 1)
  (check-equal? (dict-ref (car results) "?name") "Mary"))

(test-case "bad lines are skipped, not fatal"
  (call-with-output-file test-file
    (lambda (out)
      (displayln "\"a\" \"b\" \"c\" ." out)
      (displayln "" out)
      (displayln "this is not a triple" out)
      (displayln "\"d\" \"e\" \"f\" ." out))
    #:exists 'replace)
  (check-equal? (load-store test-file) 2))

(when (file-exists? test-file) (delete-file test-file))

;;; -----------------------------------------------------------------------------
;;; FILTER support

(test-case "numeric comparisons"
  (define older-than-30 (comparison->predicate ">" "?age" 30))
  (check-true (older-than-30 '(("?age" . "41"))))
  (check-false (older-than-30 '(("?age" . "25"))))
  (check-false (older-than-30 '(("?age" . "not-a-number"))))
  (check-false (older-than-30 '(("?other" . "41")))))

(test-case "filtered query keeps only matching rows"
  (populate-demo-store)
  (define results
    (execute-sparql-query-filtered
     "select ?name ?age where { ?name age ?age }"
     (comparison->predicate ">=" "?age" 30)))
  (define names (sort (map (lambda (r) (dict-ref r "?name")) results)
                      string<?))
  (check-equal? names '("Alice" "Bob" "John")))

(displayln "\nAll tests passed.")
