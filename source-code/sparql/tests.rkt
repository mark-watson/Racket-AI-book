#lang racket

;;; tests.rkt — Offline unit tests for the SPARQL client
;;; Apache 2 License
;;;
;;; The result-processing functions are tested against a canned reply in
;;; the standard SPARQL JSON results format, so no network is needed.
;;; Run with:  raco test tests.rkt

(require rackunit json)
(require "sparql.rkt")

(define canned-reply
  (string->jsexpr #<<JSON
{"head": {"vars": ["personuri", "comment"]},
 "results": {"bindings": [
   {"personuri": {"type": "uri", "value": "http://dbpedia.org/resource/Steve_Jobs"},
    "comment":   {"type": "literal", "value": "American entrepreneur"}},
   {"personuri": {"type": "uri", "value": "http://dbpedia.org/resource/Steve_Jobs_(film)"},
    "comment":   {"type": "literal", "value": "2015 film"}}]}}
JSON
                  ))

(test-case "json->listvals gives one list per variable"
  (define lv (json->listvals canned-reply))
  (check-equal? (length lv) 2)
  (check-equal? (first (first lv))
                '("personuri" "http://dbpedia.org/resource/Steve_Jobs"))
  (check-equal? (second (second lv)) '("comment" "2015 film")))

(test-case "gd gives one list per result row"
  (check-equal?
   (gd canned-reply)
   '((("personuri" "http://dbpedia.org/resource/Steve_Jobs")
      ("comment" "American entrepreneur"))
     (("personuri" "http://dbpedia.org/resource/Steve_Jobs_(film)")
      ("comment" "2015 film")))))

(test-case "gd handles any number of variables, including none"
  (define five
    (hash 'head (hash 'vars '("a" "b" "c" "d" "e"))
          'results
          (hash 'bindings
                (list (for/hash ([v '(a b c d e)])
                        (values v (hash 'value (symbol->string v))))))))
  (check-equal? (gd five)
                '((("a" "a") ("b" "b") ("c" "c") ("d" "d") ("e" "e"))))
  (check-equal? (gd (hash 'head (hash 'vars '()))) '()))

(test-case "a missing binding becomes an empty string"
  (define partial
    (hash 'head (hash 'vars '("x" "y"))
          'results (hash 'bindings (list (hash 'x (hash 'value "1"))))))
  (check-equal? (gd partial) '((("x" "1") ("y" ""))))) 

(test-case "extract-name-uri-and-comment pairs the two variables"
  (define lv (json->listvals canned-reply))
  (check-equal? (first (extract-name-uri-and-comment (first lv) (second lv)))
                '("http://dbpedia.org/resource/Steve_Jobs"
                  "American entrepreneur")))

(test-case "sparql-escape-string protects quoted literals"
  (check-equal? (sparql-escape-string "Dwayne \"The Rock\" Johnson")
                "Dwayne \\\"The Rock\\\" Johnson")
  (check-equal? (sparql-escape-string "a\\b") "a\\\\b")
  (check-equal? (sparql-escape-string "Steve Jobs") "Steve Jobs"))

(test-case "a name with a quote cannot close the literal early"
  (define q (sparql-dbpedia-person-uri "x\" . ?s ?p ?o . \""))
  (check-true (string-contains? q "\"x\\\" . ?s ?p ?o . \\\"\"@en")))

(test-case "generated queries contain the name and the language tag"
  (check-true (string-contains? (sparql-dbpedia-person-uri "Steve Jobs")
                                "\"Steve Jobs\"@en"))
  (check-true (string-contains? (sparql-dbpedia-place-uri "Paris")
                                "\"Paris\"@en")))
