#lang racket

;; RDF triple structure
(struct triple (subject predicate object) #:transparent)

;; RDF datastore
(define rdf-store '())

;; Add a triple to the datastore
(define (add-triple subject predicate object)
  (set! rdf-store (cons (triple subject predicate object) rdf-store)))

;; Remove a triple from the datastore
(define (remove-triple subject predicate object)
  (set! rdf-store
        (filter (lambda (t)
                  (not (and (equal? (triple-subject t) subject)
                            (equal? (triple-predicate t) predicate)
                            (equal? (triple-object t) object))))
                rdf-store)))

;; Simple string splitting function
(define (split-string string [delimiter " "])
  (string-split string delimiter))

;; Helper function to check if a string is a variable
(define (variable? str)
  (and (string? str) (> (string-length str) 0) (char=? (string-ref str 0) #\?)))

;; Convert triple to binding
(define (triple-to-binding t [pattern #f])
  (define binding '())
  (when (and pattern (variable? (first pattern)))
    (set! binding (cons (cons (first pattern) (triple-subject t)) binding)))
  (when (and pattern (variable? (second pattern)))
    (set! binding (cons (cons (second pattern) (triple-predicate t)) binding)))
  (when (and pattern (variable? (third pattern)))
    (set! binding (cons (cons (third pattern) (triple-object t)) binding)))
  binding)

(define (query-triples subject predicate object)
  (filter (lambda (t)
            (and (or (not subject) (variable? subject) (equal? (triple-subject t) subject))
                 (or (not predicate) (variable? predicate) (equal? (triple-predicate t) predicate))
                 (or (not object) (variable? object) (equal? (triple-object t) object))))
          rdf-store))

;; Print all triples in the datastore
(define (print-all-triples)
  (printf "All triples in the datastore:\n")
  (for ([t rdf-store])
    (printf "~a ~a ~a\n"
            (triple-subject t)
            (triple-predicate t)
            (triple-object t)))
  (printf "\n"))

;; SPARQL query structure
(struct sparql-query (select-vars where-patterns) #:transparent)

;; Apply bindings to a pattern
(define (apply-bindings pattern bindings)
  (map (lambda (item)
         (if (variable? item)
             (or (dict-ref bindings item #f) item)
             item))
       pattern))

;; Merge bindings
(define (merge-bindings binding1 binding2)
  (append binding1 binding2))

(define (parse-where-patterns where-clause)
  (let loop ([tokens where-clause]
             [current-pattern '()]
             [patterns '()])
    (cond
      [(null? tokens)
       (if (null? current-pattern)
           (reverse patterns)
           (reverse (cons (reverse current-pattern) patterns)))]
      [(string=? (car tokens) ".")
       (loop (cdr tokens)
             '()
             (if (null? current-pattern)
                 patterns
                 (cons (reverse current-pattern) patterns)))]
      [else
       (loop (cdr tokens)
             (cons (car tokens) current-pattern)
             patterns)])))

(define (parse-sparql-query query-string)
  (define tokens (filter (lambda (token) (not (member token '("{" "}") string=?)))
                         (split-string query-string)))
  (define select-index (index-of tokens "select" string-ci=?))
  (define where-index (index-of tokens "where" string-ci=?))
  (define (sublist lst start end)
    (take (drop lst start) (- end start)))
  (define select-vars (sublist tokens (add1 select-index) where-index))
  (define where-clause (drop tokens (add1 where-index)))
  (define where-patterns (parse-where-patterns where-clause))
  (sparql-query select-vars where-patterns))

(define (project-results results select-vars)
  (if (equal? select-vars '("*"))
      (map remove-duplicate-bindings results)
      (map (lambda (result)
             (remove-duplicate-bindings
              (map (lambda (var)
                     (cons var (dict-ref result var #f)))
                   select-vars)))
           results)))

(define (remove-duplicate-bindings bindings)
  (remove-duplicates bindings #:key car))

;; Execute WHERE patterns with bindings
(define (execute-where-patterns-with-bindings patterns bindings)
  (if (null? patterns)
      (list bindings)
      (let* ([pattern (first patterns)]
             [remaining-patterns (rest patterns)]
             [bound-pattern (apply-bindings pattern bindings)]
             [matching-triples (apply query-triples bound-pattern)])
        (let ([new-bindings (map (lambda (t)
                                   (merge-bindings bindings (triple-to-binding t pattern)))
                                 matching-triples)])
          (if (null? remaining-patterns)
              new-bindings
              (append-map (lambda (binding)
                            (execute-where-patterns-with-bindings remaining-patterns binding))
                          new-bindings))))))

(define (execute-where-patterns patterns)
  (if (null? patterns)
      (list '())
      (let* ([pattern (first patterns)]
             [remaining-patterns (rest patterns)]
             [matching-triples (apply query-triples pattern)])
        (let ([bindings (map (lambda (t) (triple-to-binding t pattern)) matching-triples)])
          (if (null? remaining-patterns)
              bindings
              (append-map (lambda (binding)
                            (let ([results (execute-where-patterns-with-bindings remaining-patterns binding)])
                              (map (lambda (result)
                                     (merge-bindings binding result))
                                   results)))
                          bindings))))))

(define (execute-sparql-query query-string)
  (let* ([query (parse-sparql-query query-string)]
         [where-patterns (sparql-query-where-patterns query)]
         [select-vars (sparql-query-select-vars query)]
         [results (execute-where-patterns where-patterns)]
         [projected-results (project-results results select-vars)])
    projected-results))

(define (main)
  (set! rdf-store '())

  (add-triple "John" "age" "30")
  (add-triple "John" "likes" "pizza")
  (add-triple "Mary" "age" "25")
  (add-triple "Mary" "likes" "sushi")
  (add-triple "Bob" "age" "35")
  (add-triple "Bob" "likes" "burger")

  (print-all-triples)

  (define (print-query-results query-string)
    (printf "Query: ~a\n" query-string)
    (let ([results (execute-sparql-query query-string)])
      (printf "Final Results:\n")
      (if (null? results)
          (printf "  No results\n")
          (for ([result results])
            (printf "  ~a\n"
                    (string-join
                     (map (lambda (pair)
                            (format "~a: ~a" (car pair) (cdr pair)))
                          result)
                     ", "))))
      (printf "\n")))

  (print-query-results "select * where { ?name age ?age . ?name likes ?food }")
  (print-query-results "select ?s ?o where { ?s likes ?o }")
  (print-query-results "select * where { ?name age ?age . ?name likes pizza }"))

;; Run the main function
(main)
