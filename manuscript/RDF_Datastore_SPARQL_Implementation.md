# Implementing a Simple RDF Datastore with Partial SPARQL Support in Racket

This chapter explains a Racket implementation of a simple RDF (Resource Description Framework) datastore with partial SPARQL (SPARQL Protocol and RDF Query Language) support. We'll cover the core RDF data structures, query parsing and execution, helper functions, and the main function with example queries. The file **rdf_sparql.rkt** can be found online at [https://github.com/mark-watson/Racket-AI-book/source-code/simple_RDF_SPARQL](https://github.com/mark-watson/Racket-AI-book/tree/main/source-code/simple_RDF_SPARQL).

Before looking at the code we look at sample use and output. The  function **test**:

```racket
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
  ```

Generate the output:

```text
All triples in the datastore:
Bob likes burger
Bob age 35
Mary likes sushi
Mary age 25
John likes pizza
John age 30

Query: select * where { ?name age ?age . ?name likes ?food }
Final Results:
  ?age: 35, ?name: Bob, ?food: burger
  ?age: 25, ?name: Mary, ?food: sushi
  ?age: 30, ?name: John, ?food: pizza

Query: select ?s ?o where { ?s likes ?o }
Final Results:
  ?s: Bob, ?o: burger
  ?s: Mary, ?o: sushi
  ?s: John, ?o: pizza

Query: select * where { ?name age ?age . ?name likes pizza }
Final Results:
  ?age: 30, ?name: John
```


## 1. Core RDF Data Structures and Basic Operations

There are two parts to this example: a simple unindexed RDF datastore and a partial SPARQL query implementation that supports compound where clause matches like: **select * where { ?name age ?age . ?name likes pizza }**.

### 1.1 RDF Triple Structure

The foundation of our RDF datastore is the `triple` structure:

```racket
(struct triple (subject predicate object) #:transparent)
```

This structure represents an RDF triple, consisting of a subject, predicate, and object. The `#:transparent` keyword makes the structure's fields accessible for easier debugging and printing.

### 1.2 RDF Datastore

The RDF datastore is implemented as a simple list:

```racket
(define rdf-store '())
```

### 1.3 Basic Operations

Two fundamental operations are defined for the datastore:

1. Adding a triple:

```racket
(define (add-triple subject predicate object)
  (set! rdf-store (cons (triple subject predicate object) rdf-store)))
```

2. Removing a triple:

```racket
(define (remove-triple subject predicate object)
  (set! rdf-store
        (filter (lambda (t)
                  (not (and (equal? (triple-subject t) subject)
                            (equal? (triple-predicate t) predicate)
                            (equal? (triple-object t) object))))
                rdf-store)))
```

## 2. Query Parsing and Execution

### 2.1 SPARQL Query Structure

A simple SPARQL query is represented by the `sparql-query` structure:

```racket
(struct sparql-query (select-vars where-patterns) #:transparent)
```

### 2.2 Query Parsing

The `parse-sparql-query` function takes a query string and converts it into a `sparql-query` structure:

```racket
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
```

### 2.3 Query Execution

The main query execution function is `execute-sparql-query`:

```racket
(define (execute-sparql-query query-string)
  (let* ([query (parse-sparql-query query-string)]
         [where-patterns (sparql-query-where-patterns query)]
         [select-vars (sparql-query-select-vars query)]
         [results (execute-where-patterns where-patterns)]
         [projected-results (project-results results select-vars)])
    projected-results))
```

This function parses the query, executes the WHERE patterns, and projects the results based on the SELECT variables.

## 3. Helper Functions and Utilities

Several helper functions are implemented to support query execution:

1. `variable?`: Checks if a string is a SPARQL variable (starts with '?').
2. `triple-to-binding`: Converts a triple to a binding based on a pattern.
3. `query-triples`: Filters triples based on a given pattern.
4. `apply-bindings`: Applies bindings to a pattern.
5. `merge-bindings`: Merges two sets of bindings.
6. `project-results`: Projects the final results based on the SELECT variables.

## 4. Main Function and Example Queries

The `main` function demonstrates the usage of the RDF datastore and SPARQL query execution:

```racket
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
```

This function:

1. Initializes the RDF store with sample data.
2. Prints all triples in the datastore.
3. Defines a `print-query-results` function to execute and display query results.
4. Executes three example SPARQL queries:
   - Query all name-age-food combinations.
   - Query all subject-object pairs for the "likes" predicate.
   - Query all people who like pizza and their ages.

## Conclusion

This implementation provides a basic framework for an RDF datastore with partial SPARQL support in Racket. While it lacks many features of a full-fledged RDF database and SPARQL engine, it demonstrates the core concepts and can serve as a starting point for more complex implementations and the code is simple enough to be fun experimenting with.