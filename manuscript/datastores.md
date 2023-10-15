# Datastores

We start with covering strategies for persistently storing data, what I consider to be infrastructure Racket code. In this chapter we develop code for using the SQLite relational database. In the next two chapters we cover data visualization using external processes to integrate programs written in different programming languages with our Racket Projects.


## Accessing Public Knowledge Graphs - a DBPedia Example

In this Racket code snippet, the primary objective is to interact with DBpedia's SPARQL endpoint to query information regarding a person based on their name or URI. The code is structured into several functions, each encapsulating a specific aspect of the querying process, thereby promoting modular design and ease of maintenance.

**Function Definitions:**

- **sparql-dbpedia-for-person**: This function takes a `person-uri` as an argument and constructs a SPARQL query to retrieve the comment and website associated with the person. The `@string-append` macro helps in constructing the SPARQL query string by concatenating the literals and the `person-uri` argument.

- **sparql-dbpedia-person-uri**: Similar to the above function, this function accepts a `person-name` argument and constructs a SPARQL query to fetch the URI and comment of the person from DBpedia.

- **sparql-query->hash**: This function encapsulates the logic for sending the constructed SPARQL query to the DBpedia endpoint. It takes a `query` argument, encodes it into a URL format, and sends an HTTP request to the DBpedia SPARQL endpoint. The response, expected in JSON format, is then converted to a Racket expression using `string->jsexpr`.

- **json->listvals**: This function is designed to transform the JSON expression obtained from the SPARQL endpoint into a more manageable list of values. It processes the hash data structure, extracting the relevant bindings and converting them into a list format.

- **gd** (Data Processing Function): This function processes the data structure obtained from `json->listvals`. It defines four inner functions `gg1`, `gg2`, `gg3`, and `gg4`, each designed to handle a specific number of variables returned in the SPARQL query result. It uses a `case` statement to determine which inner function to call based on the length of the data.

- **sparql-dbpedia**: This is the entry function which accepts a `sparql` argument, invokes `sparql-query->hash` to execute the SPARQL query, and then calls `gd` to process the resulting data structure.

**Usage Flow:**

The typical flow would be to call **sparql-dbpedia-person-uri** with a person's name to obtain the person's URI and comment from DBpedia. Following that, **sparql-dbpedia-for-person** can be invoked with the obtained URI to fetch more detailed information like websites associated with the person. The results from these queries are then processed through **sparql-query->hash**, **json->listvals**, and **gd** to transform the raw JSON response into a structured list format, making it easier to work with within the Racket environment.


```racket
#lang at-exp racket

(provide sparql-dbpedia-person-uri)
(provide sparql-query->hash)
(provide json->listvals)
(provide sparql-dbpedia)

(require net/url)
(require net/uri-codec)
(require json)
(require racket/pretty)

(define (sparql-dbpedia-for-person person-uri)
  @string-append{
     SELECT
      (GROUP_CONCAT(DISTINCT ?website; SEPARATOR="  |  ")
                                   AS ?website) ?comment {
      OPTIONAL {
       @person-uri
       <http://www.w3.org/2000/01/rdf-schema#comment>
       ?comment . FILTER (lang(?comment) = 'en')
      } .
      OPTIONAL {
       @person-uri
       <http://dbpedia.org/ontology/wikiPageExternalLink>
       ?website
        . FILTER( !regex(str(?website), "dbpedia", "i"))
      }
     } LIMIT 4})

(define (sparql-dbpedia-person-uri person-name)
  @string-append{
    SELECT DISTINCT ?personuri ?comment {
      ?personuri
        <http://xmlns.com/foaf/0.1/name>
        "@person-name"@"@"en .
      ?personuri
        <http://www.w3.org/2000/01/rdf-schema#comment>
        ?comment .
             FILTER  (lang(?comment) = 'en') .
}})


(define (sparql-query->hash query)
  (call/input-url
   (string->url
    (string-append
     "https://dbpedia.org/sparql?query="
     (uri-encode query)))
   get-pure-port
   (lambda (port)
     (string->jsexpr (port->string port)))
   '("Accept: application/json")))

(define (json->listvals a-hash)
  (let ((bindings (hash->list a-hash)))
    (let* ((head (first bindings))
           (vars (hash-ref (cdr head) 'vars))
           (results (second bindings)))
      (let* ((x (cdr results))
             (b (hash-ref x 'bindings)))
        (for/list
            ([var vars])
          (for/list ([bc b])
            (let ((bcequal
                   (make-hash (hash->list bc))))
              (let ((a-value
                     (hash-ref
                      (hash-ref
                       bcequal
                       (string->symbol var)) 'value)))
                (list var a-value)))))))))


(define gd (lambda (data)

    (let ((jd (json->listvals data)))

      (define gg1
        (lambda (jd) (map list (car jd))))
      (define gg2
        (lambda (jd) (map list (car jd) (cadr jd))))
      (define gg3
        (lambda (jd)
          (map list (car jd) (cadr jd) (caddr jd))))
      (define gg4
        (lambda (jd)
          (map list
               (car jd) (cadr jd)
               (caddr jd) (cadddr jd))))

      (case (length (json->listvals data))
        [(1) (gg1 (json->listvals data))]
        [(2) (gg2 (json->listvals data))]
        [(3) (gg3 (json->listvals data))]
        [(4) (gg4 (json->listvals data))]
        [else
         (error "sparql queries with 1 to 4 vars")]))))


(define sparql-dbpedia
  (lambda (sparql)
    (gd (sparql-query->hash sparql))))

;; (sparql-dbpedia (sparql-dbpedia-person-uri "Steve Jobs"))
```

## Sqlite

We will be using the Racket source file **sqlite.rkt** in the directory **Racket-AI-book-code/misc_code** for the code snippets in this REPL:

```racket
$ racket
Welcome to Racket v8.10 [cs].
> (require db)
> (require sqlite-table)
> (define db-file "test.db")
> (define db (sqlite3-connect #:database db-file #:mode 'create))
> (query-exec db
     "create temporary table the_numbers (n integer, d varchar(20))")
> (query-exec db
     "create  table person (name varchar(30), age integer, email varchar(20))")
> (query-exec db
     "insert into person values ('Mary', 34, 'mary@test.com')")
> (query-rows db "select * from person")
'(#("Mary" 34 "mary@test.com"))
> 
```

Here we see how to interact with a SQLite database using the **db** and **sqlite-table** libraries in Racket. The **sqlite3-connect** function is used to connect to the SQLite database specified by the string value of **db-file**. The **#:mode 'create** keyword argument indicates that a new database should be created if it doesn't already exist. The database connection object is bound to the identifier **db**. 

The **query-exec** function call is made to create a permanent table named **person** with three columns: **name** of type **varchar(30)**, **age** of type **integer**, and **email** of type **varchar(20)**. The next **query-exec** function call is made to insert a new row into the **person** table with the values 'Mary', 34, and 'mary@test.com'. There is a function **query** that we don't use here that returns the types of the columns returned by a query. We use the alternative function **query-rows** that only returns the query results with no type information.




