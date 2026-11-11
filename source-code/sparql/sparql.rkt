#lang at-exp racket

;;; sparql.rkt — A small SPARQL client for the DBpedia public endpoint
;;; Apache 2 License

(provide sparql-dbpedia-for-person
         sparql-dbpedia-person-uri
         sparql-dbpedia-place-uri
         sparql-escape-string
         sparql-query->hash
         json->listvals
         extract-name-uri-and-comment
         gd
         sparql-dbpedia
         *sparql-endpoint*)

(require net/url)
(require net/uri-codec)
(require json)
(require racket/pretty)

;; The endpoint is a parameter so you can point the same code at another
;; SPARQL service, for example https://query.wikidata.org/sparql
(define *sparql-endpoint* (make-parameter "https://dbpedia.org/sparql"))

;; Names typed by a user go inside a quoted SPARQL literal. Escape the
;; characters that would end the literal early or break the query.
(define (sparql-escape-string s)
  (let* ([s (string-replace s "\\" "\\\\")]
         [s (string-replace s "\"" "\\\"")]
         [s (string-replace s "\n" "\\n")]
         [s (string-replace s "\r" "\\r")])
    s))

(define (sparql-dbpedia-for-person person-uri)
  @string-append{
     SELECT
      (GROUP_CONCAT(DISTINCT ?website; SEPARATOR="  |  ")
                                   AS ?website) ?comment {
      OPTIONAL {
        { @person-uri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . FILTER (lang(?comment) = 'en') }
        UNION
        { @person-uri <http://dbpedia.org/ontology/description> ?comment . FILTER (lang(?comment) = 'en') }
      } .
      OPTIONAL {
        @person-uri
        <http://dbpedia.org/ontology/wikiPageExternalLink>
        ?website
         . FILTER( !regex(str(?website), "dbpedia", "i"))
      }
     } LIMIT 4})

(define (sparql-dbpedia-person-uri person-name)
  (define name (sparql-escape-string person-name))
  @string-append{
    SELECT DISTINCT ?personuri ?comment {
      ?personuri
        <http://xmlns.com/foaf/0.1/name>
        "@name"@"@"en .
      {
        ?personuri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment .
        FILTER (lang(?comment) = 'en')
      } UNION {
        ?personuri <http://dbpedia.org/ontology/description> ?comment .
        FILTER (lang(?comment) = 'en')
      }
}})

(define (sparql-dbpedia-place-uri place-name)
  (define name (sparql-escape-string place-name))
  @string-append{
    SELECT DISTINCT ?placeuri ?comment {
      ?placeuri
        <http://xmlns.com/foaf/0.1/name>
        "@name"@"@"en .
      {
        ?placeuri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment .
        FILTER (lang(?comment) = 'en')
      } UNION {
        ?placeuri <http://dbpedia.org/ontology/description> ?comment .
        FILTER (lang(?comment) = 'en')
      }
}})

;; Send a query and return the JSON reply as a hash table. A busy public
;; endpoint sometimes answers with an HTML error page instead of JSON, so
;; a reply that does not parse raises an error that says what came back.
(define (sparql-query->hash query)
  (define reply
    (call/input-url
     (string->url
      (string-append
       (*sparql-endpoint*)
       "?query="
       (uri-encode query)))
     get-pure-port
     port->string
     '("Accept: application/json")))
  (with-handlers
      ([exn:fail:read?
        (lambda (e)
          (error 'sparql-query->hash
                 "endpoint did not return JSON; reply starts with: ~a"
                 (substring reply 0 (min 200 (string-length reply)))))])
    (define result (string->jsexpr reply))
    (unless (hash? result)
      (error 'sparql-query->hash "unexpected JSON reply: ~a" result))
    result))

;; One list per query variable; each element is (variable-name value)
(define (json->listvals a-hash)
  (let* ([head (hash-ref a-hash 'head (hash))]
         [vars (hash-ref head 'vars '())]
         [results (hash-ref a-hash 'results (hash))]
         [bindings (hash-ref results 'bindings '())])
    (for/list ([var vars])
      (for/list ([bc bindings])
        (let* ([var-sym (string->symbol var)]
               [var-info (hash-ref bc var-sym (hash))]
               [a-value (hash-ref var-info 'value "")])
          (list var a-value))))))

;; Turn the per-variable lists into one list per result row. "Zipping"
;; with (apply map list ...) works for any number of query variables.
(define (gd data)
  (let ([jd (json->listvals data)])
    (if (null? jd)
        '()
        (apply map list jd))))

;; Used by the Knowledge Graph Navigator: pair up the values of the
;; first two query variables as (uri comment) lists
(define (extract-name-uri-and-comment l1 l2)
  (map (lambda (a b) (list (second a) (second b)))
       l1 l2))

(define (sparql-dbpedia sparql)
  (gd (sparql-query->hash sparql)))

(module+ main
  ;; (pretty-print (sparql-dbpedia (sparql-dbpedia-person-uri "Steve Jobs")))
  ;; (pretty-print (sparql-dbpedia (sparql-dbpedia-for-person "<http://dbpedia.org/resource/Steve_Jobs>")))
  (void))
