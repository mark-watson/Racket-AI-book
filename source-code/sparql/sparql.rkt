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
  @string-append{
    SELECT DISTINCT ?personuri ?comment {
      ?personuri
        <http://xmlns.com/foaf/0.1/name>
        "@person-name"@"@"en .
      {
        ?personuri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment .
        FILTER (lang(?comment) = 'en')
      } UNION {
        ?personuri <http://dbpedia.org/ontology/description> ?comment .
        FILTER (lang(?comment) = 'en')
      }
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

(define (gd data)
  (let ([jd (json->listvals data)])
    (define gg1 (lambda (x) (map list (car x))))
    (define gg2 (lambda (x) (map list (car x) (cadr x))))
    (define gg3 (lambda (x) (map list (car x) (cadr x) (caddr x))))
    (define gg4 (lambda (x) (map list (car x) (cadr x) (caddr x) (cadddr x))))
    (case (length jd)
      [(1) (gg1 jd)]
      [(2) (gg2 jd)]
      [(3) (gg3 jd)]
      [(4) (gg4 jd)]
      [else (error "sparql queries with 1 to 4 vars")])))

(define sparql-dbpedia
  (lambda (sparql)
    (gd (sparql-query->hash sparql))))

(module+ main
  ;; (pretty-print (sparql-dbpedia (sparql-dbpedia-person-uri "Steve Jobs")))
  ;; (pretty-print (sparql-dbpedia (sparql-dbpedia-for-person "<http://dbpedia.org/resource/Steve_Jobs>")))
  (void))
  
