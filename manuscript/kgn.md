# Knowledge Graph Navigator {#kgn}


The Knowledge Graph Navigator (which I will often refer to as KGN) is a tool for processing a set of entity names and automatically explores the public Knowledge Graph [DBPedia](http://dbpedia.org) using SPARQL queries. I started to write KGN for my own use to automate some things I used to do manually when exploring Knowledge Graphs, and later thought that KGN might be also useful for educational purposes. KGN shows the user the auto-generated SPARQL queries so hopefully the user will learn by seeing examples. KGN uses the Clojure Jena wrapper
example code from the last chapter as well the two Java classes **JenaAPis** and **QueryResults** (which wrap the Apache Jena library) that were also included in the example for the previous chapter.

**Note:** There are three separate examples for implementing SPARQL queries in this example:

- Use the code from the last chapter (Jena and query caching)
- Use a small standalone set of Clojure functions to access DBPedia
- Use a small standalone set of Clojure functions to access a local GraphDB RDF server with the data file **dbpedia_sample.nt** loaded into a graph named **dbpedia**.

The example code is set up to use Jena and query caching; edit the file **sparql.clj** to enable the other options.

I have implemented parts of KGN in several languages: Common Lisp, Java, Racket Scheme, Swift, Python, and Hy. The most full featured version of KGN, including a full user interface, is featured in my book [Loving Common Lisp, or the Savvy Programmer's Secret Weapon](https://leanpub.com/lovinglisp) that you can read free online. That version performs more speculative SPARQL queries to find information compared to the example here that I designed for ease of understanding, and modification.

We will be running an example using data containing three person entities, one company entity, and one place entity. The following figure shows a very small part of the DBPedia Knowledge Graph that is centered around these entities. The data for this figure was collected by an example Knowledge Graph Creator from my Common Lisp book:

![File dbpedia_sample.nt loaded into the free version of GraphDB](images/graphdb.jpg)

I chose to use DBPedia instead of WikiData for this example because DBPedia URIs are human readable. The following URIs represent the concept of a *person*. The semantic meanings of DBPedia and FOAF (friend of a friend) URIs are self-evident to a human reader while the WikiData URI is not:

{linenos=off}
~~~~~~~~
http://www.wikidata.org/entity/Q215627
http://dbpedia.org/ontology/Person
http://xmlns.com/foaf/0.1/name
~~~~~~~~

I frequently use WikiData in my work and WikiData is one of the most useful public knowledge bases. I have both DBPedia and WikiData Sparql endpoints in the example code that we will look at later, with the WikiData endpoint comment out. You can try manually querying WikiData at the [WikiData SPARL endpoint](https://query.wikidata.org). For example, you might explore the WikiData URI for the *person* concept using:

{lang=sparql, linenos=off}
~~~~~~~~
select ?p ?o where {
 <http://www.wikidata.org/entity/Q215627> ?p ?o .
} limit 10
~~~~~~~~

For the rest of this chapter we will just use DBPedia or data copied from DBPedia.

After looking an interactive session using the example program for this chapter we will look at the implementation.

## Entity Types Handled by KGN

To keep this example simple we handle just three entity types:

- People
- Organizations
- Places
 
In addition to finding detailed information for people, organizations, and places we will also search for relationships between entities. This search process consists of generating a series of SPARQL queries and calling the DBPedia SPARQL endpoint.

Before we design and write the code, I want to show you sample output for our example program:

{lang="clojure",linenos=off}
~~~~~~~~
(kgn {:People ["Bill Gates" "Steve Jobs" "Melinda Gates"]
      :Organization ["Microsoft"]
      :Place        ["California"]})
~~~~~~~~

The output (with some text shortened) is:

{lang="json",linenos=off}
~~~~~~~~
{:entity-summaries
 (("Bill Gates"
   "http://dbpedia.org/resource/Bill_Gates"
   "William Henry Gates III (born October 28, 1955) is an American business magnate, software developer, investor, and philanthropist. He is best known as the co-founder of Microsoft Corporation. During his career...")
  ("Steve Jobs"
   "http://dbpedia.org/resource/Steve_Jobs"
   "Steven Paul Jobs (; February 24, 1955 – October 5, 2011) was an American business magnate, industrial designer, investor, and media proprietor. He was the chairman, chief executive officer (CEO), and co-founder of Apple Inc., the chairman and majority shareholder of Pixar...")
  ("Melinda Gates"
   "http://dbpedia.org/resource/Melinda_Gates"
   "Melinda Ann Gates (née French; August 15, 1964) is an American philanthropist and a former general manager at Microsoft. In 2000, she co-founded the Bill & Melinda Gates Foundation with her husband Bill Gates...")
  ("Microsoft"
   "http://dbpedia.org/resource/Microsoft"
   "Microsoft Corporation () is an American multinational technology company with headquarters in Redmond, Washington. It develops, manufactures, licenses, supports, and sells computer software...")
  ("California"
   "http://dbpedia.org/resource/California"
   "California is a state in the Pacific Region of the United States. With 39.5 million residents across ...")),
 :discovered-relationships
 ((["<http://dbpedia.org/resource/Bill_Gates>"
    "<http://dbpedia.org/property/spouse>"
    "<http://dbpedia.org/resource/Melinda_Gates>"]
   ["<http://dbpedia.org/resource/Melinda_Gates>"
    "<http://dbpedia.org/property/spouse>"
    "<http://dbpedia.org/resource/Bill_Gates>"])
  (["<http://dbpedia.org/resource/Bill_Gates>"
    "<http://dbpedia.org/ontology/knownFor>"
    "<http://dbpedia.org/resource/Microsoft>"]
   ["<http://dbpedia.org/resource/Microsoft>"
    "<http://dbpedia.org/property/founders>"
    "<http://dbpedia.org/resource/Bill_Gates>"])
  (["<http://dbpedia.org/resource/Steve_Jobs>"
    "<http://dbpedia.org/ontology/birthPlace>"
    "<http://dbpedia.org/resource/California>"]))}
~~~~~~~~

Note that the output from the function **kgn** is a map containing two keys: **:entity-summaries** and **:discovered-relationships**.

## KGN Implementation

The example application works processing a list or Person, Place, and Organization names. We generate SPARQL queries to DBPedia to find information about the entities and relationships between them.

Since the DBPedia queries are time consuming, I created a tiny subset of DBPedia in the file **dbpedia_sample.nt** and load it into a RDF data store like **GraphDB** or **Fuseki** running on my laptop. This local setup is especially helpful during development when the same queries are repeatedly used for testing. If you don't modify the file **sparql.clj** then by default the public DBPedia SPARQL endpoint will be used.

The Clojure and Java files from the example in the last chapter were copied un-changed to the current example and the **project.clj** file contains the same dependencies as we used earlier:

{lang="clojure",linenos=on}
~~~~~~~~
(defproject knowledge_graph_navigator_clj "0.1.0-SNAPSHOT"
  :description "Knowledge Graph Navigator"
  :url "https://markwatson.com"
  :license
  {:name
   "EPL-2.0 OR GPL-2+ WITH Classpath-exception-2.0"
   :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :source-paths      ["src"]
  :java-source-paths ["src-java"]
  :javac-options     ["-target" "1.8" "-source" "1.8"]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-http "3.10.3"]
                 [com.cemerick/url "0.1.1"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/data.json "1.0.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.apache.derby/derby "10.15.2.0"]
                 [org.apache.derby/derbytools "10.15.2.0"]
                 [org.apache.derby/derbyclient
                  "10.15.2.0"]
                 [org.apache.jena/apache-jena-libs
                  "3.17.0" :extension "pom"]]
  :repl-options
  {:init-ns knowledge-graph-navigator-clj.kgn}
  :main ^:skip-aot knowledge-graph-navigator-clj.kgn)
~~~~~~~~

I copied the code from the last chapter into this project to save readers from needing to **lein install** the project in the last chapter. We won't look at that code again here.

This example is contained in several source files. We will start at the low-level code in **sparql.clj**. You can edit lines 10-11 if you want to change which SPARQL libraries and endpoints you want to use. There are utility functions for using DBPedia (lines 13-20), the free version of GraphDB (lines 22-35), and a top level function **sparql-endpoint** that can be configured to use the options you can change in lines 10-11. I have a top level wrapper function **sparql-endpoint** so the remainder of the example works without modification with all options. Lines 52-57 is a small main function to facilitate working with this file in isolation.

{lang="clojure",linenos=on}
~~~~~~~~
(ns knowledge-graph-navigator-clj.sparql
  (:require [clj-http.client :as client])
  (:require clojure.stacktrace)
  (:require [cemerick.url :refer (url-encode)])
  (:require [clojure.data.csv :as csv])
  (:require [semantic-web-jena-clj.core :as jena]))

;; see https://github.com/mark-watson/clj-sparql

(def USE-LOCAL-GRAPHDB false)
(def USE-CACHING true) ;; use Jena wrapper

(defn dbpedia [sparql-query]
  (let [q
        (str
          "https://dbpedia.org//sparql?output=csv&query="
          (url-encode sparql-query))
        response (client/get q)
        body (:body response)]
    (csv/read-csv body)))

(defn- graphdb-helper [host port graph-name sparql-query]
  (let [q
        (str host ":" port "/repositories/" graph-name
           "?query=" (url-encode sparql-query))
        response (client/get q)
        body (:body response)]
    (csv/read-csv body)))

(defn graphdb
  ([graph-name sparql-query]
  (graphdb-helper
    "http://127.0.0.1" 7200 graph-name sparql-query))
  ([host port graph-name sparql-query]
    (graphdb-helper host port graph-name sparql-query)))

(defn sparql-endpoint [sparql-query]
  (try
    (if USE-LOCAL-GRAPHDB
      (graphdb "dbpedia" sparql-query)
      (if USE-CACHING
        (jena/query-dbpedia sparql-query)
        (dbpedia sparql-query)))
    (catch Exception e
      (do
        (println
          "WARNING: query failed:\n" sparql-query)
        (println (.getMessage e))
        (clojure.stacktrace/print-stack-trace e)
        []))))

(defn -main
  "SPARQL example"
  [& _]
  (println
    (sparql-endpoint
      "select * { ?s ?p ?o } limit 10")))
~~~~~~~~

The next source file **sparql_utils.clj** contains one function that loads a SPARQL template file and performs variable substitutions from a map:

{lang="clojure",linenos=on}
~~~~~~~~
(ns knowledge-graph-navigator-clj.sparql-utils)

(defn sparql_template
  "open SPARQL template file and perform
   variable substitutions"
  [template-fpath substitution-map]
  (let [template-as-string (slurp template-fpath)]
    (clojure.string/replace
      template-as-string
      (re-pattern
        ; create a regex pattern of quoted replacements 
        ; separated by |
        ; this code is derived from a stackoverflow
        ; example by user bmillare
        (apply
          str
          (interpose
            "|"
            (map
              (fn [x] (java.util.regex.Pattern/quote x))
              (keys substitution-map)))))
      substitution-map)))
~~~~~~~~

The next source file **entities_by_name.clj** provides the functionality of finding DBPedia entity URIs for names of entities, for example "Steve Jobs." The heart of this functionality is one SPARQL query template that is used to look up URIs by name; in this example, the name is hard-wired to "Steve Jobs". The file **entities_by_name.sparql** contains:

{lang="sparql",linenos=on}
~~~~~~~~
select distinct ?s ?comment where {
    ?s <http://www.w3.org/2000/01/rdf-schema#label>
            "<NAME>"@en .
    ?s <http://www.w3.org/2000/01/rdf-schema#comment>
            ?comment  .
    FILTER  (lang(?comment) = "en") .
    ?s
        <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
        <ENTITY_TYPE> .
}
~~~~~~~~

The function **dbpedia-get-entities-by-name** takes two arguments **name** and **dbpedia-type** where **name** was set to "Steve Jobs" and **dbpedia-type** was set to the URI:

    <http://dbpedia.org/ontology/Person>

in the SPARQL query. The **FILTER** statement on line 6 is used to discard all string values that are not tagged to be English language ("en").

This SPARQL query template file is used in lines in lines 9-11:
 
{lang="clojure",linenos=on}
~~~~~~~~
(ns knowledge-graph-navigator-clj.entities-by-name
  (:require [knowledge-graph-navigator-clj.sparql
             :as sparql])
  (:require [clojure.pprint :as pp])
  (:require clojure.string))

(defn dbpedia-get-entities-by-name [name dbpedia-type]
  (let [sparql-query
        (utils/sparql_template
          "entities_by_name.sparql"
          {"<NAME>" name "<ENTITY_TYPE>" dbpedia-type})
        results (sparql/sparql-endpoint sparql-query)]
    results))

(defn -main
  "test/dev entities by name"
  [& _]
  (println
    (dbpedia-get-entities-by-name
      "Steve Jobs"
      "<http://dbpedia.org/ontology/Person>"))
  (println
    (dbpedia-get-entities-by-name
      "Microsoft"
      "<http://dbpedia.org/ontology/Organization>"))
  (pp/pprint
    (dbpedia-get-entities-by-name
      "California"
      "<http://dbpedia.org/ontology/Place>"))
  )
~~~~~~~~

The main function in lines 23-38 was useful for debugging the SPARQL query and code and I left it in the example so you can run and test this file in isolation.

The last utility function we need is defined in the source file **relationships.clj** that uses another SPARQL query template file. This SPARQL template file **relationships.sparql** contains:

{lang="sparql",linenos=on}
~~~~~~~~
SELECT DISTINCT ?p {
    <URI1>
       ?p
       <URI2> .
    FILTER (!regex(str(?p),"wikiPage","i"))
} LIMIT 5
~~~~~~~~

There are three things to note here. The DISTINCT keyword removes duplicate results, In SPARQL queries URIs are enclosed in **<** **>** angle brackets but the brackets are not included in SPARQL query results so the example code adds them. Also, we are looking for all properties that link the two subject/object entity URIs except we don't want any property URIs that provides human readable results ("follow your nose" to dereference URIs to a human readable format); these property names contain the string "wikiPage" so we filter them out of the results.

The **map** call on lines 13-16 is used to discard the first SPARQL query result that is a list of variable bindings from the SPARQL query.

{lang="clojure",linenos=on}
~~~~~~~~
(ns knowledge-graph-navigator-clj.relationships
  (:require [knowledge-graph-navigator-clj.sparql
             :as sparql])
  (:require [clojure.pprint :as pp])
  (:require clojure.string))

(defn dbpedia-get-relationships [s-uri o-uri]
  (let [query
        (utils/sparql_template
          "relationships.sparql"
          {"<URI1>" s-uri "<URI2>" o-uri})
        results (sparql/sparql-endpoint query)]
    (map
      (fn [u] (clojure.string/join "" ["<" u ">"]))
        ;;discard SPARQL variable name p (?p):
        (second results)))) 

(defn entity-results->relationship-links
  [uris-no-brackets]
  (let [uris (map
               (fn [u]
                 (clojure.string/join "" ["<" u ">"]))
               uris-no-brackets)
        relationship-statements (atom [])]
    (doseq [e1 uris]
      (doseq [e2 uris]
        (if (not (= e1 e2))
          (let [l1 (dbpedia-get-relationships e1 e2)
                l2 (dbpedia-get-relationships e2 e1)]
            (doseq [x l1]
              (let [a-tuple [e1 x e2]]
                (if (not
                      (. @relationship-statements
                        contains a-tuple))
                  (reset! relationship-statements
                    (cons a-tuple
                          @relationship-statements))
                  nil))
            (doseq [x l2]
              (let [a-tuple [e2 x e1]]
                (if (not
                      (. @relationship-statements
                        contains a-tuple))
                  (reset! relationship-statements
                    (cons a-tuple
                          @relationship-statements))
                  nil)))))
          nil)))
    @relationship-statements))

(defn -main
  "dev/test entity relationships code"
  [& _]
  (println
    "Testing entity-results->relationship-links")
  (pp/pprint
    (entity-results->relationship-links
      ["http://dbpedia.org/resource/Bill_Gates"
       "http://dbpedia.org/resource/Microsoft"])))
~~~~~~~~

The function **entity-results->relationship-links** (lines 18-49) takes a list of entity URIs (without the angle brackets) and if there are **N** input URIs it then generates SPARQL queries for all **O(N^2)** combinations of choosing two entities at a time.
 
The last source file **kgn.clj** contains the main function for this application. We use the Clojure library **clojure.math.combinatorics** to calculate all combinations of entity URIs, taken two at a time. In lines 11-17 we map entity type symbols to the DBPedia entity type URI for the symbol.

There are two parts to the main function **kgn**:

- Lines 24-39 collects comment descriptions for each input entity.
- Lines 40-50 find, for each entity URI pair, possible relationships between entities.

Function **kgn** returns a map of summaries and discovered entity relationships that we saw listed early in this chapter.

{lang="clojure",linenos=on}
~~~~~~~~
(ns knowledge-graph-navigator-clj.kgn
  (:require
    [knowledge-graph-navigator-clj.entities-by-name
     :as entity-name])
  (:require
    [knowledge-graph-navigator-clj.relationships
     :as rel])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.pprint :as pp]))

(def entity-map
  {:People
   "<http://dbpedia.org/ontology/Person>"
   :Organization
   "<http://dbpedia.org/ontology/Organization>"
   :Place
   "<http://dbpedia.org/ontology/Place>"})

(defn kgn
  "Top level function for the KGN library.
   Inputs: a map with keys Person, Place, and
   Organization. values list of names"
  [input-entity-map]
  (let [entities-summary-data
        (filter
          ;; get rid of empty SPARQL results:
          (fn [x] (> (count x) 1))
          (mapcat    ;; flatten just top level
            identity
            (for [entity-key (keys input-entity-map)]
              (for [entity-name
                    (input-entity-map entity-key)]
                (cons
                  entity-name
                  (second
                    (entity-name/dbpedia-get-entities-by-name 
                     entity-name
                     (entity-map entity-key))))))))
        entity-uris (map second entities-summary-data)
        combinations-by-2-of-entity-uris
        (combo/combinations entity-uris 2)
        discovered-relationships
        (filter
          (fn [x] (> (count x) 0))
          (for [pair-of-uris
                combinations-by-2-of-entity-uris]
            (seq
              (rel/entity-results->relationship-links
                pair-of-uris))))]
    {:entity-summaries entities-summary-data
     :discovered-relationships
     discovered-relationships}))

(defn -main
  "Main function for KGN example"
  [& _]
  (let [results
        (kgn
          {:People
           ["Bill Gates" "Steve Jobs" "Melinda Gates"]
           :Organization ["Microsoft"]
           :Place        ["California"]})]
    (println " -- results:") (pp/pprint results)))
~~~~~~~~


This KGN example was hopefully both interesting to you and simple enough in its implementation to use as a jumping off point for your own projects. 

I had the idea for the KGN application because I was spending quite a bit of time manually setting up SPARQL queries for DBPedia (and other public sources like WikiData) and I wanted to experiment with partially automating this process. I have experimented with versions of KGN written in Java, Hy language ([Lisp running on Python that I wrote a short book on](https://leanpub.com/hy-lisp-python/read)), Swift, and Common Lisp and all four implementations take different approaches as I experimented with different ideas.
