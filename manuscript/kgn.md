# Knowledge Graph Navigator {#kgn}

The Knowledge Graph Navigator (which I will often refer to as KGN) is a tool for processing a set of entity names and automatically exploring the public Knowledge Graph [DBPedia](http://dbpedia.org) using SPARQL queries. I started to write KGN for my own use to automate some things I used to do manually when exploring Knowledge Graphs, and later thought that KGN might be also useful for educational purposes. KGN shows the user the auto-generated SPARQL queries so hopefully the user will learn by seeing examples. KGN uses the SPARQL queries. I cover SPARQL and linked data/knowledge Graphs is previous books I have written and while I give you a brief background here, I ask interested users to look at either for more details:

- The chapter **Knowledge Graph Navigator** in my book **Loving Common Lisp, or the Savvy Programmer’s Secret Weapon**
- The chapters **Background Material for the Semantic Web and Knowledge Graphs**, **Knowledge Graph Navigator** in my book **Practical Artificial Intelligence Programming With Clojure**

We use the Natural Language Processing (NLP) library from the last chapter to find human and place names in input text and  then construct SPARQL queries to access data from DBPedia.

I have implemented parts of KGN in several languages: Common Lisp, Java, Clojure, Racket Scheme, Swift, Python, and Hy. The most full featured version of KGN, including a full user interface, is featured in my book [Loving Common Lisp, or the Savvy Programmer's Secret Weapon](https://leanpub.com/lovinglisp) that you can read free online. That version performs more speculative SPARQL queries to find information compared to the example here that I designed for ease of understanding, and modification. I am not covering the basics of RDF data and SPARQL queries here. While I provide sufficient background material to understand the code, please read the relevant chapters in my Common Lisp book for more background material.

We will be running an example using data containing three person entities, one company entity, and one place entity. The following figure shows a very small part of the DBPedia Knowledge Graph that is centered around these entities. The data for this figure was collected by an example Knowledge Graph Creator from my Common Lisp book:

![File dbpedia_sample.nt loaded into the free version of GraphDB](images/graphdb.jpg)

I chose to use DBPedia instead of WikiData for this example because DBPedia URIs are human readable. The following URIs represent the concept of a *person*. The semantic meanings of DBPedia and FOAF (friend of a friend) URIs are self-evident to a human reader while the WikiData URI is not:

{linenos=off}
~~~~~~~~
http://www.wikidata.org/entity/Q215627
http://dbpedia.org/ontology/Person
http://xmlns.com/foaf/0.1/name
~~~~~~~~

I frequently use WikiData in my work and WikiData is one of the most useful public knowledge bases. I have both DBPedia and WikiData SPARQL endpoints in the example code that we will look at later, with the WikiData endpoint comment out. You can try manually querying WikiData at the [WikiData SPARQL endpoint](https://query.wikidata.org). For example, you might explore the WikiData URI for the *person* concept using:

{lang=sparql, linenos=off}
~~~~~~~~
select ?p ?o where {
 <http://www.wikidata.org/entity/Q215627> ?p ?o .
} limit 10
~~~~~~~~

For the rest of this chapter we will just use DBPedia or data copied from DBPedia.

After looking at an interactive session using the example program for this chapter we will look at the implementation.

## Entity Types Handled by KGN

To keep this example simple we handle just three entity types:

- People
- Organizations
- Places
 
In addition to finding detailed information for people, organizations, and places we will also search for relationships between entities. This search process consists of generating a series of SPARQL queries and calling the DBPedia SPARQL endpoint.


TBD


## KGN Implementation

The example application works processing a list or Person, Place, and Organization names. We generate SPARQL queries to DBPedia to find information about the entities and relationships between them.

We are using two libraries developed for this book that can be found in the directories **Racket-AI-book-code/sparql** and **Racket-AI-book-code/nlp** to supply support for SPARQL queries and natural language processing.

### SPARQL Client Library

TBD

### NLP Library

TBD

### Implementation of KGN Application Code

TBD

## Knowledge Graph Navigator Wrap Up

This KGN example was hopefully both interesting to you and simple enough in its implementation to use as a jumping off point for your own projects. 

I had the idea for the KGN application because I was spending quite a bit of time manually setting up SPARQL queries for DBPedia (and other public sources like WikiData) and I wanted to experiment with partially automating this process. I have experimented with versions of KGN written in Java, Hy language ([Lisp running on Python that I wrote a short book on](https://leanpub.com/hy-lisp-python/read)), Swift, and Common Lisp and all four implementations take different approaches as I experimented with different ideas.
