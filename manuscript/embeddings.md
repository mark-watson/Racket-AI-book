# Retrieval Augmented Generation of Text Using Embeddings

Retrieval-Augmented Generation (RAG) is a framework that combines the strengths of pre-trained language models (LLMs) with retrievers. Retrievers are system components for accessing knowledge from external sources of text data. In RAG a retriever selects relevant documents or passages from a corpus, and a generator produces a response based on both the retrieved information and the input query. The process typically follows these steps that we will use in the example Racket code:

- **Query Encoding**: The input query is encoded into a vector representation.
- **Document Retrieval**: A retriever system uses the query representation to fetch relevant documents or passages from an external corpus.
- **Document Encoding**: The retrieved documents are encoded into vector representations.
- **Joint Encoding**: The query and document representations are combined, often concatenated or mixed via attention mechanisms.
- **Generation**: A generator, usually LLM, is used to produce a response based on the joint representation.

RAG enables the LLM to access and leverage external text data sources, which is crucial for tasks that require information beyond what the LLM has been trained on. It's a blend of retrieval-based and generation-based approaches, aimed at boosting the factual accuracy and informativeness of generated responses. 

## Example Implementation

In the following short Racket example program (file **Racket-AI-book-code/embeddingsdb/embeddingsdb.rkt**) I implement some ideas of a RAG architecture. At file load time the text files in the subdirectory **data** are read, split into "chunks", and each chunk along with its parent file name and OpenAI text embedding is stored in a local SQLite database. When a user enters a query, the OpenAI embedding is calculated, and this embedding is matched against the embeddings of all chunks using the dot product of two 1536 element embedding vectors. The "best" chunks are concatenated together and this "context" text is passed to GPT-4 along with the user's original query. Here I describe the code in more detail:

The provided Racket code uses a local SQLite database and OpenAI's APIs for calculating text embeddings and for text completions.

**Utility Functions:**

- `floats->string` and `string->floats` are utility functions for converting between a list of floats and its string representation.
- `read-file` reads a file’s content.
- `join-strings` joins a list of strings with a specified separator.
- `truncate-string` truncates a string to a specified length.
- `interleave` merges two lists by interleaving their elements.
- `break-into-chunks` breaks a text into chunks of a specified size.
- `string-to-list` and `decode-row` are utility functions for parsing and processing database rows.

**Database Setup:**

- Database connection is established to "test.db" and a table named "documents" is created with columns for document_path, content, and embedding.

**Document Management:**

- `insert-document` inserts a document and its associated information into the database.
- `get-document-by-document-path` and `all-documents` are utility functions for querying documents from the database.
- `create-document` reads a document from a file path, breaks it into chunks, computes embeddings for each chunk via a function `embeddings-openai`, and inserts these into the database.

**Semantic Matching and Interaction:**

- `execute-to-list` and `dot-product` are utility functions for database queries and vector operations.
- `semantic-match` performs a semantic search by calculating the dot product of embeddings of the query and documents in the database. It then aggregates contexts of documents with a similarity score above a certain threshold, and sends a new query constructed with these contexts to OpenAI for further processing.
- `QA` is a wrapper around `semantic-match` for querying.
- `CHAT` initiates a loop for user interaction where each user input is processed through `semantic-match` to generate a response, maintaining a context of the previous chat.

**Test Code:**

- `test` function creates documents by reading from specified file paths, and performs some queries using the `QA` function.

The code uses a local SQLite database to store and manage document embeddings and the OpenAI API for generating embeddings and performing semantic searches based on user queries. Two functions are exported incase you want to use this example as a library: **create-document** and **QA**.

```racket
#lang racket

(require db)
(require llmapis)

(provide create-document QA)

; Function to convert list of floats to string representation
(define (floats->string floats)
  (string-join (map number->string floats) " "))

; Function to convert string representation back to list of floats
(define (string->floats str)
  (map string->number (string-split str)))

(define (read-file infile)
  (with-input-from-file infile
    (lambda ()
      (let ((contents (read)))
        contents))))

(define (join-strings separator list)
  (string-join list separator))

(define (truncate-string string length)
  (substring string 0 (min length (string-length string))))

(define (interleave list1 list2)
  (if (or (null? list1) (null? list2))
      (append list1 list2)
      (cons (car list1)
            (cons (car list2)
                  (interleave (cdr list1) (cdr list2))))))

(define (break-into-chunks text chunk-size)
  (let loop ((start 0) (chunks '()))
    (if (>= start (string-length text))
        (reverse chunks)
        (loop (+ start chunk-size)
              (cons (substring text start (min (+ start chunk-size) (string-length text))) chunks)))))

(define (string-to-list str)
  (map string->number (string-split str)))

(define (decode-row row)
  (let ((id (vector-ref row 0))
        (context (vector-ref row 1))
        (embedding (string-to-list (read-line (open-input-string (vector-ref row 2))))))
    (list id context embedding)))

(define
  db
  (sqlite3-connect #:database "test.db" #:mode 'create
                   #:use-place #t))

(with-handlers ([exn:fail? (lambda (ex) (void))])
  (query-exec
   db
   "CREATE TABLE documents (document_path TEXT, content TEXT, embedding TEXT);"))

(define (insert-document document-path content embedding)
  (printf "~%insert-document:~%  content:~a~%~%" content)
  (query-exec
   db
   "INSERT INTO documents (document_path, content, embedding) VALUES (?, ?, ?);"
   document-path content (floats->string embedding)))

(define (get-document-by-document-path document-path)
  (map decode-row
       (query-rows
         db
         "SELECT * FROM documents WHERE document_path = ?;"
         document-path)))

(define (all-documents)
  (map
   decode-row
   (query-rows
    db
    "SELECT * FROM documents;")))
   
(define (create-document fpath)
  (let ((contents (break-into-chunks (file->string fpath) 200)))
    (for-each
     (lambda (content)
       (with-handlers ([exn:fail? (lambda (ex) (void))])
         (let ((embedding (embeddings-openai content)))
           (insert-document fpath content embedding))))
     contents)))

;; Assuming a function to fetch documents from database
(define (execute-to-list db query)
  (query-rows db query))

;; dot product of two lists of floating point numbers:
(define (dot-product a b) 
  (cond
    [(or (null? a) (null? b)) 0]
    [else
     (+ (* (car a) (car b))
        (dot-product (cdr a) (cdr b)))]))

(define (semantic-match query custom-context [cutoff 0.7])
  (let ((emb (embeddings-openai query))
        (ret '()))
    (for-each
     (lambda (doc)
       (let* ((context (second doc))
              (embedding (third doc))
              (score (dot-product emb embedding)))
         (when (> score cutoff)
           (set! ret (cons context ret)))))
     (all-documents))
    (printf "~%semantic-search: ret=~a~%" ret)
    (let* ((context (string-join (reverse ret) " . "))
           (query-with-context (string-join (list context custom-context "Question:" query) " ")))
      (question-openai query-with-context))))

(define (QA query [quiet #f])
  (let ((answer (semantic-match query "")))
    (unless quiet
      (printf "~%~%** query: ~a~%** answer: ~a~%~%" query answer))
    answer))

(define (CHAT)
  (let ((messages '(""))
        (responses '("")))
    (let loop ()
      (printf "~%Enter chat (STOP or empty line to stop) >> ")
      (let ((string (read-line)))
        (cond
         ((or (string=? string "STOP") (< (string-length string) 1))
          (list (reverse messages) (reverse responses)))
         (else
          (let* ((custom-context
                  (string-append
                   "PREVIOUS CHAT: "
                   (string-join (reverse messages) " ")))
                 (response (semantic-match string custom-context)))
            (set! messages (cons string messages))
            (set! responses (cons response responses))
            (printf "~%Response: ~a~%" response)
            (loop))))))))

;; ... test code ...

(define (test)
  "Test Semantic Document Search Using GPT APIs and local vector database"
  (create-document
    "/Users/markw/GITHUB/Racket-AI-book-code/embeddingsdb/data/sports.txt")
  (create-document
    "/Users/markw/GITHUB/Racket-AI-book-code/embeddingsdb/data/chemistry.txt")
  (QA "What is the history of the science of chemistry?")
  (QA "What are the advantages of engaging in sports?"))
```

Let's look at a few examples form a Racket REPL:

```
> (QA "What is the history of the science of chemistry?")
** query: What is the history of the science of chemistry?
** answer: The history of the science of chemistry dates back thousands of years. Ancient civilizations such as the Egyptians, Greeks, and Chinese were experimenting with various substances and observing chemical reactions even before the term "chemistry" was coined.

The foundations of modern chemistry can be traced back to the works of famous scholars such as alchemists in the Middle Ages. Alchemists sought to transform common metals into gold and discover elixirs of eternal life. Although their practices were often based on mysticism and folklore, it laid the groundwork for the understanding of chemical processes and experimentation.

In the 17th and 18th centuries, significant advancements were made in the field of chemistry. Prominent figures like Robert Boyle and Antoine Lavoisier began to understand the fundamental principles of chemical reactions and the concept of elements. Lavoisier is often referred to as the "father of modern chemistry" for his work in establishing the law of conservation of mass and naming and categorizing elements.

Throughout the 19th and 20th centuries, chemistry continued to progress rapidly. The development of the periodic table by Dmitri Mendeleev in 1869 revolutionized the organization of elements. The discovery of new elements, the formulation of atomic theory, and the understanding of chemical bonding further expanded our knowledge.

Chemistry also played a crucial role in various industries and technologies, such as the development of synthetic dyes, pharmaceuticals, plastics, and materials. The emergence of quantum mechanics and spectroscopy in the early 20th century opened up new avenues for understanding the behavior of atoms and molecules.

Today, chemistry is an interdisciplinary science that encompasses various fields such as organic chemistry, inorganic chemistry, physical chemistry, analytical chemistry, and biochemistry. It continues to evolve and make significant contributions to society, from developing sustainable materials to understanding biological processes and addressing global challenges such as climate change.

In summary, the history of the science of chemistry spans centuries, starting from ancient civilizations to the present day, with numerous discoveries and advancements shaping our understanding of the composition, properties, and transformations of matter.
```

This output is the combination of data found in the text files in the directory **Racket-AI-book-code/embeddingsdb/data** and the data that OpenAI GPT-4 was trained on. Since the local "document" file **chemistry.txt** is very short, most of this output is derived from the innate knowledge GPT-4 has from its training data.

In order to show that this example is also using data in the local "document" text files, I manually edited the file **data/chemistry.txt** adding the following made-up organic compound:

    ZorroOnian Alcohol is another organic compound with the formula C 6 H 10 O.
    
GPT-4 was never trained on my made-up data so it has no idea what the non-existent compound ZorroOnian Alcohol is. The following answer is retrieved via RAG from the local document data (for brevity, most of the output for adding the local document files to the embedding index is not shown):

```
> (create-document
   "/Users/markw/GITHUB/Racket-AI-book-code/embeddingsdb/data/chemistry.txt")

insert-document:
  content:Amyl alcohol is an organic compound with the formula C 5 H 12 O. ZorroOnian Alcohol is another organic compound with the formula C 6 H 10 O. All eight isomers of amyl alcohol are known.

  ...

> (QA "what is the formula for ZorroOnian Alcohol")

** query: what is the formula for ZorroOnian Alcohol
** answer: The formula for ZorroOnian Alcohol is C6H10O.
```


There is also a chat interface:

```
Enter chat (STOP or empty line to stop) >> who is the chemist Robert Boyle

Response: Robert Boyle was an Irish chemist and physicist who is known as one of the pioneers of modern chemistry. He is famous for Boyle's Law, which describes the inverse relationship between the pressure and volume of a gas, and for his experiments on the properties of gases. He lived from 1627 to 1691.

Enter chat (STOP or empty line to stop) >> Where was he born?

Response: Robert Boyle was born in Lismore Castle, County Waterford, Ireland.

Enter chat (STOP or empty line to stop) >> 
```

## Retrieval Augmented Generation Wrap Up

Retrieval Augmented Generation (RAG) is one of the best use cases for semantic search. Another way to write RAG applications is to use a web search API to get context text for a query, and add this context data to whatever context data you have in a local embeddings data store.

