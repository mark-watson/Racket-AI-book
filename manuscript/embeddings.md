# Retrieval Augmented Generation of Text Using Embeddings

Retrieval-Augmented Generation (RAG) is a framework that combines the strengths of pre-trained language models (LLMs) with retrievers. Retrievers are system components for accessing knowledge from external sources of text data. In RAG a retriever selects relevant documents or passages from a corpus, and a generator produces a response based on both the retrieved information and the input query. The process typically follows these steps that we will use in the example Racket code:

- **Query Encoding**: The input query is encoded into a vector representation.
- **Document Retrieval**: A retriever system uses the query representation to fetch relevant documents or passages from an external corpus.
- **Document Encoding**: The retrieved documents are encoded into vector representations.
- **Joint Encoding**: The query and document representations are combined, often concatenated or mixed via attention mechanisms.
- **Generation**: A generator, usually LLM, is used to produce a response based on the joint representation.

RAG enables the LLM to access and leverage external text data sources, which is crucial for tasks that require information beyond what the LLM has been trained on. It's a blend of retrieval-based and generation-based approaches, aimed at boosting the factual accuracy and informativeness of generated responses. 

## Example Implementation

In the following short Racket example program (file **Racket-AI-book/source-code/embeddingsdb
/embeddingsdb.rkt**) I implement some ideas of a RAG architecture. At file load time the text files in the subdirectory **data** are read, split into "chunks", and each chunk along with its parent file name and OpenAI text embedding is stored in a local SQLite database. When a user enters a query, the OpenAI embedding is calculated, and this embedding is matched against the embeddings of all chunks using the dot product of two 1536 element embedding vectors. The "best" chunks are concatenated together and this "context" text is passed to GPT-4 along with the user's original query. Here I describe the code in more detail:

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

The code uses a local SQLite database to store and manage document embeddings and the OpenAI API for generating embeddings and performing semantic searches based on user queries. Two functions are exported in case you want to use this example as a library: **create-document** and **QA**.

```racket
#lang racket

(require db)
(require llmapis)
(require racket/runtime-path)

(provide create-document QA CHAT semantic-match)

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
        (embedding (string-to-list (vector-ref row 2))))
    (list id context embedding)))

(define db (sqlite3-connect #:database "test.db" #:mode 'create #:use-place #t))

(with-handlers ([exn:fail? (lambda (ex) (void))])
  (query-exec
   db
   "CREATE TABLE documents (document_path TEXT, content TEXT, embedding TEXT);"))
      
;; ... database setup, error handling, and queries ...

(define (insert-document document-path content embedding)
  (printf "~%insert-document:~%  content:~a~%~%" content)
  (query-exec
   db
   "INSERT INTO documents (document_path, content, embedding) VALUES (?, ?, ?);"
   document-path content (floats->string embedding)))

(define (get-document-by-document-path document-path)
  (map decode-row
       (query-rows db
                    "SELECT * FROM documents WHERE document_path = ?;"
                    document-path)))

(define (all-documents)
  (map
   decode-row
   (query-rows
    db
    "SELECT * FROM documents;")))
   
;; ... remaining database query functions ...

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

(define (dot-product a b) ;; dot product of two lists of floating point numbers
  (for/sum ([x a] [y b])
    (* x y)))


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

(define-runtime-path data-dir "data")

(define (test)
  "Test code for Semantic Document Search Using OpenAI GPT APIs and local vector database"
  (create-document (path->string (simplify-path (build-path data-dir "sports.txt"))))
  (create-document (path->string (simplify-path (build-path data-dir "chemistry.txt"))))
  (QA "What is the history of the science of chemistry?")
  (QA "What are the advantages of engaging in sports?"))

(module+ main
  ;; Uncomment below if you want to execute tests when running this module
  ;; (test)
  )



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

This output is the combination of data found in the text files in the directory **Racket-AI-book/source-code/embeddingsdb/data** and the data that OpenAI GPT-4 was trained on. Since the local "document" file **chemistry.txt** is very short, most of this output is derived from the innate knowledge GPT-4 has from its training data.

In order to show that this example is also using data in the local "document" text files, I manually edited the file **data/chemistry.txt** adding the following made-up organic compound:

    ZorroOnian Alcohol is another organic compound with the formula C 6 H 10 O.
    
GPT-4 was never trained on my made-up data so it has no idea what the non-existent compound ZorroOnian Alcohol is. The following answer is retrieved via RAG from the local document data (for brevity, most of the output for adding the local document files to the embedding index is not shown):

```
> (create-document
   "/Users/markw/GITHUB/Racket-AI-book/source-code/embeddingsdb/data/chemistry.txt")

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

Notice how the second question, "Where was he born?", contains no name at all. The CHAT loop prepends the previous turns as custom context ("PREVIOUS CHAT: ..."), so the model sees the word "he" against the earlier mention of Robert Boyle. This is the whole trick behind chat-over-documents systems: the retrieval machinery stays the same, and conversation history is just more context.

## What an Embedding Actually Is

The code above treats embeddings as opaque lists of 1536 floats. It is worth building an intuition for what those numbers are, because every design decision in a RAG system follows from it.

A text embedding model is a neural network trained so that *distance* in its output space corresponds to *meaning*. Two sentences about the same topic get vectors that point in nearly the same direction, even if they share no words ("My dog ate my homework" and "The puppy chewed up the assignment"). Two sentences that share many words but are unrelated ("The bank of the river" and "The bank approved the loan") get vectors that point far apart. The vector's direction encodes what the text is about; its length is typically normalized to 1.

Concretely, think of each of the 1536 dimensions as a soft answer to a learned question the model found useful during training: some dimensions fire for sports text, some for chemistry, some for grammar structure, and the rest have no human-readable interpretation at all. We never pick the questions; training does.

That geometric view explains two things.

First, why we compare vectors at all: if the model pushes related texts to nearby points in space, then finding the chunks closest to the query's point *is* finding the most relevant chunks, with no keyword matching anywhere in the loop. This is why a query "the formula for ZorroOnian Alcohol" can find a chunk containing the words "formula" and "ZorroOnian" even though no synonym list connects them.

Second, why the raw `dot-product` in `embeddingsdb.rkt` is subtly fragile. For two vectors `a` and `b` with angle `\theta`$ between them:

```$
a \cdot b = \|a\| \, \|b\| \cos\theta
```

The dot product rewards long vectors as much as aligned directions. If your embedding model does not promise unit-length vectors, a long but tangentially related chunk can outscore a short, perfectly on-topic one. `text-embedding-ada-002` returns unit vectors, so for that model `\|a\| = \|b\| = 1`$ and the dot product *equals* cosine similarity. But that is a property of one model, not of embeddings in general. As soon as you swap in a local embedding model, use cosine similarity:

```$
\mathrm{cosine}(a, b) = \frac{a \cdot b}{\|a\| \, \|b\|}
```

The new file **rag_extensions.rkt** in the **embeddingsdb** directory implements this and the other upgrades in this chapter, all runnable without an API key. First, the similarity functions:

```racket
(define (magnitude v)
  (sqrt (for/sum ([x v]) (* x x))))

(define (cosine-similarity a b)
  (let ([ma (magnitude a)]
        [mb (magnitude b)])
    (if (or (zero? ma) (zero? mb))
        0.0
        (/ (for/sum ([x a] [y b]) (* x y))
           (* ma mb)))))
```

The zero check matters more than it looks: a chunk of text that produces an all-zero vector (it happens, for example with some models on empty or whitespace-only input) would otherwise crash the division in the middle of an indexing run. Return 0.0 instead, and that chunk simply never matches anything.

## Chunking: the Most Underrated Part of RAG

Look again at how `create-document` in **embeddingsdb.rkt** prepares text for the index: `break-into-chunks` splits the file every 200 characters. Here is what that does to a normal paragraph, compared with sentence-aware chunking. The output below is real, produced by the code in this section:

```
old break-into-chunks style, every 60 chars:
  [Robert Boyle was born in Ireland in 1627. He studied the beh]
  [avior of gases under pressure. His law states that pressure ]
  [and volume are inversely related. He also wrote The Sceptica]
  [l Chymist, a founding text of modern chemistry. He died in L]
  [ondon in 1691.]
```

Every chunk except the first starts mid-word and mid-thought. Each chunk gets embedded separately, so the vector for `"avior of gases under pressure. His law states that pressure "` describes a sentence that nobody ever wrote. Retrieval still half-works, because the surrounding words provide signal, but you have handed the embedding model garbage on every boundary.

The chunker in **rag_extensions.rkt** splits on sentence boundaries, packs whole sentences into chunks up to a target size, and then prepends the tail of the previous chunk to each new one. Overlap exists because an answer can sit right at a boundary: if the sentence "His law states that pressure and volume are inversely related" got split from the sentence that introduced "he" as Boyle, neither chunk alone answers "who was Robert Boyle." The overlap carries that context across:

```
chunked (60 chars, 25 overlap):
  [Robert Boyle was born in Ireland in 1627.]
  [born in Ireland in 1627. He studied the behavior of gases under pressure.]
  [of gases under pressure. His law states that pressure and volume are inversely related.]
  [me are inversely related. He also wrote The Sceptical Chymist, a founding text of modern chemistry.]
  [text of modern chemistry. He died in London in 1691.]
```

Chunk 2 repeats the tail of chunk 1, so the query "where was Boyle born" and the query "what did he study" each find a chunk containing both the setup and the payoff. The overlap is cheap: it duplicates a few dozen characters of storage in exchange for covering the boundary cases.

Here is the code:

```racket
(define sentence-end (pregexp "[.!?]+[\"'\\)]*\\s+"))

(define (split-sentences text)
  (let loop ([rest (string-trim text)] [acc '()])
    (if (zero? (string-length rest))
        (reverse acc)
        (let ([m (regexp-match-positions sentence-end rest)])
          (if (not m)
              (reverse (cons rest acc))
              (let* ([end (cdar m)]
                     [sentence (string-trim (substring rest 0 end))])
                (loop (string-trim (substring rest end))
                      (cons sentence acc))))))))

(define (chunk-by-sentences text
                            #:chunk-size [chunk-size 500]
                            #:overlap [overlap 40])
  (define sentences (split-sentences text))
  (define (with-overlap chunk prev-chunk)
    (if (and prev-chunk (> overlap 0))
        (let ([tail (substring prev-chunk
                               (max 0 (- (string-length prev-chunk) overlap)))])
          (string-trim (string-append tail " " chunk)))
        chunk))
  ;; First pass: pack whole sentences into raw chunks of at most
  ;; CHUNK-SIZE characters (a sentence longer than CHUNK-SIZE stands alone).
  (define raw-chunks
    (let loop ([todo sentences] [current ""] [chunks '()])
      (cond
        [(null? todo)
         (if (zero? (string-length current))
             (reverse chunks)
             (reverse (cons current chunks)))]
        [else
         (define sentence (car todo))
         (define candidate
           (if (zero? (string-length current))
               sentence
               (string-append current " " sentence)))
         (cond
           [(not (> (string-length candidate) chunk-size))
            (loop (cdr todo) candidate chunks)]
           [(zero? (string-length current))
            ;; Single oversized sentence gets its own chunk.
            (loop (cdr todo) "" (cons sentence chunks))]
           [else
            (loop todo "" (cons current chunks))])])))
  ;; Second pass: prepend the trailing OVERLAP characters of each chunk to
  ;; the next, so context survives chunk boundaries.
  (if (null? raw-chunks)
      '()
      (cons (car raw-chunks)
            (for/list ([prev raw-chunks] [cur (cdr raw-chunks)])
              (with-overlap cur prev)))))
```

The structure is deliberately simple: one pass packs sentences, a second pass adds overlap. The edge case to handle is a single sentence longer than the target chunk size, which gets its own chunk rather than looping forever. (A production system would split such a sentence at a clause boundary or fall back to character splitting; for this chapter, keeping it whole is honest.)

How do you pick `chunk-size` and `overlap`? Smaller chunks retrieve more precisely (each vector is about one thing) but carry less context; larger chunks are more forgiving but blur several topics into one vector. Around 300 to 1000 characters with a 10 to 20 percent overlap is a good default for prose. The only real rule: treat chunking as an experiment you rerun against your own documents, not a constant you set once.

## Retrieval, Without Thresholds

`semantic-match` filters with a hard similarity cutoff of 0.7. Any chunk scoring above it goes into the prompt, everything else vanishes. That means one bad day can return zero context (and the LLM answers from training alone), while a query where twenty chunks all score 0.71 drowns the model in context and blows the token budget.

Ranking instead of thresholding fixes both: always take the *top k* chunks, whatever their scores. The generalization in **rag_extensions.rkt** also lets the caller swap in any embedding function with the `#:embed` keyword, so the same code ranks with OpenAI embeddings in production and with the deterministic local embedder (below) in tests:

```racket
(define (rank-chunks query chunks
                     #:embed [embed hash-embed]
                     #:top-k [top-k (length chunks)])
  "Rank CHUNKS (list of strings) against QUERY by embedding similarity.
   Returns a list of (score . chunk) pairs, best first, at most TOP-K."
  (define q-emb (embed query))
  (define scored
    (for/list ([chunk chunks])
      (cons (cosine-similarity q-emb (embed chunk)) chunk)))
  (take (sort scored > #:key car) (min top-k (length scored))))

(define (assemble-prompt contexts custom-context query)
  "Build the exact string sent to the LLM: retrieved context, any extra
   context the caller supplies, then the question."
  (string-join (list (string-join contexts " . ")
                     custom-context
                     "Question:" query)
               " "))
```

### Seeing the Whole Pipeline Offline

The problem with developing a RAG system is that every experiment costs API calls. The last piece of **rag_extensions.rkt** is a tiny deterministic embedder that lets you run and test the entire pipeline (chunk, embed, rank, assemble) offline. It is a stand-in with the same *shape* as a real embedder: text in, unit-length vector of floats out. It hashes each word into one of 256 buckets and normalizes, so texts that share words score well together. Never use it in production; do use it to test your plumbing:

```racket
(define vocab-dim 256)

(define (tokenize text)
  (regexp-split #px"[^a-z0-9]+" (string-downcase text)))

(define (hash-embed text)
  "Deterministic unit-length embedding of TEXT as a list of VOCAB-DIM floats."
  (define v (make-vector vocab-dim 0.0))
  (for ([tok (tokenize text)])
    (when (> (string-length tok) 0)
      (define h (modulo (equal-hash-code tok) vocab-dim))
      (vector-set! v h (+ 1.0 (vector-ref v h)))))
  (define m (magnitude (vector->list v)))
  (if (zero? m)
      (vector->list v)
      (map (lambda (x) (/ x m)) (vector->list v))))
```

The demo at the bottom of the file runs three queries against four miniature documents with a `top-k` of 2. Real output:

```
$ racket rag_extensions.rkt

== Retrieval over 4 tiny documents (hash embedder) ==

Query: what is the formula for ZorroOnian Alcohol?
  score 0.428  Amyl alcohol is an organic compound with the formula C 5 H 12 O. Zorro...
  score 0.3904  Robert Boyle is known as one of the pioneers of modern chemistry. He i...

Query: who is Robert Boyle?
  score 0.4743  Robert Boyle is known as one of the pioneers of modern chemistry. He i...
  score 0.1849  Amyl alcohol is an organic compound with the formula C 5 H 12 O. Zorro...

Query: tell me about team sports and exercise
  score 0.3571  Playing sports improves cardiovascular health, builds muscle, and teac...
  score 0.0845  Dmitri Mendeleev published the periodic table in 1869, organizing the ...

== Assembled RAG prompt for the top match ==

Robert Boyle is known as one of the pioneers of modern chemistry. He is famous for Boyle's Law, which describes the inverse relationship between the pressure and volume of a gas.  Question: who is Robert Boyle?
```

Each query's best match lands on the right document, and the scores show the useful failure mode too: the sports query's runner-up scores 0.0845, far below the winner. The score gap between first and second place is itself signal worth watching when you tune a RAG system. A query whose top two scores are close and low usually means the answer is not in your documents at all.

The last block shows the honest output of `assemble-prompt`: retrieved context, a blank custom context, then the question. The word "Question:" is not decoration. It gives the LLM a stable separator between evidence to read and the task to do, and prompts that keep that structure consistent across every call get more consistent answers.

## Testing the Pipeline

**tests.rkt** in the same directory runs the whole stack with rackunit, still offline. A few of the properties it pins down:

```racket
(test-case "cosine never throws on a zero vector"
  (check-equal? (cosine-similarity '(0 0 0) '(1 2 3)) 0.0))

(test-case "chunk-by-sentences keeps whole sentences"
  (define text
    "Alpha beta gamma delta. Epsilon zeta eta theta. Iota kappa lambda mu.")
  (define chunks (chunk-by-sentences text #:chunk-size 30 #:overlap 0))
  (check-true (> (length chunks) 1))
  ;; no chunk ends mid-word
  (for ([c chunks])
    (check-true (regexp-match? #px"[.!?]\\s*$" (string-trim c)))))

(test-case "hash-embed distinguishes related and unrelated text"
  (define chem (hash-embed "chemistry atoms molecules elements"))
  (define chem2 (hash-embed "the chemistry of molecules and atoms"))
  (define sports (hash-embed "sports players teams and goals"))
  (check-true (> (cosine-similarity chem chem2)
                 (cosine-similarity chem sports))))

(test-case "rank-chunks orders best first and honors top-k"
  (define test-docs
    '("The periodic table organizes elements by atomic weight."
      "Boyle's Law relates gas pressure and volume."
      "Sports improve cardiovascular health."))
  (define ranked (rank-chunks "gas pressure law" test-docs #:top-k 2))
  (check-equal? (length ranked) 2)
  (check-true (> (car (first ranked)) (car (second ranked))))
  (check-true (string-contains? (cdr (first ranked)) "Boyle")))
```

```text
$ raco test tests.rkt
raco test: "tests.rkt"

All tests passed.
11 tests passed
```

## Production Notes

A few hard-won notes if you carry this design further:

- **Embedding calls dominate indexing cost.** `create-document` calls the embedding API once per chunk, and re-running `test` re-inserts every document. Cache by file content (a hash) and skip unchanged files.
- **The vector column is a TEXT string.** `floats->string` stores 1536 numbers as space-separated text, and every query parses all of them back with `string->floats`. For a few thousand chunks that is fine. Beyond that, keep vectors as blobs or move to a database with a vector index.
- **Linear scans do not scale.** Every query walks every row. Approximate nearest neighbor indexes (HNSW is the usual choice in SQLite-adjacent tooling like sqlite-vec) turn the scan into a near-constant lookup.
- **Duplicates pollute prompts.** If two documents repeat the same paragraph, both copies can rank in the top k and waste context. Deduplicate by chunk text before indexing, and drop near-duplicate retrieved chunks before assembling the prompt.

The following diagram shows the high-level architecture of the RAG pipeline developed in this chapter:

{width: "100%"}
![Architecture diagram](images/embeddings_architecture.jpg)

## Retrieval Augmented Generation Wrap Up

Retrieval Augmented Generation (RAG) is one of the best use cases for semantic search. Another way to write RAG applications is to use a web search API to get context text for a query, and add this context data to whatever context data you have in a local embeddings data store.

## Optional Practice Problems

1. **Wire the Upgrades In**: Modify `create-document` in **embeddingsdb.rkt** to use `chunk-by-sentences` instead of `break-into-chunks`, and `semantic-match` to use `cosine-similarity` and top-k ranking instead of `dot-product` with a cutoff. Run the chemistry and sports queries before and after and compare answers.
2. **Measure Chunking**: Write a utility that reports, for a given document and chunker settings, the number of chunks, their min/max/mean lengths, and how many chunks end mid-word. Use it to compare the old and new chunkers on **data/chemistry.txt**.
3. **Hybrid Retrieval**: Some questions are pure keyword ("what is the formula for ZorroOnian Alcohol") and some are conceptual ("why is teamwork valuable"). Add a keyword score (a simple word-overlap count) alongside the embedding score, and combine them with a weight. Show a query where hybrid beats embeddings alone, and one where it does not.
4. **Extend Database Operations**: Add a function `delete-document` that deletes all chunks and vector representations associated with a given file path from the SQLite database, and a test that proves re-adding a modified file does not leave stale chunks behind.
5. **Score-Gap Detection**: Add a check to `rank-chunks` callers that detects "no good match": top score below a floor *and* a small gap between first and second place. When detected, have the prompt tell the LLM explicitly that the local documents may not contain the answer, and observe how its answers change.


