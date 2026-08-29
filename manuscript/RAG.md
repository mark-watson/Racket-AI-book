# Agentic RAG Using the Gemini API

Standard RAG systems, like the one we built in the earlier chapter on embeddings, do one thing: embed a question, retrieve a few text chunks, and hand them to an LLM to write an answer. That is fine for simple questions, but it falls apart when a question needs facts from several documents, or when the first retrieval misses part of the answer. In this chapter we build an **agentic RAG** system in Racket where four specialized agents cooperate: a query rewriter, a search fanout, a sufficiency checker, and an answer synthesizer. The design follows Google's research on agentic RAG.

The key idea is a feedback loop. Instead of retrieving once and hoping for the best, the system asks "do I have enough context to answer this question?" If not, it rewrites the question, searches again, and only then writes the answer. We implement this loop with a configurable iteration bound so cost stays predictable.

## Architecture

The pipeline has four agents, each with a single responsibility:

```
User Query
    |
    v
+------------------+
|  Query Rewriter   |  Decomposes complex questions into
|  Agent            |  1-3 focused sub-queries
+---------+--------+
          |
          v
+------------------+
|  Search Fanout    |  Embeds each sub-query, searches
|  Agent            |  across multiple corpora
+---------+--------+
          |
          v
+------------------+     +-----------------+
| Sufficient Context|---->|  Refine Queries  |
| Agent             | NO  |  (iterate)       |
+---------+--------+     +---------+--------+
          | YES                  |
          v                      |
+------------------+             |
| Synthesis Agent   |<----------+
| (final answer)    |
+------------------+
```

The critical piece is the Sufficient Context Agent. After retrieval it scores the chunks against the original question and decides whether they cover every part of it. If they do, we synthesize; if not, it tells us what is missing, and a second rewriter produces follow-up queries. This is the difference between a search engine and a system that answers questions.

The code is in the directory `Racket-AI-book/source-code/RAG` and is split into four modules plus tests:

| File | Purpose |
|---|---|
| `embeddings.rkt` | Gemini embedding API, batching, caching, retry, vector math |
| `vector-store.rkt` | In-memory store: chunking, save/load, cosine similarity search |
| `agents.rkt` | The four agents and the orchestrating pipeline |
| `main.rkt` | Public API, the `test` demo, and an interactive REPL |
| `tests.rkt` | 15 offline unit tests (no network needed) |

Before we walk through the code, a note on configuration. The system uses one environment variable, `GOOGLE_API_KEY`, and nothing else. It calls `gemini-embedding-001` for embeddings and `gemini-3-flash-preview` for all LLM calls.

## Embeddings and Vector Math

The `embeddings.rkt` module handles all contact with the Gemini embedding API. Documents are split into chunks of about 500 characters with 50 characters of overlap, each chunk is embedded once, and the vectors are kept in a hash table so repeated runs do not re-embed text. The cache is bounded by `*embedding-cache-cap*` and clears itself when full; this is deliberately simple, and a production system would use a proper LRU cache.

The public entry points are `get-embedding` for one string and `get-embeddings` for a list. The list version is the one that matters for speed: all cache misses are sent in `batchEmbedContents` requests of at most 100 texts each, which is the API limit. This is why the demo below takes seconds instead of minutes.

```racket
(define (get-embeddings texts)
  (when (eq? (*embedding-fn*) %fetch-embedding)
    (define misses
      (remove-duplicates
       (filter (lambda (text)
                 (not (hash-has-key? (*embedding-cache*) (embedding-cache-key text))))
               texts)))
    (unless (null? misses)
      (for ([text misses]
            [vec (%fetch-embeddings-batch misses)])
        (%cache-put text
                    (if (vector? vec)
                        vec
                        (list->vector (map exact->inexact vec)))))))
  (map get-embedding texts))
```

Network calls are wrapped by `call-with-retries`, which retries HTTP 429 and 5xx errors with exponential backoff and fails fast on 4xx errors. This keeps a flaky network from throwing away a whole query.

The vector math is three small functions. `dot-product` checks that both vectors have the same length, because a length mismatch almost always means the embedding model was changed after the corpus was built, and silently truncating would corrupt every score. `vector-magnitude` computes the L2 norm, and `cosine-similarity` divides the dot product by the two magnitudes.

Chunk vectors are stored **normalized** (unit length), so scoring a query against every chunk is a dot product divided by the query norm. The query's own norm appears only once per search, in the denominator:

```racket
(define (score-chunks chunks query-embedding #:query-norm [query-norm 1.0])
  (map (lambda (chunk)
         (cons (/ (dot-product query-embedding (document-chunk-embedding chunk))
                  query-norm)
               chunk))
       chunks))
```

## The Vector Store

`vector-store.rkt` defines the data structures and the retrieval logic. Two structs hold state:

```racket
(struct document-chunk (text source embedding norm)
  #:methods gen:custom-write
  [(define write-proc print-document-chunk)])

(struct corpus (name description [chunks #:mutable])
  #:methods gen:custom-write
  [(define write-proc print-corpus)])
```

Both have custom printers. Printing a corpus shows its name, description, and chunk count; printing a chunk shows its text and source but only the first 10 floats of the embedding, followed by the dimension. Without this, printing an embedded corpus at the REPL would dump thousands of numbers.

Splitting text into chunks looks for a sentence end (period or newline) within 80 characters of the target boundary, and falls back to a hard cut if none is found. A forward-progress guard ensures that we can never loop forever on input whose only sentence break is at the start of the text:

```racket
(define (split-into-chunks text
                           #:chunk-size [chunk-size *default-chunk-size*]
                           #:overlap [overlap *chunk-overlap*])
  (define len (string-length text))
  (let loop ([start 0] [chunks '()])
    (if (>= start len)
        (reverse chunks)
        (let* ([end (min (+ start chunk-size) len)]
               [break-pos
                (if (>= end len)
                    end
                    (let* ([search-from (max start (- end 80))]
                           [window (substring text search-from end)]
                           [period-idx (last-index-of-char window #\.)]
                           [newline-idx (last-index-of-char window #\newline)]
                           [idx (max (or period-idx -1) (or newline-idx -1))])
                      (if (>= idx 0)
                          (+ search-from idx)
                          end)))]
               [actual-end* (if (< break-pos end) (+ break-pos 1) end)])
          (define actual-end
            (if (<= actual-end* start)
                (min (+ start chunk-size) len)
                actual-end*))
          ...
          (loop new-start new-chunks)))))
```

Corpora can be saved to disk and loaded back. `save-corpus` writes one s-expression with the name, description, and every chunk with its embedding as a plain list of numbers. `load-corpus` reads it back, validates that every chunk has non-empty text and source plus a numeric embedding, and re-normalizes the vectors so files written by older versions of the code still work.

Retrieval scores every chunk in a corpus against a query embedding and returns the top *k* as `(score . chunk)` pairs sorted by descending score. `search-corpora` does the same across several corpora, which is what makes cross-corpus questions possible.

## The Agents

The interesting logic is in `agents.rkt`. Every LLM call goes through `rag-generate`, which applies the retry wrapper and signals an error if the model returns nothing. The model is stored in the parameter `*rag-model*` and can be overridden per call with a keyword argument.

### Agent 1: Query Rewriter

The rewriter asks Gemini to break the user's question into 1 to 3 short search queries. The prompt is strict: one query per line, no numbering, no bullets, under 15 words each. The raw response often comes back with markdown formatting, so `parse-query-lines` strips leading `-`, `*`, `+`, and numbered prefixes like `4. ` while preserving digits that are part of the query text itself (`2024 lithium prices` stays intact; `4. fourth query` loses its prefix).

The original question is always appended to the rewritten list as a fallback, so the fanout always searches for exactly what the user asked.

```racket
(define (rewrite-queries user-query #:model [model (*rag-model*)])
  (define prompt
    (format (string-append
             "You are a search query rewriter for a RAG system. "
             "Your job is to break a complex user question into "
             "1-3 simple, focused search queries that will help "
             "retrieve relevant information from a document collection.\n"
             "\nRules:\n"
             "- Output ONLY the queries, one per line\n"
             "- No numbering, bullets, or extra text\n"
             "- Each query should target a specific fact or concept\n"
             "- Keep queries concise (under 15 words each)\n"
             "\nUser question: ~a")
            user-query))
  (define queries (parse-query-lines (rag-generate prompt #:model model)))
  (remove-duplicates (append queries (list user-query))))
```

### Agent 2: Search Fanout

The fanout embeds every sub-query in one batched API call, searches all corpora with each embedding, and deduplicates the results by `(source . text)`. Keeping the source in the key matters: the same text in a different file is a different chunk and must not be collapsed. Results are sorted by descending score before being returned.

```racket
(define (search-fanout corpora sub-queries #:top-k [top-k 3])
  (define query-embeddings (get-embeddings sub-queries))
  (define seen-keys (make-hash))
  (define all-results '())
  (for ([query sub-queries]
        [query-embedding query-embeddings])
    (for ([result (search-corpora corpora query-embedding #:top-k top-k)])
      (define key (document-chunk-key (cdr result)))
      (unless (hash-ref seen-keys key #f)
        (hash-set! seen-keys key #t)
        (set! all-results (cons result all-results)))))
  (sort all-results > #:key car))
```

### Agent 3: Sufficient Context Agent

This agent separates agentic RAG from vanilla RAG. The prompt gives the model the question and every retrieved passage, and asks it to answer in exactly three lines: `VERDICT`, `REASON`, and `MISSING`. The parser is deliberately defensive. If the model forgets the format, we treat the context as sufficient. This bounds cost, because the only other safeguard is the iteration limit, and a wrong "insufficient" verdict wastes API calls without improving the answer.

```racket
(define (parse-verdict-response response)
  (define lines (regexp-split #rx"\n" (or response "")))
  (define verdict-line
    (findf (lambda (line) (string-contains-ci? line "VERDICT:")) lines))
  (define missing-line
    (findf (lambda (line) (string-contains-ci? line "MISSING:")) lines))
  ...
  (cond
    [(and verdict-word (string-contains-ci? verdict-word "INSUFFICIENT"))
     (values #f feedback)]
    [(and verdict-word (string-contains-ci? verdict-word "SUFFICIENT"))
     (values #t feedback)]
    [else
     (debug-log "WARNING parse-verdict-response: unparseable verdict ~s; treating as SUFFICIENT~%"
                verdict-word)
     (values #t feedback)]))
```

### Agent 4: Synthesis Agent

The last agent writes the answer. The prompt is simple: use only the retrieved passages, cite the source file for each fact, and if the passages do not cover the question, say what is missing.

### The Orchestrator

`agentic-rag` ties the four agents together. It runs the rewriter, then the fanout, then enters the sufficiency loop. On each iteration it assesses the current context; if sufficient, it synthesizes and returns. If not, it refines the queries using the feedback from the sufficiency agent, searches again, merges the new chunks with the old ones (deduplicating by `(source . text)`), and tries once more.

Two details matter. First, `max-iterations` bounds the loop, and at the last iteration we skip the sufficiency check entirely: at that point "synthesize with what we have" is the only sensible move, so the check would be wasted. Second, `max-context-chunks` caps how many passages are sent to the LLM no matter how many iterations have accumulated; without this, a hard question could grow the prompt without bound.

```racket
(define (agentic-rag corpora user-query
                     #:max-iterations [max-iterations 3]
                     #:top-k [top-k 3]
                     #:model [model (*rag-model*)]
                     #:max-context-chunks [max-context-chunks 8])
  (define sub-queries (rewrite-queries user-query #:model model))
  (define initial-chunks (search-fanout corpora sub-queries #:top-k top-k))
  (call/ec
   (lambda (return)
     (let loop ([iteration 1] [all-chunks initial-chunks])
       (when (null? all-chunks)
         (return "I could not find any relevant information in the available documents."))
       (define context-chunks (cap-context all-chunks max-context-chunks))
       (when (>= iteration max-iterations)
         (return (synthesize-answer user-query context-chunks #:model model)))
       (define-values (sufficient? feedback)
         (assess-sufficiency user-query context-chunks #:model model))
       (when sufficient?
         (return (synthesize-answer user-query context-chunks #:model model)))
       (define refined-queries (refine-queries user-query feedback #:model model))
       (define new-chunks (search-fanout corpora refined-queries #:top-k top-k))
       ...
       (loop (+ iteration 1)
             (sort accumulated > #:key car))))))
```

## Running the Demo

The demo in `main.rkt` builds three corpora from the sample texts in `data/` (renewable energy, electric vehicles, climate science), loads them, and asks three questions in order of difficulty. Load the module and call `(test)`:

```racket
(require "main.rkt")
(test)
```

With `*rag-verbose*` at its default of `#t`, we see each agent report its decisions. Here is the start of the run, with most of the trace removed; the first line of each sub-query shows what the rewriter produced:

```
============================================
  Agentic RAG Demo -- Loading Documents
============================================

DEBUG add-document: loading .../data/renewable-energy.txt
DEBUG add-document: split into 9 chunks
DEBUG get-embeddings: batch-fetching 9 embeddings
DEBUG add-document: added 9 chunks from renewable-energy.txt
DEBUG add-document: loading .../data/electric-vehicles.txt
DEBUG add-document: split into 9 chunks
DEBUG get-embeddings: batch-fetching 9 embeddings
DEBUG add-document: added 9 chunks from electric-vehicles.txt
DEBUG add-document: loading .../data/climate-science.txt
DEBUG add-document: split into 9 chunks
DEBUG get-embeddings: batch-fetching 9 embeddings
DEBUG add-document: added 9 chunks from climate-science.txt

Loaded 27 total chunks across 3 corpora.

===== TEST QUERY 1 (single topic) =====

========================================
  AGENTIC RAG PIPELINE
  Query: What is the current cost of lithium-ion battery storage per kilowatt-hour?
========================================

DEBUG rewrite-queries: decomposing query...

DEBUG rewrite-queries: generated 2 sub-queries:
  - current cost lithium-ion battery storage per kWh
  - lithium-ion battery storage price trend
DEBUG search-fanout: searching 3 corpora with 3 queries
DEBUG search-fanout: searching with: "current cost lithium-ion battery storage per kWh"
DEBUG search-fanout: searching with: "lithium-ion battery storage price trend"
DEBUG search-fanout: searching with: "What is the current cost of lithium-ion battery storage per kilowatt-hour?"
DEBUG search-fanout: found 9 unique chunks

--- Iteration 1/3 ---

DEBUG assess-sufficiency: evaluating 8 chunks
DEBUG assess-sufficiency response:
VERDICT: SUFFICIENT
REASON: The passages state the price has fallen to under $140 per kilowatt-hour.
MISSING: NONE
DEBUG assess-sufficiency: verdict=SUFFICIENT

DEBUG synthesize-answer: generating answer from 8 chunks
DEBUG synthesize-answer: generated response (342 chars)

ANSWER 1:
The current cost of lithium-ion battery storage is under $140 per kilowatt-hour. This price has fallen by approximately 90% since 2010, when it was over $1,100 per kilowatt-hour, according to renewable-energy.txt.
```

Two things are worth noticing in that trace. The original user question appears verbatim in the search fanout, which is the fallback the rewriter adds so the search always covers the exact wording the user typed. And the sufficiency agent answered `SUFFICIENT` on the first pass, so the pipeline went straight to synthesis.

The second question is a multi-hop question and needs two corpora:

```
===== TEST QUERY 2 (multi-hop, cross-corpus) =====
... ANSWER 2:
Manufacturing an EV battery produces approximately 75-100 kg of CO2 per kilowatt-hour of battery capacity (electric-vehicles.txt). In return, an EV charged from renewable energy produces zero operational emissions (electric-vehicles.txt), and even on the average US grid mix it produces roughly 50-60% fewer lifecycle greenhouse gas emissions than a comparable gasoline vehicle (electric-vehicles.txt). Transitioning to electric vehicles powered by renewable energy is one of the most effective strategies for reducing transportation emissions (climate-science.txt).
```

The answer cites `electric-vehicles.txt` for the battery manufacturing emissions and `climate-science.txt` for the transportation context, confirming that the fanout crossed corpus boundaries.

The third question asks about two technologies that live in the same document but in different chunks:

```
===== TEST QUERY 3 (complex, iterative) =====
... ANSWER 3:
Solid-state batteries and pumped-storage hydroelectricity play complementary roles in solving the intermittency of wind and solar. Solid-state designs respond within milliseconds to smooth output when clouds pass over a solar farm or wind speeds drop, handle the fast, frequent charge cycles, and shift solar generation into the evening demand peak (renewable-energy.txt). Pumped-storage hydroelectricity provides bulk, long-duration storage that moves large amounts of energy across many hours or days, handling the deep, infrequent discharges (renewable-energy.txt). Together they let wind and solar installations deliver firm, dispatchable power around the clock (renewable-energy.txt).
```

`test` returns the list of corpora, so we can keep them and start an interactive session without re-embedding:

```racket
(define *corpora* (test))
(interactive-demo *corpora*)
```

This prints a `RAG>` prompt and answers questions until you type `quit`.

## Testing Without the Network

Everything that talks to the outside world is behind a parameter. `*embedding-fn*` produces embedding vectors, `*batch-request-fn*` posts one batch of texts to Gemini, and `*generate-fn*` calls the LLM. In `tests.rkt` all three are replaced with stubs using `parameterize`, so the entire suite runs offline:

```racket
(parameterize ([*embedding-fn* (lambda (text) '(1.0 0.0 0.0))]
               [*embedding-cache* (make-hash)]
               [*rag-verbose* #f])
  ...)
```

The suite has 15 tests. They cover the chunker (including the forward-progress guard), the query-line parser (digits inside queries survive; list prefixes of any number are stripped), vector math (including the error on dimension mismatch), retrieval ranking, deduplication across sources, batched query embedding, batch splitting at the 100-text API cap, cache eviction, retry behavior, verdict parsing, save/load round-trip with corruption checks, and the full pipeline with a stubbed LLM (sufficient on the first try, insufficient then sufficient, and the skipped sufficiency check at the last iteration). Run them with:

```bash
racket tests.rkt
```

You should see:

```
15 success(es) 0 failure(s) 0 error(s) 15 test(s) run
```

## Wrap Up

The agentic pipeline costs more API calls than vanilla RAG: one call to rewrite the query, at least one to assess sufficiency, and one more to synthesize. In exchange you get answers that hold together for multi-hop questions, with citations that point back to the source files. The iteration bound and the context cap keep the cost predictable even on hard questions.

## Optional Practice Problems

1. **Persist the cache**: `*embedding-cache*` lives in memory and is lost when the program exits. Change `get-embedding` so the cache is written to a file after each batch call and read back at startup. What happens to startup time for the demo?

2. **Score-weighted context**: The synthesis agent receives chunks in descending score order but no scores. Modify `format-retrieved-chunks` and the synthesis prompt so each passage shows its relevance score, and experiment with instructing the model to prefer higher-scoring passages when they conflict.

3. **Async fanout**: `search-fanout` searches each sub-query in turn. Use `racket/async-channel` or futures to search all sub-queries in parallel and measure the speedup on a corpus of a few hundred chunks.
