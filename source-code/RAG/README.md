# Agentic RAG in Racket

**Book Chapter:** Agentic RAG Using the Gemini API — *Practical Artificial Intelligence Development With Racket*

An implementation of **Agentic Retrieval-Augmented Generation (RAG)** in Racket, inspired by Google's research on [Unlocking Dependable Responses with Agentic RAG](https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/).

Unlike traditional "vanilla" RAG which performs a single retrieve-then-generate pass, agentic RAG uses multiple specialized agents that **plan, rewrite queries, assess context sufficiency, and iteratively search** until enough information is gathered to produce a reliable answer.

## Architecture

The system implements a multi-agent pipeline:

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

### Key Innovation: Sufficient Context Agent

The critical difference from standard RAG is the **Sufficient Context Agent**, which acts as a quality-control inspector. After retrieval, it evaluates:

1. **Retrieved snippets** — Are the actual text chunks relevant and informative?
2. **Completeness** — Does the context address ALL parts of the user's question?
3. **Missing pieces** — What specific information is still needed?

If context is insufficient, the system generates refined search queries and iterates (up to a configurable limit).

## Dependencies

- **net/http-easy** — HTTP client for Gemini API calls
- **json** — JSON encoding/decoding
- **rackunit** — Unit test framework (tests only)

**Environment variable:** `GOOGLE_API_KEY` must be set.

**Models used:**
- `gemini-3-flash-preview` — Default for all agent LLM calls (`*rag-model*`; override per call with `#:model`)
- `gemini-embedding-001` — Free-tier embedding model for document/query vectors (`text-embedding-004` was retired from the v1beta API). The API key is sent in the `x-goog-api-key` header, never in the URL.

Set `*embedding-dimension*` to `768` (or `1536`) before building or loading a corpus to cut embedding memory and search time by 4x (2x) with little quality loss; the model default is 3072. If you change the embedding model or dimension, re-embed your corpora: `search-corpus` signals a dimension-mismatch error rather than silently scoring with truncated vectors.

Embeddings are computed with batched `batchEmbedContents` calls (at most 100 texts per request, the API cap) and memoized in an in-memory cache (`clear-embedding-cache` resets it; `*embedding-cache-cap*` bounds its size). Transient API failures (HTTP 429/5xx, connection errors) are retried with exponential backoff; permanent 4xx errors signal immediately.

## Quick Start

```racket
(require "main.rkt")

;; Run the built-in demo with sample documents
(test)

;; Or use interactively with the returned corpora:
(define *corpora* (test))
(interactive-demo *corpora*)
```

## API Reference

### `make-corpus #:name name #:description description` -> corpus
Create an empty corpus (document collection).

### `add-document corpus filepath #:chunk-size [chunk-size 500]` -> count
Read a text file, split it into overlapping chunks, compute embeddings, and store in the corpus. Returns the number of chunks added.

### `query corpora question #:max-iterations [3] #:top-k [3] #:model [*rag-model*] #:max-context-chunks [8]` -> string
Ask a question using the full agentic RAG pipeline. `corpora` can be a single corpus or a list of corpora for cross-corpus retrieval.

### `agentic-rag corpora question #:max-iterations [3] #:top-k [3] #:model [*rag-model*] #:max-context-chunks [8]` -> string
Low-level entry point with full control. `max-iterations` bounds the sufficiency/refinement loop (default 3), `top-k` sets passages retrieved per query (default 3), `model` overrides `*rag-model*`, and `max-context-chunks` caps how many top-scoring passages are sent to the LLM regardless of iteration count (default 8).

### `save-corpus corpus pathname` / `load-corpus pathname` -> corpus
Persist a corpus (chunks and embeddings) to an s-expression file and load it back, avoiding re-embedding (and re-paying API calls) on every run.

### `corpus-chunk-count corpus` -> integer
Number of chunks stored in a corpus.

### `*rag-verbose*`
When true (default), each agent prints DEBUG tracing of its decisions — useful for following the pipeline in the book's examples. Set to `#f` for quiet library use.

### `interactive-demo corpora`
Start an interactive REPL for querying loaded corpora.

### `test` -> corpora
Run the built-in demo: loads sample documents about renewable energy, electric vehicles, and climate science, then runs three progressively harder queries.

## Tests

Offline unit tests (no network access; the LLM and embedding functions are stubbed via `*generate-fn*` / `*embedding-fn*`):

```bash
racket tests.rkt
```

Covers chunking boundary cases (including the forward-progress guard), query line parsing (digits in query text survive, list prefixes of any number are stripped), vector math (including the dimension-mismatch error), retrieval ranking, cross-source deduplication, batched query embedding, batch splitting at the 100-text API cap, cache eviction, retry behavior (transient vs permanent errors), sufficiency-verdict parsing, corpus save/load round-trip with corruption checks, and the full agentic pipeline with a stubbed LLM (including the skipped sufficiency call at the last iteration and quiet-mode operation).

## File Structure

| File | Description |
|---|---|
| `info.rkt` | Package definition |
| `embeddings.rkt` | Gemini embedding integration: batched API, cache with eviction, retries, `*rag-verbose*` |
| `vector-store.rkt` | In-memory vector store with normalized embeddings, cosine similarity, chunking, corpus persistence with validation |
| `agents.rkt` | Multi-agent pipeline (rewriter, search fanout, sufficiency, synthesis) |
| `main.rkt` | Public API, interactive demo, and `test` demo code |
| `tests.rkt` | Offline unit tests (rackunit) |
| `data/` | Sample text documents for the demo |

## Example Output

A multi-hop query like *"How does the carbon footprint of manufacturing EV batteries compare to the emissions saved by charging EVs from renewable energy sources?"* requires the system to:

1. **Rewrite** into sub-queries about EV battery manufacturing emissions AND renewable energy charging benefits
2. **Search** across the electric-vehicles corpus AND climate-science corpus
3. **Assess** whether both pieces of information were found
4. **Synthesize** an answer combining facts from multiple sources with citations

## Reference

- [Unlocking Dependable Responses with Agentic RAG](https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/) — Google Research Blog, June 2026
- [FRAMES Benchmark](https://arxiv.org/abs/2409.12941) — Evaluation dataset for multi-hop RAG
