---
name: racket-ai-dev
description: Racket language tutorial, idioms, and API reference for all examples in Mark Watson's Racket book "Practical Artificial Intelligence Development With Racket". Use this skill for writing Racket code that accesses LLMs (Gemini, OpenAI, Anthropic, Mistral, Ollama), SPARQL queries, NLP, web scraping, embeddings databases, tool calling, and more.
---

# Notes for Using AGENT Skills with Racket Book Examples

This document helps readers set up coding agent skills so that AI assistants can reference the Racket APIs and patterns from this book when generating code.

## Source code for Gemini, OpenAI, Anthropic, Ollama, SPARQL, NLP, web scraping, embeddings example code

```bash
git clone https://github.com/markwatson/Racket-AI-book.git
```

All the Racket examples are in the `source-code` directory. Look in ~/GITHUB/Racket-AI-book/source-code/ for code to reuse.

---

## Racket Language Tutorial and Idioms

Racket is a descendant of Scheme (a Lisp dialect) with a powerful module system, batteries-included standard library, and rich macro support.

### Core Syntax

```racket
#lang racket

;; Printing
(displayln "Hello from Racket!")

;; Variable definition
(define x 42)
(define name "Mark")

;; Arithmetic (prefix notation)
(+ 1 2 3)       ; => 6
(* 2 (+ 3 4))   ; => 14
```

### Requires (Imports)

```racket
;; Require a built-in module
(require json)
(require net/http-easy)
(require racket/pretty)

;; Require a local file
(require "tools.rkt")

;; Require an installed package
(require llmapis)
(require nlp)

;; Require with prefix
(require (prefix-in xml: xml))
```

### Functions

```racket
;; Define a function
(define (greet name)
  (string-append "Hello, " name "!"))

;; Optional / default arguments
(define (fetch url [timeout 10] [retries 3])
  (displayln (format "Fetching ~a with timeout=~a" url timeout)))

;; Rest arguments
(define (helper prompt . model-name)
  (displayln (list "args:" model-name)))

;; Anonymous functions (lambda)
(filter (lambda (x) (> x 3)) '(1 2 3 4 5))  ; => '(4 5)
```

### Control Flow

```racket
;; if (requires both then and else branches)
(if (= x 1)
    (displayln "one")
    (displayln "not one"))

;; when (single branch, no else)
(when (> x 0)
  (displayln "positive"))

;; unless (opposite of when)
(unless (= x 0)
  (displayln "non-zero"))

;; cond (multiple branches)
(cond
  [(< x 0) (displayln "negative")]
  [(= x 0) (displayln "zero")]
  [else     (displayln "positive")])

;; let (local bindings)
(let ([a 10]
      [b 20])
  (+ a b))  ; => 30

;; let* (sequential bindings)
(let* ([a 10]
       [b (+ a 5)])
  b)  ; => 15
```

### Loops and Comprehensions

```racket
;; for-each (side effects, returns void)
(for-each displayln '("a" "b" "c"))

;; for/list — list comprehension (returns a list)
(for/list ([x (in-range 5)]) (* x x))          ; => '(0 1 4 9 16)
(for/list ([x '(1 2 3 4 5)] #:when (> x 3))
  (* x 2))                                     ; => '(8 10)

;; for ([i (in-range n)]) — imperative loop
(for ([i (in-range 5)])
  (displayln i))

;; map
(map string-upcase '("hello" "world"))  ; => '("HELLO" "WORLD")
```

### Data Structures

```racket
;; Lists
(define fruits '("apple" "banana" "cherry"))
(first fruits)               ; => "apple"
(rest fruits)                ; => '("banana" "cherry")
(cons "date" fruits)         ; => '("date" "apple" "banana" "cherry")

;; Hash tables (immutable)
(define config (hash 'host "localhost" 'port 8080))
(hash-ref config 'host)     ; => "localhost"
(hash-ref config 'missing "default")

;; Hash tables (mutable)
(define h (make-hash))
(hash-set! h 'key "value")
(hash-ref h 'key)           ; => "value"

;; Vectors
(define v #(1 2 3))
(vector-ref v 0)            ; => 1

;; Structs
(struct triple (subject predicate object) #:transparent)
(define t (triple "John" "likes" "pizza"))
(triple-subject t)          ; => "John"
```

### String Formatting

```racket
;; format (printf-style)
(format "Hello, ~a!" name)
(format "Result: ~a" (+ 1 2))

;; string-append
(string-append "Hello" ", " "world")

;; string-join
(string-join '("a" "b" "c") ", ")  ; => "a, b, c"
```

### Exception Handling

```racket
(with-handlers ([exn:fail? (lambda (e)
                              (displayln (exn-message e)))])
  (/ 10 0))
```

### Provide (Exports)

```racket
;; Export specific bindings
(provide generate generate-with-search)

;; Export everything
(provide (all-defined-out))
```

### HTTP Requests (net/http-easy)

```racket
(require net/http-easy)
(require json)

;; GET request
(define response (get "https://api.example.com/data"))
(response-json response)

;; POST with JSON
(post url #:json (hash 'key "value"))

;; POST with raw data
(post url #:data json-string)

;; POST with auth
(post url
      #:auth (lambda (uri headers params)
               (values (hash-set* headers
                                   'authorization "Bearer TOKEN"
                                   'content-type "application/json")
                       params))
      #:json data)

;; Streaming response
(define stream (get url #:stream? #t))
(response-output stream)
(response-close! stream)
```

### Package Management

```racket
;; Install a package
;; raco pkg install <package-name>

;; Install a local directory as a linked package
;; raco pkg install --scope user

;; Rebuild after changes
;; raco make main.rkt
```

---

# Racket Book APIs — Quick Reference

Knowledge of public APIs and usage patterns for the Racket examples in Mark Watson's book *Practical Artificial Intelligence Development With Racket*.

## Project Setup

Each subdirectory is a standalone Racket project. Some (like `llmapis` and `nlp`) are installable as local packages:

```bash
cd source-code/<example_name>
racket <script>.rkt

# For package-style projects:
raco pkg install --scope user
raco make main.rkt
```

---

## llmapis

**Directory:** `llmapis/`
**Deps:** `net/http-easy`, `json`, `pprint`
**Install:** `raco pkg install --scope user` (from the `llmapis/` directory)

Provides unified access to multiple LLM providers. Install as a local package to use from other projects via `(require llmapis)`.

### gemini.rkt

**Env var:** `GOOGLE_API_KEY`
**Model:** `gemini-3-flash-preview`

- `(generate prompt [model])` — Send a prompt to Gemini. Returns text string.
- `(generate-with-search prompt [model])` — Prompt with Google Search grounding. Returns text.
- `(generate-with-search-and-citations prompt [model])` — Search grounding with citations. Returns `(values text citations)`.

```racket
(require "gemini.rkt")
(displayln (generate "What is the capital of France?"))
(displayln (generate-with-search "Latest AI news"))
(let-values ([(text citations) (generate-with-search-and-citations "Latest AI news")])
  (displayln text)
  (displayln citations))
```

### anthropic.rkt

**Env var:** `ANTHROPIC_API_KEY`
**Model:** `claude-sonnet-4-6`

- `(generate prompt max-tokens)` — Send prompt to Claude. Returns text.
- `(question-anthropic-with-search prompt)` — Claude with web search tool. Returns text.
- `(question-anthropic-with-search-and-citations prompt)` — Web search with citations. Returns `(values text citations)`.

```racket
(require "anthropic.rkt")
(displayln (generate "What is the capital of France?" 100))
(displayln (question-anthropic-with-search "Latest AI news"))
```

### openai.rkt

**Env var:** `OPENAI_API_KEY`
**Model:** `gpt-5`

- `(question-openai prompt)` — Ask a question via OpenAI chat completions. Returns text.
- `(completion-openai prompt)` — Text completion. Returns text.
- `(embeddings-openai text)` — Get embedding vector. Returns list of floats.

```racket
(require "openai.rkt")
(displayln (question-openai "Who is older, Mary (30) or Harry (25)?"))
(displayln (embeddings-openai "some text"))
```

### mistral.rkt

**Env var:** `MISTRAL_API_KEY`
**Model:** `mistral-small`

- `(question-mistral prompt)` — Ask via Mistral chat API. Returns text.
- `(completion-mistral prompt)` — Text completion. Returns text.
- `(embeddings-mistral text)` — Get embedding via `mistral-embed`. Returns list of floats.

### ollama_ai_local.rkt

**Server:** Requires Ollama running locally on port 11434
**Model:** `mistral` (default)

- `(question-ollama-ai-local question [model-name])` — Ask via local Ollama. Returns text.
- `(completion-ollama-ai-local prompt [model-name])` — Text completion. Returns text.
- `(embeddings-ollama text)` — Get embedding vector from local Ollama. Returns list of floats.

### llama_local.rkt

**Server:** Requires llama.cpp server on port 8080

- `(question-llama-local question)` — Ask via local llama.cpp. Returns text.
- `(completion-llama-local prompt)` — Text completion. Returns text.

### main.rkt (Unified Re-exports)

Re-exports all LLM provider functions for convenient use:

```racket
(require llmapis)  ;; after raco pkg install
;; Now you have: question-openai, completion-openai, embeddings-openai,
;;   question-ollama-ai-local, completion-ollama-ai-local, embeddings-ollama,
;;   question-llama-local, completion-llama-local,
;;   question-anthropic, completion-anthropic
```

---

## ollama_tools

**Directory:** `ollama_tools/`
**Deps:** `net/http-easy`, `json`, `racket/date`
**Server:** Requires Ollama running locally
**Model:** `qwen3:1.7b` (default, configurable via `OLLAMA_MODEL` env var)

### API (tools.rkt)

- `(register-tool name description parameters handler)` — Register a callable tool.
- `(get-tool name)` — Retrieve a registered tool by name.
- `(call-ollama-with-tools prompt tool-names #:model [model])` — Call Ollama with tool-calling loop. Returns final text response.
- `(get-current-datetime args)` — Returns current date/time string.
- `(get-weather args)` — Fetches weather via wttr.in. Args: `'location`.
- `(list-directory args)` — List files in current directory.
- `(read-file-contents args)` — Read a file. Args: `'file_path`.

### Example

```racket
(require "tools.rkt")
(displayln (call-ollama-with-tools
            "What is the weather in Phoenix Arizona?"
            '("get_weather")))
(displayln (call-ollama-with-tools
            "What is the current date and time?"
            '("get_current_datetime")))
```

---

## search_brave

**Directory:** `search_brave/`
**Deps:** `net/http-easy`, `json`, `net/url`, `net/uri-codec`
**Env var:** `BRAVE_SEARCH_API_KEY`

### API

- `(brave-search query [num-results 3])` — Search via Brave Search API. Returns list of hashes with `'title`, `'url`, `'description`.

### Example

```racket
(require "brave_search.rkt")
(pretty-print (brave-search "Fun things to do in Sedona Arizona"))
```

---

## search_tavily

**Directory:** `search_tavily/`
**Deps:** `net/http-easy`, `json`
**Env var:** `TAVILY_API_KEY`

### API

- `(tavily-search query)` — Search via Tavily API. Returns list of `(url title content)` triples.

### Example

```racket
(require "tavily_search.rkt")
(pretty-print (tavily-search "Fun things to do in Sedona Arizona"))
```

---

## nlp

**Directory:** `nlp/`
**Deps:** `srfi/13`, `racket/runtime-path`
**Install:** `raco pkg install --scope user` (from the `nlp/` directory)

### API

- `(parts-of-speech word-vector)` — POS tagging using FastTag. Input: vector of word strings. Returns vector of POS tags.
- `(find-human-names word-vector exclusion-list)` — Extract human names from text. Returns list of name strings.
- `(find-place-names word-vector exclusion-list)` — Extract place names from text. Returns list of place name strings.

### Example

```racket
(require nlp)
(display (parts-of-speech '#("the" "cat" "ran")))
(display (find-human-names '#("President" "George" "Bush" "went" "to" "San" "Diego") '()))
(display (find-place-names '#("George" "Bush" "went" "to" "San" "Diego" "and" "London") '()))
```

---

## kgn (Knowledge Graph Navigator)

**Directory:** `kgn/`
**Deps:** `racket/gui`, `htdp/gui`, `scribble/text/wrap`, `nlp` (package), `net/url`, `json`
**Install:** Requires `nlp` package installed first

### Key Modules

- **`sparql-utils.rkt`** — SPARQL query helpers for DBpedia:
  - `(sparql-dbpedia-person-uri person-name)` — Build SPARQL to find person URIs.
  - `(sparql-dbpedia-for-person person-uri)` — Build SPARQL for person details.
  - `(sparql-dbpedia-place-uri place-name)` — Build SPARQL to find place URIs.
  - `(sparql-query->hash query)` — Execute SPARQL on DBpedia, return JSON hash.
  - `(json->listvals a-hash)` — Parse SPARQL JSON results into lists.
  - `(extract-name-uri-and-comment l1 l2)` — Zip two result lists.

- **`dialog-utils.rkt`** — GUI dialog helpers:
  - `(make-selection-functions parent-frame title)` — Returns `(list set-new-items-and-show-dialog get-selection-index)`.

- **`kgn.rkt`** — Main GUI application for interactive knowledge graph exploration.

### Example

```racket
(require "sparql-utils.rkt")
(sparql-query->hash (sparql-dbpedia-person-uri "Steve Jobs"))
```

---

## sparql

**Directory:** `sparql/`
**Deps:** `net/url`, `net/uri-codec`, `json`

### API

- `(sparql-dbpedia-person-uri person-name)` — SPARQL query template for person URIs on DBpedia.
- `(sparql-query->hash query)` — Execute SPARQL on DBpedia. Returns JSON hash.
- `(json->listvals a-hash)` — Parse SPARQL results to lists.
- `(sparql-dbpedia sparql)` — High-level: execute SPARQL, return zipped results list.

### Example

```racket
(require "sparql.rkt")
(sparql-dbpedia (sparql-dbpedia-person-uri "Steve Jobs"))
```

---

## simple_RDF_SPARQL

**Directory:** `simple_RDF_SPARQL/`
**Deps:** (none, pure Racket)

An in-memory RDF triple store with a SPARQL query parser.

### API

- `(add-triple subject predicate object)` — Add a triple to the store.
- `(remove-triple subject predicate object)` — Remove a triple.
- `(query-triples subject predicate object)` — Query with pattern matching (use `#f` or `?var` for wildcards).
- `(execute-sparql-query query-string)` — Parse and execute a SPARQL SELECT query. Returns list of bindings.
- `(print-all-triples)` — Display all stored triples.

### Example

```racket
(require "rdf_sparql.rkt")
(add-triple "John" "likes" "pizza")
(add-triple "Mary" "likes" "sushi")
(execute-sparql-query "select ?s ?o where { ?s likes ?o }")
```

---

## embeddingsdb

**Directory:** `embeddingsdb/`
**Deps:** `db`, `llmapis` (package)
**Env var:** `OPENAI_API_KEY` (uses OpenAI embeddings)

### API

- `(create-document fpath)` — Read a file, chunk it, embed each chunk, store in SQLite.
- `(QA query [quiet #f])` — Semantic search + OpenAI QA over indexed documents. Returns answer text.
- `(CHAT)` — Interactive RAG chat loop with conversation memory.

### Example

```racket
(require "embeddingsdb.rkt")
(create-document "data/sports.txt")
(QA "What are the advantages of engaging in sports?")
```

---

## webscrape

**Directory:** `webscrape/`
**Deps:** `net/http-easy`, `html-parsing`, `net/url`, `xml`, `xml/path`, `srfi/13`

### API

- `(web-uri->xexp a-uri)` — Fetch a URL and parse HTML to XEXP (S-expression DOM).
- `(web-uri->text a-uri)` — Fetch a URL and extract paragraph text as a single string.
- `(web-uri->links a-uri)` — Fetch a URL and extract all external HTTP links.

### Example

```racket
(require "webscrape.rkt")
(displayln (web-uri->text "https://knowledgebooks.com"))
(displayln (web-uri->links "https://knowledgebooks.com"))
```

---

## llm-agent

**Directory:** `llm-agent/`
**Deps:** `racket/hash`, `racket/contract`, `rackunit`

A blackboard-based agent framework with tool registration and simulated LLM calls.

### API

- `(make-context)` — Create a new context (mutable hash blackboard).
- `(context-set! context key value)` / `(context-get context key #:default val)` — Get/set context values.
- `(register-tool context tool-name tool-function)` — Register a tool function.
- `(get-tool context tool-name)` — Retrieve a tool by name.
- `(make-agent #:name name #:instructions instructions #:process-fn fn)` — Create an agent.
- `(agent-process agent prompt context)` — Process a prompt. Returns `(values response updated-context)`.

### Example

```racket
(require "llb-agent.rkt")
(define ctx (make-context))
(register-tool ctx 'racket-eval racket-eval)
(define my-agent
  (make-agent #:name "CalcAgent"
              #:instructions "You are a calculator agent."
              #:process-fn agent-default-process-fn))
(define-values (response updated-ctx)
  (agent-process my-agent "calculate 2 + 2" ctx))
```

---

## racket_llm_language

**Directory:** `racket_llm_language/`
**Deps:** `llm` package
**Install:** `raco pkg install llm`

Experiments with William J. Bowman's `#lang llm` — a Racket language that integrates LLM generations natively within Racket syntax.

### Run

```bash
racket test_llm_ollama.rkt
racket test_llm_openai.rkt
racket test_lang_mode_llm_openai.rkt
```

---

## pdf_chat

**Directory:** `pdf_chat/`
**Deps:** `pdf-read`
**Install:** `raco pkg install pdf-read`

### API (pdf2text.rkt)

- `(pdf->string pdf-file-path)` — Extract all text from a PDF file. Returns concatenated string.

### Example

```racket
(require "pdf2text.rkt")
(displayln (pdf->string "test_pdfs/alice_in_wonderland.pdf"))
```

---

## Karpathy_MicroGPT

**Directory:** `Karpathy_MicroGPT/`
**Deps:** `racket/math` (stdlib only, no external deps)

A dependency-free implementation of Karpathy's microGPT in pure Racket with custom autograd.

### API

- `(run-microgpt)` — Train a character-level transformer on `names.txt` and generate samples.

### Run

```bash
racket microgpt.rkt
```

---

## misc_code

**Directory:** `misc_code/`
**Deps:** `db`, `sqlite-table`, `net/http-easy`, `html-parsing`

Miscellaneous utility examples:

- **`sqlite.rkt`** — SQLite database CRUD operations.
- **`html_text.rkt`** — Fetch and parse HTML from a URL, extract paragraph text.
- **`hash_tests.rkt`** — Hash table operations and patterns.
- **`lists_tests.rkt`** — List manipulation examples.
- **`http_links.rkt`** — HTTP link extraction.

---

## General Notes

- All examples use `#lang racket` (or `#lang racket/gui` for GUI apps, `#lang at-exp racket` for SPARQL templates).
- Use `raco pkg install <package>` to install external packages. Key packages: `net/http-easy`, `html-parsing`, `pdf-read`, `llm`, `pprint`.
- The `llmapis` and `nlp` directories can be installed as local packages with `raco pkg install --scope user` for cross-project reuse.
- Environment variables must be set before use: `GOOGLE_API_KEY`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `MISTRAL_API_KEY`, `BRAVE_SEARCH_API_KEY`, `TAVILY_API_KEY`.
- Racket uses `define` for bindings, `let`/`let*` for local scope, and `lambda` for anonymous functions.
- Use `(provide ...)` to export bindings and `(require ...)` to import them.
- HTTP APIs use the `net/http-easy` library with `get`, `post`, `response-json`, and `#:auth` for authentication.
- Use `(hash ...)` for immutable hashes and `(make-hash)` for mutable ones.
