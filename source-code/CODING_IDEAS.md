# Coding ideas

Notes on examples that might be added to the book. Nothing in this file is implemented yet.

## Persistent agent memory

Give an agent memory that survives between sessions, comparing a few stores: SQLite (already used in `embeddingsdb`), Redis, and a graph store.

## Racket that runs in the browser

**Awesome-Racket anchors:** `Compilers` → [racketscript](https://github.com/racketscript/racketscript) (Racket → JavaScript), [urlang](https://github.com/soegaard/urlang) (Racket syntax for JS), [wasm-adventure](https://github.com/euhmeuh/wasm-adventure) (WASM DSL), [lens](https://github.com/jackfirth/lens)

**Gap:** `lightpanda` wraps a headless browser externally; the book never compiles Racket *to* the browser.

**Example idea:** `browser_agent/` — compile the `webscrape` helpers and a small `lens`-based state machine to JavaScript with `racketscript`, inject it as a content script that extracts page text, and send it to a local Racket HTTP service (which would also need to be written) over `fetch`. A second variant could compile a tight loop (for example, embedding cosine similarity) to WASM with `wasm-adventure`.

**Install:** `raco pkg install racketscript urlang wasm-adventure lens`
