# CODING_IDEAS.md

## Remembering — Persistent Agent Memory with Deta, Redis, SQLite, and Graph

## Racket That Runs Everywhere — Browser Agents with RacketScript, Urlang, and WASM

**Awesome-Racket anchors:** `Compilers` → [racketscript](https://github.com/racketscript/racketscript) (Racket → JavaScript), [urlang](https://github.com/soegaard/urlang) (Racket syntax for JS), [wasm-adventure](https://github.com/euhmeuh/wasm-adventure) (WASM DSL), [lens](https://github.com/jackfirth/lens)

**Gap:** `lightpanda` wraps a headless browser externally; the book never compiles Racket *to* the browser. That is a missed story for an AI book — shipping assistants as browser extensions.

**Example:** `browser_agent/` — compile the `webscrape` helpers + a small `lens`-based state machine to JavaScript with `racketscript`, inject it as a content script that extracts page text, and stream it to the `ai_service` (Idea 5) over `fetch`. A second variant compiles a tight loop (e.g., embedding cosine) to WASM with `wasm-adventure` for in-page speed.

```racket
;; urlang example: generate JS that runs in the page
#lang urlang
(require lens)
(define current-url (string-trim js/window.location.href))
(fetch "http://localhost:8080/ask" #:method "POST" #:body (extract-article-text))
```

**Install:** `raco pkg install racketscript urlang wasm-adventure lens`
