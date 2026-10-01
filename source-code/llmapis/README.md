# Using LLM APIs in Racket

**Book Chapter:** [Using the Google Gemini, OpenAI, Anthropic, Mistral, and Local LLM APIs](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This directory contains examples of using various Large Language Model APIs (OpenAI, Anthropic, Mistral, Google Gemini, and local models like Ollama/Llama) natively from Racket. It demonstrates how to send prompts, manage context, and receive generations from different providers.

The **uniform entry point** is `llmapis.rkt`: one API for all providers, modeled on the Common Lisp `litelm` library. Models are addressed as `"provider/model-name"` strings, messages and tool definitions use a Racket-friendly nested-list format (no JSON in user code), and tools are plain Racket functions. The per-provider modules (`openai.rkt`, `anthropic.rkt`, `gemini.rkt`, ...) remain for provider-specific extras.

## Uniform API (`llmapis.rkt`)

```racket
(require "llmapis.rkt")

;; basic completion -- returns an llm-response struct
(llm-response-content
 (llm-completion "openai/gpt-5-mini" #:messages "What is 2+2?"))

;; one-shot question, any provider
(llm-ask "gemini/gemini-flash-latest" "Capital of France?")

;; local models need no API key
(llm-ask "ollama/qwen3:1.7b" "What is 2+2?")
(llm-ask "llama-local/local-model" "What is 2+2?")

;; tools are Racket functions the model can call
(define (get-weather args)
  (format "sunny and 22C in ~a" (hash-ref args 'location "nowhere")))
(define tools
  (list (make-llm-tool "get_weather"
                       "Get the current weather for a location"
                       '(("location" "string" "City name, e.g. Paris"))
                       get-weather)))
;; full request/execute/reply loop:
(llm-response-content
 (llm-chat-with-tools "openai/gpt-5-mini"
                      "What is the weather in Paris?"
                      tools))

;; embeddings (OpenAI-compatible providers)
(llm-embedding "openai/text-embedding-ada-002" "hello world")

;; register another OpenAI-compatible provider at runtime
(define-provider 'groq "https://api.groq.com/openai/v1"
  #:env-keys '("GROQ_API_KEY"))
```

### Model routing

| Provider | Prefix | API key env vars | Base URL |
|---|---|---|---|
| OpenAI | `openai/` | `OPENAI_API_KEY`, `OPENAI_KEY` | `https://api.openai.com/v1` |
| Gemini | `gemini/` | `GEMINI_API_KEY`, `GOOGLE_API_KEY` | `.../v1beta/openai` (OpenAI-compatible) |
| Mistral | `mistral/` | `MISTRAL_API_KEY` | `https://api.mistral.ai/v1` |
| DeepSeek | `deepseek/` | `DEEPSEEK_API_KEY` | `https://api.deepseek.com/v1` |
| Fireworks AI | `fireworks-ai/` | `FIREWORKS_API_KEY` | `https://api.fireworks.ai/inference/v1` |
| Ollama (local) | `ollama/` | — | `http://localhost:11434/v1` |
| Anthropic | `anthropic/` | `ANTHROPIC_API_KEY` | native Messages API |
| llama.cpp (local) | `llama-local/` | — | `http://localhost:8080` (`/completion`, text only) |

Model names may contain slashes (e.g. `"fireworks-ai/accounts/fireworks/models/deepseek-v4"`). Any provider endpoint can be overridden per call with `#:api-key` / `#:api-base`.

### Message format

A message is `(list role content [options])` with roles `system`, `user`, `assistant`, `tool`; content is a string (or `#f` for assistant messages carrying only tool calls). A plain string is shorthand for a user message. Tool continuations use `#:tool-calls`, `#:tool-call-id`, and the `llm-assistant-message` / `llm-tool-message` constructors.

### Tool definition format

```racket
(make-llm-tool "get_weather"              ; name (symbol or string)
               "Get the current weather"  ; description
               '(("location" "string" "City name")              ; required by default
                 ("units" "string" "celsius or fahrenheit"
                  #:required #f #:enum ("celsius" "fahrenheit")))
               get-weather)               ; Racket proc of one hash arg
```

Tool calls are *returned*, not executed — execution is the caller's job (`execute-tool-calls`, or `llm-chat-with-tools` for the full loop). Failures (unknown tool, missing argument, bad JSON, handler exception) become `"Error: ..."` result strings for the model, never uncaught exceptions.

### Error handling

HTTP failures map onto an exception hierarchy mirroring `litelm`: `exn:fail:llm:authentication` (401/403), `exn:fail:llm:rate-limit` (429), `exn:fail:llm:not-found` (404), `exn:fail:llm:context-window` (400 mentioning "context"), all under `exn:fail:llm:api` (readers: `exn:fail:llm:api-status`, `exn:fail:llm:api-body`).

Newer OpenAI models reject `max_tokens`; when a 400 names `max_completion_tokens` instead, the OpenAI-compatible path retries once with the renamed parameter automatically.

### File structure

| File | Contents |
|---|---|
| `llmapis.rkt` | Uniform entry point: routing, messages, Racket-function tools, completion, embeddings |
| `openai.rkt` | OpenAI chat + embeddings |
| `anthropic.rkt` | Anthropic chat, web search, search with citations |
| `gemini.rkt` | Gemini chat, Google Search grounding, search with citations |
| `mistral.rkt` | Mistral chat + embeddings |
| `ollama_ai_local.rkt` | Local Ollama chat + embeddings |
| `llama_local.rkt` | Local llama.cpp chat |
| `main.rkt` | Re-exports the per-provider modules |

## Architecture

![Generated image](architecture.png)

## Install as a local package

    raco pkg remove
    raco pkg install --scope user

If you change the source code, run the following to update the linked (installed in place) package **llmapis**:

    raco make main.rkt

## Gemini API Details (`gemini.rkt`)

- **Endpoint:** `POST https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent`
- **Features:** Text generation (`generate`), Google Search grounding (`generate-with-search`), search with citations (`generate-with-search-and-citations`)
- The uniform API (`llmapis.rkt`) reaches Gemini through its OpenAI-compatible base URL instead (`.../v1beta/openai`); use `gemini.rkt` directly for native grounding/citations.

## Notes

- As of March 1, 2026, I no longer have an OpenAI account so I won't be updating the OpenAI API code.

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2026 Mark Watson. All rights reserved.
