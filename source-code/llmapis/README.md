# Using LLM APIs in Racket

**Book Chapter:** [Using the Google Gemini, OpenAI, Anthropic, Mistral, and Local LLM APIs](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This directory contains examples of using various Large Language Model APIs (OpenAI, Anthropic, Mistral, Google Gemini, and local models like Ollama/Llama) natively from Racket. It demonstrates how to send prompts, manage context, and receive generations from different providers.

The **Gemini module** (`gemini.rkt`) uses the new **Interactions API** (`v1beta/interactions`) with the `steps` response schema (Api-Revision: 2026-05-20), replacing the legacy `generateContent` endpoint.

## Install as a local package

    raco pkg remove
    raco pkg install --scope user

If you change the source code, run the following to update the linked (installed in place) package **llmapis**:

    raco make main.rkt

## Gemini API Details

- **Endpoint:** `POST https://generativelanguage.googleapis.com/v1beta/interactions`
- **Schema:** New `steps` array — response text is in `steps[].content[].text` for `model_output` steps
- **Features:** Text generation, Google Search grounding, search with citations

## Notes

- As of March 1, 2026, I no longer have an OpenAI account so I won't be updating the OpenAI API code.

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2026 Mark Watson. All rights reserved.
