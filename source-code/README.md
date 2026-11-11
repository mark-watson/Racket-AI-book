# Example code for Mark Watson's book "Practical Artificial Intelligence Development With Racket"

Racket implementations of many short AI examples: LLM clients and agents, embeddings and RAG, deep learning, reinforcement learning, NLP, the semantic web, and rule-based systems.

Book on Leanpub (free to read online): [https://leanpub.com/racket-ai](https://leanpub.com/racket-ai)

## Setup

1. Install [Racket](https://racket-lang.org) (the examples are tested with Racket CS 9.x; 8.10 or later should work).
2. Install the two packages most examples use:

       raco pkg install --auto http-easy html-parsing

3. Check your setup. Neither command needs an API key or a network connection:

       make compile
       make test

Examples load each other by relative path (for example `(require "../llmapis/llmapis.rkt")`), so you do not need to install any of these directories as packages.

## Examples by chapter

"Offline tests" means the directory has a test suite that `make test` runs with no API key and no network.

| Directory | Chapter | Extra packages | Environment variables | Offline tests |
|---|---|---|---|---|
| `misc_code` | A quick Racket tutorial; Datastores | | | yes (`sqlite.rkt`) |
| `sparql` | Datastores; Knowledge Graph Navigator | | | yes |
| `simple_RDF_SPARQL` | Implementing a simple RDF datastore | | | yes |
| `webscrape` | Web scraping | | | yes |
| `lightpanda` | A Lightpanda browser client | the `lightpanda` program | | yes |
| `Karpathy_MicroGPT` | Building a MicroGPT in Racket | | | yes |
| `llmapis` | Using the LLM APIs in Racket | | see the table below | yes |
| `racket_llm_language` | Using the LLM APIs in Racket (`#lang llm` section) | `llm` | `OPENAI_API_KEY` | no |
| `ollama_tools` | Ollama tools/function calling | Ollama running locally | `OLLAMA_MODEL`, `OLLAMA_HOST` (optional) | yes |
| `coding-agent-harness` | A Racket coding agent (AGPL-3.0, see below) | | `FIREWORKS_API_KEY` or a local MLX server; `EXA_SEARCH_API_KEY` (optional web search) | yes |
| `embeddingsdb` | Retrieval augmented generation using embeddings | | `OPENAI_API_KEY` | yes |
| `RAG` | Agentic RAG using the Gemini API | | `GEMINI_API_KEY` | yes |
| `Malt_deep_learning` | Deep learning in Racket with Malt | `malt` | | no |
| `deep_learning_category_theory` | Category-theory deep learning | | | yes |
| `ReinforcementLearning` | Reinforcement learning in Racket | | | yes |
| `nlp` | Natural language processing | | | yes |
| `kgn` | Knowledge Graph Navigator | | | yes (parsing only) |
| `OPS5_in_Racket` | Implementing OPS5 in Racket | | | yes |
| `Racklog_expert_systems_logic` | Logic programming with Racklog | | | yes |
| `math-pi-e` | Computing pi and e | | | yes |

Not covered by a chapter yet: `genetic_programming` (draft chapter in `CHAPTER.md`), `llm-agent`, `pdf_chat` (needs `pdf-read` and poppler), `search_brave`, `search_tavily`, `Bridge_Game_webkit`. The two `AGENT_SKILLS_*.md` files describe optional setup for using these examples from AI coding agents.

## API keys

Set only the ones for the providers you use.

| Variable | Used for |
|---|---|
| `OPENAI_API_KEY` (or `OPENAI_KEY`) | OpenAI chat and embeddings |
| `GEMINI_API_KEY` (or `GOOGLE_API_KEY`) | Google Gemini |
| `ANTHROPIC_API_KEY` | Anthropic Claude |
| `MISTRAL_API_KEY` | Mistral |
| `DEEPSEEK_API_KEY` | DeepSeek |
| `FIREWORKS_API_KEY` | Fireworks AI |
| `BRAVE_SEARCH_API_KEY`, `TAVILY_API_KEY`, `EXA_SEARCH_API_KEY` | web search tools |

Local models through Ollama or llama.cpp need no key.

Examples that need "a chat model" or "an embedding model" take the name from `llmapis/llmapis.rkt`. To change it everywhere without editing code:

    export LLM_CHAT_MODEL=ollama/qwen3:1.7b
    export LLM_EMBEDDING_MODEL=ollama/nomic-embed-text

## License

The example code is released under the Apache 2 license (see `LICENSE.txt`), with one exception: `coding-agent-harness` is released under AGPL-3.0 (see the `LICENSE` file in that directory).
