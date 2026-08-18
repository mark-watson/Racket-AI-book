# coding-agent (Racket)

> **AI pair programmer in your terminal, written in Racket** — a multi-turn agentic loop (`read_file`, `list_dir`, `grep`, `run_shell`, `propose_edit`) with human-approved colored diffs, ESC-to-interrupt, and [Fireworks AI](https://fireworks.ai) under the hood.

This project is documented in my book "Practical Artificial Intelligence Development With Racket" [https://leanpub.com/racket-ai](https://leanpub.com/racket-ai), and is a Racket port of Mark Watson's experimental Python [py-coding-agent](https://github.com/mark-watson/py-coding-agent) project.


## Features

- Multi-turn agentic loop with five coding tools: `read_file`, `list_dir`, `grep`, `run_shell`, `propose_edit`
- Colorized unified diffs with `y/n/s` approval before any file is touched
- `make check` gate — runs after every accepted edit and reports failures back to the model
- Intent classifier (keyword heuristic + LLM fallback) routes general questions vs. coding tasks
- **ESC to interrupt** — press ESC at any time to stop the agent after the current tool step; terminal state is always restored

## Requirements

- Racket 8.11+ (`racket --version`)
- `FIREWORKS_API_KEY` environment variable (Fireworks cloud provider), **or** a local [Ollama](https://ollama.com) server (no API key needed)
- Packages: `http-easy` (install with `raco pkg install --auto http-easy` if needed)

## Setup

### 1. Install Racket

Download Racket 8.11+ from <https://download.racket-lang.org/> and run the installer, or use your OS package manager:

```bash
# Debian/Ubuntu
sudo apt install racket

# macOS (Homebrew)
brew install --cask racket
```

Verify the install:

```bash
racket --version
```

### 2. Install the `http-easy` package

`fireworks-ai.rkt` requires `net/http-easy`, which is **not** part of Racket's standard distribution — it must be installed as a third-party package:

```bash
raco pkg install --auto http-easy
```

The `--auto` flag accepts dependencies (`http-easy-lib`) without prompting.

Verify it loads:

```bash
racket -e '(require net/http-easy)'
```

### 3. Set API keys

The agent reads `FIREWORKS_API_KEY` from the environment and uses it for every API call:

```bash
export FIREWORKS_API_KEY=your_key_here
```

Optional web-search backends (only needed for the `/search` commands):

```bash
export BRAVE_SEARCH_API_KEY=your_key
export EXA_SEARCH_API_KEY=your_key
```

To make the keys persist across shell sessions, add the `export` lines to your `~/.bashrc` or `~/.zshrc`.

### 4. Verify the setup

```bash
make check   # byte-compiles all modules
make run     # starts the REPL
```

## Usage

```bash
make run         # start the REPL
```

Or directly:

```bash
racket agent.rkt
```

## Building a standalone executable

```bash
make make-executable   # builds ./coding-agent
```

The executable runs from any directory on this machine. If you ever want to copy it to a machine without Racket, you'd need `raco distribute` to package the runtime alongside it.

## REPL commands

| Command | Description |
|---|---|
| `/reset` | Clear conversation history |
| `/history` | Dump the full message log |
| `/context` | Show a formatted summary of the current context |
| `/compact` | Compact history into a summary, then show the new context |
| `/model <id>` | Switch model (for the current provider) |
| `/provider` | Show current LLM provider |
| `/provider fireworks` / `/provider ollama` | Switch between Fireworks (cloud) and Ollama (local) |
| `/debug` | Toggle raw request/response logging |
| `/search` | Toggle web search on/off |
| `/search brave` | Switch to Brave Search (requires `BRAVE_SEARCH_API_KEY`) |
| `/search exa` | Switch to Exa AI search (requires `EXA_SEARCH_API_KEY`) |
| `/tokens` | Show session token usage and estimated cost |
| `/quit` | Exit |
| `ESC` | Interrupt the running task (stops before the next tool call) |

## Default model

Fireworks provider: `accounts/fireworks/models/deepseek-v4-flash-0731`

Change it at the `> ` prompt with `/model <id>` or edit `FIREWORKS-MODEL` in `fireworks-ai.rkt`.

## Local models with Ollama

The agent can run entirely against a local [Ollama](https://ollama.com) server instead of Fireworks — no API key, no usage cost:

```bash
ollama pull nemotron-3.5-lightning:30b-mlx   # default local model
AGENT_PROVIDER=ollama make run               # or: /provider ollama at the prompt
```

The Ollama provider (`ollama-ai.rkt`) talks to `http://localhost:11434/api/chat` (non-streaming, tool-calling enabled) and normalizes responses into the same shape the Fireworks client produces, so both backends share the agentic loop in `chat-loop.rkt`. Defaults: model `nemotron-3.5-lightning:30b-mlx` (`OLLAMA-MODEL` parameter), "thinking" disabled (`OLLAMA-THINK` parameter). Switch the local model at the prompt with `/model <name>` after `/provider ollama`.

## Project layout

```
agent.rkt          REPL loop, intent classifier, search integration, ESC interrupt, provider dispatch
fireworks-ai.rkt   Fireworks API client (SSE streaming), session stats, chat/chat-with-tools
ollama-ai.rkt      Local Ollama API client (/api/chat), session stats, chat/chat-with-tools
chat-loop.rkt      Provider-agnostic agentic tool-calling loop shared by both backends
tools.rkt          Tool registry + read_file/list_dir/grep/run_shell/propose_edit
approval.rkt       Colored diff printer, y/n/s prompt, ESC-aware line reader
search.rkt         Brave Search and Exa AI search backends
Makefile           run / lint / check targets
```

## Notes on the Racket port

- `fireworks-ai.rkt` mirrors `fireworks_ai.py`: same endpoint, pricing, token accounting (with a semaphore for thread safety), and `task-interrupted?` flag shared with `tools.rkt`/`approval.rkt` via `dynamic-require` to avoid a circular dependency.
- `tools.rkt` uses `racket/subprocess` for `grep`/`make check`/shell commands and `racket/file` for directory listing and file I/O. `propose_edit` follows the same stale-base / no-op / empty-file guards as the Python version.
- `approval.rkt` runs `diff -u` via temp files and `stty raw -echo` / `stty sane` for ESC-aware `y/n/s` prompts, falling back to cooked `read-line` when not on a TTY.
- `agent.rkt` replicates the Python REPL, heuristic + LLM intent classifier, and the thread + `stty` ESC poll loop.

## License

AGPL-3.0 — GNU Affero General Public License v3.0 — Copyright (C) 2026 Mark Watson <markw@markwatson.com>. See [LICENSE](LICENSE) for the full text.
