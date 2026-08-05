# coding-agent (Racket)

An interactive coding-agent REPL backed by [Fireworks AI](https://fireworks.ai).
Racket port of [py-coding-agent](https://github.com/mark-watson/py-coding-agent)
(python translation of [cl-coding-agent](https://github.com/mark-watson/cl-coding-agent)).

## Features

- Multi-turn agentic loop with five coding tools: `read_file`, `list_dir`, `grep`, `run_shell`, `propose_edit`
- Colorized unified diffs with `y/n/s` approval before any file is touched
- `make check` gate — runs after every accepted edit and reports failures back to the model
- Intent classifier (keyword heuristic + LLM fallback) routes general questions vs. coding tasks
- **ESC to interrupt** — press ESC at any time to stop the agent after the current tool step; terminal state is always restored

## Requirements

- Racket 8.11+ (`racket --version`)
- `FIREWORKS_API_KEY` environment variable
- Packages: `http-easy` (install with `raco pkg install http-easy` if needed)

## Setup

```bash
export FIREWORKS_API_KEY=your_key_here
# optional search keys:
export BRAVE_SEARCH_API_KEY=your_key
export EXA_SEARCH_API_KEY=your_key
```

## Usage

```bash
make run         # start the REPL
```

Or directly:

```bash
racket agent.rkt
```

## REPL commands

| Command | Description |
|---|---|
| `/reset` | Clear conversation history |
| `/history` | Dump the full message log |
| `/model <id>` | Switch Fireworks model |
| `/debug` | Toggle raw request/response logging |
| `/search` | Toggle web search on/off |
| `/search brave` | Switch to Brave Search (requires `BRAVE_SEARCH_API_KEY`) |
| `/search exa` | Switch to Exa AI search (requires `EXA_SEARCH_API_KEY`) |
| `/tokens` | Show session token usage and estimated cost |
| `/quit` | Exit |
| `ESC` | Interrupt the running task (stops before the next tool call) |

## Default model

`accounts/fireworks/models/deepseek-v4-flash-0731`

Change it at the `> ` prompt with `/model <id>` or edit `FIREWORKS-MODEL` in `fireworks-ai.rkt`.

## Project layout

```
agent.rkt          REPL loop, intent classifier, search integration, ESC interrupt
fireworks-ai.rkt   Fireworks API client, session stats, chat/chat-with-tools
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

Apache 2.0 — Copyright (C) 2026 Mark Watson <markw@markwatson.com>
