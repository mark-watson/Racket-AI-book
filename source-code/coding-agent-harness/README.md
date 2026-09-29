# coding-agent (Racket)

> **AI pair programmer in your terminal, written in Racket** — a multi-turn agentic loop (`read_file`, `list_dir`, `grep`, `run_shell`, `propose_edit`) with human-approved colored diffs, and [Fireworks AI](https://fireworks.ai) under the hood.

This project is documented in my book "Practical Artificial Intelligence Development With Racket" [https://leanpub.com/racket-ai](https://leanpub.com/racket-ai), and is a Racket port of Mark Watson's experimental Python [py-coding-agent](https://github.com/mark-watson/py-coding-agent) project.


## Features

- Multi-turn agentic loop with five coding tools: `read_file`, `list_dir`, `grep`, `run_shell`, `propose_edit`
- Colorized unified diffs with `y/n/s` approval before any file is touched
- `make check` gate — runs after every accepted edit and reports failures back to the model
- Intent classifier (keyword heuristic + LLM fallback) routes general questions vs. coding tasks

## Requirements

- Racket 8.11+ (`racket --version`)
- `FIREWORKS_API_KEY` environment variable (Fireworks cloud provider), **or** a local [MLX](https://github.com/ml-explore/mlx) server via `mlx_lm.server` (no API key needed)
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

### Quick CLI (one-shot) — sections 2-4

The same binary works as a Unix CLI. If any prompt is given, it runs once and exits; otherwise it drops into the REPL.

```bash
racket agent.rkt --help                    # usage, exits 0
racket agent.rkt --version                 # coding-agent 0.2.0
racket agent.rkt "fix bug in tools.rkt"    # one-shot positional
racket agent.rkt -p "refactor foo" -p "add tests"  # repeatable
echo "explain approval.rkt" | racket agent.rkt --stdin  # pipe
cat task.txt | racket agent.rkt --stdin --quiet --plain > out.txt  # script
racket agent.rkt --cwd /tmp/proj "summarize"  # run in another dir
racket agent.rkt --provider mlx --model "mlx-community/gemma-4-26B-A4B-it-OptiQ-4bit" "hi"
racket agent.rkt -y "apply the fix"        # auto-approve diffs (still shows diff)
racket agent.rkt --dry-run "show what would change"
racket agent.rkt --quiet --plain -p "hello"  # no banner, no ANSI, no [intent] line
```

Flags (high-value, easy):

```
-p, --prompt TEXT     prompt text, repeatable, joined with newlines
--stdin               read prompt from stdin (pipe/heredoc)
-y, --yes             auto-approve propose_edit after showing diff
--dry-run             show diffs but do not write files
--provider FW|mlx     override AGENT_PROVIDER
--model ID            override model for this provider
--cwd DIR             chdir before running
--debug               enable debug logging (same as /debug)
-q, --quiet           no banner, no [intent] line, less tool chatter
--plain, --no-color   no ANSI colors in diffs
-v, --version         show version and exit
-h, --help            show help and exit
```

Exit codes (script-friendly, section 5): `0` ok, `1` model/network error, `2` `make check` failed, `3` rejected/skipped, `5` bad args.

## Hierarchical configuration (harness config)

Two JSON config layers are merged at startup — project-local wins over global:

* **Global:** `~/.coding_harness.json` — base configuration
* **Local:**  `.local_coding_harness.json` in the project directory — optional per-project override. Nested objects merge recursively; scalar/array values are replaced wholesale by the local value.

The format follows the rough shape of the Pi coding-harness config: named **providers** with explicit type/endpoint/model/generation/pricing parameters. The global config is in ~/.coding_harness.json and if a local config file .local_coding_harness.json exists it overrides the global file. Example:


```json
{
  "default_provider": "omlx",
  "providers": {
    "fireworks": {
      "type": "openai",
      "endpoint": "https://api.fireworks.ai/inference/v1/chat/completions",
      "api_key_env": "FIREWORKS_API_KEY",
      "model": "accounts/fireworks/models/deepseek-v4p1-flash",
      "generation": {
        "temperature": 0.6,
        "max_tokens": 32768
      },
      "pricing": {
        "input": 0.14,
        "cached_input": 0.028,
        "output": 0.28
      }
    },
    "deepseek": {
      "type": "openai",
      "endpoint": "https://api.deepseek.com/v1/chat/completions",
      "api_key_env": "DEEPSEEK_API_KEY",
      "model": "deepseek-flash",
      "generation": {
        "temperature": 0.6,
        "max_tokens": 32768
      },
      "pricing": {
        "input": 0.15,
        "cached_input": 0.003,
        "output": 0.6
      }
    },
    "mlx": {
      "type": "mlx",
      "endpoint": "http://localhost:11434/v1/chat/completions",
      "model": "qwen3.8:27b-mlx",
      "generation": {
        "temperature": 0.6,
        "max_tokens": 32768
      }
    },
    "omlx": {
      "type": "mlx",
      "endpoint": "http://127.0.0.1:8000/v1/chat/completions",
      "model": "mlx-community--Qwen3.6-35B-A3B-4bit",
      "generation": {
        "temperature": 0.6,
        "max_tokens": 32768
      }
    },
    "sushi": {
      "type": "mlx",
      "endpoint": "http://127.0.0.1:12345/v1/chat/completions",
      "model": "Qwen3.8-Flash-Next-Sushi-2.6bpw",
      "generation": {
        "temperature": 1.0,
        "max_tokens": 32000
      }
    }
  }
}
```

Notes:

* Provider **type** is `"mlx"` (the local `mlx-serve` backend — OpenAI-style `/v1/chat/completions` served by `mlx_lm.server` on port 11434, oMLX on port 8000, sushi on port 12345, optionally with a Bearer `api_key_env` for a remote endpoint) or `"openai"` (OpenAI-style chat completions — Fireworks.ai and compatible endpoints). The type strings `"ollama"`, `"omlx"`, and `"sushi"` are also accepted and mapped to `"mlx"`.
* Switch profiles at the prompt with `/provider <name>` (e.g. `/provider omlx` or `/provider sushi`) — `/provider` alone lists all profiles and the active one.
* **Providers live only in the config files.** No provider data — endpoint, model, `api_key_env`, generation parameters, or pricing — is compiled into the Racket code, so adding or changing a provider needs no code change. With no providers configured the harness refuses to start and prints the config file locations.
* Optional per-profile **`pricing`** gives `/tokens` its cost estimate: `input`, `cached_input`, and `output` in USD per 1M tokens. A profile without it reports token counts but no cost estimate, rather than guessing a rate.

Legacy (still supported, section 9, precedence CLI > env > file > profile):

* File: `$XDG_CONFIG_HOME/coding-agent/config.rktd` or `~/.config/coding-agent/config.rktd` (also `config.rkt`). Contents is a hash literal, e.g. `#hash((provider . "mlx") (model . "my-model") (quiet . #t) (plain . #t))` or an alist `'((provider . "mlx"))`. Unknown keys ignored.
* Env: `AGENT_PROVIDER`, `CODING_AGENT_PROVIDER`, `CODING_AGENT_MODEL`, `CODING_AGENT_QUIET`, `CODING_AGENT_PLAIN`, `CODING_AGENT_DEBUG`.

Safety (section 10): `-y` still prints the colored diff (or plain) before applying; `--dry-run` never writes.

## Building a standalone executable

```bash
make make-executable   # builds ./coding-agent
make install PREFIX=/usr/local  # copy to $PREFIX/bin
make completions       # bash/zsh/fish into ./completions/
make dist              # raco distribute -> ./dist/
```

The executable runs from any directory on this machine. If you ever want to copy it to a machine without Racket, use `make dist` to package the runtime alongside it.

## REPL commands

| Command | Description |
|---|---|
| `/reset` | Clear conversation history |
| `/history` | Dump the full message log |
| `/context` | Show a formatted summary of the current context |
| `/compact` | Compact history into a summary, then show the new context |
| `/model <id>` | Switch model (for the current provider) |
| `/provider` | Show current provider and available profiles |
| `/provider fireworks` / `/provider mlx` / `/provider <profile>` | Switch provider profile (from harness config) or legacy built-in |
| `/debug` | Toggle raw request/response logging |
| `/search` | Toggle web search on/off |
| `/search brave` | Switch to Brave Search (requires `BRAVE_SEARCH_API_KEY`) |
| `/search exa` | Switch to Exa AI search (requires `EXA_SEARCH_API_KEY`) |
| `/tokens` | Show session token usage and estimated cost |
| `/quit` | Exit |

## Line editing and history

When stdin is a terminal, the REPL reads input through Racket's bundled `readline` collection (Editline/libedit by default, GNU Readline if installed), giving rlwrap-style editing in-process:

| Key | Action |
|---|---|
| Left/Right, `Ctrl-A` / `Ctrl-E`, `Ctrl-K` / `Ctrl-U`, `Ctrl-W` | Cursor movement and line editing |
| Up / Down | Previous / next input line |
| `Ctrl-R` | Reverse history search |
| `Tab` | Complete commands, provider profiles, search engines, and model ids |

* History persists to `~/.coding_agent_history` — the last 1000 entries, written when the REPL exits.
* `Tab` on a word starting with `/` completes slash commands; elsewhere it completes provider profiles (so `/provider ml` + `Tab` → `mlx`), search engines (`brave`, `exa`), and model ids from the harness config.
* Piped input (`--stdin`, `-p`, `cmd | coding-agent`) is unaffected: the prompt is printed by hand and plain `read-line` is used, so scripted output is byte-for-byte what it was before.
* If no Editline/Readline shared library is present — or the `readline` collection is unavailable for any reason — the REPL silently falls back to plain line input instead of failing to start.
* `make make-executable` passes `++lib readline/readline` to `raco exe` so the standalone executable keeps line editing. That embeds the module without instantiating it, so a machine without libedit still falls back cleanly rather than failing at build or startup time.

## Default model

DeepSeek Flash is available from two providers, both declared in `.local_coding_harness.json`:

| Profile | Endpoint | Model | API key |
|---|---|---|---|
| `fireworks` | `api.fireworks.ai` | `accounts/fireworks/models/deepseek-v4p1-flash` | `FIREWORKS_API_KEY` |
| `deepseek` | `api.deepseek.com` | `deepseek-flash` | `DEEPSEEK_API_KEY` |

Switch between them at the `> ` prompt with `/provider deepseek` or `/provider fireworks` (or start with `--provider deepseek`). Change the model with `/model <id>` for the session, or edit the profile's `model` field in the harness config.

## Local models with MLX

The agent can run entirely against a local [MLX](https://github.com/ml-explore/mlx) server via `mlx_lm.server` instead of Fireworks — no API key, no usage cost:

```bash
mlx_lm.server --model mlx-community/gemma-4-26B-A4B-it-OptiQ-4bit --port 11434
AGENT_PROVIDER=mlx make run               # or: /provider mlx at the prompt
```

The MLX provider (`mlx-serve.rkt`) talks to `http://localhost:11434/v1/chat/completions` — the OpenAI-compatible endpoint that `mlx_lm.server` (and Ollama's own OpenAI shim) exposes. It is non-streaming, tool-calling enabled, and returns the same shape the Fireworks client produces, so both backends share the agentic loop in `chat-loop.rkt`. The endpoint, model, and generation parameters all come from the `mlx` provider profile in the harness config. Switch the local model at the prompt with `/model <name>` after `/provider mlx`, or edit the profile.

## Project layout

```
agent.rkt          REPL loop, intent classifier, search integration, provider dispatch
harness-config.rkt Hierarchical JSON config (~/.coding_harness.json + .local_coding_harness.json): providers, generation params
fireworks-ai.rkt   Fireworks API client (SSE streaming), session stats, chat/chat-with-tools
mlx-serve.rkt      Local MLX client (mlx_lm.server /v1/chat/completions) — session stats, chat/chat-with-tools
chat-loop.rkt      Provider-agnostic agentic tool-calling loop shared by both backends
tools.rkt          Tool registry + read_file/list_dir/grep/run_shell/propose_edit
approval.rkt       Colored diff printer, y/n/s prompt
search.rkt         Brave Search and Exa AI search backends
line-input.rkt     Optional readline-backed line editing, history, and Tab completion
Makefile           run / lint / check targets
```

## Notes on the Racket port

- `fireworks-ai.rkt` mirrors `fireworks_ai.py`: same endpoint, pricing, and token accounting (with a semaphore for thread safety).
- `tools.rkt` uses `racket/subprocess` for `grep`/`make check`/shell commands and `racket/file` for directory listing and file I/O. `propose_edit` follows the same stale-base / no-op / empty-file guards as the Python version.
- `approval.rkt` runs `diff -u` via temp files and prompts `y/n/s`, falling back to cooked `read-line` when not on a TTY.
- `agent.rkt` replicates the Python REPL and heuristic + LLM intent classifier.

## License

AGPL-3.0 — GNU Affero General Public License v3.0 — Copyright (C) 2026 Mark Watson <markw@markwatson.com>. See [LICENSE](LICENSE) for the full text.
