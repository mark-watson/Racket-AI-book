# Ollama Tools/Function Calling Example

This directory contains a Racket implementation of Ollama's tool/function calling feature.

## Files

- `tools.rkt` - Core library with tool registry, built-in tools, and API communication
- `main.rkt` - Interactive demo program

## Requirements

- Racket 8.0 or later
- Ollama running locally or accessible via HTTP
- A model that supports tool calling (e.g., `llama3.2`, `qwen2.5`, `mistral`)

## Setup

1. Install Ollama and pull a model:
   ```bash
   ollama pull llama3.2
   ```

2. Set environment variables (optional):
   ```bash
   export OLLAMA_MODEL=llama3.2
   export OLLAMA_HOST=http://localhost:11434
   ```

## Usage

Run the interactive demo:

```bash
racket main.rkt
```

Or use the library programmatically:

```racket
#lang racket
(require "tools.rkt")

;; Get weather
(call-ollama-with-tools
 "What's the weather in Phoenix?"
 '("get_weather"))

;; Multiple tools
(call-ollama-with-tools
 "What day is it and what's the weather in London?"
 '("get_current_datetime" "get_weather"))

;; Search Wikipedia
(call-ollama-with-tools
 "Tell me about the Eiffel Tower"
 '("search_wikipedia"))
```

## Built-in Tools

| Tool | Description |
|------|-------------|
| `get_current_datetime` | Returns current date/time |
| `get_weather` | Fetches weather from wttr.in |
| `list_directory` | Lists files in current directory |
| `read_file_contents` | Reads a file's contents |
| `search_wikipedia` | Searches Wikipedia for a summary |

## Creating Custom Tools

```racket
(define (my-custom-tool args)
  ;; args is a hash with the parameters
  (let ([param1 (hash-ref args 'param1)])
    ;; Do something and return a string
    (format "Result: ~a" param1)))

(register-tool
 "my_custom_tool"
 "Description of what it does"
 (hash 'type "object"
       'properties (hash 'param1 (hash 'type "string"))
       'required '("param1"))
 my-custom-tool)
```

## License

Apache 2.0 - Copyright 2026 Mark Watson