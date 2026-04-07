# Ollama Tools/Function Calling in Racket

One of the most powerful features of modern LLMs is their ability to call external functions (tools) during a conversation. This allows the model to perform actions beyond just generating text — it can fetch live data, interact with files, call APIs, and more.

Ollama supports tool/function calling through its chat API. When you provide a list of available tools with their schemas, the model can decide to call one or more tools, and your code executes them and returns the results back to the model.

## How Tool Calling Works

The flow is:

1. **You define tools** — functions with JSON schemas describing their parameters
2. **Send request to Ollama** — include the tool definitions and user prompt
3. **Model decides** — if it needs a tool, it returns a `tool_calls` array
4. **You execute the tool** — call your Racket function with the arguments
5. **Return result** — add the tool result to the message history
6. **Model responds** — uses the tool output to generate its final answer

This creates a conversation loop where the LLM can request information it doesn't have.

## A Racket Tools Library

The following code defines a reusable library for Ollama tool calling. It provides:

- A **tool registry** to register functions with their schemas
- **Built-in tools** for common operations (weather, files, Wikipedia)
- **API communication** to call Ollama and handle tool responses

```racket
#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Ollama Tools/Function Calling Example for Racket

(require net/http-easy)
(require json)
(require racket/date)

(provide register-tool
         get-tool
         call-ollama-with-tools
         get-current-datetime
         get-weather
         list-directory
         read-file-contents
         *available-tools*
         *default-model*)

;;; -----------------------------------------------------------------------------
;;; Configuration

(define *default-model* (or (getenv "OLLAMA_MODEL") "qwen3:1.7b"))
(define *ollama-host* (or (getenv "OLLAMA_HOST") "http://localhost:11434"))

;;; -----------------------------------------------------------------------------
;;; Tool Registry

(define *available-tools* (make-hash))

(define (register-tool name description parameters handler)
  "Register a tool that can be called by the LLM.
   NAME: string - the tool name
   DESCRIPTION: string - what the tool does
   PARAMETERS: hash - JSON schema for parameters
   HANDLER: function - Racket function to execute the tool"
  (hash-set! *available-tools* name
             (hash 'name name
                   'description description
                   'parameters parameters
                   'handler handler)))

(define (get-tool name)
  "Get a registered tool by name."
  (hash-ref *available-tools* name #f))

;;; -----------------------------------------------------------------------------
;;; Tool Implementations

(define (get-current-datetime args)
  "Returns the current date and time as a string."
  (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))

(define (get-weather args)
  "Fetches current weather for a location using wttr.in.
   ARGS should contain 'location' key."
  (let ([location (hash-ref args 'location "unknown")])
    (with-handlers ([exn:fail? (lambda (e)
                                 (format "Error fetching weather: ~a" (exn-message e)))])
      (let* ([url (format "https://wttr.in/~a?format=3"
                          (string-replace location " " "+"))]
             [response (get url)]
             [body (response-body response)])
        (string-trim (bytes->string/utf-8 body))))))

(define (list-directory args)
  "Lists files in the current directory."
  (let ([files (directory-list (current-directory))])
    (format "Files in ~a: ~a"
            (current-directory)
            (string-join (map path->string files) ", "))))

(define (read-file-contents args)
  "Reads contents of a file.
   ARGS should contain 'file_path' key."
  (let ([file-path (hash-ref args 'file_path #f)])
    (if (and file-path (file-exists? file-path))
        (with-handlers ([exn:fail? (lambda (e)
                                     (format "Error reading file: ~a" (exn-message e)))])
          (file->string file-path))
        (format "File not found: ~a" file-path))))

(define (search-wikipedia args)
  "Searches Wikipedia for a query and returns summary.
   ARGS should contain 'query' key."
  (let ([query (hash-ref args 'query #f)])
    (if query
        (with-handlers ([exn:fail? (lambda (e)
                                     (format "Error searching Wikipedia: ~a" (exn-message e)))])
          (let* ([url (format "https://en.wikipedia.org/api/rest_v1/page/summary/~a"
                              (string-replace query " " "_"))]
                 [response (get url
                               #:headers (hash 'user-agent "RacketOllamaTools/1.0"))]
                 [data (response-json response)])
            (hash-ref data 'extract "No summary available")))
        "No query provided")))

;;; -----------------------------------------------------------------------------
;;; Register Default Tools

(register-tool
 "get_current_datetime"
 "Get the current date and time"
 (hash 'type "object"
       'properties (hash)
       'required '())
 get-current-datetime)

(register-tool
 "get_weather"
 "Get the current weather for a location"
 (hash 'type "object"
       'properties (hash 'location (hash 'type "string"
                                          'description "City name, e.g., 'London' or 'New York'"))
       'required '("location"))
 get-weather)

(register-tool
 "list_directory"
 "List files in the current directory"
 (hash 'type "object"
       'properties (hash)
       'required '())
 list-directory)

(register-tool
 "read_file_contents"
 "Read the contents of a file"
 (hash 'type "object"
       'properties (hash 'file_path (hash 'type "string"
                                          'description "Path to the file to read"))
       'required '("file_path"))
 read-file-contents)

(register-tool
 "search_wikipedia"
 "Search Wikipedia and return a summary"
 (hash 'type "object"
       'properties (hash 'query (hash 'type "string"
                                      'description "Search query"))
       'required '("query"))
 search-wikipedia)

;;; -----------------------------------------------------------------------------
;;; Ollama API Communication

(define (make-tool-schemas tool-names)
  "Build tool schemas for the Ollama API request."
  (for/list ([name tool-names])
    (let ([tool (get-tool name)])
      (if tool
          (hash 'type "function"
                'function (hash 'name (hash-ref tool 'name)
                               'description (hash-ref tool 'description)
                               'parameters (hash-ref tool 'parameters)))
          (error (format "Unknown tool: ~a" name))))))

(define (call-ollama-api messages tools)
  "Call the Ollama chat API with tools."
  (let* ([data (hash 'model *default-model*
                     'messages messages
                     'tools tools
                     'stream #f)]
         [json-data (jsexpr->string data)]
         [response (post (string-append *ollama-host* "/api/chat")
                        #:data json-data
                        #:headers (hash 'content-type "application/json"))]
         [result (response-json response)])
    result))

(define (handle-tool-call tool-call)
  "Execute a tool call from the LLM response."
  (let* ([name (hash-ref tool-call 'function (hash))]
         [func-name (hash-ref name 'name #f)]
         [args-str (hash-ref name 'arguments "{}")]
         [args (if (string? args-str)
                   (string->jsexpr args-str)
                   args-str)]
         [tool (get-tool func-name)])
    (if tool
        (let ([handler (hash-ref tool 'handler #f)])
          (if handler
              (let ([result (handler args)])
                (hash 'role "tool"
                      'content result))
              (hash 'role "tool"
                    'content (format "No handler for tool: ~a" func-name))))
        (hash 'role "tool"
              'content (format "Unknown tool: ~a" func-name)))))

(define (call-ollama-with-tools prompt tool-names #:model [model *default-model*])
  "Call Ollama with tools and handle the tool calling loop.
   PROMPT: the user's prompt
   TOOL-NAMES: list of tool names to make available
   MODEL: optional model override

   Returns the final response text after any tool calls are processed."
  (parameterize ([*default-model* model])
    (let* ([tools (make-tool-schemas tool-names)]
           [messages (list (hash 'role "user" 'content prompt))])
      (let loop ([msgs messages]
                 [max-iterations 10])
        (if (<= max-iterations 0)
            "Max iterations reached"
            (let* ([response (call-ollama-api msgs tools)]
                   [message (hash-ref response 'message (hash))]
                   [tool-calls (hash-ref message 'tool_calls #f)])
              (if tool-calls
                  ;; Process tool calls and continue
                  (let* ([tool-results (for/list ([tc tool-calls])
                                         (handle-tool-call tc))]
                         [assistant-msg (hash 'role "assistant"
                                              'content (hash-ref message 'content #f)
                                              'tool_calls tool-calls)]
                         [new-msgs (append msgs (list assistant-msg)
                                           tool-results)])
                    (loop new-msgs (- max-iterations 1)))
                  ;; No tool calls, return the content
                  (hash-ref message 'content "No response"))))))))
```

## Using the Tools

Once the library is loaded, using it is straightforward. You pass a prompt and a list of available tool names:

```racket
> (require "tools.rkt")
> (call-ollama-with-tools
    "What is the weather in Phoenix Arizona?"
    '("get_weather"))
"It's currently sunny and 95°F in Phoenix, Arizona."
```

The model decided it needed the `get_weather` tool, called it with `location: "Phoenix Arizona"`, and used the result to answer.

Here's an example with multiple tools available:

```racket
> (call-ollama-with-tools
    "What day is it today and what's the weather in London?"
    '("get_current_datetime" "get_weather"))
"Today is Tuesday, April 7th, 2026. The weather in London is partly cloudy with temperatures around 15°C (59°F)."
```

The model called both tools to fully answer the question.

## Creating Custom Tools

You can register your own tools using `register-tool`. The pattern is:

1. Define a handler function that takes an `args` hash
2. Call `register-tool` with the name, description, JSON schema, and handler

```racket
;; Define a custom tool
(define (calculate-sum args)
  (let ([numbers (hash-ref args 'numbers '())])
    (format "The sum is: ~a" (apply + numbers))))

;; Register it
(register-tool
 "calculate_sum"
 "Calculate the sum of a list of numbers"
 (hash 'type "object"
       'properties (hash 'numbers (hash 'type "array"
                                        'items (hash 'type "number")
                                        'description "List of numbers to sum"))
       'required '("numbers"))
 calculate-sum)

;; Use it
(call-ollama-with-tools
 "What is 23 + 45 + 67?"
 '("calculate_sum"))
```

## Model Selection

Tool calling works best with models specifically trained for it. Ollama models that support tool calling include:

- `llama3.2` (recommended)
- `qwen2.5` series
- `mistral` and `mixtral`

Set the model via environment variable:

```bash
export OLLAMA_MODEL=llama3.2
```

Or pass it directly:

```racket
(call-ollama-with-tools
 "Get the weather in Tokyo"
 '("get_weather")
 #:model "llama3.2")
```

## Complete Example: Interactive Demo

The file `main.rkt` in the `ollama_tools` directory provides an interactive menu for testing the tools:

```racket
#lang racket

(require "tools.rkt")

(define (display-menu)
  (displayln "\n=== Ollama Tools Demo ===")
  (displayln "1. Get current date and time")
  (displayln "2. Get weather for a location")
  (displayln "3. List files in current directory")
  (displayln "4. Read a file")
  (displayln "5. Search Wikipedia")
  (displayln "6. Custom prompt (all tools available)")
  (displayln "7. Exit"))

(define (run-demo)
  (displayln (format "Using model: ~a" *default-model*))
  (newline)
  (let loop ()
    (display-menu)
    (let ([choice (read-line)])
      (cond
        [(string=? choice "1")
         (displayln (call-ollama-with-tools
                      "What is the current date and time?"
                      '("get_current_datetime")))
         (loop)]
        [(string=? choice "2")
         (display "Enter location: ")
         (let ([location (read-line)])
           (displayln (call-ollama-with-tools
                        (format "What is the weather in ~a?" location)
                        '("get_weather"))))
         (loop)]
        ;; ... other options ...
        [(string=? choice "7") (displayln "Goodbye!")]
        [else (loop)]))))

(run-demo)
```

## Comparison with Other Languages

This Racket implementation follows the same pattern as the Hy and Common Lisp examples in my other books:

| Language | File | Key Features |
|----------|------|--------------|
| **Racket** | `ollama_tools/tools.rkt` | Tool registry, handlers, API loop |
| **Hy** | `ollama_examples/ollama_tools_examples.hy` | Uses `ollama` Python library |
| **Common Lisp** | `ollama/ollama-tools.lisp` | CLOS structures for tools |

The Racket version is concise thanks to Racket's hash tables and functional style. The `for/list` form makes processing tool calls clean, and `with-handlers` provides elegant error handling for tool execution.

## Summary

Tool calling transforms LLMs from passive text generators into active agents that can:

- **Access live data** — weather, news, stock prices
- **Interact with the system** — read/write files, run commands
- **Call external APIs** — databases, web services
- **Chain operations** — multiple tools in sequence

This is foundational for building AI agents and assistants. In the next chapter on agents, we'll see how tools enable more complex autonomous behavior.