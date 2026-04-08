# Ollama Tools/Function Calling in Racket

One of the most powerful features of modern LLMs is their ability to call external functions (tools) during a conversation. This allows the model to perform actions beyond just generating text — it can fetch live data, interact with files, call APIs, and more.

Ollama supports tool/function calling through its chat API. When you provide a list of available tools with their schemas, the model can decide to call one or more tools, and your code executes them and returns the results back to the model.

The examples for this chapter are in the directory **Racket-AI-book/source-code/ollama_tools**.


## How Tool Calling Works

The flow is:

1. **You define tools** — functions with JSON schemas describing their parameters
2. **Send request to Ollama** — include the tool definitions and user prompt
3. **Model decides** — if it needs a tool, it returns a `tool_calls` array
4. **You execute the tool** — call your Racket function with the arguments
5. **Return result** — add the tool result to the message history
6. **Model responds** — uses the tool output to generate its final answer

This creates a conversation loop where the LLM can request information it doesn't have intrinsically from its training data.

## A Racket Tools Library

The following code defines a reusable library for Ollama tool calling. It provides:

- A **tool registry** to register functions with their schemas
- **Built-in tools** for common operations (weather, files, Wikipedia)
- **API communication** to call Ollama and handle tool responses

This example demonstrates how to bridge the gap between Large Language Models and local system capabilities by implementing a tool-calling framework in Racket. The code provides a structured way to register Racket functions as "tools" that Ollama-hosted models can invoke to perform real-world tasks such as fetching live weather data, searching Wikipedia, or interacting with the local file system. By defining a clear registry system and using JSON schema for parameter validation, the module automates the complex loop of sending prompts to the LLM, parsing its request for a function call, executing the corresponding Racket code, and returning the results back to the model for a final synthesis. This pattern is essential for building "agentic" applications where the AI is not just a chatbot, but a functional interface capable of executing logic and retrieving dynamic data.

The following file **tools.rkt** contains both the library code for creating and using tools and also example tool implementations:

```racket
#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Ollama Tools/Function Calling Example for Racket
;;;
;;; This module demonstrates how to use Ollama's tool/function calling
;;; capability from Racket. It defines tools (functions) that the LLM
;;; can call, registers them, and handles the tool call flow.

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
         *ollama-host*
         *default-model*)

;;; -----------------------------------------------------------------------------
;;; Configuration

(define *default-model* (make-parameter (or (getenv "OLLAMA_MODEL") "qwen3:1.7b")))
(define *ollama-host* (make-parameter (or (getenv "OLLAMA_HOST") "http://localhost:11434")))

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
  "Lists files in the current directory.
   ARGS: empty hash (no parameters needed)"
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
  "Call the Ollama chat API with tools.
   MESSAGES: list of message hashes with 'role and 'content
   TOOLS: list of tool schemas"
  (let* ([data (hash 'model (*default-model*)
                     'messages messages
                     'tools tools
                     'stream #f)]
         [json-data (jsexpr->string data)]
         [response (post (string-append (*ollama-host*) "/api/chat")
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

(define (call-ollama-with-tools prompt tool-names #:model [model (*default-model*)])
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

;;; -----------------------------------------------------------------------------
;;; Example Usage (commented out for library use)

#|
(require "tools.rkt")

;; Example 1: Get current date/time
(displayln (call-ollama-with-tools
            "What is the current date and time?"
            '("get_current_datetime")))

;; Example 2: Get weather
(displayln (call-ollama-with-tools
            "What is the weather in Phoenix Arizona?"
            '("get_weather")))

;; Example 3: Multiple tools available
(displayln (call-ollama-with-tools
            "Tell me about the Eiffel Tower"
            '("get_weather" "search_wikipedia" "get_current_datetime")))

;; Example 4: List files
(displayln (call-ollama-with-tools
            "What files are in the current directory?"
            '("list_directory")))
|#
```

This tool use implementation relies on a central registry, **available-tools** which stores tool metadata and their associated handler functions. When a user sends a prompt, the `call-ollama-with-tools` function packages the available tool definitions into the format expected by the Ollama API. The model then decides whether to answer the query directly or request a tool execution. If the model provides a tool_calls object, the Racket handler dynamically dispatches the request to the local function, processes the output, and feeds it back into the conversation history.

A key technical highlight is the use of the `net/http-easy` and `json` libraries to manage the RESTful communication with the Ollama service. The recursive loop within `call-ollama-with-tools` ensures that the system can handle multi-step reasoning where a model might need to call one tool to get a piece of information before calling another to complete the task. This robust structure allows developers to expand the LLM's capabilities indefinitely by simply registering new Racket functions to the registry.

## Complete Example Using the Tools Library and Example Tools

Here we use the example tool that we previously saw implemented in the file **tools.rkt**.

The file `main.rkt` in the `ollama_tools` directory provides an interactive menu for testing the tools:

```racket
#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Ollama Tools Example - Interactive Demo
;;;
;;; Run with: racket main.rkt

(require "tools.rkt")

(define (display-menu)
  (displayln "\n=== Ollama Tools Demo ===")
  (displayln "1. Get current date and time")
  (displayln "2. Get weather for a location")
  (displayln "3. List files in current directory")
  (displayln "4. Read a file")
  (displayln "5. Search Wikipedia")
  (displayln "6. Custom prompt (all tools available)")
  (displayln "7. Exit")
  (display "Select option: "))

(define (run-demo)
  (displayln (format "Using model: ~a" (*default-model*)))
  (displayln (format "Ollama host: ~a" (*ollama-host*)))
  (displayln "Make sure Ollama is running and the model is pulled.")
  (newline)

  (let loop ()
    (display-menu)
    (let ([choice (read-line)])
      (cond
        [(string=? choice "1")
         (displayln "\n>>> Calling get_current_datetime...")
         (displayln (call-ollama-with-tools
                     "What is the current date and time?"
                     '("get_current_datetime")))
         (loop)]

        [(string=? choice "2")
         (display "Enter location: ")
         (let ([location (read-line)])
           (displayln (format "\n>>> Getting weather for ~a..." location))
           (displayln (call-ollama-with-tools
                       (format "What is the weather in ~a?" location)
                       '("get_weather"))))
         (loop)]

        [(string=? choice "3")
         (displayln "\n>>> Listing directory...")
         (displayln (call-ollama-with-tools
                     "What files are in the current directory?"
                     '("list_directory")))
         (loop)]

        [(string=? choice "4")
         (display "Enter file path: ")
         (let ([filepath (read-line)])
           (displayln (format "\n>>> Reading ~a..." filepath))
           (displayln (call-ollama-with-tools
                       (format "Read the contents of ~a and summarize it" filepath)
                       '("read_file_contents"))))
         (loop)]

        [(string=? choice "5")
         (display "Enter search query: ")
         (let ([query (read-line)])
           (displayln (format "\n>>> Searching Wikipedia for ~a..." query))
           (displayln (call-ollama-with-tools
                       (format "Tell me about ~a" query)
                       '("search_wikipedia"))))
         (loop)]

        [(string=? choice "6")
         (display "Enter your prompt: ")
         (let ([prompt (read-line)])
           (displayln "\n>>> Processing with all tools...")
           (displayln (call-ollama-with-tools
                       prompt
                       '("get_current_datetime" "get_weather" 
                         "list_directory" "read_file_contents" 
                         "search_wikipedia"))))
         (loop)]

        [(string=? choice "7")
         (displayln "Goodbye!")]

        [else
         (displayln "Invalid choice, try again.")
         (loop)]))))

(run-demo)
```


## Summary

Tool calling transforms LLMs from passive text generators into active agents that can:

- **Access live data** — weather, news, stock prices
- **Interact with the system** — read/write files, run commands
- **Call external APIs** — databases, web services
- **Chain operations** — multiple tools in sequence

This is foundational for building AI agents and assistants. In the next chapter on agents, we'll see how tools enable more complex autonomous behavior.
