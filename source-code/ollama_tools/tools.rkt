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