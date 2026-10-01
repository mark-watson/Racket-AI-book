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
(require net/uri-codec)
(require "../llmapis/llmapis.rkt")

(provide register-tool
         get-tool
         call-ollama-with-tools
         make-tool-schemas
         registry->llm-tools
         handle-tool-call
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
  (define d (current-date))
  (define (pad n) (~r n #:min-width 2 #:pad-string "0"))
  (format "~a-~a-~a ~a:~a:~a"
          (date-year d)
          (pad (date-month d))
          (pad (date-day d))
          (pad (date-hour d))
          (pad (date-minute d))
          (pad (date-second d))))

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
  "Lists files in the current directory or specified directory.
   ARGS: optional 'dir_path'"
  (let* ([dir-path (hash-ref args 'dir_path (current-directory))]
         [resolved-dir (simplify-path (path->complete-path dir-path))]
         [resolved-sandbox (simplify-path (path->complete-path (current-directory)))])
    (if (string-prefix? (path->string resolved-sandbox) (path->string resolved-dir))
        (if (directory-exists? resolved-dir)
            (let ([files (directory-list resolved-dir)])
              (format "Files in ~a: ~a"
                      resolved-dir
                      (string-join (map path->string files) ", ")))
            (format "Directory not found: ~a" dir-path))
        (format "Access denied: ~a is outside the sandbox directory" dir-path))))

(define (read-file-contents args)
  "Reads contents of a file.
   ARGS should contain 'file_path' key."
  (let* ([file-path (hash-ref args 'file_path #f)]
         [resolved-path (and file-path (simplify-path (path->complete-path file-path)))]
         [resolved-sandbox (simplify-path (path->complete-path (current-directory)))])
    (if (and resolved-path (string-prefix? (path->string resolved-sandbox) (path->string resolved-path)))
        (if (file-exists? resolved-path)
            (with-handlers ([exn:fail? (lambda (e)
                                         (format "Error reading file: ~a" (exn-message e)))])
              (file->string resolved-path))
            (format "File not found: ~a" file-path))
        (format "Access denied: file path is invalid or outside the sandbox directory"))))

(define (search-wikipedia args)
  "Searches Wikipedia for a query and returns summary.
   ARGS should contain 'query' key."
  (let ([query (hash-ref args 'query #f)])
    (if query
        (with-handlers ([exn:fail? (lambda (e)
                                     (format "Error searching Wikipedia: ~a" (exn-message e)))])
          (let* ([url (format "https://en.wikipedia.org/api/rest_v1/page/summary/~a"
                              (uri-encode (string-replace query " " "_")))]
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
;;; Uniform-API communication (via llmapis.rkt)

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

(define (ollama-model-name model)
  "Uniform-API model address: names with a slash pass through, bare
Ollama tags route to the local Ollama provider."
  (if (regexp-match? #rx"/" model)
      model
      (string-append "ollama/" model)))

(define (ollama-api-base)
  "Uniform-API base URL derived from *ollama-host*."
  (string-append (string-trim (*ollama-host*) "/" #:left? #f) "/v1"))

(define (registry->llm-tools tool-names)
  "Adapt registered tools (by name) to uniform-API llm-tool structs.
Registry handlers already take one args hash, which is exactly the
llm-tool calling convention, so they are reused as-is."
  (for/list ([name tool-names])
    (define tool (get-tool name))
    (unless tool (error (format "Unknown tool: ~a" name)))
    (define params (hash-ref tool 'parameters (hash)))
    (define props (hash-ref params 'properties (hash)))
    (define required (hash-ref params 'required '()))
    (make-llm-tool
     (hash-ref tool 'name)
     (hash-ref tool 'description "")
     (for/list ([(key spec) (in-hash props)])
       (define pname (if (symbol? key) (symbol->string key) (format "~a" key)))
       ;; NOTE: '#:required / '#:enum are quoted data elements of the
       ;; spec list (make-llm-tool parses them), not keyword arguments.
       (list pname
             (hash-ref spec 'type "string")
             (hash-ref spec 'description "")
             '#:required (and (member pname required) #t)
             '#:enum (hash-ref spec 'enum #f)))
     (hash-ref tool 'handler))))

(define (handle-tool-call tool-call)
  "Execute a tool call from the LLM response."
  (with-handlers ([exn:fail? (lambda (e)
                               (hash 'role "tool"
                                     'content (format "Error processing tool call: ~a" (exn-message e))))])
    (let* ([name (hash-ref tool-call 'function (hash))]
           [func-name (hash-ref name 'name #f)]
           [args-str (hash-ref name 'arguments "{}")]
           [args (cond
                   [(hash? args-str) args-str]
                   [(string? args-str) (string->jsexpr args-str)]
                   [else (hash)])]
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
                'content (format "Unknown tool: ~a" func-name))))))

(define (call-ollama-with-tools prompt tool-names #:model [model (*default-model*)])
  "Call Ollama with tools and handle the tool calling loop.
   PROMPT: the user's prompt
   TOOL-NAMES: list of tool names to make available
   MODEL: optional model override (a bare Ollama tag or a full
   \"provider/model\" address)

   Returns the final response text after any tool calls are processed."
  (llm-response-content
   (llm-chat-with-tools (ollama-model-name model)
                        prompt
                        (registry->llm-tools tool-names)
                        #:api-base (ollama-api-base))))

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