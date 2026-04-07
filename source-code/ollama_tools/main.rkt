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
  (displayln (format "Using model: ~a" *default-model*))
  (displayln (format "Ollama host: ~a" *ollama-host*))
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

        [(string=: choice "3")
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