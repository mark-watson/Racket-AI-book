#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Unit tests for the Ollama tools libraries.
;;;
;;; These tests need no Ollama server. They exercise the tool handlers and
;;; the dispatch machinery directly, so you can develop and test tools even
;;; while offline.

(require rackunit)
(require json)
(require "tools.rkt")
(require "custom-tools.rkt")

(register-custom-tools)

;;; -----------------------------------------------------------------------------
;;; Calculator tests

(test-case "calculator handles basic arithmetic"
  (check-equal? (eval-arithmetic "2 + 3 * 4") 14)
  (check-equal? (eval-arithmetic "(2 + 3) * 4") 20)
  (check-equal? (eval-arithmetic "2 ^ 10") 1024)
  (check-equal? (eval-arithmetic "12.5 * 640 / 100") 80.0)
  (check-equal? (eval-arithmetic "-4 + 9") 5)
  (check-equal? (eval-arithmetic "17 % 5") 2))

(test-case "calculator errors are returned as strings, not exceptions"
  (check-true (string-prefix? (eval-arithmetic "1 / 0") "Error"))
  (check-true (string-prefix? (eval-arithmetic "(2 +") "Error"))
  (check-true (string-prefix? (eval-arithmetic "1 2 3") "Error")))

(test-case "calculate tool formats results for the model"
  (check-equal? (calculate (hash 'expression "6 * 7")) "6 * 7 = 42"))

;;; -----------------------------------------------------------------------------
;;; Scratchpad tests

(define test-notes-file (build-path (current-directory) "notes.jsonl"))
(when (file-exists? test-notes-file) (delete-file test-notes-file))

(test-case "notes scratchpad round trip"
  (check-equal? (list-notes (hash)) "No notes saved yet.")
  (save-note (hash 'note "test note one"))
  (save-note (hash 'note "test note two"))
  (define listing (list-notes (hash)))
  (check-true (string-contains? listing "test note one"))
  (check-true (string-contains? listing "test note two"))
  (check-true (string-contains? listing "2."))
  (check-equal? (clear-notes (hash)) "All notes deleted.")
  (check-equal? (list-notes (hash)) "No notes saved yet."))

;;; -----------------------------------------------------------------------------
;;; Registry and dispatch tests

(test-case "all expected tools are registered"
  (for ([name '("get_current_datetime" "get_weather" "list_directory"
                "read_file_contents" "search_wikipedia"
                "calculate" "fetch_url" "save_note" "list_notes"
                "clear_notes")])
    (check-not-false (get-tool name) name)))

(test-case "schemas are built in Ollama wire format"
  (define schemas (make-tool-schemas '("calculate")))
  (check-equal? (length schemas) 1)
  (define schema (car schemas))
  (check-equal? (hash-ref schema 'type) "function")
  (define fn (hash-ref schema 'function))
  (check-equal? (hash-ref fn 'name) "calculate")
  (check-true (hash-has-key? fn 'description))
  (define params (hash-ref fn 'parameters))
  (check-equal? (hash-ref params 'required) '("expression")))

(test-case "handle-tool-call dispatches and returns a tool message"
  ;; Ollama returns arguments as a JSON string; make sure we handle both
  ;; that form and the already-parsed hash form.
  (define result-string-args
    (handle-tool-call
     (hash 'function (hash 'name "calculate"
                           'arguments "{\"expression\": \"2 + 2\"}"))))
  (check-equal? (hash-ref result-string-args 'role) "tool")
  (check-equal? (hash-ref result-string-args 'content) "2 + 2 = 4")

  (define result-hash-args
    (handle-tool-call
     (hash 'function (hash 'name "calculate"
                           'arguments (hash 'expression "2 + 2")))))
  (check-equal? (hash-ref result-hash-args 'content) "2 + 2 = 4"))

(test-case "unknown tools produce a tool message, never an exception"
  (define result
    (handle-tool-call
     (hash 'function (hash 'name "nonexistent_tool" 'arguments "{}"))))
  (check-equal? (hash-ref result 'role) "tool")
  (check-true (string-contains? (hash-ref result 'content) "Unknown tool")))

(test-case "datetime tool returns the expected format"
  (check-match (get-current-datetime (hash))
               (pregexp #px"^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")))

(displayln "\nAll tests passed.")
