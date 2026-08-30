#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Custom Tools for Ollama Tool Calling
;;;
;;; This module shows how to write your own tools and register them with
;;; the library in tools.rkt. It implements:
;;;
;;;   calculate               - evaluate an arithmetic expression
;;;   fetch_url               - fetch a web page and return an excerpt
;;;   save_note / list_notes / clear_notes - a persistent scratchpad
;;;
;;; Tests that need no Ollama server:  racket tests.rkt

(require net/http-easy)
(require json)
(require racket/date)
(require "tools.rkt")

(provide register-custom-tools
         eval-arithmetic
         calculate
         save-note
         list-notes
         clear-notes
         fetch-url
         html->text)

;;; -----------------------------------------------------------------------------
;;; Calculator Tool
;;;
;;; The calculator never passes the model's text to Racket's read or eval.
;;; Instead it tokenizes the expression and parses it with a recursive
;;; descent parser for this grammar:
;;;
;;;   expr   := term (('+' | '-') term)*
;;;   term   := factor (('*' | '/' | '%' | '^') factor)*
;;;   factor := number | '(' expr ')' | '-' factor
;;;
;;; Each parse function returns a cons of (value . remaining-tokens) which
;;; makes backtracking-free parsing straightforward in a functional style.

(define (tokenize s)
  (regexp-match* #px"[0-9]+(\\.[0-9]+)?|[+\\-*/%()^]" s))

(define (apply-op op a b)
  (match op
    ["+" (+ a b)]
    ["-" (- a b)]
    ["*" (* a b)]
    ["/" (if (zero? b) (error "division by zero") (/ a b))]
    ["%" (if (zero? b) (error "modulo by zero") (remainder a b))]
    ["^" (expt a b)]))

(define (eval-arithmetic s)
  "Evaluate an arithmetic string. Returns a number or an error string."
  (with-handlers ([exn:fail? (lambda (e)
                               (format "Error evaluating expression: ~a"
                                       (exn-message e)))])
    (define tokens (tokenize s))
    (define (peek ts) (and (pair? ts) (car ts)))
    (define (parse-expression ts)
      (let loop ([acc (parse-term ts)])
        (define op (peek (cdr acc)))
        (if (and op (member op '("+" "-")))
            (let ([rhs (parse-term (cdr (cdr acc)))])
              (loop (cons (apply-op op (car acc) (car rhs)) (cdr rhs))))
            acc)))
    (define (parse-term ts)
      (let loop ([acc (parse-factor ts)])
        (define op (peek (cdr acc)))
        (if (and op (member op '("*" "/" "%" "^")))
            (let ([rhs (parse-factor (cdr (cdr acc)))])
              (loop (cons (apply-op op (car acc) (car rhs)) (cdr rhs))))
            acc)))
    (define (parse-factor ts)
      (define t (peek ts))
      (cond
        [(not t) (error "unexpected end of expression")]
        [(equal? t "-")
         (define f (parse-factor (cdr ts)))
         (cons (- (car f)) (cdr f))]
        [(equal? t "(")
         (define e (parse-expression (cdr ts)))
         (unless (equal? (peek (cdr e)) ")")
           (error "missing closing parenthesis"))
         (cons (car e) (cdr (cdr e)))]
        [else (cons (or (string->number t)
                        (error (format "not a number: ~a" t)))
                    (cdr ts))]))
    (define parsed (parse-expression tokens))
    (when (pair? (cdr parsed))
      (error "trailing characters in expression"))
    (car parsed)))

(define (calculate args)
  (define expr (hash-ref args 'expression ""))
  (define result (eval-arithmetic expr))
  (if (number? result)
      (format "~a = ~a" expr result)
      result))

;;; -----------------------------------------------------------------------------
;;; URL Fetch Tool
;;;
;;; Fetches a page, strips the HTML, and truncates. Small local models do
;;; much better with a few hundred characters of clean text than with a
;;; full page of raw markup.

(define *fetch-max-chars* 600)

(define (fetch-url args)
  (define url (hash-ref args 'url #f))
  (if (not url)
      "No url provided"
      (with-handlers ([exn:fail? (lambda (e)
                                   (format "Error fetching URL: ~a"
                                           (exn-message e)))])
        (define response
          (get url #:headers (hash 'user-agent "RacketOllamaTools/1.0")))
        (define body (bytes->string/utf-8 (response-body response)))
        (define text (html->text body))
        (string-append
         (substring text 0 (min (string-length text) *fetch-max-chars*))
         (if (> (string-length text) *fetch-max-chars*)
             " ... [truncated]"
             "")))))

(define (html->text html)
  "Very small HTML to text conversion: drop scripts, styles, and tags."
  (define no-scripts
    (regexp-replace* #px"(?s:<script.*?</script>)" html " "))
  (define no-styles
    (regexp-replace* #px"(?s:<style.*?</style>)" no-scripts " "))
  (define no-tags
    (regexp-replace* #px"<[^>]+>" no-styles " "))
  (string-normalize-spaces no-tags))

;;; -----------------------------------------------------------------------------
;;; Notes Scratchpad Tool
;;;
;;; Gives the model persistent memory across runs. Notes are JSON lines in
;;; notes.jsonl inside the current directory. One JSON object per line is
;;; easy to append, easy to read, and easy to inspect by hand.

(define *notes-file* (build-path (current-directory) "notes.jsonl"))

(define (save-note args)
  (define note (hash-ref args 'note ""))
  (define record
    (jsexpr->string
     (hash 'timestamp (date->string (current-date) "~Y-~m-~d ~H:~M:~S")
           'note note)))
  (with-handlers ([exn:fail? (lambda (e)
                               (format "Error saving note: ~a" (exn-message e)))])
    (call-with-output-file *notes-file*
      (lambda (out) (displayln record out))
      #:exists 'append)
    (format "Saved note: ~a" note)))

(define (list-notes args)
  (with-handlers ([exn:fail? (lambda (e)
                               (format "Error listing notes: ~a" (exn-message e)))])
    (if (file-exists? *notes-file*)
        (let ([lines (file->lines *notes-file*)])
          (if (null? lines)
              "No notes saved yet."
              (string-join
               (for/list ([line lines] [i (in-naturals 1)])
                 (define rec (string->jsexpr line))
                 (format "~a. [~a] ~a"
                         i
                         (hash-ref rec 'timestamp "")
                         (hash-ref rec 'note "")))
               "\n")))
        "No notes saved yet.")))

(define (clear-notes args)
  (when (file-exists? *notes-file*)
    (delete-file *notes-file*))
  "All notes deleted.")

;;; -----------------------------------------------------------------------------
;;; Registration

(define (register-custom-tools)
  "Register all tools defined in this file with the tools.rkt registry."
  (register-tool
   "calculate"
   "Evaluate an arithmetic expression. Supports + - * / % ^ and parentheses."
   (hash 'type "object"
         'properties (hash 'expression
                           (hash 'type "string"
                                 'description "Arithmetic expression, e.g. '2 * (3 + 4)'"))
         'required '("expression"))
   calculate)

  (register-tool
   "fetch_url"
   "Fetch a web page and return a short plain-text excerpt"
   (hash 'type "object"
         'properties (hash 'url
                           (hash 'type "string"
                                 'description "Full URL starting with http:// or https://"))
         'required '("url"))
   fetch-url)

  (register-tool
   "save_note"
   "Save a short note to a persistent scratchpad file"
   (hash 'type "object"
         'properties (hash 'note
                           (hash 'type "string"
                                 'description "The note text to save"))
         'required '("note"))
   save-note)

  (register-tool
   "list_notes"
   "List all notes in the persistent scratchpad"
   (hash 'type "object"
         'properties (hash)
         'required '())
   list-notes)

  (register-tool
   "clear_notes"
   "Delete all notes from the persistent scratchpad"
   (hash 'type "object"
         'properties (hash)
         'required '())
   clear-notes))

;;; -----------------------------------------------------------------------------
;;; Example Usage
;;;
;;; Commented out so the file can also be used as a library from tests.rkt.
;;; Requires a running Ollama server and a tool-capable model.

#|
(register-custom-tools)

(displayln (call-ollama-with-tools
            "What is 12.5% of 640?"
            '("calculate")))

(displayln (call-ollama-with-tools
            "Remember that my project deadline is next Friday. Then tell me what you saved."
            '("save_note" "list_notes")))

(displayln (call-ollama-with-tools
            "Fetch https://en.wikipedia.org/wiki/Racket_(programming_language) and tell me what the Racket language is."
            '("fetch_url")))
|#
