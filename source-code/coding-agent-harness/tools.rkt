#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; tools.rkt -- tool registry and coding-agent tools
;;; Racket port of py-coding-agent/tools.py
;;;
;;; Five tools: read_file, list_dir, grep, run_shell, propose_edit
;;; propose_edit shows a colored diff, asks y/n/s, and gates on `make check`.

(require racket/file
         racket/port
         racket/string
         racket/system
         racket/list
         json
         "approval.rkt")

(provide define-tool
         render-tools
         execute-tool-calls
         register-all
         ENABLED-TOOLS
         auto-approve?
         dry-run?
         quiet-mode?)

;; ---------------------------------------------------------------------------
;; Registry

(define registry (make-hash))

(define SHELL-WHITELIST (set "make" "ls" "pwd" "cat" "uv"))
(define MAX-CHECK-OUTPUT-CHARS 2000)

;; CLI-controlled modes
(define auto-approve? (make-parameter #f))
(define dry-run? (make-parameter #f))
(define quiet-mode? (make-parameter #f))

(define (define-tool name params description handler)
  ;; params : list of (list pname ptype pdesc)
  (hash-set! registry name
             (hash 'name name
                   'description description
                   'parameters params
                   'handler handler)))

(define (render-tools names)
  (for/list ([name (in-list names)])
    (define tool (hash-ref registry name #f))
    (unless tool (error 'render-tools "Undefined tool: ~a" name))
    (define props (make-hash))
    (define required '())
    (for ([p (in-list (hash-ref tool 'parameters))])
      (define pname (first p))
      (define ptype (second p))
      (define pdesc (third p))
      (hash-set! props (string->symbol pname)
                 (hash 'type ptype 'description pdesc))
      ;; required must be a JSON array of strings, not symbols
      (set! required (cons pname required)))
    (hash 'type "function"
          'function (hash 'name (hash-ref tool 'name)
                          'description (hash-ref tool 'description)
                          'parameters (hash 'type "object"
                                            'properties props
                                            'required (reverse required))))))

;; ---------------------------------------------------------------------------
;; Tool dispatch

(define (call-tool name args)
  (define tool (hash-ref registry name #f))
  (unless tool (error 'call-tool "Unknown tool: ~a" name))
  (define params (hash-ref tool 'parameters))
  ;; Missing required args? Return an actionable error describing the expected
  ;; argument list -- small models frequently emit malformed/truncated
  ;; arguments, and silently receiving #f tends to send them into retry loops.
  (define missing
    (for/list ([p (in-list params)]
               #:when (not (hash-ref args (string->symbol (first p)) #f)))
      (first p)))
  (cond
    [(pair? missing)
     (format "Error: tool '~a' missing required argument(s): ~a. Expected arguments (JSON object): ~a"
             name
             (string-join missing ", ")
             (string-join (for/list ([p (in-list params)]) (first p)) ", "))]
    [else
     (define positional
       (for/list ([p (in-list params)])
         (hash-ref args (string->symbol (first p)) #f)))
     (with-handlers ([exn:fail? (lambda (e) (format "Error: tool '~a' raised: ~a  (check argument types/values)"
                                                    name (exn-message e)))])
       (define result (apply (hash-ref tool 'handler) positional))
       (if result (format "~a" result) ""))]))

(define (execute-tool-calls tool-calls)
  ;; tool-calls : list of hashes with 'id, 'function {name, arguments}
  ;; Returns list of (list call-id name result-str)
  (define results '())
  (for ([call (in-list tool-calls)])
    (define call-id (hash-ref call 'id ""))
    (define func (hash-ref call 'function (hash)))
    (define name (hash-ref func 'name ""))
    (define args-json (hash-ref func 'arguments "{}"))
    (define short
      (if (<= (string-length args-json) 120)
          args-json
          (string-append (substring args-json 0 117) "...")))
    (unless (quiet-mode?)
      (displayln (format "* ~a ~a" name short)))
    (define args-parsed
      (with-handlers ([exn:fail? (lambda (_) 'BAD-JSON)])
        (let ([j (string->jsexpr args-json)])
          (if (hash? j) j 'NOT-OBJECT))))
    (define result
      (cond
        ;; Truncated tool call -- the model stopped mid-generation, so no
        ;; function name survived. Feed that back instead of crashing.
        [(string=? (string-trim name) "")
         (format "Error: the model's tool call was truncated mid-generation (no function name provided). Received arguments: ~a"
                 short)]
        [(eq? args-parsed 'BAD-JSON)
         (format "Error: invalid JSON in arguments for tool '~a'. Received: ~a"
                 name short)]
        [(eq? args-parsed 'NOT-OBJECT)
         (format "Error: arguments for tool '~a' must be a JSON object. Received: ~a"
                 name short)]
        [else
         ;; Unknown tool names, contract violations, etc. become feedback to the
         ;; model rather than an uncaught exception that aborts the loop.
         (with-handlers ([exn:fail? (lambda (e)
                                      (format "Error: tool '~a' raised: ~a"
                                              name (exn-message e)))])
           (call-tool name args-parsed))]))
    (set! results (append results (list (list call-id name result)))))
  results)

;; ---------------------------------------------------------------------------
;; Helpers: run subprocess and capture combined output

(define (run-external exe args)
  ;; exe : string, args : (listof string)  -> (values combined-output exit-code)
  (define exe-path (find-executable-path exe))
  (unless exe-path
    (error 'run-external "Executable not found: ~a" exe))
  (define-values (sp stdout stdin stderr)
    (apply subprocess #f #f #f exe-path args))
  (close-output-port stdin)
  (define out-str (port->string stdout))
  (define err-str (port->string stderr))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait sp)
  (define status (subprocess-status sp))
  (define code (if (number? status) status 1))
  (values (string-append out-str err-str) code))

(define (shell-quote s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))

(define (truncate-string s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 max-len)
                     (format "\n... (truncated, ~a total chars)" (string-length s)))
      s))

;; Hidden files (ignored from listings and reject read attempts):
;;   - names ending in ~  (e.g. foo.rkt~)
;;   - names wrapped in #...#  (e.g. #foo.rkt#)
;;   - names starting with .  (e.g. .git, .gitignore, .env)
(define (hidden-file? name)
  (or (string-suffix? name "~")
      (and (string-prefix? name "#")
           (string-suffix? name "#"))
      (string-prefix? name ".")))

;; ---------------------------------------------------------------------------
;; Tool implementations

(define (tool-read-file path)
  (with-handlers ([exn:fail? (lambda (e) (format "Error reading ~a: ~a" path (exn-message e)))])
    (define fname (path->string (file-name-from-path path)))
    (if (hidden-file? fname)
        (format "refusing to read hidden/internal file: ~a" path)
        (file->string path))))

(define (tool-list-dir path)
  (with-handlers ([exn:fail? (lambda (e) (format "Error listing ~a: ~a" path (exn-message e)))])
    (define entries (directory-list path))
    (define lines
      (for/list ([e (in-list (sort (map path->string entries) string<?))]
                 #:unless (hidden-file? (path->string e)))
        (define full (build-path path e))
        (if (directory-exists? full)
            (string-append e "/")
            e)))
    (string-join lines "\n")))

(define (tool-grep pattern path)
  (with-handlers ([exn:fail? (lambda (e) (format "Error running grep: ~a" (exn-message e)))])
    (define-values (out code) (run-external "grep" (list "-rnE" pattern path)))
    out))

(define (strip-shell-quotes s)
  (if (and (>= (string-length s) 2)
           (let ([first (string-ref s 0)]
                 [last  (string-ref s (sub1 (string-length s)))])
             (or (and (char=? first #\") (char=? last #\"))
                 (and (char=? first #\') (char=? last #\')))))
      (substring s 1 (sub1 (string-length s)))
      s))

(define (hidden-arg? s)
  (define cleaned (strip-shell-quotes s))
  (and (not (string-prefix? cleaned "-"))
       (hidden-file? (path->string (file-name-from-path cleaned)))))

(define (filter-ls-output out)
  (define lines (string-split out "\n"))
  (define filtered
    (for/list ([line (in-list lines)]
               #:unless (let ([t (string-trim line)])
                          (or (string-prefix? t "total ")
                              (equal? t ""))))
      (define trimmed (string-trim line))
      (define tokens (string-split trimmed))
      (cond
        [(null? tokens) #f]
        [(regexp-match? #rx"^[-d]" (first tokens))
         (define fname (last tokens))
         (if (or (hidden-file? fname) (member fname '("." "..")))
             #f
             line)]
        [else
         (if (hidden-file? trimmed) #f line)])))
  (define result-lines (for/list ([f (in-list filtered)] #:when f) f))
  (if (null? result-lines) "" (string-join result-lines "\n")))

(define (tool-run-shell command)
  (define tokens (string-split (string-trim command)))
  (cond
    [(null? tokens) "empty command"]
    [else
     (define cmd (first tokens))
     (cond
       [(not (set-member? SHELL-WHITELIST cmd))
        (format "Command '~a' not whitelisted. Allowed: ~a"
                cmd (string-join (sort (set->list SHELL-WHITELIST) string<?) ", "))]
       [(and (not (equal? cmd "ls"))
             (ormap hidden-arg? (rest tokens)))
        => (lambda (bad)
             (format "refusing to run command referencing hidden/internal file: ~a" bad))]
       [else
        (with-handlers ([exn:fail? (lambda (e) (format "Error running command: ~a" (exn-message e)))])
          (define args (rest tokens))
          (define-values (out code) (run-external cmd args))
          (define filtered-out (if (equal? cmd "ls") (filter-ls-output out) out))
          (string-append filtered-out (format "(exit ~a)" code)))])]))

(define (run-make-check)
  (with-handlers ([exn:fail? (lambda (e) (values (format "make check error: ~a" (exn-message e)) 1))])
    (run-external "make" (list "check"))))

(define (tool-propose-edit path old new)
  (define exists? (file-exists? path))
  (define current
    (if exists?
        (with-handlers ([exn:fail? (lambda (e) (format "Error reading ~a: ~a" path (exn-message e)))])
          (file->string path))
        ""))
  ;; If read failed and returned error string, treat as error
  (when (and exists? (string-prefix? current "Error reading"))
    current)
  (cond
    [(and exists? (not (string=? current old)))
     (format "stale base: on-disk contents of ~a do not match the 'old' you provided. Read the file again and retry." path)]
    [(and exists? (string=? current new))
     "no changes (proposed content matches current file)"]
    [(and (not exists?) (string=? new ""))
     "refused: cannot create an empty file"]
    [else
     (define diff-text (unified-diff current new (string-append "a/" path) (string-append "b/" path)))
     (displayln "")
     (unless exists? (displayln (format "(new file: ~a)" path)))
     (print-colored-diff diff-text)
     (cond
       [(dry-run?)
        "dry-run: diff shown, file not written (use without --dry-run to apply)"]
       [(auto-approve?)
        ;; Safety: still show diff above, then auto-apply without prompting
        (unless (quiet-mode?)
          (displayln "[auto-approve: applying change without prompt]"))
        (make-parent-directory* path)
        (call-with-output-file path #:exists 'truncate
          (lambda (out) (display new out)))
        (define-values (out status) (run-make-check))
        (if (= status 0)
            "applied (auto-approved); make check passed"
            (format "applied (auto-approved); make check FAILED (exit ~a):\n~a"
                    status (truncate-string out MAX-CHECK-OUTPUT-CHARS)))]
        [else
         (define answer (prompt-yes-no-skip))
         (cond
           [(eq? answer 'no) "user rejected the change"]
           [(eq? answer 'skip)
           (define reason (prompt-reason))
           (format "user skipped: ~a" reason)]
          [else ; 'yes
           (make-parent-directory* path)
           (call-with-output-file path #:exists 'truncate
             (lambda (out) (display new out)))
           (define-values (out status) (run-make-check))
           (if (= status 0)
               "applied; make check passed"
               (format "applied; make check FAILED (exit ~a):\n~a"
                       status (truncate-string out MAX-CHECK-OUTPUT-CHARS)))])])]))

;; ---------------------------------------------------------------------------
;; Registration

(define (register-all)
  (define-tool
    "read_file"
    (list (list "path" "string" "File path relative to the working directory."))
    "Read and return the contents of a file. Refuses to read hidden/internal files (~, #...#, and dotfiles)."
    tool-read-file)
  (define-tool
    "list_dir"
    (list (list "path" "string" "Directory path. Use \".\" for the working directory."))
    "List files and subdirectories (with trailing /) in a directory. Hidden/internal files (~, #...#, and dotfiles) are excluded."
    tool-list-dir)
  (define-tool
    "grep"
    (list (list "pattern" "string" "Extended regex pattern to search for.")
          (list "path" "string" "Directory or file path to search."))
    "Recursively grep files for PATTERN. Wraps `grep -rnE`."
    tool-grep)
  (define-tool
    "run_shell"
    (list (list "command" "string" "Shell command. Only whitelisted commands may run: make, ls, pwd, cat, uv."))
    "Run a whitelisted shell command and return its combined output. Refuses commands that reference hidden/internal files."
    tool-run-shell)
  (define-tool
    "propose_edit"
    (list (list "path" "string" "Path to the file to edit or create.")
          (list "old" "string" "For an existing file: the exact current contents. For a new file: pass empty string.")
          (list "new" "string" "The proposed new contents of the file, in full."))
    "Propose an edit or new-file creation. The user is shown a unified diff and asked to approve. On approval the file is written and `make check` is run."
    tool-propose-edit))

(define ENABLED-TOOLS (list "read_file" "list_dir" "grep" "run_shell" "propose_edit"))
