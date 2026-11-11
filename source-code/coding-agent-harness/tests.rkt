#lang racket

;;; tests.rkt — Offline tests for the coding agent's tools and diff display
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; AGPL-3.0 (see LICENSE)
;;;
;;; No model or network is needed. Internal functions are reached with
;;; require/expose. Run with:  raco test tests.rkt

(require rackunit)
(require "approval.rkt")
(require/expose "tools.rkt"
                (hidden-file? truncate-string strip-shell-quotes hidden-arg?
                 filter-ls-output tool-read-file tool-list-dir tool-run-shell))

(define (with-temp-dir proc)
  (define dir (make-temporary-file "agent-test-~a" 'directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda () (delete-directory/files dir))))

(define (write-file! path text)
  (call-with-output-file path #:exists 'truncate
    (lambda (out) (display text out))))

;;; ---- approval.rkt ----

(test-case "unified-diff shows removed and added lines with the labels"
  (define diff (unified-diff "one\ntwo\nthree\n" "one\nTWO\nthree\n"
                             "a/file.txt" "b/file.txt"))
  (check-true (string-contains? diff "--- a/file.txt"))
  (check-true (string-contains? diff "+++ b/file.txt"))
  (check-true (string-contains? diff "-two"))
  (check-true (string-contains? diff "+TWO")))

(test-case "unified-diff of identical content is empty"
  (check-equal? (unified-diff "same\n" "same\n" "a" "b") ""))

(test-case "labels containing quotes do not break the diff command"
  (define diff (unified-diff "x\n" "y\n" "it's old" "it's new"))
  (check-true (string-contains? diff "--- it's old")))

;;; ---- tools.rkt: hidden files ----

(test-case "hidden-file? covers dotfiles, backups, and editor lock files"
  (check-true (hidden-file? ".env"))
  (check-true (hidden-file? ".git"))
  (check-true (hidden-file? "notes.rkt~"))
  (check-true (hidden-file? "#notes.rkt#"))
  (check-false (hidden-file? "notes.rkt"))
  (check-false (hidden-file? "README.md")))

(test-case "hidden-arg? looks at the file name, not at option flags"
  (check-true (hidden-arg? ".env"))
  (check-true (hidden-arg? "config/.env"))
  (check-true (hidden-arg? "'.env'"))
  (check-false (hidden-arg? "-la"))
  (check-false (hidden-arg? "src/main.rkt")))

(test-case "strip-shell-quotes removes one matching pair of quotes"
  (check-equal? (strip-shell-quotes "\"a b\"") "a b")
  (check-equal? (strip-shell-quotes "'a'") "a")
  (check-equal? (strip-shell-quotes "'a\"") "'a\"")
  (check-equal? (strip-shell-quotes "a") "a"))

(test-case "truncate-string keeps short text and marks long text"
  (check-equal? (truncate-string "short" 10) "short")
  (define long (truncate-string (make-string 50 #\x) 10))
  (check-true (string-prefix? long (make-string 10 #\x)))
  (check-true (string-contains? long "truncated, 50 total chars")))

;;; ---- tools.rkt: file tools ----

(test-case "read_file returns content and refuses hidden files"
  (with-temp-dir
   (lambda (dir)
     (write-file! (build-path dir "hello.txt") "hello")
     (write-file! (build-path dir ".env") "SECRET=1")
     (check-equal? (tool-read-file (path->string (build-path dir "hello.txt")))
                   "hello")
     (define refused (tool-read-file (path->string (build-path dir ".env"))))
     (check-true (string-contains? refused "refusing to read"))
     (check-false (string-contains? refused "SECRET"))
     (check-true (string-prefix?
                  (tool-read-file (path->string (build-path dir "missing.txt")))
                  "Error reading")))))

(test-case "list_dir sorts entries, marks directories, hides hidden files"
  (with-temp-dir
   (lambda (dir)
     (write-file! (build-path dir "b.txt") "")
     (write-file! (build-path dir "a.txt") "")
     (write-file! (build-path dir ".secret") "")
     (write-file! (build-path dir "a.txt~") "")
     (make-directory (build-path dir "sub"))
     (check-equal? (tool-list-dir (path->string dir))
                   "a.txt\nb.txt\nsub/"))))

(test-case "filter-ls-output drops totals, dot entries, and hidden files"
  (define ls-output
    (string-join
     '("total 16"
       "drwxr-xr-x  5 me  staff  160 Jan  1 10:00 ."
       "drwxr-xr-x  9 me  staff  288 Jan  1 10:00 .."
       "-rw-r--r--  1 me  staff   10 Jan  1 10:00 .env"
       "-rw-r--r--  1 me  staff   10 Jan  1 10:00 main.rkt")
     "\n"))
  (define filtered (filter-ls-output ls-output))
  (check-true (string-contains? filtered "main.rkt"))
  (check-false (string-contains? filtered ".env"))
  (check-false (string-contains? filtered "total")))

;;; ---- tools.rkt: shell tool ----

(test-case "run_shell only runs whitelisted commands"
  (check-true (string-contains? (tool-run-shell "rm -rf /") "not whitelisted"))
  (check-true (string-contains? (tool-run-shell "curl http://example.com")
                                "not whitelisted"))
  (check-equal? (tool-run-shell "   ") "empty command"))

(test-case "run_shell refuses hidden files and runs allowed commands"
  (with-temp-dir
   (lambda (dir)
     (write-file! (build-path dir "hello.txt") "hello")
     (write-file! (build-path dir ".env") "SECRET=1")
     (parameterize ([current-directory dir])
       (define refused (tool-run-shell "cat .env"))
       (check-true (string-contains? refused "refusing to run"))
       (check-false (string-contains? refused "SECRET"))
       (check-equal? (tool-run-shell "cat hello.txt") "hello(exit 0)")
       (define listing (tool-run-shell "ls -la"))
       (check-true (string-contains? listing "hello.txt"))
       (check-false (string-contains? listing ".env"))))))
