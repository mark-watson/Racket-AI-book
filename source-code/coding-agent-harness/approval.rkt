#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; approval.rkt -- colored diffs and y/n/s approval prompts
;;;               Racket port of py-coding-agent/approval.py

(require racket/file
         racket/port
         racket/string
         racket/system)

(provide unified-diff
         print-colored-diff
         prompt-yes-no-skip
         prompt-reason
         color-enabled?)

;; ---------------------------------------------------------------------------
;; ANSI colours

(define ANSI-RED   "\033[31m")
(define ANSI-GREEN "\033[32m")
(define ANSI-CYAN  "\033[36m")
(define ANSI-RESET "\033[0m")

;; When #f, print diffs without ANSI (for --plain / --no-color / piped output)
(define color-enabled? (make-parameter #t))

;; ---------------------------------------------------------------------------
;; Shell quoting helper (single-quote, escape embedded single quotes)

(define (shell-quote s)
  (string-append "'"
                 (string-replace s "'" "'\\''")
                 "'"))

;; ---------------------------------------------------------------------------
;; unified-diff : string string string string -> string
;; Runs `diff -u` on two temporary files and returns stdout.

(define (unified-diff old-content new-content old-label new-label)
  (define old-path (make-temporary-file "rk-diff-old~a"))
  (define new-path (make-temporary-file "rk-diff-new~a"))
  (define out-path (make-temporary-file "rk-diff-out~a"))
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file old-path #:exists 'truncate
        (lambda (out) (display old-content out)))
      (call-with-output-file new-path #:exists 'truncate
        (lambda (out) (display new-content out)))
      (define cmd
        (format "diff -u -L ~a -L ~a ~a ~a > ~a 2>&1"
                (shell-quote old-label)
                (shell-quote new-label)
                (shell-quote (path->string old-path))
                (shell-quote (path->string new-path))
                (shell-quote (path->string out-path))))
      (system cmd)
      (with-handlers ([exn:fail? (lambda (_) "")])
        (file->string out-path)))
    (lambda ()
      (when (file-exists? old-path) (delete-file old-path))
      (when (file-exists? new-path) (delete-file new-path))
      (when (file-exists? out-path) (delete-file out-path)))))

;; ---------------------------------------------------------------------------
;; print-colored-diff : string -> void

(define (print-colored-diff diff-text)
  (for ([line (in-list (string-split diff-text "\n"))])
    (cond
      [(color-enabled?)
       (cond
         [(or (string-prefix? line "+++")
              (string-prefix? line "---")
              (string-prefix? line "@@"))
          (displayln (string-append ANSI-CYAN line ANSI-RESET))]
         [(string-prefix? line "+")
          (displayln (string-append ANSI-GREEN line ANSI-RESET))]
         [(string-prefix? line "-")
          (displayln (string-append ANSI-RED line ANSI-RESET))]
         [else (displayln line)])]
      [else (displayln line)])))

;; ---------------------------------------------------------------------------
;; prompt-yes-no-skip : -> (or 'yes 'no 'skip)

(define (prompt-yes-no-skip)
  (let loop ()
    (display "\nApply this change? [y]es / [n]o / [s]kip and tell the model why: ")
    (flush-output)
    (define line (read-line (current-input-port)))
    (define norm (if (eof-object? line) "" (string-downcase (string-trim line))))
    (cond
      [(member norm '("y" "yes")) 'yes]
      [(member norm '("n" "no")) 'no]
      [(member norm '("s" "skip")) 'skip]
      [else
       (displayln "Please answer y, n, or s.")
       (loop)])))

;; ---------------------------------------------------------------------------
;; prompt-reason : -> string

(define (prompt-reason)
  (display "Reason (one line): ")
  (flush-output)
  (define line (read-line (current-input-port)))
  (if (eof-object? line) "" line))
