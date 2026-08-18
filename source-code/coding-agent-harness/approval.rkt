#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; approval.rkt -- colored diffs and y/n/s approval prompts
;;;               Racket port of py-coding-agent/approval.py
;;;               ESC-aware: bare ESC aborts the prompt and signals
;;;               the global interrupt flag in fireworks-ai.rkt.

(require racket/file
         racket/port
         racket/string
         racket/system
         "interrupt.rkt")

(provide unified-diff
         print-colored-diff
         prompt-yes-no-skip
         prompt-reason
         save-stty-state
         restore-stty-state
         enter-cbreak-mode)

;; ---------------------------------------------------------------------------
;; ANSI colours

(define ANSI-RED   "\033[31m")
(define ANSI-GREEN "\033[32m")
(define ANSI-CYAN  "\033[36m")
(define ANSI-RESET "\033[0m")

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
      [(or (string-prefix? line "+++")
           (string-prefix? line "---")
           (string-prefix? line "@@"))
       (displayln (string-append ANSI-CYAN line ANSI-RESET))]
      [(string-prefix? line "+")
       (displayln (string-append ANSI-GREEN line ANSI-RESET))]
      [(string-prefix? line "-")
       (displayln (string-append ANSI-RED line ANSI-RESET))]
      [else (displayln line)])))

;; ---------------------------------------------------------------------------
;; Terminal helpers -- raw-mode ESC detection
;; We use `stty` when available, otherwise fall back to cooked reads.
;; The raw path is best-effort; the prompt still works without it.

(define (stty-available?)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (system "stty -g >/dev/null 2>&1")))

;; Capture the current terminal state as a single opaque token that can
;; be handed back to `restore-stty-state` later.  Returns #f if stty is
;; unavailable or we're not on a TTY.
(define (save-stty-state)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define stty-path (find-executable-path "stty"))
    (cond
      [(not stty-path) #f]
      [else
       (define-values (sp stdout stdin stderr)
         (subprocess #f #f #f stty-path "-g"))
       (close-output-port stdin)
       (define out (port->string stdout))
       (close-input-port stdout)
       (close-input-port stderr)
       (subprocess-wait sp)
       (define code (subprocess-status sp))
       (if (and (number? code) (= code 0))
           (string-trim out)
           #f)])))

(define (restore-stty-state state)
  (when (and state (string? state) (not (string=? state "")))
    (with-handlers ([exn:fail? void])
      (define stty-path (find-executable-path "stty"))
      (when stty-path
        (define-values (sp stdout stdin stderr)
          (subprocess #f #f #f stty-path state))
        (close-output-port stdin)
        (close-input-port stdout)
        (close-input-port stderr)
        (subprocess-wait sp)))))

;; Non-canonical, no echo, but leave output post-processing (opost) alone
;; so `\n` still translates to `\r\n` on output.  Full `stty raw` disables
;; opost, which stair-steps any multi-line output printed while we're in
;; raw mode (e.g. the colored diff).
(define (enter-cbreak-mode)
  (when (stty-available?)
    (system "stty -icanon -echo 2>/dev/null")
    #t))

(define (try-raw-mode)
  (enter-cbreak-mode))

(define (restore-mode saved)
  (if saved
      (restore-stty-state saved)
      (when (stty-available?)
        (system "stty sane 2>/dev/null"))))

;; Read one line in (possibly) raw mode.
;; Returns two values: the line string and whether ESC was pressed.
;; A bare ESC (not followed by ANSI bytes within ~50ms) counts as ESC.
;; Arrow-key ANSI sequences are drained and ignored.
;;
;; Raw mode has echo off, so we manually echo printable characters and
;; handle backspace/delete.  On newline we emit a bare "\r" to bring the
;; cursor back to column 0; the caller adds the final "\n".
(define (read-line-raw)
  (define esc? #f)
  (define chars '())
  (define done? #f)
  (let loop ()
    (unless done?
      (define ready? (char-ready? (current-input-port)))
      (if ready?
          (let ([ch (read-char (current-input-port))])
            (cond
              [(eof-object? ch) (set! done? #t)]
              [(char=? ch #\u001b)
               ;; Possible ESC or ANSI sequence -- wait briefly
               (sleep 0.05)
               (if (char-ready? (current-input-port))
                   ;; ANSI sequence -- drain it and continue
                   (let drain ()
                     (when (char-ready? (current-input-port))
                       (read-char (current-input-port))
                       (sleep 0.02)
                       (drain)))
                   (begin (set! esc? #t) (set! done? #t)))
               (unless done? (loop))]
              [(or (char=? ch #\newline) (char=? ch #\return))
               (display "\r")
               (flush-output)
               (set! done? #t)]
              [(or (char=? ch #\backspace) (char=? ch #\rubout))
               (when (not (null? chars))
                 (set! chars (rest chars))
                 (display "\b \b")
                 (flush-output))
               (loop)]
              [(char=? ch (integer->char 3)) ; Ctrl-C -- treat like ESC
               (set! esc? #t)
               (set! done? #t)]
              [else
               (display ch)
               (flush-output)
               (set! chars (cons ch chars))
               (loop)]))
          ;; No char ready -- small sleep to avoid busy loop
          ;; but we are inside a blocking raw read; poll with timeout
          (begin (sleep 0.02) (loop)))))
  (values (list->string (reverse chars)) esc?))

;; ---------------------------------------------------------------------------
;; prompt-yes-no-skip : -> (or 'yes 'no 'skip 'interrupted)

(define (prompt-yes-no-skip)
  (define saved (save-stty-state))
  (define raw? (try-raw-mode))
  (dynamic-wind
    void
    (lambda ()
      (let loop ()
        (when (task-interrupted?) (values 'interrupted))
        (display "\nApply this change? [y]es / [n]o / [s]kip and tell the model why: ")
        (flush-output)
        (define-values (line esc?)
          (if raw?
              (read-line-raw)
              (let ([l (read-line (current-input-port))])
                (values (if (eof-object? l) "" (string-trim l)) #f))))
        (when raw? (displayln ""))
        (cond
          [esc?
           (task-interrupted-set!)
           (displayln "[interrupted]")
           'interrupted]
          [(task-interrupted?) 'interrupted]
          [else
           (define norm (string-downcase (string-trim line)))
           (cond
             [(member norm '("y" "yes")) 'yes]
             [(member norm '("n" "no")) 'no]
             [(member norm '("s" "skip")) 'skip]
             [else
              (displayln "Please answer y, n, or s.")
              (loop)])])))
    (lambda () (when raw? (restore-mode saved)))))

;; ---------------------------------------------------------------------------
;; prompt-reason : -> string  ("" if ESC)

(define (prompt-reason)
  (define saved (save-stty-state))
  (define raw? (try-raw-mode))
  (dynamic-wind
    void
    (lambda ()
      (display "Reason (one line): ")
      (flush-output)
      (define-values (line esc?)
        (if raw?
            (read-line-raw)
            (let ([l (read-line (current-input-port))])
              (values (if (eof-object? l) "" l) #f))))
      (when raw? (displayln ""))
      (if esc?
          (begin (task-interrupted-set!) "")
          line))
    (lambda () (when raw? (restore-mode saved)))))
