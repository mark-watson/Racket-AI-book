#lang racket/base

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; line-input.rkt -- rlwrap-style line editing for the coding-agent REPL.
;;;
;;; Drives Racket's bundled `readline` collection to give the REPL cursor
;;; editing, in-session history (arrows, Ctrl-R), and Tab completion.  The
;;; backend is Editline (libedit) by default and GNU Readline when installed
;;; (install the "readline-gpl" package or set PLT_READLINE_LIB).
;;;
;;; Two things can go wrong, and both degrade to a plain read-line instead of
;;; failing the harness:
;;;
;;;   * (require readline/readline) raises at instantiation when no Editline /
;;;     Readline shared library is present.  This module therefore loads it with
;;;     dynamic-require inside a handler, and every entry point works without it.
;;;
;;;   * stdin may not be a terminal (pipes, --stdin, CI).  Editing is skipped and
;;;     the prompt is printed by hand so piped output is byte-identical to before.
;;;
;;; `dynamic-require` is invisible to `raco exe`'s module embedder, so the
;;; Makefile builds the executable with `++lib readline/readline`.  That embeds
;;; the module without instantiating it, so it is still loaded lazily and the
;;; fallback above continues to apply if libedit is missing at run time.

(require racket/file
         racket/string)

(provide line-input-available?
         read-input-line
         default-history-file
         set-history-file!
         save-history!
         install-completion!)

;; ---------------------------------------------------------------------------
;; Guarded load of the readline collection

;; All #f until (and unless) the backend loads successfully.
(define readline-proc #f)
(define add-history-proc #f)
(define history-length-proc #f)
(define history-get-proc #f)
(define set-completion-proc #f)
(define set-completion-append-char-proc #f)

(define backend-tried? #f)

(define (load-backend!)
  ;; Instantiate readline/readline once, tolerating a missing module or a
  ;; missing native library.  All lookups happen before any assignment so a
  ;; failure can never leave the module half-initialised.
  (unless backend-tried?
    (set! backend-tried? #t)
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (define (get name) (dynamic-require 'readline/readline name))
      (define rl (get 'readline))
      (define add-history (get 'add-history))
      (define history-length (get 'history-length))
      (define history-get (get 'history-get))
      (define set-completion (get 'set-completion-function!))
      (define set-completion-append-char (get 'set-completion-append-character!))
      (set! readline-proc rl)
      (set! add-history-proc add-history)
      (set! history-length-proc history-length)
      (set! history-get-proc history-get)
      (set! set-completion-proc set-completion)
      (set! set-completion-append-char-proc set-completion-append-char))))

(define (terminal-stdin?)
  ;; readline captures the input port when its module is instantiated, so only
  ;; engage it when that port really is the terminal's stdin.
  (and (terminal-port? (current-input-port))
       (eq? 'stdin (object-name (current-input-port)))))

(define (line-input-available?)
  ;; -> boolean.  Loads the backend on first use, and only when stdin is a
  ;; terminal, so piped/--stdin runs never touch libedit at all.
  (and (terminal-stdin?)
       (begin (load-backend!) (and readline-proc #t))))

;; ---------------------------------------------------------------------------
;; History bookkeeping

;; Editline does not add accepted lines to the history itself (verified in this
;; repository's development environment); GNU Readline does.  Probe once on the
;; first non-empty line and fill the gap only if the backend left it empty.
(define history-probed? #f)
(define backend-adds-history? #f)

(define (remember-line! line before-count)
  (define grew? (> (history-length-proc) before-count))
  (cond
    [(not history-probed?)
     (set! history-probed? #t)
     (set! backend-adds-history? grew?)
     (when (and (not grew?) (not (string=? line "")))
       (add-history-proc line))]
    [(and (not backend-adds-history?) (not (string=? line "")))
     (add-history-proc line)]))

;; History file -------------------------------------------------------------

(define history-file #f)
(define max-history 1000)

(define (default-history-file)
  (build-path (find-system-path 'home-dir) ".coding_agent_history"))

(define (load-history!)
  (when (and readline-proc history-file (file-exists? history-file))
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (for ([line (in-list (file->lines history-file))])
        (unless (string=? (string-trim line) "")
          (add-history-proc line))))))

(define (set-history-file! path)
  ;; Point at `path`, loading any existing entries.  A #f path disables
  ;; persistence; #f is also the default until this is called.
  (set! history-file path)
  (load-history!))

(define (save-history!)
  (when (and readline-proc history-file)
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (define n (history-length-proc))
      (when (> n 0)
        (define count (min n max-history))
        ;; Negative indices count back from the newest entry, which sidesteps
        ;; the 0-vs-1 base difference between libedit and GNU Readline.
        (define lines
          (for/list ([i (in-range (- count) 0)])
            (history-get-proc i)))
        (call-with-atomic-output-file history-file
          (lambda (out _tmp)
            (for ([line (in-list lines)])
              (displayln line out))))))))

;; ---------------------------------------------------------------------------
;; Completion

(define (install-completion! candidates)
  ;; candidates : string -> (listof string), mapping the word under the cursor
  ;; to matching completions.  Ignored when the backend is unavailable.
  (when (line-input-available?)
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (set-completion-proc
       (lambda (word)
         ;; Called from the readline backend in atomic mode on Racket CS, so
         ;; this stays pure string work: no I/O, threads, or subprocesses.
         ;; The append character is reset by the library on every call.
         (set-completion-append-char-proc #\space)
         (define w (if (bytes? word) (bytes->string/utf-8 word) word))
         (define matches
           (with-handlers ([exn:fail? (lambda (_) '())])
             (candidates w)))
         (if (bytes? word)
             (for/list ([m (in-list matches)]) (string->bytes/utf-8 m))
             matches))))))

;; ---------------------------------------------------------------------------
;; Reading

(define (read-input-line prompt)
  ;; -> string or eof.  Always prints the prompt, with readline handling editing
  ;; on a terminal and a plain read-line used otherwise.
  (cond
    [(line-input-available?)
     (define before (history-length-proc))
     (define line (readline-proc prompt))
     (unless (eof-object? line)
       (remember-line! line before))
     line]
    [else
     (display prompt)
     (flush-output)
     (read-line (current-input-port) 'any)]))
