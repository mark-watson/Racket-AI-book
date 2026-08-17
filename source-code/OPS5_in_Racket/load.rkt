#lang racket
;; load.rkt — OPS5-in-Racket driver
;;
;; Converted from load.s (Mark Watson, "OPS5 in Scheme", 1995).
;;
;; The original load.s simply loaded the system files into one global
;; Scheme environment.  Racket modules are hygienic, so this driver
;; instead creates a dedicated namespace, loads the compatibility layer
;; and the converted system files into it, optionally loads any .ops
;; program files named on the command line, and then starts an
;; interactive read-eval-print loop in that namespace.  Inside the REPL
;; you can type ordinary OPS5 commands: (load "draw.ops"), (i-g-v),
;; (p ...), (make ...), (run), (wm), (exit), ...
;;
;; Usage:
;;   racket load.rkt                 ; start the REPL
;;   racket load.rkt draw.ops        ; load a program, then the REPL
;;   racket load.rkt monkey.ops      ; load another program, then the REPL

(require racket/load)

(define ops5-namespace (make-base-namespace))

(define (load-file-into-ns! file)
  (parameterize ([current-namespace ops5-namespace])
    (load file)))

(define (start-ops5)
  (parameterize ([current-namespace ops5-namespace])
    ;; `load` is already bound in a base namespace, so `(load "draw.ops")`
    ;; works at the OPS5> prompt.  (Do not `(require racket/load)` into
    ;; the namespace: it also exports #%top-interaction, which would
    ;; silently reroute every subsequent top-level form, including the
    ;; loaded definitions, into racket/load's private namespace.)
    (for-each load-file-into-ns!
              '("compat.rkt" "ops5.rkt" "compiler.rkt"
                "network.rkt" "rhs.rkt" "lit.rkt"))))

(define (repl)
  (parameterize ([current-namespace ops5-namespace])
    (displayln "")
    (displayln "OPS5 Scheme interpreter (Racket conversion)")
    (displayln "Type OPS5 expressions, e.g.:")
    (displayln "  (load \"draw.ops\")   load a program file")
    (displayln "  (i-g-v)              initialize (or reset) OPS5")
    (displayln "  (p name lhs --> rhs) define a production")
    (displayln "  (make class ...)     add a working-memory element")
    (displayln "  (run)                run the productions")
    (displayln "  (wm)                 print working memory")
    (displayln "  (exit)               leave the REPL")
    (displayln "")
    (let loop ()
      (display "OPS5> ")
      (flush-output)
      (let ([form (read)])
        (cond
          [(eof-object? form)
           (displayln "bye")]
          [else
           (let ([v (with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
                      (eval form))])
             (unless (void? v)
               (write v)
               (newline)))
           (loop)])))))

;; Any .ops files given on the command line (after load.rkt) are
;; loaded into the OPS5 namespace before the REPL starts.
(define ops-files
  (let ([argv (vector->list (current-command-line-arguments))])
    (if (pair? argv) (cdr argv) '())))

(start-ops5)
(for-each load-file-into-ns! ops-files)
(repl)
