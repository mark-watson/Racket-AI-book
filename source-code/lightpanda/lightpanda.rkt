#lang racket

(require html-parsing)
(require xml/path)

(provide fetch-url
         fetch-and-extract-links
         demo-fetch
         lightpanda-binary)

;;; -------------------------------------------------------------------------
;;; Configuration
;;; -------------------------------------------------------------------------

(define lightpanda-binary (make-parameter "lightpanda"))

;;; -------------------------------------------------------------------------
;;; Internal helpers
;;; -------------------------------------------------------------------------

(define (run-command exe-path args)
  "Run a command directly, return stdout as a string (or #f on error)."
  (let ([resolved-path (find-executable-path exe-path)])
    (if (not resolved-path)
        (begin
          (eprintf "Executable not found: ~a\n" exe-path)
          #f)
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (eprintf "Command execution error: ~a\n" (exn-message e))
                           #f)])
          (define-values (proc stdout stdin stderr)
            (apply subprocess #f #f #f resolved-path args))
          (dynamic-wind
            (lambda () (close-output-port stdin))
            (lambda ()
              (let ([output (port->string stdout)])
                (subprocess-wait proc)
                (if (zero? (subprocess-status proc))
                    output
                    (begin
                      (eprintf "Command exited with status ~a\n" (subprocess-status proc))
                      #f))))
            (lambda ()
              (close-input-port stdout)
              (close-input-port stderr)))))))

(define (extract-links html)
  "Return a list of href strings found in <a> tags within an HTML string."
  (with-handlers ([exn:fail? (lambda (e)
                               (eprintf "Error parsing HTML for links: ~a\n" (exn-message e))
                               '())])
    (define xexp (html->xexp html))
    (se-path*/list '(href) xexp)))

;;; -------------------------------------------------------------------------
;;; Fetch interface  (lightpanda fetch <url>)
;;; -------------------------------------------------------------------------

(define (fetch-url url
                   #:log-level [log-level "warn"]
                   #:obey-robots [obey-robots #f]
                   #:dump [dump "html"])
  "Fetch URL using `lightpanda fetch`, returning the JS-rendered content string.
DUMP controls what is written to stdout; valid values are:
  \"html\"               - full rendered HTML (default)
  \"markdown\"           - page as Markdown
  \"semantic_tree\"      - semantic tree
  \"semantic_tree_text\" - semantic tree as plain text
No server process is required; lightpanda is invoked directly.

  (fetch-url \"https://markwatson.com/\")
  (fetch-url \"https://markwatson.com/\" #:dump \"markdown\")
"
  (define args
    (append
     (list "fetch")
     (if obey-robots '("--obey_robots") '())
     (list "--dump" dump
           "--log_level" log-level
           "--log_format" "pretty"
           url)))
  (run-command (lightpanda-binary) args))

;;; -------------------------------------------------------------------------
;;; Helpers
;;; -------------------------------------------------------------------------

(define (fetch-and-extract-links url)
  "Fetch URL with lightpanda and return a list of href link strings.

  (fetch-and-extract-links \"https://markwatson.com/\")
"
  (define html (fetch-url url))
  (if html
      (extract-links html)
      (begin
        (eprintf "Failed to fetch ~a\n" url)
        '())))

(define (demo-fetch url)
  "Fetch URL, print a snippet of HTML and the discovered links.

  (demo-fetch \"https://markwatson.com/\")
"
  (printf "\n=== Fetch demo: ~a ===\n" url)
  (define html (fetch-url url))
  (if html
      (let ([links (extract-links html)])
        (printf "Received ~a bytes of HTML.\n" (string-length html))
        (printf "First 500 chars:\n~a\n\n"
                (substring html 0 (min 500 (string-length html))))
        (printf "Found ~a link(s):\n" (length links))
        (for ([l (in-list links)])
          (printf "  ~a\n" l)))
      (printf "No HTML returned.\n")))

;; Quick self-test when run directly
(module+ main
  (demo-fetch "https://markwatson.com/"))
