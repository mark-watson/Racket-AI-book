#lang racket

;;; lightpanda.rkt — Racket interface to the Lightpanda headless browser
;;;
;;; Lightpanda is invoked via the `fetch` command which runs the browser
;;; and returns rendered HTML without needing a server process.
;;;
;;; For shell execution this library uses process/ports from racket/system.

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

(define (run-command cmd)
  "Run a shell command string, return stdout as a string (or #f on error)."
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (eprintf "Command error: ~a\nCommand: ~a\n"
                              (exn-message e) cmd)
                     #f)])
    (define-values (proc stdout stdin stderr)
      (subprocess #f #f #f "/bin/sh" "-c" cmd))
    (close-output-port stdin)
    (define output (port->string stdout))
    (close-input-port stdout)
    (close-input-port stderr)
    (subprocess-wait proc)
    (if (zero? (subprocess-status proc))
        output
        (begin
          (eprintf "Command exited with status ~a: ~a\n"
                   (subprocess-status proc) cmd)
          #f))))

(define (extract-links html)
  "Return a list of href strings found in <a> tags within an HTML string."
  (define marker "href=\"")
  (define marker-len (string-length marker))
  (let loop ([pos 0] [links '()])
    (define found (regexp-match-positions (regexp-quote marker) html pos))
    (cond
      [(not found) (reverse links)]
      [else
       (define start (cdr (car found)))
       (define end-pos
         (let ([idx (string-index-of html "\"" start)])
           (if idx idx #f)))
       (if end-pos
           (loop (add1 end-pos)
                 (cons (substring html start end-pos) links))
           (loop (add1 start) links))])))

(define (string-index-of str char-str start)
  "Find the first occurrence of char-str in str starting from start."
  (define found (regexp-match-positions (regexp-quote char-str) str start))
  (if found (car (car found)) #f))

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
  (define parts
    (append
     (list (lightpanda-binary) "fetch")
     (if obey-robots '("--obey_robots") '())
     (list "--dump" dump
           "--log_level" log-level
           "--log_format" "pretty")
     (list url)))
  (define cmd (string-join parts " "))
  (run-command cmd))

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
