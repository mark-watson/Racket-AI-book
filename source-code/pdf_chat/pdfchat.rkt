#lang racket

(require "pdf2text.rkt")
(require embeddingsdb)
(require llmapis)

;; This script implements a simple command-line PDF chat / QA utility
;; using pdf2text, embeddingsdb, and llmapis.

(define (chat-with-pdf pdf-path query)
  (printf "Reading PDF: ~a...\n" pdf-path)
  (define text (pdf->string pdf-path))
  (printf "Extracted ~a characters of text. Indexing...\n" (string-length text))
  
  (define temp-txt-path (path->string (make-temporary-file "pdfchat_tmp_~a.txt")))
  (dynamic-wind
   (lambda ()
     (with-output-to-file temp-txt-path
       (lambda () (display text))
       #:exists 'truncate/replace))
   (lambda ()
     ;; Create/Update document in embeddingsdb using the text file
     (create-document temp-txt-path)
     (printf "Running QA query: ~a\n" query)
     (define response (QA query))
     (printf "Response: ~a\n" response)
     response)
   (lambda ()
     (when (file-exists? temp-txt-path)
       (delete-file temp-txt-path)))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "pdfchat"
   #:args (pdf-path query)
   (chat-with-pdf pdf-path query)))
