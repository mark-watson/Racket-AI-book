#lang racket

(require pdf-read)
(require racket/runtime-path)

(provide pdf->string)

(define (pdf->string pdf-file-path)
  (let* ((page-count (pdf-count-pages pdf-file-path)))
    (string-join
     (for/list ([i page-count])
       (page-text (pdf-page pdf-file-path i))))))

(module+ main
  (define-runtime-path test-pdf "test_pdfs/alice_in_wonderland.pdf")
  (if (file-exists? test-pdf)
      (displayln (substring (pdf->string test-pdf) 0 500))
      (displayln "Test PDF not found, please check path.")))