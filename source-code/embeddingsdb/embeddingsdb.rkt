#lang racket

(require db)
(require "../llmapis/llmapis.rkt")
(require racket/runtime-path)

(provide create-document delete-document QA CHAT semantic-match best-matches
         *embeddingsdb-chat-model*
         *embeddingsdb-embedding-model*
         *embeddingsdb-file*
         *embedding-fn*)

;; Models used through the uniform llmapis.rkt API. Rebind (e.g. to
;; "ollama/qwen3:1.7b" and an Ollama embedding model) to run locally.
(define *embeddingsdb-chat-model*
  (make-parameter (default-chat-model)))
(define *embeddingsdb-embedding-model*
  (make-parameter (default-embedding-model)))

;; The function that turns text into embeddings. Tests rebind it to an
;; offline stub; it has the same calling convention as llm-embedding.
(define *embedding-fn* (make-parameter llm-embedding))

; Function to convert list of floats to string representation
(define (floats->string floats)
  (string-join (map number->string floats) " "))

; Function to convert string representation back to list of floats
(define (string->floats str)
  (map string->number (string-split str)))


(define (break-into-chunks text chunk-size)
  (let loop ((start 0) (chunks '()))
    (if (>= start (string-length text))
        (reverse chunks)
        (loop (+ start chunk-size)
              (cons (substring text start (min (+ start chunk-size) (string-length text))) chunks)))))

(define (decode-row row)
  (let ((id (vector-ref row 0))
        (context (vector-ref row 1))
        (embedding (string->floats (vector-ref row 2))))
    (list id context embedding)))

;; The database file. It is a parameter so tests and other programs can
;; use their own file; the connection is opened on first use, not when
;; this module is required.
(define *embeddingsdb-file* (make-parameter "test.db"))

(define connections (make-hash)) ; database file -> open connection

(define (get-db)
  (hash-ref! connections (*embeddingsdb-file*)
             (lambda ()
               (define conn
                 (sqlite3-connect #:database (*embeddingsdb-file*)
                                  #:mode 'create))
               (query-exec
                conn
                "CREATE TABLE IF NOT EXISTS documents (document_path TEXT, content TEXT, embedding TEXT);")
               conn)))

(define (insert-document document-path content embedding)
  (printf "~%insert-document:~%  content:~a~%~%" content)
  (query-exec
   (get-db)
   "INSERT INTO documents (document_path, content, embedding) VALUES (?, ?, ?);"
   document-path content (floats->string embedding)))

(define (get-document-by-document-path document-path)
  (map decode-row
       (query-rows (get-db)
                    "SELECT * FROM documents WHERE document_path = ?;"
                    document-path)))

(define (all-documents)
  (map
   decode-row
   (query-rows
    (get-db)
    "SELECT * FROM documents;")))

(define (delete-document document-path)
  (query-exec (get-db)
              "DELETE FROM documents WHERE document_path = ?;"
              document-path))

;; Index one text file. Any rows from an earlier run on the same file are
;; removed first, so indexing a file twice does not create duplicates.
;; A chunk whose embedding call fails is reported and skipped; if the very
;; first call fails (for example a missing API key) the error is raised.
(define (create-document fpath)
  (let ((contents (break-into-chunks (file->string fpath) 200))
        (stored 0))
    (delete-document fpath)
    (for-each
     (lambda (content)
       (with-handlers ([exn:fail?
                        (lambda (ex)
                          (when (zero? stored) (raise ex))
                          (eprintf "create-document: skipping a chunk of ~a: ~a~%"
                                   fpath (exn-message ex)))])
         (let ((embedding (first ((*embedding-fn*) (*embeddingsdb-embedding-model*)
                                                content))))
           (insert-document fpath content embedding)
           (set! stored (+ stored 1)))))
     contents)
    stored))

(define (dot-product a b) ;; dot product of two lists of floating point numbers
  (for/sum ([x a] [y b])
    (* x y)))


;; Score every stored chunk against the query and keep the TOP-K best.
;; Ranking and taking the best few works with any embedding model; a fixed
;; score cutoff does not, because each model has its own score range.
(define (best-matches query [top-k 3])
  (let* ((emb (first ((*embedding-fn*) (*embeddingsdb-embedding-model*) query)))
         (scored
          (map (lambda (doc)
                 (cons (dot-product emb (third doc)) (second doc)))
               (all-documents)))
         (ranked (sort scored > #:key car)))
    (map cdr (take ranked (min top-k (length ranked))))))

(define (semantic-match query custom-context [top-k 3])
  (let* ((matches (best-matches query top-k))
         (context (string-join matches " . "))
         (query-with-context
          (string-join (list context custom-context "Question:" query) " ")))
    (printf "~%semantic-search: matches=~a~%" matches)
    (llm-ask (*embeddingsdb-chat-model*) query-with-context)))

(define (QA query [quiet #f])
  (let ((answer (semantic-match query "")))
    (unless quiet
      (printf "~%~%** query: ~a~%** answer: ~a~%~%" query answer))
    answer))

(define (CHAT)
  (let ((messages '(""))
        (responses '("")))
    (let loop ()
      (printf "~%Enter chat (STOP or empty line to stop) >> ")
      (let ((string (read-line)))
        (cond
         ((or (string=? string "STOP") (< (string-length string) 1))
          (list (reverse messages) (reverse responses)))
         (else
          (let* ((custom-context
                  (string-append
                   "PREVIOUS CHAT: "
                   (string-join (reverse messages) " ")))
                 (response (semantic-match string custom-context)))
            (set! messages (cons string messages))
            (set! responses (cons response responses))
            (printf "~%Response: ~a~%" response)
            (loop))))))))

(define-runtime-path data-dir "data")

(define (test)
  "Test code for Semantic Document Search using the uniform llmapis.rkt API and local vector database"
  (create-document (path->string (simplify-path (build-path data-dir "sports.txt"))))
  (create-document (path->string (simplify-path (build-path data-dir "chemistry.txt"))))
  (QA "What is the history of the science of chemistry?")
  (QA "What are the advantages of engaging in sports?"))

(module+ main
  ;; Uncomment below if you want to execute tests when running this module
  ;; (test)
  )



