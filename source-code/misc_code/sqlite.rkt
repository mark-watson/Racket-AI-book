#lang racket

;;; sqlite.rkt — Using SQLite through Racket's built-in db library
;;; Apache 2 License

(require db)

(provide open-person-db add-person! find-person update-person-age!)

;; Open (creating if needed) a database and make sure the table exists.
;; Pass 'memory for a throwaway in-memory database.
(define (open-person-db [db-file "test.db"])
  (define conn (sqlite3-connect #:database db-file #:mode 'create))
  (query-exec conn
              "create table if not exists person
                 (name varchar(30) primary key, age integer, email varchar(40))")
  conn)

;; Always pass values as query parameters (the ? placeholders) and never
;; build SQL by pasting strings together: parameters are escaped for you.
(define (add-person! conn name age email)
  (query-exec conn
              "insert or replace into person values (?, ?, ?)"
              name age email))

;; query-maybe-row returns one row as a vector, or #f when nothing matches.
(define (find-person conn name)
  (query-maybe-row conn "select * from person where name = ?" name))

(define (update-person-age! conn name new-age)
  (query-exec conn "update person set age = ? where name = ?" new-age name))

(module+ main
  (define conn (open-person-db 'memory))
  (add-person! conn "Mary" 34 "mary@test.com")
  ;; A transaction: either both inserts happen or neither does.
  (call-with-transaction
   conn
   (lambda ()
     (add-person! conn "Carlos" 51 "carlos@test.com")
     (add-person! conn "Ana" 28 "ana@test.com")))
  (println (query-rows conn "select * from person order by name"))
  (println (find-person conn "Mary"))
  (update-person-age! conn "Mary" 35)
  (println (find-person conn "Mary"))
  (println (find-person conn "Nobody"))
  (println (query-value conn "select count(*) from person"))
  (disconnect conn))

(module+ test
  (require rackunit)
  (define conn (open-person-db 'memory))
  (add-person! conn "Mary" 34 "mary@test.com")
  (check-equal? (find-person conn "Mary") (vector "Mary" 34 "mary@test.com"))
  (update-person-age! conn "Mary" 35)
  (check-equal? (vector-ref (find-person conn "Mary") 1) 35)
  (check-false (find-person conn "Nobody"))
  ;; a name containing a quote is safe because it is a parameter
  (add-person! conn "O'Brien" 40 "ob@test.com")
  (check-equal? (query-value conn "select count(*) from person") 2)
  (disconnect conn))
