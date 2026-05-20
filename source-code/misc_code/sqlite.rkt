#lang racket

(require db)

(define db-file "test.db")

(module+ main
  (define db (sqlite3-connect #:database db-file #:mode 'create))
  ;; Clean up any old table or create if not exists
  (query-exec db "drop table if exists person")
  (query-exec db "create table person (name varchar(30), age integer, email varchar(20))")
  (query-exec db "insert into person values ('Mary', 34, 'mary@test.com')")
  (println (query-rows db "select * from person"))
  (disconnect db))
