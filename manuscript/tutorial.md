# A Quick Racket Tutorial

If you are an experienced Racket developer then feel free to skip this chapter! I wrote this tutprial to cover just the aspects of using Racket that you, dear reader, will need in the book example programs.

## Lists

TBD

## Hash Tables

The following listing shows the file **misc_code/hash_tests.rkt**:

```racket
#lang racket

(define h1 (hash "dog" '("friendly" 5) "cat" '("not friendly" 2))) ;; not mutable

(define cat (hash-ref h1 "cat"))

(define h2 (make-hash)) ;; mutable
(hash-set! h2 "snake" '("smooth" 4))

;; make-hash also accepts a second argument that is a list of pairs:
(define h3 (make-hash '(("dog" '("friendly" 5)) ("cat" '("not friendly" 2)))))
(hash-set! h3 "snake" '("smooth" 4))
(hash-set! h3 "cat" '("sometimes likeable" 3)) ;; overwrite key value

;; for can be used with hash tables:

(for ([(k v) h3]) (println '("key:" k "value:" v)))
```

Here is a lising of the output window after running this file and then manually evaluating *h1*, *h2*, and *h3* in the REPL (like all listings in this book, I manually edit the output to fit page width):

```
Welcome to DrRacket, version 8.10 [cs].
Language: racket, with debugging; memory limit: 128 MB.
'("key:" k "value:" v)
'("key:" k "value:" v)
'("key:" k "value:" v)
> h1
'#hash(("cat" . ("not friendly" 2)) ("dog" . ("friendly" 5)))
> h2
'#hash(("snake" . ("smooth" 4)))
> h3
'#hash(("cat" . ("sometimes likeable" 3))
                ("dog" . ('("friendly" 5)))
                ("snake" . ("smooth" 4)))
> 
```
## TBD ????

## Simple HTTP GET and POST Operations 

TBD - we will need this later in the book

