# A Quick Racket Tutorial

If you are an experienced Racket developer then feel free to skip this chapter! I wrote this tutprial to cover just the aspects of using Racket that you, dear reader, will need in the book example programs.

## Testing for Equality

TBD

## Lists

literal lists

using: (range 10), for example

etc.

TBD

## Defining Functions

TBD

We will be using functions that take other functions as arguments. The following examples will get you through the rest of this book:

```racket
> (range 5)
'(0 1 2 3 4)
> (define (1+ n) (+ n 1))
> (map 1+ (range 5))
'(1 2 3 4 5)
> (map + (range 5) '(100 101 102 103 104))
'(100 102 104 106 108)
> 
```
TBD: explain the **map** functions ability to adjust its expected arguments to the arity of the function being mapped over data.

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

## Racket Structure Types

A structurer type is like a list that has named list elements. When you define a structure the Racket system writes getter and setter methods to access and change structure attribute values. Racket also generates a constructor function with the structure name. Let's look at a simple example in a Racket REPL of creating a structure with mutable elements:

```racket
> (struct person (name age email) #:mutable)
> (define henry (person "Henry Higgans" 45 "henry@higgans.com"))
> (person-age henry)
45
> (set-person-age! henry 46)
> (person-age henry)
46
> (set-person-email! henry "henryh674373551@gmail.com")
> (person-email henry)
"henryh674373551@gmail.com"
> 
```

If you don't add **#:mutable** to a **struct** definition, then no **set-NAME-ATTRIBYE!** methods are generated.

Racket also supports object oriented programming style classes with methods. I don't use classes in the book examples so you, dear reader, can read the official Racket [documentatiuon on classes](https://docs.racket-lang.org/guide/classes.html) if you want to use Racket in a non-functional way.


## TBD ????

## Simple HTTP GET and POST Operations 

TBD - we will need this later in the book
