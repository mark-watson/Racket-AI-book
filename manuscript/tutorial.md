# A Quick Racket Tutorial

If you are an experienced Racket developer then feel free to skip this chapter! I wrote this tutorial to cover just the aspects of using Racket that you, dear reader, will need to understand, modify, and generally reuse the example programs in this book.

I assume that you have read the section **Racket Essentials** in the [The Racket Guide](https://docs.racket-lang.org/guide/) written by Matthew Flatt, Robert Bruce Findler, and the PLT group. Here I just cover some basics of getting started so you can enjoy the later code examples without encountering "road blocks."

## Installing Packages

The DrRacket IDE lets you interactively install packages. I prefer using the command line so, for example, I would install SQlite support using:

    raco pkg install sqlite-table

We can then require the code in this package in our Racket programs:

```racket
(require sqlite-table)
```

Note that when the argument to **require** is a symbol (not a string) then modules are searched and loaded from your system. When the argument is a string like "utils.rkt" that a module is loaded from a file in the current directory.

## Installing Local Packages In Place

In a later chapter **Natural Language Processing (NLP)** we define a fairly complicated local package. This package has one unusual requirement that you may or may not need in your own projects: My NLP library requires static linguistic data files that are stored in the directory **Racket-AI-book-code/nlp/data**. If I am in the directory **Racket-AI-book-code/nlp** working on the Racket code, it is simple enough to just open the files in **./data/...**.

The default for installing your own Racket packages is to link to the original source directory on your laptop's file system. Let's walk through this. First, I will make sure my library code is compiled and then install the code in the current directory:

```bash
cd Racket-AI-book-code/nlp/
raco make *.rkt
raco pkg install --scope user
```

Then I can run the **racket** REPL (or DrRacket) on my laptop and use my NLP package by requiring the code in this package in our Racket programs (shown in a REPL):

```racket
> (require nlp)
loading lex-hash......done.
> (parts-of-speech (list->vector '("the" "cat" "ran")))
'#("DT" "NN" "VBD")
> (find-place-names
    '#("George" "Bush" "went" "to" "San" "Diego"
       "and" "London") '())
'("London" "San Diego")
> (find-place-names
    '#("George" "Bush" "went" "to" "San" "Diego"
       "and" "London") '())
'("London" "San Diego")
> 
```


## Mapping Over Lists

We will be using functions that take other functions as arguments:

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

If you don't add **#:mutable** to a **struct** definition, then no **set-NAME-ATTRIBUTE!** methods are generated.

Racket also supports object oriented programming style classes with methods. I don't use classes in the book examples so you, dear reader, can read the official Racket [documentation on classes](https://docs.racket-lang.org/guide/classes.html) if you want to use Racket in a non-functional way.


## Simple HTTP GET and POST Operations 

We will be using HTTP GET and POST instructions in later chapters for web scraping and accessing remote APIs, such as those for OpenAI GPT-4, Hugging Face, etc. We will see more detail later but for now, you can try a simple example:

```racket
#lang racket

(require net/http-easy)
(require html-parsing)
(require net/url xml xml/path)
(require racket/pretty)

(define res-stream
  (get "https://markwatson.com" #:stream? #t))

(define lst
  (html->xexp (response-output res-stream)))

(response-close! res-stream)

(displayln "\nParagraph text:\n")

(pretty-print (take (se-path*/list '(p) lst) 8))

(displayln "\nLI text:\n")

(pretty-print (take (se-path*/list '(li) lst) 8))
```

The output is:

```

Paragraph text:

'("My customer list includes: Google, Capital One, Babylist, Olive AI, CompassLabs, Mind AI, Disney, SAIC, Americast, PacBell, CastTV, Lutris Technology, Arctan Group, Sitescout.com, Embed.ly, and Webmind Corporation."
  "I have worked in the fields of general\n"
  "      artificial intelligence, machine learning, semantic web and linked data, and\n"
  "      natural language processing since 1982."
  "My eBooks are available to read for FREE or you can purchase them at "
  (a (@ (href "https://leanpub.com/u/markwatson")) "leanpub")
  "."
  "My standard ")

LI text:

'((@ (class "list f4 f3-ns fw4 dib pr3"))
  "\n"
  "            "
  (& nbsp)
  (& nbsp)
  (a
   (@
    (class "hover-white no-underline white-90")
    (href "https://mark-watson.blogspot.com")
    (target "new")
    (title "Mark's Blog on Blogspot"))
   "\n"
   "              Read my Blog\n"
   "            ")
  "\n"
  "          ")
```

## Using Racket ~/.racketrc Initialization File

In my Racket workflow I don’t usually use **~/.racketrc** to define initial forms that are automatically loaded when starting the **racket** command line tool or the **DrRacket** application. That said I do like to use **~/.racketrc** for temporary initialization forms when working on a specific project to increase the velocity of interactive development.

Here is an example use:

```console
$ cat ~/.racketrc
(define (foo-list x)
  (list x x x))
$ racket
Welcome to Racket v8.10 [cs].
> (foo-list 3.14159)
'(3.14159 3.14159 3.14159)
> 
```

If you have local and public libraries you frequently load you can permanently keep **require** forms for them in **~/.racketrc** but that will slightly slow down the startup times of **racket** and **DrRacket**.

## Tutorial Wrap Up

The rest of this book is comprised of example Racket programs that I have written for my own enjoyment that I hope will also be useful to you, dear reader. Please refer to the [https://docs.racket-lang.org/guide/](The Racket Guide) for more technical detail on effectively using the Racket language and ecosystem.
