# Web Scraping

I often write software to automatically collect and use data from the web and other sources. As a practical matter, much of the data that many people use for machine learning comes from either the web or from internal data sources. This section provides some guidance and examples for getting text data from the web.

Before we start a technical discussion about web scraping I want to point out that much of the information on the web is copyright and the first thing that you should do is to read the terms of service for web sites to insure that your use of "scraped" or "spidered" data conforms with the wishes of the persons or organizations who own the content and pay to run scraped web sites.

We start with low-level Racket code examples in the GitHub repository for this book in the directory **Racket-AI-book-code/misc_code**. We will then implement a standalone library in the directory **Racket-AI-book-code/webscrape**.

## Getting Started Web Scraping

All of the examples in the section can be found in the Racket code snippet files in the directory **Racket-AI-book-code/misc_code**.

I have edited the output for brevity in the following REPL outoput:

```racket
$ racket
Welcome to Racket v8.10 [cs].
> (require net/http-easy)
> (require html-parsing)
> (require net/url xml xml/path)
> (require racket/pretty)
> (define res-stream
    (get "https://markwatson.com" #:stream? #t))
> res-stream
#<response>
> (define lst
    (html->xexp (response-output res-stream)))
> lst
'(*TOP*
  (*DECL* DOCTYPE html)
  "\n"
  (html
   (@ (lang "en-us"))
   "\n"
   "  "
   (head
    (title
     "Mark Watson: AI Practitioner and Author of 20+ AI Books | Mark Watson")
  ...
```


```racket
> (se-path*/list '(p) lst) ;; get text from all p elements
'("My customer list includes: Google, Capital One, Babylist, Olive AI, CompassLabs, Mind AI, Disney, SAIC, Americast, PacBell, CastTV, Lutris Technology, Arctan Group, Sitescout.com, Embed.ly, and Webmind Corporation."
  "I have worked in the fields of general\n"
  "      artificial intelligence, machine learning, semantic web and linked data, and\n"
  "      natural language processing since 1982."
  "My eBooks are available to read for FREE or you can purchase them at "
  (a (@ (href "https://leanpub.com/u/markwatson")) "leanpub")
  ...
```


```racket
> (define lst-p (se-path*/list '(p) lst))
> (define lst-p (se-path*/list '(p) lst))
> (filter (lambda (s) (string? s)) lst-p) ;; keep only text strings
'("My customer list includes: Google, Capital One, Babylist, Olive AI, CompassLabs, Mind AI, Disney, SAIC, Americast, PacBell, CastTV, Lutris Technology, Arctan Group, Sitescout.com, Embed.ly, and Webmind Corporation."
```


```racket
#<procedure:string-normalize-spaces>
> (string-normalize-spaces
   (string-join
    (filter (lambda (s) (string? s)) lst-p)
    "\n"))
"My customer list includes: Google, Capital One, Babylist, Olive AI, CompassLabs, Mind AI, Disney, SAIC, Americast, PacBell, CastTV, Lutris Technology, Arctan Group, Sitescout.com, Embed.ly, and Webmind Corporation. I have worked in the fields of general artificial intelligence, machine learning, semantic web and linked data, and natural language processing since 1982.
  ...
"
```

Now we will extract HTML anchor links:

```racket
> (se-path*/list '(href) lst) ;; get all links from HTML as a lisp
'("/index.css"
  "https://mark-watson.blogspot.com"
  "#fun"
  "#books"
  "#opensource"
  "https://markwatson.com/privacy.html"
  "https://leanpub.com/u/markwatson"
  "/nda.txt"
  "https://mastodon.social/@mark_watson"
  "https://twitter.com/mark_l_watson"
  "https://github.com/mark-watson"
  "https://www.linkedin.com/in/marklwatson/"
  "https://markwatson.com/index.rdf"
  "https://www.wikidata.org/wiki/Q18670263"
 ...
)
```

## Implementation of a Racket Web Scraping Library

TBD
