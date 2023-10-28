# Web Scraping

I often write software to automatically collect and use data from the web and other sources. As a practical matter, much of the data that many people use for machine learning comes from either the web or from internal data sources. This section provides some guidance and examples for getting text data from the web.

Before we start a technical discussion about web scraping I want to point out that much of the information on the web is copyright, so first you should read the terms of service for web sites to insure that your use of "scraped" or "spidered" data conforms with the wishes of the persons or organizations who own the content and pay to run scraped web sites.

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

Different element types are **html**, **head**, **p**, **h1**, **h2**, etc. If you are familiar with XPATH operations for XML data, then the function **se-path/list** will make more sense to your. The function **se-path/list** takes a list of element types from a list and recursively searches an input s-expression for lists starting with one of the target element types. In the following example we extract all elements of type **p**:

```racket
> (se-path*/list '(p) lst) ;; get text from all p elements
'("My customer list includes: Google, Capital One, Babylist, Olive AI, CompassLabs, Mind AI, Disney, SAIC, Americast, PacBell, CastTV, Lutris Technology, Arctan Group, Sitescout.com, Embed.ly, and Webmind Corporation."
  "I have worked in the fields of general\n"
  "   artificial intelligence, machine learning, semantic web and linked data, and\n"
  "      natural language processing since 1982."
  "My eBooks are available to read for FREE or you can   purchase them at "
  (a (@ (href "https://leanpub.com/u/markwatson")) "leanpub")
  ...
```


```racket
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

The web scraping library listed below can be found in the directory **Racket-AI-book/manuscript**. The following listing of **webscrape.rkt** should look familiar after reading the code snippets in the last section.

The provided Racket Scheme code defines three functions to interact with and process web resources: `web-uri->xexp`, `web-uri->text`, and `web-uri->links`.

**`web-uri->xexp`**:
   - Requires three libraries: `net/http-easy`, `html-parsing`, and `net/url xml xml/path`.
   - Given a URI (`a-uri`), it creates a stream (`a-stream`) using the `get` function from the `net/http-easy` library to fetch the contents of the URI.
   - Converts the HTML content of the URI to an S-expression (`xexp`) using the `html->xexp` function from the `html-parsing` library.
   - Closes the response stream using `response-close!` and returns the `xexp`.

**`web-uri->text`**:
   - Calls `web-uri->xexp` to convert the URI content to an `xexp`.
   - Utilizes `se-path*/list` from the `xml/path` library to extract all paragraph elements (`p`) from the `xexp`.
   - Filters the paragraph elements to retain only strings (excluding nested tags or other structures).
   - Joins these strings with a newline separator, normalizing spaces using `string-normalize-spaces` from the `srfi/13` library.

**`web-uri->links`**:
   - Similar to `web-uri->text`, it starts by converting URI content to an `xexp`.
   - Utilizes `se-path*/list` to extract all `href` attributes from the `xexp`.
   - Filters these `href` attributes to retain only those that are external links (those beginning with "http").

In summary, these functions collectively enable the extraction and processing of HTML content from a specified URI, converting HTML to a more manageable S-expression format, and then extracting text and links as required.

```racket
#lang racket

(require net/http-easy)
(require html-parsing)
(require net/url xml xml/path)
(require srfi/13) ;; for strings

(define (web-uri->xexp a-uri)
  (let* ((a-stream
          (get a-uri #:stream? #t))
         (lst (html->xexp (response-output a-stream))))
    (response-close! a-stream)
    lst))

(define (web-uri->text a-uri)
  (let* ((a-xexp
          (web-uri->xexp a-uri))
         (p-elements (se-path*/list '(p) a-xexp))
         (lst-strings
          (filter
           (lambda (s) (string? s))
           p-elements)))
    (string-normalize-spaces
     (string-join lst-strings "\n"))))

(define (web-uri->links a-uri)
  (let* ((a-xexp
          (web-uri->xexp a-uri)))
    ;; we want only external links so filter out local links:
    (filter
     (lambda (s) (string-prefix? "http" s))
     (se-path*/list '(href) a-xexp))))
```

Here are a few examples in a Racket REPL (most output omitted for brevity):

```racket
> (web-uri->xexp "https://knowledgebooks.com")
'(*TOP*
  (*DECL* DOCTYPE html)
  "\n"
  (html
   "\n"
   "\n"
   (head
    "\n"
    "    "
    (title "KnowledgeBooks.com - research on the Knowledge Management, and the Semantic Web ")
    "\n"
 ...

> (web-uri->text "https://knowledgebooks.com")
"With the experience of working on Machine Learning and Knowledge Graph applications
 ...

> (web-uri->links "https://knowledgebooks.com")
'("http://markwatson.com"
  "https://oceanprotocol.com/"
  "https://commoncrawl.org/"
  "http://markwatson.com/consulting/"
  "http://kbsportal.com")
```

If you want to install this library on your laptop using linking (requiring the library access a link to the source code in the directory **Racket-AI-book-code/webscrape**) run the following in the library source directory **Racket-AI-book-code/webscrape**:

   raco pkg install --scope user