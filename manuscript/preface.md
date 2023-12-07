# Preface

I have been using Lisp languages since the 1970s. In 1982 my company bought a Lisp Machine for my use. A Lisp Machine provided an "all batteries included" working environment, but now no one seriously uses Lisp Machines. In this book I try to lead you, dear reader, through a process of creating a "batteries included" working environment using Racket Scheme.

The latest edition is always available for purchase at [https://leanpub.com/racket-ai](https://leanpub.com/racket-ai).  You can also read free online at [https://leanpub.com/racket-ai/read](https://leanpub.com/racket-ai/read). I offer the purchase option for readers who wish to directly support my work.

This is a “live book:” there will never be a second edition. As I add material and make corrections, I simply update the book and the free to read online copy and all eBook formats for purchase get updated.

I have been developing commercial Artificial Intelligence (AI) tools and applications since the 1980s and I usually use the Lisp languages Common Lisp, Clojure, Racket Scheme, and Gambit Scheme. Here you will find Racket code that I wrote for my own use and I am wrapping in a book in the hopes that it will also be useful to you, dear reader.

![Mark Watson](images/Mark.png)

I wrote this book for both professional programmers and home hobbyists who already know how to program in Racket (or another Scheme dialect) and who want to learn practical AI programming and information processing techniques. I have tried to make this an enjoyable book to work through. In the style of a “cook book,” the chapters can be read in any order. 

You can find the code examples in the following GitHub repository:

[https://github.com/mark-watson/Racket-AI-book-code](https://github.com/mark-watson/Racket-AI-book-code)

Git pull requests with code improvements will be appreciated by me and the readers of this book.

## Book Example Programs

The following diagram showing Racket software examples configured for your local laptop. There are several combined examples that build both to a Racket package that get installed locally, as well as command line programs that get built and deployed to **~/bin**. Other examples are either a command line tool or a Racket package.

![Example programs are packages and/or command line tools](images/software.png)

## Racket, Scheme, and Common Lisp

I like Common Lisp slightly more than Racket and other Scheme dialects, even though Common Lisp is ancient and has defects. Then why do I use Racket?
Racket is a family of languages, a very good IDE, and a rich ecosystem supported by many core Racket developers and Racket library authors. Choosing Racket Scheme was an easy decision, but there are also other Scheme dialects that I enjoy using:

- Gambit/C Scheme
- Gerbil Scheme (based on Gambit/C)
- Chez Scheme


## Personal Artificial Intelligence Journey: or, Life as a Lisp Developer

I have been interested in AI since reading Bertram Raphael’s excellent book *Thinking Computer: Mind Inside Matter* in the early 1980s. I have also had the good fortune to work on many interesting AI projects including the development of commercial expert system tools for the Xerox LISP machines and the Apple Macintosh, development of commercial neural network tools, application of natural language and expert systems technology, medical information systems, application of AI technologies to Nintendo and PC video games, and the application of AI technologies to the financial markets. I have also applied statistical natural language processing techniques to analyzing social media data from Twitter and Facebook. I worked at Google on their Knowledge Graph and I managed a deep learning team at Capital One where I was awarded 55 US patents.

I enjoy AI programming, and hopefully this enthusiasm will also infect you.


## Acknowledgements

I produced the manuscript for this book using the [leanpub.com](http://leanpub.com) publishing system and I recommend leanpub.com to other authors.


**Editor: Carol Watson**

Thanks to the following people who found typos in this and earlier book editions: David Rupp.
