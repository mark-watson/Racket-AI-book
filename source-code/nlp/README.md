# Natural Language Processing in Racket

**Book Chapter:** [Natural Language Processing](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This project implements various NLP utilities in Racket, including Part-Of-Speech (POS) tagging and named entity recognition. It demonstrates foundational text processing techniques essential for AI applications.

## Install as a local package

    raco pkg install --scope user

When you create a package with "--scope user" you are defining a link to the location of your Racket library source code. For example, after doing this install, look in the file:

    ~/Library/Racket/8.10/links.rktd

On macOS, you might see the following list added:

    ("nlp" (up up up #"GITHUB" #"Racket-AI-book-code" #"nlp"))

## Run

    racket main.rkt
    # Or to test the tagger directly:
    racket fasttag.rkt

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2025 Mark Watson. All rights reserved.
