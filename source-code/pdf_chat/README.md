# PDF Chat and Parsing

**Book Chapter:** [Retrieval Augmented Generation of Text Using Embeddings](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This project demonstrates extracting text from PDF files using Racket, which is a foundational step for Retrieval-Augmented Generation (RAG) pipelines over documents.

*Note: These examples are currently experimental.*

## Install dependencies

On macOS:
```bash
brew install poppler
```

Then install the following in your `.profile`, `.zshrc`, `.bashrc`, etc.:
```bash
export DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/lib/
```

Then install Racket packages:
```bash
raco pkg install pdf-read
raco pkg install racket-poppler
```

## Run

Currently on macOS, these examples may not work in DrRacket. Run them on the command line, for example:

    racket pdf2text.rkt
    racket pdfchat.rkt

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2025 Mark Watson. All rights reserved.
