# Embeddings Database

**Book Chapter:** [Retrieval Augmented Generation of Text Using Embeddings](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This project provides a simple vector database implementation for managing and querying text embeddings, useful for building Retrieval-Augmented Generation (RAG) pipelines in Racket.

## Install as a local package

    raco pkg remove
    raco pkg install --scope user

If you change the source code, run the following to update the linked (installed in place) package **embeddingsdb**:

    raco make main.rkt

## Run

    racket main.rkt

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2025 Mark Watson. All rights reserved.
