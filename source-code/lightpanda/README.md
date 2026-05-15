# Racket Lightpanda Browser Client

**Book Chapter:** [Interfacing with External Programs: A Lightpanda Browser Client](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This library provides a Racket interface to the [Lightpanda](https://lightpanda.io/) headless browser. Lightpanda renders JavaScript-heavy web pages from the command line, and this library wraps it in a clean Racket API for fetching rendered HTML, Markdown, or semantic tree output, and extracting links.

## Prerequisites

Install the Lightpanda command-line tool following the [Lightpanda documentation](https://lightpanda.io/docs/open-source/installation).

## Run

    racket lightpanda.rkt

## Install as a Racket package

    raco pkg install --scope user

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2025 Mark Watson. All rights reserved.
