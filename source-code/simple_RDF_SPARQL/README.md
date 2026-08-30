# Simple RDF Datastore with SPARQL

**Book Chapter:** [Implementing a Simple RDF Datastore With Partial SPARQL Support](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This project implements a simple RDF (Resource Description Framework) datastore in Racket, featuring partial support for querying with SPARQL. This is useful for representing and querying knowledge graphs locally.

## Architecture

![Generated image](architecture.png)

## Files

- `rdf_sparql.rkt` - Core library: triple store, parser, and query engine, plus a demo
- `rdf_extended.rkt` - Extensions: N-Triples persistence, FILTER support, and a larger demo dataset
- `tests.rkt` - rackunit tests for the store, parser, joins, persistence, and filters

## Run

    racket rdf_sparql.rkt    # original demo
    racket rdf_extended.rkt  # persistence and FILTER demo
    raco test tests.rkt      # test suite

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2026 Mark Watson. All rights reserved.
