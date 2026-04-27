# Brave Search API Utilities

**Book Chapter:** [Web Scraping](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This project demonstrates how to perform automated web searches in Racket using the Brave Search API. This is useful for fetching real-time data to ground LLM generations.

## Setup

You will need to get a free API key at [https://brave.com/search/api](https://brave.com/search/api) to use these code examples. You can use the search API 2000 times a month for free, or pay $5/month to get 20 million API calls a month.
Set the key as an environment variable (e.g., `BRAVE_API_KEY`, check the source for the exact name).

## Run

    racket brave_search.rkt

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2025 Mark Watson. All rights reserved.
