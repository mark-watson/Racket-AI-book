# LLM Language for Racket

**Book Chapter:** [Using the Google Gemini, OpenAI, Anthropic, Mistral, and Local LLM APIs](https://leanpub.com/racket-ai/read) — *Practical Artificial Intelligence Development With Racket* (free to read online).

This directory experiments with the `#llm` language created by William J. Bowman for Racket. It allows integrating LLM generations natively within Racket syntax.

- **Documentation:** [https://docs.racket-lang.org/llm/index.html](https://docs.racket-lang.org/llm/index.html)
- **GitHub:** [https://github.com/wilbowma/llm-lang](https://github.com/wilbowma/llm-lang)

## Install package

    raco pkg install llm

## Run

You can run the different test files depending on which API you have configured:

    racket test_lang_mode_llm_openai.rkt
    racket test_llm_ollama.rkt
    racket test_llm_openai.rkt

## License and Copyright

This example is released using the Apache 2 license.
Copyright 2022-2025 Mark Watson. All rights reserved.
