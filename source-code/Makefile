# Top-level Makefile for the book examples.
#
#   make compile   byte-compile every example that needs no optional package
#   make test      run every offline test suite (no API keys, no network)
#   make clean     remove compiled files
#
# Examples that need an optional package are compiled by "make compile-optional":
#   Malt_deep_learning   raco pkg install --auto malt
#   pdf_chat             raco pkg install --auto pdf-read   (plus poppler)
#   racket_llm_language  raco pkg install --auto llm

RACO ?= raco

# Directories whose .rkt files compile with a stock Racket plus the packages
# listed in README.md (http-easy, html-parsing, racklog).
CORE_DIRS = misc_code simple_RDF_SPARQL sparql webscrape lightpanda \
            Karpathy_MicroGPT llmapis llm-agent ollama_tools \
            coding-agent-harness embeddingsdb RAG \
            deep_learning_category_theory ReinforcementLearning nlp kgn \
            OPS5_in_Racket Racklog_expert_systems_logic math-pi-e \
            genetic_programming search_brave search_tavily Bridge_Game_webkit

OPTIONAL_DIRS = Malt_deep_learning pdf_chat racket_llm_language

# Offline test suites. Every one of these runs with no API key and no network.
TEST_FILES = misc_code/sqlite.rkt \
             simple_RDF_SPARQL/tests.rkt \
             sparql/tests.rkt \
             webscrape/tests.rkt \
             lightpanda/tests.rkt \
             Karpathy_MicroGPT/tests.rkt \
             llmapis/tests.rkt \
             ollama_tools/tests.rkt \
             coding-agent-harness/tests.rkt \
             embeddingsdb/tests.rkt \
             RAG/tests.rkt \
             deep_learning_category_theory/tests.rkt \
             ReinforcementLearning/tests.rkt \
             nlp/tests.rkt \
             kgn/tests.rkt \
             OPS5_in_Racket/tests.rkt \
             Racklog_expert_systems_logic/tests.rkt \
             math-pi-e/math-tests.rkt \
             Bridge_Game_webkit/tests.rkt

.PHONY: compile compile-optional test clean

compile:
	@for d in $(CORE_DIRS); do \
	  echo "compiling $$d"; \
	  $(RACO) make $$d/*.rkt || exit 1; \
	done

compile-optional:
	@for d in $(OPTIONAL_DIRS); do \
	  echo "compiling $$d"; \
	  $(RACO) make $$d/*.rkt || exit 1; \
	done

test:
	@for f in $(TEST_FILES); do \
	  echo "=== $$f"; \
	  (cd `dirname $$f` && $(RACO) test `basename $$f`) || exit 1; \
	done
	@echo "All offline test suites passed."

clean:
	rm -r -f */compiled */*.rkt~
