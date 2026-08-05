.PHONY: run check lint help

RACKET := racket

help:
	@echo "Targets:"
	@echo "  run           start the coding-agent REPL"
	@echo "  check         run raco make (byte-compile) on all sources"
	@echo "  lint          alias for check"

run:
	$(RACKET) agent.rkt

check: lint

lint:
	raco make agent.rkt fireworks-ai.rkt tools.rkt approval.rkt search.rkt
