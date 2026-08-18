.PHONY: run check lint help make-executable

RACKET := racket

help:
	@echo "Targets:"
	@echo "  run              start the coding-agent REPL"
	@echo "  check            run raco make (byte-compile) on all sources"
	@echo "  lint             alias for check"
	@echo "  make-executable  build standalone executable ./coding-agent"

run:
	$(RACKET) agent.rkt

check: lint

lint:
	raco make agent.rkt fireworks-ai.rkt ollama-ai.rkt chat-loop.rkt tools.rkt approval.rkt search.rkt interrupt.rkt

make-executable:
	raco exe -o coding-agent agent.rkt

