.PHONY: run check lint help make-executable install uninstall dist completions

RACKET := racket
PREFIX ?= /usr/local
BINDIR := $(PREFIX)/bin

help:
	@echo "Targets:"
	@echo "  run              start the coding-agent REPL (racket agent.rkt)"
	@echo "  check            run raco make (byte-compile) on all sources"
	@echo "  lint             alias for check"
	@echo "  make-executable  build standalone executable ./coding-agent"
	@echo "  install          install ./coding-agent to \$$(PREFIX)/bin (default /usr/local)"
	@echo "  uninstall        remove installed binary"
	@echo "  dist             build distributable folder via raco distribute (no Racket needed on target)"
	@echo "  completions      generate shell completions into ./completions/"
	@echo "  clean            clean directory of dev artifacts"

run:
	$(RACKET) agent.rkt

check: lint

clean:
	rm -rf compiled dist coding-agent *~

lint:
	raco make agent.rkt fireworks-ai.rkt mlx-serve.rkt chat-loop.rkt tools.rkt approval.rkt search.rkt harness-config.rkt line-input.rkt

# ++lib embeds the readline collection so the standalone executable keeps line
# editing; it embeds without instantiating, so a machine without libedit still
# falls back to plain read-line at run time instead of failing to build.
make-executable:
	raco exe -o coding-agent ++lib readline/readline agent.rkt

install: make-executable
	@mkdir -p $(DESTDIR)$(BINDIR)
	@install -m 0755 coding-agent $(DESTDIR)$(BINDIR)/coding-agent
	@echo "Installed to $(DESTDIR)$(BINDIR)/coding-agent"

uninstall:
	@rm -f $(DESTDIR)$(BINDIR)/coding-agent
	@echo "Removed $(DESTDIR)$(BINDIR)/coding-agent"

dist: make-executable
	raco distribute dist coding-agent
	@echo "Distributable in ./dist/ — run ./dist/bin/coding-agent on target without Racket"

completions:
	@mkdir -p completions
	@echo "# bash completion for coding-agent" > completions/coding-agent.bash
	@echo "_coding_agent() {" >> completions/coding-agent.bash
	@printf '  local cur="$${COMP_WORDS[COMP_CWORD]}"\n' >> completions/coding-agent.bash
	@echo "  local opts=\"--help --version --prompt --stdin --yes --dry-run --provider --model --cwd --debug --quiet --plain --no-color -h -p -y -q -v\"" >> completions/coding-agent.bash
	@printf '  COMPREPLY=($$(compgen -W "$$opts" -- "$$cur"))\n' >> completions/coding-agent.bash
	@echo "}" >> completions/coding-agent.bash
	@echo "complete -F _coding_agent coding-agent" >> completions/coding-agent.bash
	@echo "# zsh completion for coding-agent" > completions/coding-agent.zsh
	@echo "#compdef coding-agent" >> completions/coding-agent.zsh
	@echo "_coding_agent() { _arguments '-h[help]' '--help[help]' '-v[version]' '--version[version]' '-p[prompt]:prompt:' '--prompt[prompt]:prompt:' '--stdin[stdin]' '-y[yes]' '--yes[yes]' '--dry-run[dry-run]' '--provider[provider]:provider:(fireworks mlx)' '--model[model]:model:' '--cwd[cwd]:dir:_files -/' '--debug[debug]' '-q[quiet]' '--quiet[quiet]' '--plain[plain]' '--no-color[no-color]' '*:prompt:_files' }" >> completions/coding-agent.zsh
	@echo "compdef _coding_agent coding-agent" >> completions/coding-agent.zsh 2>/dev/null || true
	@echo "# fish completion for coding-agent" > completions/coding-agent.fish
	@echo "complete -c coding-agent -s h -l help -d 'Show help'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -s v -l version -d 'Show version'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -s p -l prompt -d 'Prompt text' -r" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -l stdin -d 'Read prompt from stdin'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -s y -l yes -d 'Auto-approve edits'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -l dry-run -d 'Show diffs without writing'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -l provider -d 'Provider' -r -a 'fireworks mlx'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -l model -d 'Model id' -r" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -l cwd -d 'Working directory' -r -a '(__fish_complete_directories)'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -l debug -d 'Debug logging'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -s q -l quiet -d 'Quiet'" >> completions/coding-agent.fish
	@echo "complete -c coding-agent -l plain -l no-color -d 'No colors'" >> completions/coding-agent.fish
	@echo "Generated completions/coding-agent.{bash,zsh,fish}"

