# zsh completion for coding-agent
#compdef coding-agent
_coding_agent() { _arguments '-h[help]' '--help[help]' '-v[version]' '--version[version]' '-p[prompt]:prompt:' '--prompt[prompt]:prompt:' '--stdin[stdin]' '-y[yes]' '--yes[yes]' '--dry-run[dry-run]' '--provider[provider]:provider:(fireworks mlx)' '--model[model]:model:' '--cwd[cwd]:dir:_files -/' '--debug[debug]' '-q[quiet]' '--quiet[quiet]' '--plain[plain]' '--no-color[no-color]' '*:prompt:_files' }
compdef _coding_agent coding-agent
