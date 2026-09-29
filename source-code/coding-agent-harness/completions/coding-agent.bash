# bash completion for coding-agent
_coding_agent() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  local opts="--help --version --prompt --stdin --yes --dry-run --provider --model --cwd --debug --quiet --plain --no-color -h -p -y -q -v"
  COMPREPLY=($(compgen -W "$opts" -- "$cur"))
}
complete -F _coding_agent coding-agent
