# fish completion for coding-agent
complete -c coding-agent -s h -l help -d 'Show help'
complete -c coding-agent -s v -l version -d 'Show version'
complete -c coding-agent -s p -l prompt -d 'Prompt text' -r
complete -c coding-agent -l stdin -d 'Read prompt from stdin'
complete -c coding-agent -s y -l yes -d 'Auto-approve edits'
complete -c coding-agent -l dry-run -d 'Show diffs without writing'
complete -c coding-agent -l provider -d 'Provider' -r -a 'fireworks mlx'
complete -c coding-agent -l model -d 'Model id' -r
complete -c coding-agent -l cwd -d 'Working directory' -r -a '(__fish_complete_directories)'
complete -c coding-agent -l debug -d 'Debug logging'
complete -c coding-agent -s q -l quiet -d 'Quiet'
complete -c coding-agent -l plain -l no-color -d 'No colors'
