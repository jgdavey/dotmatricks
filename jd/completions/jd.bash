_jd() {
  COMPREPLY=()
  local word="${COMP_WORDS[COMP_CWORD]}"

  if [ "$COMP_CWORD" -eq 1 ]; then
    COMPREPLY=( $(compgen -W "$(jd commands)" -- "$word") )
  else
    local command="${COMP_WORDS[1]}"
    local args
    if [ "$COMP_CWORD" -eq 2 ] || [ -z "$COMP_WORDS[2]" ]; then
      args=""
    else
      args="${COMP_WORDS[@]:2}"
    fi
    local completions="$(jd completions "$command" $args)"
    if [ "$completions" ]; then
      COMPREPLY=( $(compgen -W "$completions" -- "$word") )
    else
      return 1
    fi
  fi
}

complete -o default -F _jd jd
