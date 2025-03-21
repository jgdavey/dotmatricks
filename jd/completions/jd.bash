_jd() {
  COMPREPLY=()
  local word="${COMP_WORDS[COMP_CWORD]}"
  local completions

  if [ "$COMP_CWORD" -eq 1 ]; then
    completions="$(jd completions --commands | sed -e 's/\[.*//g')"
  else
    export COMP_WORD="$word"
    completions="$(jd completions "${COMP_WORDS[1]}" "${COMP_WORDS[@]:2}" | sed -e 's/\[.*//g')"
  fi
  COMPREPLY=( $(compgen -W "$completions" -- "$word") )
}

complete -F _jd jd
