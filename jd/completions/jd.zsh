_jd_commands() {
  local cmds
  local -a commands
  cmds="$(jd commands)"
  commands=(${(ps:\n:)cmds})
  _wanted command expl "jd command" compadd -a commands
}

_jd_subcommands() {
  local cmd subcmds
  local -a commands
  cmd="${words[2]}"
  subcmds="$(jd completions $cmd ${words[3,$(($CURRENT - 1))]})"
  if [ -n "$subcmds" ]; then
    commands=(${(ps:\n:)subcmds})
    _wanted subcommand expl "jd $cmd subcommand" compadd -a commands
  else
    _default
  fi
}

_jd() {
  case $CURRENT in
    2) _jd_commands ;;
    *) _jd_subcommands ;;
  esac
}

compdef _jd jd
