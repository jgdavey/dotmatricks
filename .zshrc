fpath=(
  $fpath
  ~/.rvm/scripts/zsh/Completion
  ~/.zsh/functions
  /usr/local/share/zsh/site-functions
)

[ ! -f "$HOME/.profile" ] || source "$HOME/.profile"
[ ! -f "$HOME/.sharedrc" ] || source "$HOME/.sharedrc"

# color term
export LC_CTYPE=en_US.UTF-8

# make with the nice completion
autoload -U compinit; compinit

# Completion for kill-like commands
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"
zstyle ':completion:*:ssh:*' tag-order hosts users
zstyle ':completion:*:ssh:*' group-order hosts-domain hosts-host users hosts-ipaddr

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zshcache

# matches case insensitive for lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Shows files in color, same as LSCOLOR
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _approximate

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# make with the pretty colors
autoload colors; colors

# just say no to zle vim mode:
bindkey -e

# history options
setopt appendhistory extended_history histignoredups inc_append_history share_history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=10000

# options
setopt extendedglob interactivecomments prompt_subst no_list_beep no_bg_nice always_to_end nonomatch

# Bindings
# external editor support
autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# Partial word history completion
bindkey '\ep' up-line-or-search
bindkey '\en' down-line-or-search
bindkey '\ew' kill-region

alias lsd='ls -ld *(-/DN)'
l.() {
  ls -ld "${1:-$PWD}"/.[^.]*
}

(( ${+aliases[e]} )) && unalias e

# prompt
p=
if [ -n "$SSH_CONNECTION" ]; then
  p='%{$fg_bold[yellow]%}%n@%m'
else
  p='%{$fg_bold[green]%}%n@%m'
fi
PROMPT="$p%{\$reset_color%}:%{\$fg[cyan]%}%~ %{\$reset_color%}\$(git_prompt_info)%{\$fg_bold[yellow]%}%# %{\$reset_color%}"

# show non-success exit code in right prompt
RPROMPT="%(?..{%{$fg[red]%}%?%{$reset_color%}})"

# default apps
(( ${+PAGER}   )) || export PAGER='less'

# import local zsh customizations, if present
zrcl="$HOME/.zshrc.local"
[[ ! -a $zrcl ]] || source $zrcl

# remove duplicates in $PATH
typeset -aU path

# added by travis gem
[ -f /Users/jgdavey/.travis/travis.sh ] && source /Users/jgdavey/.travis/travis.sh
