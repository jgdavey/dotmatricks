fpath=(
  $fpath
  ~/.zsh/functions
  $HOMEBREW_PREFIX/share/zsh/site-functions
)

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

## History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 50000 ] && SAVEHIST=50000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data
setopt appendhistory
setopt inc_append_history     # incrementally flush history (immediate)

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
  prefix=
  if [ -n "$1" ]; then
    prefix="${1%/}/"
  fi
  ls -ld -F ${prefix}.[^.]*
}

(( ${+aliases[e]} )) && unalias e

export EXTENDED_PROMPT_COMMAND=""
extended_prompt() {
    if [ -n "$EXTENDED_PROMPT_COMMAND" ]; then
        $EXTENDED_PROMPT_COMMAND
    fi
}

# show non-success exit code in right prompt
RPROMPT="%(?..{%{$fg[red]%}%?%{$reset_color%}})"

# default apps
(( ${+PAGER} )) || export PAGER='less'

ORIGINAL_PROMPT="$PROMPT"

# import local zsh customizations, if present
zrcl="$HOME/.zshrc.local"
[[ ! -a $zrcl ]] || source $zrcl

# prompt
if [ "$PROMPT" = "$ORIGINAL_PROMPT" ]; then
    p=
    if [ -n "$SSH_CONNECTION" ]; then
        p='%{$fg_bold[yellow]%}%n@%m'
    else
        p='%{$fg_bold[green]%}%n@%m'
    fi
    PROMPT="$p%{\$reset_color%}:%{\$fg[cyan]%}%~ %{\$reset_color%}\$(git_prompt_info)\$(extended_prompt)%{\$reset_color%}%{\$fg_bold[yellow]%}%# %{\$reset_color%}"
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi

# remove duplicates in $PATH
typeset -aU path
