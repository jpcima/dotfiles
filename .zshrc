autoload -Uz promptinit && promptinit
prompt walters green
PS1=$(sed 's/%n/%F{blue}%n%f/g' <<< "$PS1")
PS1=$(sed 's/%m/%F{magenta}%m%f/g' <<< "$PS1")

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

setopt prompt_subst
precmd() { vcs_info }
RPS1="$RPS1 "$'%F{blue}${vcs_info_msg_0_}%f'

# for linux console and RH/Debian xterm
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[7~" beginning-of-line
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\e[5D" backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
# for rxvt
bindkey "\e[8~" end-of-line
# for non RH/Debian xterm, can't hurt for RH/DEbian xterm
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
# for freebsd console
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line

setopt rmstarsilent
setopt interactivecomments
setopt nobeep
setopt histnostore
setopt histignoredups

fpath=(~/.zsh/completion $fpath)

zmodload zsh/complist
autoload -Uz compinit && compinit

#zstyle ':completion:*' completer _expand _complete
#zstyle ':completion:*' insert-unambiguous true
#zstyle :compinstall filename '/home/jpc/.zshrc'
#zstyle ':completion:*' menu no
#zstyle ':completion:*:paths' accept-exact '*(N)'
zstyle ':completion:*' accept-exact '*(N)'

zstyle ':completion:::::' completer _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' list-colors ''

if test -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ; then 
  . "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
fi
