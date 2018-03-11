# make sure to load it once only
test "$DOT_PROFILE_INCLUDED" = 1 && return
DOT_PROFILE_INCLUDED=1

# enable more colors
test "$TERM" = "xterm" && export TERM=xterm-256color

export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

export PATH="$HOME/Documents/LocalPrefix/bin:$PATH"

LSFLAGS="--group-directories-first --color=auto -p"
alias l="ls $LSFLAGS"
alias ls="ls $LSFLAGS"
alias ll="ls $LSFLAGS -l"

GREPFLAGS="--color=auto --exclude-dir=\\.git"
alias grep="grep $GREPFLAGS"
alias fgrep="fgrep $GREPFLAGS"
alias egrep="egrep $GREPFLAGS"

LESSFLAGS="-R -S"
alias less="less $LESSFLAGS"

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export EDITOR="emacs -nw"
alias edit="$EDITOR"

export LESSHISTFILE=/dev/null

function man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
      man "$@"
}

function aurget {
  git clone "https://aur.archlinux.org/$1.git/" "$1" && \
  cd "${1}"
}

function colorcat() {
  if test -t 1 -a "$#" -eq 1 ; then
      highlight --out-format=ansi --failsafe "$1" 2>/dev/null
  else
    cat "$@"
  fi
}

export WINEPREFIX="$HOME/.wine/temp32"
export WINEARCH="win32"
export WINEDLLOVERRIDES="winemenubuilder.exe=d"

function use_gcc() {
  export CC=gcc CXX=g++
}
function use_clang() {
  export CC=clang CXX=clang++
}

CMAKEFLAGS="-G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=RelWithDebInfo"
function cmake() {
    local cmd="/usr/bin/cmake"
    case "$1" in
        -E|--build|--find-package) "$cmd" "$@" ;;
        *) "$cmd" $CMAKEFLAGS "$@" ;;
    esac
}

export LV2_PATH="/usr/local/lib/lv2:/usr/lib/lv2:$HOME/.lv2"

# Github configuration
github_ssh_key=jpcima
github_fullname='JP Cimalando'
github_email='jpcima@users.noreply.github.com'

function github() {
  GIT_SSH_COMMAND="ssh -i $HOME/.ssh/$github_ssh_key" git "$@"
}

function github_config() {
  git config user.name "$github_fullname"
  git config user.email "$github_email"
}

# Pure Data
export DEKEN_USERNAME=jpcima

# Docker
alias enter='docker run -u "$UID" -e "DISPLAY=$DISPLAY" -i -t -v /home:/home -v /tmp:/tmp -w "$HOME"'

# OSXCross
export PATH="$PATH:/opt/osxcross/target/bin"
export MACOSX_DEPLOYMENT_TARGET=10.7
