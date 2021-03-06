[[ $- != *i* ]] && return

set -o vi

export PATH=$PATH:~/bin

if [[ $OSTYPE == "darwin"* ]]; then
  export CLICOLOR=true
elif [[ $OSTYPE == "linux-gnu" ]]; then
  alias ls='ls --color=auto'
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
[ -f /etc/bash_completion ] && . /etc/bash_completion

export EDITOR=vim
export GIT_EDITOR=vim
export REACT_EDITOR=none

if which direnv > /dev/null; then eval "$(direnv hook bash)"; fi

if [ -f ~/.config/z/z.sh ]; then
  . ~/.config/z/z.sh
fi

if [ -f /usr/local/etc/profile.d/z.sh ]; then
  . /usr/local/etc/profile.d/z.sh
fi

# Helper to see if a command exists
command_exists () {
  type "$1" &> /dev/null ;
}

# Use neovim if it's installed
if command_exists nvim ; then
  export EDITOR=nvim
  export GIT_EDITOR=nvim
  alias vim='nvim'
fi

if command_exists exa ; then
  alias ls='exa'
fi
alias ll="ls -l"

# Add all sensitive stuff to different file
if [ -f ~/.profile_secrets ]; then
  . ~/.profile_secrets
fi

alias t='tmux attach || tmux new'
alias xclip='xclip -selection clipboard'

# fix python locale.getlocale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:~/go/bin

export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt

# disable ctrl-s
stty -ixon

. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

export HISTCONTROL=ignoreboth

if command_exists starship ; then
  eval "$(starship init bash)"
fi
