[[ $- != *i* ]] && return

set -o vi

export PATH=$PATH:~/bin

if [[ $OSTYPE == "darwin"* ]]; then
  export CLICOLOR=true
elif [[ $OSTYPE == "linux-gnu" ]]; then
  alias ls='ls --color=auto'
fi

[ -f /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

export EDITOR=vim
export GIT_EDITOR=vim
export REACT_EDITOR=none

if which direnv > /dev/null; then eval "$(direnv hook bash)"; fi

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

alias t='tmux attach || tm'
alias xclip='xclip -selection clipboard'
alias cdroot='cd $(git rev-parse --show-toplevel)'

# fix python locale.getlocale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:~/go/bin

export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-bundle.crt

# disable ctrl-s
stty -ixon

. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

export HISTCONTROL=ignoreboth

if command_exists starship ; then
  eval "$(starship init bash)"
fi

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        source "$BASE16_SHELL/profile_helper.sh"

source <(kubectl completion bash)
source <(helm completion bash)
. "$HOME/.cargo/env"
source <(rustup completions bash)
source <(rustup completions bash cargo)
