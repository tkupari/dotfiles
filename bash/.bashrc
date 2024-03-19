[[ $- != *i* ]] && return

set -o vi

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

if command_exists eza ; then
  alias ls='eza'
fi
alias ll="ls -l"

# Add all sensitive stuff to different file
if [ -f ~/.profile_secrets ]; then
  . ~/.profile_secrets
fi

# Add all sensitive stuff to different file
if [ -f ~/.config/rg/ripgreprc ]; then
  export RIPGREP_CONFIG_PATH=~/.config/rg/ripgreprc
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

. "$HOME/.cargo/env"

source <(kubectl completion bash)
source <(helm completion bash)
source <(rustup completions bash)
source <(rustup completions bash cargo)
source <(kind completion bash)
source <(k3d completion bash)
source <(aws-vault --completion-script-bash)
complete -C '$(which aws_completer)' aws

eval "$(zoxide init --cmd cd bash)"

export PATH=$PATH:~/bin
