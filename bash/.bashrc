[[ $- != *i* ]] && return

export PATH=$PATH:~/bin

if [[ $OSTYPE == "darwin"* ]]; then
  export CLICOLOR=true
elif [[ $OSTYPE == "linux-gnu" ]]; then
  alias ls='ls --color=auto'
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

export EDITOR=vim
export GIT_EDITOR=vim

if which direnv > /dev/null; then eval "$(direnv hook bash)"; fi

if [ -f /usr/local/etc/profile.d/z.sh ]; then
  . /usr/local/etc/profile.d/z.sh
fi

if [ -f /usr/local/share/liquidprompt ]; then
  . /usr/local/share/liquidprompt
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

# Add all sensitive stuff to different file
if [ -f ~/.profile_secrets ]; then
  . ~/.profile_secrets
fi

alias t='tmux attach || tmux new'

# fix python locale.getlocale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# use base16 shell for terminal colors
BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

if [ -f /usr/share/liquidprompt/liquidprompt ]; then
  # Only load liquidprompt in interactive shells, not from a script or from scp
  echo $- | grep -q i 2>/dev/null && . /usr/share/liquidprompt/liquidprompt
fi
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:~/go/bin

export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt

# disable ctrl-s
stty -ixon
