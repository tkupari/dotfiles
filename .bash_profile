# Fix nvm with tmux
# https://github.com/creationix/nvm/issues/1652
PATH="/usr/local/bin:$(getconf PATH)"

export PYTHONSTARTUP=~/.pythonrc
export CLICOLOR=true

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

export EDITOR=vim
export GIT_EDITOR=vim

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
if which direnv > /dev/null; then eval "$(direnv hook bash)"; fi

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

export NVM_DIR="$HOME/.nvm"
# load nvm "on-demand"
# https://github.com/creationix/nvm/issues/539#issuecomment-245791291
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" --no-use # This loads nvm
alias node='unalias node ; unalias npm ; nvm use default ; node $@'
alias npm='unalias node ; unalias npm ; nvm use default ; npm $@'


set -o vi

# Add all sensitive stuff to different file
if [ -f ~/.profile_secrets ]; then
  . ~/.profile_secrets
fi

alias t='tmux attach || tmux new'

# fix python locale.getlocale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
