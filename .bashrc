export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.npm-global/bin

alias dotfiles="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias ls="ls -l --color"

export PS1="\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\W\[\033[00m\] \$ "

export TERM=screen-256color

. "$HOME/.cargo/env"
