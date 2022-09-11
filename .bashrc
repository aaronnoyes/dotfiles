export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.npm-global/bin

# fnm
export PATH=/home/user/.fnm:$PATH
eval "`fnm env`"

alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

export PS1="\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\W\[\033[00m\] \$ "

