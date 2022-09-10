export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.npm-global/bin

# fnm
export PATH=/home/user/.fnm:$PATH
eval "`fnm env`"

alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
