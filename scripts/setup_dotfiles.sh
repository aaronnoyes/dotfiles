#! /bin/sh
git clone --bare git@github.com:aaronnoyes/dotfiles.git ~/.dotfiles
rm $HOME/.bashrc
/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME config --local status.showUntrackedFiles no
/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME checkout
source $HOME/bashrc
