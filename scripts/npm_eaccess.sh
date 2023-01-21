#! /bin/sh
[ -d "~/.npm-global/bin" ] && mkdir -p ~/.npm-global/bin
npm config set prefix '~/.npm-global'
