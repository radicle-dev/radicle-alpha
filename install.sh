#!/usr/bin/env bash


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BIN_DIR=$(stack path --local-bin)

get-ipfs-git () {
    git clone git@github.com:oscoin/ipfs.git /tmp/ipfs
    cd /tmp/ipfs/git-remote-ipfs
    stack install
}

install-hs-bins () {
    cd $DIR
    stack install
}

set-paths () {
    export PATH=$PATH:$BIN_DIR:$DIR/bin
    export RADPATH=$DIR/rad
    printf "Set your PATH and RADPATH. This only applies to the current shell"
}

start-daemons () {
    rad-ipfs-daemon &
    rad-server &
}

stop-daemons () {
    pkill rad-ipfs-daemon
    pkill rad-server
}

case "$1" in
    install) install-hs-bins
             get-ipfs-git
             ;;
    start) set-paths
           start-daemons
           ;;
    stop) stop-daemons
          ;;
esac
