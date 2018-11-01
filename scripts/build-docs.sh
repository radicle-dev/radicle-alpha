#!/usr/bin/env bash

set -o nounset
set -o errexit

BUCKET="gs://docs.radicle.xyz"
IS_NIX=${NIX_PATH:-}

help(){
    echo "./build-docs.sh [-d|-h]" 
    echo " Builds the radicle documentation"
    echo " Options:"
    echo "   -h : Show this help text"
    exit 0
}

# Helper to prevent pushd/popd from printing stuff to the console
pushd () {
    command pushd "$@" > /dev/null
}

popd () {
    command popd "$@" > /dev/null
}

build(){
    stack exec ref-doc
    pushd "$PWD"/docs
    [ -z "$IS_NIX" ] && pip install -r requirements.txt
    make html
    popd
}


while getopts ":h" opt; do
  case $opt in
    h) 
      help 
      ;;
    \?) 
      echo "Invalid option: -$OPTARG" 
      help
      ;; 
  esac 
done

build
