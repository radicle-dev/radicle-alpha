#!/usr/bin/env bash

set -o nounset
set -o errexit

DEPLOY=false
BUCKET="gs://docs.radicle.xyz"

help(){
    echo "./build-docs.sh [-d|-h]" 
    echo " Builds the radicle documentation"
    echo " Options:"
    echo "   -d : Also deploy the documentation"
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
    pushd "$PWD"/docs
    [ -z "$NIX_PATH" ] && pip install -r requirements.txt
    make html
    popd
}

deploy(){
    pushd "$PWD"/docs
    gsutil -m cp -r build/html "$BUCKET"
    popd
}

while getopts ":d:h" opt; do
  case $opt in
    h) 
      help 
      ;;
    d)
      DEPLOY=true
      ;;
    \?) 
      echo "Invalid option: -$OPTARG" 
      help
      ;; 
  esac 
done

build

if [ "$DEPLOY" = true ] ; then
    deploy
fi
