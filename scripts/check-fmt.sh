#!/usr/bin/env bash

set -euo pipefail

shopt -s globstar

base=$(mktemp -d "/tmp/oscoin-base.XXXXX")
for f in **/**.hs; do
  path=$base/$(dirname $f)
  mkdir -p $path
  cp $f $path
done

formatted=$(mktemp -d "/tmp/oscoin-formatted.XXXXX")
cp -R $base/* $formatted

stack exec -- stylish-haskell $formatted/**/**.hs --inplace

diff -rq $base $formatted
