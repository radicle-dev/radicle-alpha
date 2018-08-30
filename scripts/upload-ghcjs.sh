#!/bin/bash -
#===============================================================================
#
#          FILE: upload-ghcjs.sh
#
#         USAGE: ./upload-ghcjs.sh
#
#   DESCRIPTION: Build and upload radicle JS
#
#       OPTIONS: ---
#  REQUIREMENTS: UglifyJS, gsutil
#       CREATED: 08/29/2018 14:52
#===============================================================================

set -o nounset
set -o errexit

export STACK_YAML=stack-ghcjs.yaml

[[ -d out ]] || mkdir out
stack build
ORIG=$(find .stack-work/dist -name all.js)

uglifyjs $ORIG > out/all.min.js

gsutil cp out/all.min.js gs://static.radicle.xyz
