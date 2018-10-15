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

IS_CI=${PROJECT_ID:-}

export STACK_YAML=stack-ghcjs.yaml

build() {
    [[ -d out ]] || mkdir out
    stack build
    ORIG=$(find .stack-work/dist -name all.js)
    
    uglifyjs $ORIG > out/all.min.js
    
    gzip --keep out/all.min.js
}

deploy() {
    gsutil cp out/all.min.js gs://static.radicle.xyz
    gsutil cp out/all.min.js.gz gs://static.radicle.xyz
}

