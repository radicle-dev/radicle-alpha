#!/bin/bash

set -eo pipefail

image_root=$(dirname $0)

echo "Building radicle-server in container"
stack build --docker :radicle-server

bin_path=$(stack exec --docker -- which radicle-server)

mkdir -p "$image_root/bin"
cp -a "$bin_path" "$image_root/bin"

docker build -t eu.gcr.io/opensourcecoin/radicle-server $image_root
