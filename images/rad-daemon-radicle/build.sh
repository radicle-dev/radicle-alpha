#!/bin/bash

set -eo pipefail

image_root=$(dirname $0)

echo "Building rad-daemon-radicle in container"
stack build --docker :rad-daemon-radicle

bin_path=$(stack exec --docker -- which rad-daemon-radicle)

mkdir -p "$image_root/bin"
cp -a "$bin_path" "$image_root/bin"

docker build -t eu.gcr.io/opensourcecoin/rad-daemon-radicle $image_root
