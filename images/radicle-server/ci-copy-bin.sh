#!/bin/bash

set -eo pipefail

image_root=$(dirname $0)

bin_path=$(stack exec -- which radicle-server)

mkdir -p "$image_root/bin"
cp -a "$bin_path" "$image_root/bin"
