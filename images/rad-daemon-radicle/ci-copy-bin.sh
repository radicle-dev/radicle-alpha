#!/bin/bash

set -eo pipefail

image_root=$(dirname $BASH_SOURCE)

if [ "$STACK_DOCKER" = "1" ]; then
  bin_path=$(stack exec --docker -- which rad-daemon-radicle)
else
  bin_path=$(stack exec -- which rad-daemon-radicle)
fi

mkdir -p "$image_root/bin"
cp -a "$bin_path" "$image_root/bin"
