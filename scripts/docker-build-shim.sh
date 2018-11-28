#!/bin/sh
#
# This is a shim for `docker build -t TAG DIR` that copies `DIR` to
# `images/TAG`.
#
# It is in the cloud build when running `stack image container`. This
# allows us to build the image in a separate step.

set -e

if [ $1 != "build" ]; then
  echo "docker-build-shim: Unknown command $1"
  exit 1
fi

if [ $2 != "-t" ]; then
  echo "docker-build-shim: Unknown option $2"
  exit 1
fi

image_tag=$3
image_dir=$4

target_dir=./images/$image_tag
mkdir -p $target_dir
cp -a $image_dir/* $target_dir
echo "Added image source $target_dir"
