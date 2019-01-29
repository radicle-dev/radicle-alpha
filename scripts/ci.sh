#!/bin/bash

set -euo pipefail

dependency_hash=$( (cat package.yaml; cat snapshot.yaml) | sha256sum | cut -d ' ' -f 1)

remote="gs://radicle-build-cache/v3"

local_cache_archive="stack-root.tar.gz"
remote_cache_master="$remote/stack-root-master.tar.gz"
remote_cache_hashed="$remote/stack-root-${dependency_hash}.tar.gz"

function load-cache() {
  if gsutil -q ls "$remote_cache_hashed"; then
    echo "Using hashed stack cache"
    gsutil -m cp "$remote_cache_hashed" "$local_cache_archive"
    tar xzf "$local_cache_archive"
    rm "$local_cache_archive"
  elif gsutil -q ls "$remote_cache_master"; then
    echo "Using master stack cache"
    gsutil -m cp "$remote_cache_master" "$local_cache_archive"
    tar xzf "$local_cache_archive"
    rm "$local_cache_archive"
  fi
}

function save-cache() {
  if ! gsutil -q ls "$remote_cache_hashed"; then
    echo "Uploading stack cache"
    # This file is not needed and unecessarily large
    rm -rf .stack/indices/Hackage/00-index.tar*
    tar czf $local_cache_archive .stack
    gsutil -m cp $local_cache_archive "$remote_cache_hashed"
  fi

  if [ "$BRANCH_NAME" = "master" ]; then
    echo "Setting master cache to current cache"
    gsutil -m cp "$remote_cache_hashed" "$remote_cache_master"
  fi
}
