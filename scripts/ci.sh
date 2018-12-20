#!/bin/bash

set -euo pipefail

dependency_hash=$( (cat package.yaml; cat snapshot.yaml) | sha256sum | cut -d ' ' -f 1)

remote="gs://radicle-build-cache/v2"

local_cache_archive="stack-root.tar.gz"
remote_cache_master="$remote/stack-root-master.tar.gz"
remote_cache_hashed="$remote/stack-root-${dependency_hash}.tar.gz"

function load-cache() {
  if gsutil ls "$remote_cache_hashed"; then
    gsutil -m cp "$remote_cache_hashed" "$local_cache_archive"
    tar xzf "$local_cache_archive"
  elif gsutil ls "$remote_cache_master"; then
    gsutil -m cp "$remote_cache_master" "$local_cache_archive"
    tar xzf "$local_cache_archive"
  fi
}

function build-cache() {
  if [ ! -f "$local_cache_archive" ]; then
    # This file is not needed and unecessarily large
    rm -rf .stack/indices/Hackage/00-index.tar*

    tar czf $local_cache_archive .stack
  fi
}

function save-cache() {
  if ! gsutil ls "$remote_cache_hashed"; then
    build-cache
    gsutil -m cp $local_cache_archive "$remote_cache_hashed" || true
  fi

  if [ "$BRANCH_NAME" = "master" ]; then
    gsutil -m cp "$remote_cache_hashed" "$remote_cache_master" || true
  fi
}
