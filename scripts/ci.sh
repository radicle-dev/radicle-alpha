#!/bin/bash

set -euo pipefail

function load-cache() {
  bucket="gs://radicle-build-cache"
  key="stack-work-$(cat stack.yaml | sha256sum | cut -d ' ' -f 1)".tar.gz
  gsutil -m cp -r "$bucket/v1/*" "$bucket/$key" . || true
  for f in stack.tar.gz $key; do
    if [[ -e $f ]]; then
      tar xzf $f
    fi
  done
}

function save-cache() {
  bucket="gs://radicle-build-cache"
  key="stack-work-$(cat stack.yaml | sha256sum | cut -d ' ' -f 1)".tar.gz
  if ! gsutil ls "$bucket/$key"; then
    tar czf $key .stack-work
    gsutil -m cp $key "$bucket/$key" || true
  fi

  rm -rf .stack/indices/Hackage/00-index.tar*
  tar czf stack.tar.gz .stack

  if ! sha256sum --check stack.tar.gz.sha256; then
    sha256sum stack.tar.gz >stack.tar.gz.sha256
    gsutil -m cp stack.tar.gz* "$bucket/v1/" || true
  fi
}
