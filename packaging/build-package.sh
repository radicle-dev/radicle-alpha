#!/bin/bash

set -euo pipefail

function help () {
  cat <<DOC
Usage: TARGET=<target> VERSION=<version> $0

Builds package for a given target and puts it into ./packaging/out.

ENVIRONMENT VARIABLES

  TARGET (required)
    The packaging target. Available targets are

    - debian
    - pacman

  VERSION (required)
    Version of the package.

  STACK_USE_DOCKER
    If set to '1' then 'stack' is invoked with the '--docker' flag.
    This means the build outputs from 'stack build --docker' are
    used.
DOC
}


# Wrapper for the stack that uses docker if `STACK_USE_DOCKER` is set
# to `1`.
function stack () {
  if [ "${STACK_USE_DOCKER:-0}" = "1" ]; then
    /usr/bin/env stack --docker "$@"
  else
    /usr/bin/env stack "$@"
  fi
}

# Creates an executable file `$1` with content from stdin.
function install-stdin () {
  local target=$1
  mkdir -p $(dirname "$target")
  cat >"$target"
  chmod +x "$target"
}

# Download the `ipfs` binary and put it into the directory `$1`.
function get-ipfs () {
  local target_dir=$1
  mkdir -p "$target_dir"
  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://dist.ipfs.io/go-ipfs/v0.4.18/go-ipfs_v0.4.18_linux-amd64.tar.gz" \
  | tar -xz --strip-components=1 -C "$target_dir" go-ipfs/ipfs
}

function prepare-package-root () {
  install-stdin "$package_root/usr/bin/rad" <<EOF
#!/bin/sh
export radicle_bindir=$radicle_bindir
export RADPATH=\${RADPATH:-$radpath}
exec $radicle_bindir/rad "\$@"
EOF

  install-stdin "$package_root/usr/bin/radicle" <<EOF
#!/bin/sh
export radicle_bindir=$radicle_bindir
export RADPATH=\${RADPATH:-$radpath}
exec $radicle_bindir/radicle "\$@"
EOF

  install -D $(stack exec -- which git-remote-ipfs) -t "$package_root/usr/bin"

  install -D -t "$package_root/$radicle_bindir" $stack_bin_install_dir/radicle
  install -D -t "$package_root/$radicle_bindir" $stack_bin_install_dir/rad
  install -D -t "$package_root/$radicle_bindir" $stack_bin_install_dir/rad-*

  install -D --mode 0644 "$project_dir/packaging/radicle-ipfs.service" -t "$package_root/usr/lib/systemd/user"
  install -D --mode 0644 "$project_dir/packaging/radicle-daemon.service" -t "$package_root/usr/lib/systemd/user"

  mkdir -p "$package_root/$radpath"
  cp --archive $project_dir/rad -T "$package_root/$radpath"

  if [ $include_ipfs = 1 ]; then
    get-ipfs "$package_root/$radicle_bindir"
  fi
}

function package () {
  pushd $project_dir/packaging/out
  fpm \
    --verbose \
    --force \
    --input-type dir \
    --name radicle \
    --version $VERSION \
    "$@" \
    "$package_root/=/"
  popd
}

function package-pacman () {
  prepare-package-root
  package \
    --output-type pacman \
    --depends git \
    --depends go-ipfs \
    --pacman-compression none
}

function package-debian () {
  include_ipfs=1
  prepare-package-root
  package \
    --output-type deb \
    --depends git \
    --depends libgmp10 \
    --depends libc6 \
    --depends libncurses5
}

function package-darwin () {
  tarball="$project_dir/packaging/out/radicle_${VERSION}_x86_64-darwin.tar.gz"
  prepare-package-root
  tar -czf "$tarball" -C "$package_root" .
}


if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  help
  exit 0
fi

if test -z ${TARGET:-}; then
  echo "error: TARGET is not set."
  exit 1
fi

if ! type package-$TARGET 2>/dev/null 1>/dev/null; then
  echo "error: Unknown package target \"$TARGET\""
  exit 1
fi

if test -z "${VERSION:-}"; then
  echo "error: VERSION is not set"
  exit 1
fi

set -x

include_ipfs=0
project_dir=$(realpath "$(dirname $BASH_SOURCE)/..")
radicle_bindir=/usr/lib/radicle/bin
radpath=/usr/lib/radicle/modules
stack_bin_install_dir=$(stack path --local-install-root)/bin

package_root=$(mktemp -td "radicle-package.XXXX")
package-$TARGET
rm -rf "$package_root"
