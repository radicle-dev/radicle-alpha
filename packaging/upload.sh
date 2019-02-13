#!/bin/bash

set -euxo pipefail

package_dir=$(realpath "$(dirname $BASH_SOURCE)/out")

for pkg in $package_dir/radicle*; do
  gsutil -m cp $pkg gs://static.radicle.xyz/releases
done
