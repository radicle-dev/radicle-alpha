#!/usr/bin/env bash
#
# Run all the tests that are run by CI.
#
# If STACK_DOCKER=1 is set all stack commands are executed with the
# `--docker` flag. This ensures that the binaries built on the host
# can run in the docker containers.

set -exuo pipefail

stack_exe=$(which stack)

cd $(dirname $BASH_SOURCE)/..

# Wrapper for `stack` that sets the `--docker` flag when `STACK_DOCKER`
# is set.
function stack () {
  if [[ "${STACK_DOCKER:-0}" =~ 1 ]]; then
    $stack_exe --docker "$@"
  else
    $stack_exe "$@"
  fi
}

stack build weeder hlint
stack exec -- hlint .
./scripts/check-fmt.sh

stack build --fast --pedantic --test
stack exec -- weeder --match

./images/radicle-server/ci-copy-bin.sh

export COMPOSE_FILE=test/docker-compose.yaml
docker-compose up -d postgres
sleep 5 # Wait for the DB to be ready
docker-compose up -d
sleep 3 # Wait for service to be booted

echo '{"radicle": true}' | docker exec -i test_ipfs-test-network_1 ipfs dag put --pin

export RAD_IPFS_API_URL=http://localhost:19301
export RADPATH="$(pwd)/rad"
stack test :e2e
stack exec -- radicle test/machine-backends.rad localhost

stack exec -- radicle-ref-doc
stack exec -- radicle-doc docs/source/guide/Basics.lrad
