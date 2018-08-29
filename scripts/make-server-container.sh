#!/bin/bash -
#===============================================================================
#
#          FILE: make-server-container.sh
#
#         USAGE: ./make-server-container.sh
#
#   DESCRIPTION: Creates and uploads docker container for the radicle server
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: YOUR NAME (),
#  ORGANIZATION:
#       CREATED: 08/29/2018 10:30
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

COMMIT=$(git rev-parse HEAD)
NAME=eu.gcr.io/opensourcecoin/radicle-server:$COMMIT


# Check that the current commit has been pushed
git fetch origin
if [[ -z `git branch -r --contains $COMMIT | grep origin` ]]; then
    echo "Commit not pushed. Aborting."
    exit 1
fi

stack image container --image radicle/server
docker tag radicle/server $NAME
docker push $NAME
