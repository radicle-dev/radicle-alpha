#!/usr/bin/env bash

# Run all the tests that are run by CI (but without using local docker because
# how does a mortal even set that up!?)

set -e -x
BASEDIR=$(dirname $BASH_SOURCE)/..
cd $BASEDIR
hothasktags -XLambdaCase -XTypeFamilies -XScopedTypeVariables -XFunctionalDependencies -XTupleSections -XExplicitNamespaces -XStandaloneDeriving -XDefaultSignatures -R src > TAGS
stack exec hlint .
./scripts/check-fmt.sh
stack test --fast
stack exec radicle-ref-doc
