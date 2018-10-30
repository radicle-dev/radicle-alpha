#!/usr/bin/env bash

# Run hothasktags on the repo
# http://hackage.haskell.org/package/hothasktags

BASEDIR=$(dirname $BASH_SOURCE)/..

cd $BASEDIR
hothasktags -XLambdaCase -XTypeFamilies -XScopedTypeVariables -XFunctionalDependencies -XTupleSections -XExplicitNamespaces -XStandaloneDeriving -XDefaultSignatures -R src > TAGS
