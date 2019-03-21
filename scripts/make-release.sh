#!/bin/env bash

VERSION=$1

if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+ ]]; then
    echo "Bad version: $VERSION"
    echo "Should have format 'x.y.z', where x, y and z are numbers"
fi

if [ "$(git rev-parse --abbrev-ref HEAD)" != "master" ]; then
    echo "Not on branch 'master'. Aborting."
    exit 1
fi

if [ "$(git rev-parse master)" != "$(git rev-parse origin/master)" ]; then
    echo "master and origin/master differ. Aborting"
    exit 1
fi


if [ "$(git status --untracked-files=no --porcelain)" != " M ChangeLog.md" ]; then
    echo "No changes to ChangeLog.md, or other changes found in working directory."
    echo "Make sure you've updated the changelog, and have no uncommited changes."
    exit 1
fi

# Update package.yaml
mv package.yaml old-package.yaml
sed "s/^version: '.*'$/version: '$VERSION'" old-package.yaml > package.yaml
rm old-package.yaml

echo "git add package.yaml"
echo "git add ChangeLog.md"
echo "git commit -m \"Version update: $VERSION\""
echo "git tag v$VERSION"
