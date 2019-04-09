#!/usr/bin/env bash

VERSION=$1

set -o errexit

if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+ ]]; then
    echo "Bad version: $VERSION"
    echo "Should have format 'x.y.z', where x, y and z are numbers"
    exit 1
fi

if [ "$(git status --untracked-files=no --porcelain)" != " M ChangeLog.md" ]; then
    echo "No changes to ChangeLog.md, or other changes found in working directory."
    echo "Make sure you've updated the changelog, and have no uncommited changes."
    exit 1
fi

# Update package.yaml
mv package.yaml old-package.yaml
sed "s/^version: '.*'$/version: '$VERSION'/" old-package.yaml > package.yaml
rm old-package.yaml

# Update daemon-api.yaml
mv docs/source/daemon-api.yaml old-daemon-api.yaml
sed "s/^  version: .*$/  version: $VERSION/" old-daemon-api.yaml > docs/source/daemon-api.yaml
rm old-daemon-api.yaml

git add package.yaml
git add ChangeLog.md
git add docs/source/daemon-api.yaml
git commit -m "Version update: $VERSION"
git tag "v$VERSION"
