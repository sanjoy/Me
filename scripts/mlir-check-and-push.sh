#!/bin/bash

set -e

git fetch origin
git rebase origin/main

echo "COMMIT LIST"
echo "==========="
echo ""
git log --stat origin/main..HEAD
echo ""
read -p "Ok? " yn

if [[ $yn != "y" ]]; then
    echo "Not pushing"
    exit 1
fi

echo "Running tests and pushing"

ninja check-mlir && git push origin HEAD:main
