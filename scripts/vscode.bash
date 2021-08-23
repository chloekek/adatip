#!/usr/bin/env bash

# This script is used by .vscode/tasks.json.
# It is run as part of the VS Code build task.

set -efuo pipefail

cd adatipd

cabal --offline new-build
cabal --offline new-test
cabal --offline new-haddock
