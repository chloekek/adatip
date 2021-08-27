#!/usr/bin/env bash

# This script is used by Hivemind.

set -efuo pipefail

cd adatipd

# Run the daemon with "cabal new-run", but reload it when any of the Git-tracked
# files in "adatipd" change. (Only evaluated at the start, if you add a new
# file, you need to reload manually.)
# -n: Make entr not ask for input on an interactive TTY.
# -r: The child-process is persistent; kill it and restart when triggered.
git ls-files | exec entr -nr -- cabal --offline new-run -- \
    adatipd                                     \
    --cardano-node-socket-path cardano.socket   \
    --instance-title adatip.social
