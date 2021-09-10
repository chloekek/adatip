#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by Hivemind.

set -efuo pipefail

cd adatipd

export PGUSER=adatip_app
export PGPASSWORD=$PGUSER

# Run adatipd with "cabal new-run", but reload it when a source file changes.
# Source files are only discovered at the start;
# you must manually reload after adding new source files.
#
# -n: Make entr not ask for input on an interactive TTY.
# -r: The child-process is persistent; kill it and restart when triggered.
find -type f -name '*.cabal' -or -name '*.hs' |   \
    exec entr -nr -- cabal --offline new-run --   \
        adatipd                                   \
        --debug-mode                              \
        --instance-title adatip.social
