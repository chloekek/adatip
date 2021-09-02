#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by Hivemind.

set -efuo pipefail

cd adatipd

# adatipd will connect to PostgreSQL.
export PGHOST=127.0.0.1
export PGPORT=8082
export PGUSER=adatip_app
export PGPASSWORD=$PGUSER
export PGDATABASE=adatip

# Run adatipd with "cabal new-run", but reload it when a source file changes.
# Source files are only discovered at the start;
# you must manually reload after adding new source files.
#
# -n: Make entr not ask for input on an interactive TTY.
# -r: The child-process is persistent; kill it and restart when triggered.
find -type f -name '*.cabal' -or -name '*.hs' |   \
    exec entr -nr -- cabal --offline new-run --   \
        adatipd                                   \
        --cardano-node-socket-path cardano.socket \
        --instance-title adatip.social
