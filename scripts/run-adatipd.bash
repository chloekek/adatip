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

# Run the daemon with "cabal new-run", but reload it when any of the Git-tracked
# files in "adatipd" change. (Only evaluated at the start, if you add a new
# file, you need to reload manually.)
# -n: Make entr not ask for input on an interactive TTY.
# -r: The child-process is persistent; kill it and restart when triggered.
git ls-files | exec entr -nr -- cabal --offline new-run -- \
    adatipd                                     \
    --cardano-node-socket-path cardano.socket   \
    --instance-title adatip.social
