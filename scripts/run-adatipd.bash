#!/usr/bin/env bash

# This script is used by Hivemind.

set -efuo pipefail

cd adatipd

exec cabal --offline new-run --                 \
    adatipd                                     \
    --cardano-node-socket-path cardano.socket   \
    --instance-title adatip.social
