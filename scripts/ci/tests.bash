#!/usr/bin/env bash

# This script is used by GitHub actions.

set -efuo pipefail -o xtrace

cd adatipd

cabal --offline new-build
cabal --offline new-test
