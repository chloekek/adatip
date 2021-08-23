#!/usr/bin/env bash

# This script is used by GitHub actions.

set -efuo pipefail

cd adatipd

cabal --offline new-build
cabal --offline new-test
