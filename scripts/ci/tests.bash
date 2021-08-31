#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by GitHub actions.

set -efuo pipefail -o xtrace

cd adatipd

cabal --offline new-build
cabal --offline new-test
