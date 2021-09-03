#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by GitHub actions.

set -efuo pipefail -o xtrace

# Kill postgres when script exits.
trap 'kill $(jobs -p)' EXIT

# Start postgres in the background.
scripts/run-postgres.bash &

# These scripts should now all succeed.
scripts/wait-postgres-ready.bash
scripts/setup-database.bash
scripts/check-migrations.py
dbmate migrate
scripts/seed-database.bash
