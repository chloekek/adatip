#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by GitHub actions.

set -efuo pipefail -o xtrace

# Kill postgres when script exits.
trap 'kill $(jobs -p)' EXIT

# Start postgres in the background.
scripts/run-postgres.bash &

# Wait for postgres to be running.
for (( i = 0; i < 10; ++i )); do
    if pg_isready -h 127.0.0.1 -p 8082; then
        break
    else
        sleep 1
    fi
done

# These scripts should now all succeed.
scripts/setup-database.bash
scripts/check-migrations.py
dbmate migrate
scripts/seed-database.bash
