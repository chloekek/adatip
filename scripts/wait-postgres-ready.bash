#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# Wait until PostgreSQL is ready.
# This script crashes if it takes too long.

set -efuo pipefail

for (( i=0; i < 100; ++i )); do
    if pg_isready; then
        exit 0
    else
        sleep 0.1
    fi
done

exit 1
