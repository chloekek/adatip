#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by Hivemind.

set -efuo pipefail

if ! [[ -e "$PGDATA" ]]; then

    # First create the data directory.
    initdb                        \
        --locale=C                \
        --encoding=UTF8           \
        --username=postgres       \
        --pwfile=<(echo postgres)

    # initdb generates default configuration files.
    # We already have our own, so we will delete these.
    rm "$PGDATA"/{pg_hba,pg_ident,postgresql{,.auto}}.conf

fi

exec postgres -c config_file=scripts/postgresql.conf
