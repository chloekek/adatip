#!/usr/bin/env bash

# This script is used by Hivemind.

set -efuo pipefail

pgdata=state/postgresql/pgdata

if ! [[ -e "$pgdata" ]]; then

    # First create the data directory.
    initdb                        \
        --pgdata="$pgdata"        \
        --locale=C                \
        --username=postgres       \
        --pwfile=<(echo postgres)

    # initdb generates default configuration files.
    # We already have our own, so we will delete these.
    rm "$pgdata"/{pg_hba,pg_ident,postgresql{,.auto}}.conf

fi

exec postgres -c config_file=scripts/postgresql.conf
