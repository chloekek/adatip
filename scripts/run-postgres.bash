#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by Hivemind and is also used by the tests.
# We place a directory for Unix sockets next to the data directory.
# By using Unix sockets instead of TCP sockets
# we can easily spin up multiple instances
# without having to find unique port numbers.

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

socket_dir=$PGDATA/../pgsocket

exec postgres -c config_file=scripts/postgresql.conf -k "$socket_dir"
