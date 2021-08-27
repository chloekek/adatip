#!/usr/bin/env bash

# This script forwards all arguments to dbmate [1].
# It first configures dbmate so it can connect.
# PostgreSQL must already be running.
# [1]: https://github.com/amacneil/dbmate

set -efuo pipefail

pg_url='postgres://adatip_setup:adatip_setup@127.0.0.1:8082/adatip?sslmode=disable'

exec dbmate                   \
    --migrations-dir database \
    --no-dump-schema          \
    --url "$pg_url"           \
    "$@"
