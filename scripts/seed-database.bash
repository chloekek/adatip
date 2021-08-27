#!/usr/bin/env bash

# This script inserts useful test data into the database.
# PostgreSQL must already be running, and migrations must already be run.

set -efuo pipefail

export PGHOST=127.0.0.1
export PGPORT=8082
export PGUSER=adatip_app
export PGPASSWORD=adatip_app
export PGDATABASE=adatip

psql <<'SQL'

\set ON_ERROR_STOP

INSERT INTO creators
    (id, nickname, biography)
VALUES
    ( '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588'
    , 'henkdevries'
    , 'Hallo, ik ben Henk de Vries!' ),
    ( '584c76d5-d016-4c17-9cd7-bc469cec43f6'
    , 'cookingwithalex'
    , 'Join me in my kitchen! I post weekly cooking videos.' );

SQL
