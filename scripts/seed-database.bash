#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script inserts useful test data into the database.
# PostgreSQL must already be running, and migrations must already be run.

set -efuo pipefail

# We should not need adatip_setup for these statements.
# Using adatip_app is a nice privilege verification mechanism.
export PGUSER=adatip_app
export PGPASSWORD=$PGUSER

psql <<'SQL'

\set ON_ERROR_STOP

START TRANSACTION;

INSERT INTO creators
    (id)
VALUES
    ( '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588' ),
    ( '584c76d5-d016-4c17-9cd7-bc469cec43f6' );

INSERT INTO creator_nicknames
    (nickname, creator_id, created)
VALUES
    ( 'ingriddevries'
    , '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588'
    , '2021-08-28T00:00:00Z'
    ),
    ( 'henkdevries'
    , '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588'
    , '2021-08-29T00:00:00Z'
    ),
    ( 'cookingwithalex'
    , '584c76d5-d016-4c17-9cd7-bc469cec43f6'
    , '2021-08-29T00:00:00Z'
    );

INSERT INTO creator_names
    (creator_id, created, name)
VALUES
    ( '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588'
    , now()
    , 'Henk de Vries' ),
    ( '584c76d5-d016-4c17-9cd7-bc469cec43f6'
    , now()
    , 'Cooking with Alex' );

INSERT INTO creator_biographies
    (creator_id, created, biography)
VALUES
    ( '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588'
    , now()
    , 'Hallo, ik ben Henk de Vries!' ),
    ( '584c76d5-d016-4c17-9cd7-bc469cec43f6'
    , now()
    , 'Join me in my kitchen! I post weekly cooking videos.' );

COMMIT WORK;

SQL
