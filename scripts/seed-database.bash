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
    , '2021-08-28T12:45:00Z'
    , 'Henk de Vries' ),
    ( '584c76d5-d016-4c17-9cd7-bc469cec43f6'
    , '2021-08-30T15:00:00Z'
    , 'Cooking with Alex' );

INSERT INTO creator_biographies
    (creator_id, created, biography)
VALUES
    ( '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588'
    , '2021-08-28T12:45:00Z'
    , 'Hallo, ik ben Henk de Vries!' ),
    ( '584c76d5-d016-4c17-9cd7-bc469cec43f6'
    , '2021-08-30T15:00:00Z'
    , 'Join me in my kitchen! I post weekly cooking videos.' );

INSERT INTO posts
    (id, creator_id)
VALUES
    ( 'f79fb2c5-42d7-4abd-8dda-acd7761b4f7a'
    , '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588' ),
    ( 'aebadd6c-894b-4a46-9448-9702231ee019'
    , '6fdc4c36-91b6-4946-9b8e-3cdf7ad56588' );

INSERT INTO post_visibilities
    (post_id, created, visible)
VALUES
    ( 'f79fb2c5-42d7-4abd-8dda-acd7761b4f7a'
    , '2021-08-30T15:45:00Z'
    , TRUE ),
    ( 'aebadd6c-894b-4a46-9448-9702231ee019'
    , '2021-09-01T12:15:00Z'
    , TRUE );

INSERT INTO post_titles
    (post_id, created, title)
VALUES
    ( 'f79fb2c5-42d7-4abd-8dda-acd7761b4f7a'
    , '2021-08-30T15:45:00Z'
    , 'Haskell Podcast #1' ),
    ( 'aebadd6c-894b-4a46-9448-9702231ee019'
    , '2021-09-01T12:15:00Z'
    , 'Haskell Podcast #2' );

INSERT INTO post_contents
    (post_id, created, content)
VALUES
    ( 'f79fb2c5-42d7-4abd-8dda-acd7761b4f7a'
    , '2021-08-30T15:45:00Z'
    , 'In this episode we will take a look at recursion schemes.' ),
    ( 'aebadd6c-894b-4a46-9448-9702231ee019'
    , '2021-09-01T12:15:00Z'
    , 'In this episode we will take a look at GADTs.' );

COMMIT WORK;

SQL
