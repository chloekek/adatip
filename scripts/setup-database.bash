#!/usr/bin/env bash

# This script sets up the database and roles.
# When you run this script, PostgreSQL must already be running.
# Running this script multiple times doesn’t harm,
# despite the errors that it spews out.

set -efuo pipefail

export PGHOST=127.0.0.1
export PGPORT=8082
export PGUSER=postgres
export PGPASSWORD=postgres

psql <<'SQL'

    -- Role used by the application.
    CREATE ROLE adatip_app LOGIN PASSWORD 'adatip_app';

    -- Role used for migrations and administration.
    CREATE ROLE adatip_setup LOGIN PASSWORD 'adatip_setup';

    CREATE DATABASE adatip OWNER adatip_setup;

SQL

export PGDATABASE=adatip

psql <<'SQL'

    -- By default, PostgreSQL creates a schema “public”
    -- that can be modified by any user (“PUBLIC”).
    -- That is insecure; we want more selective permissions.
    REVOKE ALL PRIVILEGES ON SCHEMA public FROM PUBLIC;
    GRANT USAGE ON SCHEMA public TO adatip_app;
    GRANT USAGE, CREATE ON SCHEMA public TO adatip_setup;

SQL
