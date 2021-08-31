-- SPDX-License-Identifier: AGPL-3.0-only

-- migrate:up

ALTER DEFAULT PRIVILEGES
FOR USER adatip_setup
GRANT SELECT, INSERT, UPDATE, DELETE
ON TABLES
TO adatip_app;

-- migrate:down

ALTER DEFAULT PRIVILEGES
FOR USER adatip_setup
REVOKE SELECT, INSERT, UPDATE, DELETE
ON TABLES
FROM adatip_app;
