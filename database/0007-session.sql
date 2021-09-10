-- SPDX-License-Identifier: AGPL-3.0-only

-- migrate:up

CREATE TABLE sessions
(
    id_hash    bytea       NOT NULL,
    created    timestamptz NOT NULL,
    creator_id uuid,

    PRIMARY KEY (id_hash),

    FOREIGN KEY (creator_id)
        REFERENCES creators (id)
        ON DELETE SET NULL
);

COMMENT ON COLUMN sessions.id_hash IS '
    We store the hash of the session identifier,
    so that if the database is compromised read-only,
    the attacker can still not log in as any other user.
';

-- migrate:down

DROP TABLE sessions;
