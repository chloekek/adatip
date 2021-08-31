-- SPDX-License-Identifier: AGPL-3.0-only

-- migrate:up

CREATE TABLE creator_names
(
    creator_id  uuid        NOT NULL,
    created     timestamptz NOT NULL,
    name        text        NOT NULL,

    FOREIGN KEY (creator_id)
        REFERENCES creators (id)
        ON DELETE CASCADE
);

CREATE UNIQUE INDEX ix_creator_names_creator_id_created
    ON creator_names (creator_id, created DESC);

CREATE TABLE creator_biographies
(
    creator_id  uuid        NOT NULL,
    created     timestamptz NOT NULL,
    biography   text        NOT NULL,

    FOREIGN KEY (creator_id)
        REFERENCES creators (id)
        ON DELETE CASCADE
);

CREATE UNIQUE INDEX ix_creator_biographies_creator_id_created
    ON creator_biographies (creator_id, created DESC);

INSERT INTO creator_names (creator_id, created, name)
SELECT id, now(), 'Anonymous Creator ' || id :: text
FROM creators;

INSERT INTO creator_biographies (creator_id, created, biography)
SELECT id, now(), biography
FROM creators;

ALTER TABLE creators
    DROP COLUMN biography;

CREATE FUNCTION creator_current_name(uuid)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT name
        FROM creator_names
        WHERE creator_id = $1
        ORDER BY created DESC
        LIMIT 1
    $$;

CREATE FUNCTION creator_current_biography(uuid)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT biography
        FROM creator_biographies
        WHERE creator_id = $1
        ORDER BY created DESC
        LIMIT 1
    $$;

-- migrate:down

ALTER TABLE creators
    ADD COLUMN biography text;

UPDATE creators
SET biography = creator_current_biography(id);

ALTER TABLE creators
    ALTER COLUMN biography SET NOT NULL;

DROP FUNCTION creator_current_name(uuid);
DROP FUNCTION creator_current_biography(uuid);

DROP TABLE creator_biographies;
DROP TABLE creator_names;
