-- SPDX-License-Identifier: AGPL-3.0-only

-- migrate:up

CREATE TABLE creator_nicknames
(
    nickname   text        NOT NULL PRIMARY KEY,
    creator_id uuid        NOT NULL REFERENCES creators (id),
    created    timestamptz NOT NULL
);

-- For looking up the current nickname of a creator. A creator's current
-- nickname is the nickname with the highest created time. We make this a unique
-- index to ensure that there is a unique current nickname.
CREATE UNIQUE INDEX
  ix_creator_nicknames_creator_id_created
  ON creator_nicknames (creator_id, created);

INSERT INTO
    creator_nicknames (nickname, creator_id, created)
SELECT
    nickname, id, now()
FROM
    creators;

ALTER TABLE creators DROP COLUMN nickname CASCADE;

CREATE FUNCTION creator_current_nickname(uuid)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT nickname
        FROM creator_nicknames
        WHERE creator_id = $1
        ORDER BY created DESC
        LIMIT 1
    $$;

-- migrate:down

ALTER TABLE creators ADD COLUMN nickname text NULL;
ALTER TABLE creators ADD CONSTRAINT creators_nickname_ix UNIQUE (nickname);

UPDATE creators
SET nickname = creator_current_nickname(id);

DROP FUNCTION creator_current_nickname(uuid);

ALTER TABLE creators ALTER COLUMN nickname SET NOT NULL;
DROP TABLE creator_nicknames;
