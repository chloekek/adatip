-- migrate:up

CREATE TABLE nicknames
(
    nickname text        NOT NULL PRIMARY KEY,
    creator  uuid        NOT NULL REFERENCES creators (id),
    created  timestamptz NOT NULL
);

GRANT SELECT, INSERT, UPDATE, DELETE
    ON TABLE nicknames
    TO adatip_app;

-- For looking up the current nickname of a creator. A creator's current
-- nickname is the nickname with the highest created time. We make this a unique
-- index to ensure that there is a unique current nickname.
CREATE UNIQUE INDEX ix_nicknames_creator_created ON nicknames (creator, created);

INSERT INTO
    nicknames (nickname, creator, created)
SELECT
    nickname, id, now()
FROM
    creators;

ALTER TABLE creators DROP COLUMN nickname CASCADE;

-- migrate:down

ALTER TABLE creators ADD COLUMN nickname text NULL;
ALTER TABLE creators ADD CONSTRAINT creators_nickname_ix UNIQUE (nickname);

UPDATE creators
SET nickname = (
    SELECT nickname
    FROM nicknames
    WHERE creator = creators.id
    ORDER BY created DESC
    LIMIT 1
);

ALTER TABLE creators ALTER COLUMN nickname SET NOT NULL;
DROP TABLE nicknames;
