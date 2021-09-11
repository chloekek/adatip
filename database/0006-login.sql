-- SPDX-License-Identifier: AGPL-3.0-only

-- migrate:up

CREATE TABLE creator_password_hashes
(
    creator_id     uuid        NOT NULL,
    created        timestamptz NOT NULL,
    password_hash  text        NOT NULL,

    FOREIGN KEY (creator_id)
        REFERENCES creators (id)
        ON DELETE CASCADE
);

COMMENT ON COLUMN creator_password_hashes.password_hash IS '
    The password hash is stored in PHC string format [1].
    Therefore it embeds the algorithm name, parameters, and salt.
    [1]: https://github.com/P-H-C/phc-string-format/blob/master/phc-sf-spec.md
';

CREATE UNIQUE INDEX ix_creator_password_hashes_creator_id_created
    ON creator_password_hashes (creator_id, created DESC);

CREATE FUNCTION creator_current_password_hash(uuid)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT password_hash
        FROM creator_password_hashes
        WHERE creator_id = $1
        ORDER BY created DESC
        LIMIT 1
    $$;

-- migrate:down

DROP FUNCTION creator_current_password_hash(uuid);

DROP TABLE creator_password_hashes;
