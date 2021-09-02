-- SPDX-License-Identifier: AGPL-3.0-only

-- migrate:up

--------------------------------------------------------------------------------
-- Posts

CREATE TABLE posts
(
    id         uuid NOT NULL,
    creator_id uuid NOT NULL,

    PRIMARY KEY (id),

    FOREIGN KEY (creator_id)
        REFERENCES creators (id)
        ON DELETE CASCADE
);

CREATE INDEX ix_posts_creator_id
    ON posts (creator_id);

--------------------------------------------------------------------------------
-- Post visibilities

CREATE TABLE post_visibilities
(
    post_id uuid        NOT NULL,
    created timestamptz NOT NULL,
    visible boolean     NOT NULL,

    FOREIGN KEY (post_id)
        REFERENCES posts (id)
        ON DELETE CASCADE
);

CREATE UNIQUE INDEX ix_post_visibilities_post_id_created
    ON post_visibilities (post_id, created DESC);

CREATE FUNCTION post_current_visibility(uuid)
    RETURNS boolean
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT visible
        FROM post_visibilities
        WHERE post_id = $1
        ORDER BY created DESC
        LIMIT 1
    $$;

CREATE FUNCTION post_first_published(uuid)
    RETURNS timestamptz
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT created
        FROM post_visibilities
        WHERE
            post_id = $1
            AND visible
        ORDER BY created ASC
        LIMIT 1
    $$;

--------------------------------------------------------------------------------
-- Post titles

CREATE TABLE post_titles
(
    post_id uuid        NOT NULL,
    created timestamptz NOT NULL,
    title   text        NOT NULL,

    FOREIGN KEY (post_id)
        REFERENCES posts (id)
        ON DELETE CASCADE
);

CREATE UNIQUE INDEX ix_post_titles_post_id_created
    ON post_titles (post_id, created DESC);

CREATE FUNCTION post_current_title(uuid)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT title
        FROM post_titles
        WHERE post_id = $1
        ORDER BY created DESC
        LIMIT 1
    $$;

--------------------------------------------------------------------------------
-- Post contents

CREATE TABLE post_contents
(
    post_id uuid        NOT NULL,
    created timestamptz NOT NULL,
    content   text        NOT NULL,

    FOREIGN KEY (post_id)
        REFERENCES posts (id)
        ON DELETE CASCADE
);

CREATE UNIQUE INDEX ix_post_contents_post_id_created
    ON post_contents (post_id, created DESC);

CREATE FUNCTION post_current_content(uuid)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT content
        FROM post_contents
        WHERE post_id = $1
        ORDER BY created DESC
        LIMIT 1
    $$;

-- migrate:down

DROP FUNCTION post_current_content(uuid);
DROP TABLE post_contents;

DROP FUNCTION post_current_title(uuid);
DROP TABLE post_titles;

DROP FUNCTION post_first_published(uuid);
DROP FUNCTION post_current_visibility(uuid);
DROP TABLE post_visibilities;

DROP TABLE posts;
