-- migrate:up

CREATE TABLE creators
(
    id        uuid NOT NULL,
    nickname  text NOT NULL,
    biography text NOT NULL,

    CONSTRAINT creators_pk
        PRIMARY KEY (id),

    CONSTRAINT creators_nickname_ix
        UNIQUE (nickname)
);

GRANT SELECT, INSERT, UPDATE, DELETE
    ON TABLE creators
    TO adatip_app;

-- migrate:down

DROP TABLE creators;
