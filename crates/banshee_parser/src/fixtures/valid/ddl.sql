-- DDL coverage: CREATE TABLE, ALTER TABLE, CREATE INDEX, DROP, TRUNCATE.
CREATE TABLE IF NOT EXISTS users (
    id         bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    email      varchar(255) NOT NULL UNIQUE,
    name       text NOT NULL,
    role       text NOT NULL DEFAULT 'member',
    org_id     bigint REFERENCES organizations (id) ON DELETE CASCADE,
    created_at timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT email_lower CHECK (email = lower(email))
);

ALTER TABLE users ADD COLUMN last_login timestamptz;
ALTER TABLE users ALTER COLUMN role SET DEFAULT 'guest';
ALTER TABLE users ADD CONSTRAINT org_fk FOREIGN KEY (org_id) REFERENCES organizations (id) NOT VALID;

CREATE UNIQUE INDEX CONCURRENTLY users_email_idx ON users USING btree (lower(email)) WHERE last_login IS NOT NULL;

DROP INDEX CONCURRENTLY IF EXISTS users_email_idx;
TRUNCATE TABLE users RESTART IDENTITY CASCADE;
