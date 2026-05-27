-- Demo schema introspected by `just demo-db`.
CREATE TABLE patient (
    id       integer PRIMARY KEY,
    name     text NOT NULL,
    resource jsonb
);

CREATE TABLE orders (
    id         integer PRIMARY KEY,
    patient_id integer REFERENCES patient (id),
    amount     numeric
);
