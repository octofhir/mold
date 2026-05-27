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

-- Seed rows so JSONB shape sampling has data to infer `resource` structure.
INSERT INTO patient (id, name, resource) VALUES
    (1, 'Ann', '{"resourceType":"Patient","active":true,"name":[{"given":["Ann"],"family":"Lee"}]}'),
    (2, 'Bob', '{"resourceType":"Patient","active":false,"name":[{"given":["Bob"],"family":"Roy"}],"birthDate":"1990-01-01"}');
