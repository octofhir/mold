-- Sample exercised by `just demo-db`. With the live schema, `patient`/`orders`
-- and their columns resolve; the lint rules below still fire.

-- AM04 (SELECT *) + AM05 (implicit cross join)
SELECT * FROM patient, orders WHERE patient.id = orders.patient_id;

-- SF01 (UPDATE without WHERE)
UPDATE patient SET name = 'anon';

-- RF01 with a "did you mean?" hint: 'naem' is close to the real column 'name'
-- (flagged only when the schema is known)
SELECT naem FROM patient;

-- RF02 ambiguous column: 'id' exists in both tables, so it must be qualified
SELECT id FROM patient JOIN orders ON orders.patient_id = patient.id;
