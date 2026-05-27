-- Sample exercised by `just demo-db`. With the live schema, `patient`/`orders`
-- and their columns resolve; the lint rules below still fire.

-- AM04 (SELECT *) + AM05 (implicit cross join)
SELECT * FROM patient, orders WHERE patient.id = orders.patient_id;

-- SF01 (UPDATE without WHERE)
UPDATE patient SET name = 'anon';

-- references an unknown column; flagged only when the schema is known
SELECT nonexistent_column FROM patient;
