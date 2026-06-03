-- Demonstrates inline `-- noqa` lint suppression. Run:
--
--     banshee lint examples/noqa.sql
--
-- Only the unsuppressed findings should remain.

-- (1) baseline: this line reports both AM04 (SELECT *) and AM05 (cross join).
SELECT * FROM patient, orders WHERE patient.id = orders.patient_id;

-- (2) bare noqa: silence every rule on this line.
SELECT * FROM patient, orders WHERE patient.id = orders.patient_id;   -- noqa

-- (3) specific code: silence AM04 only — AM05 still fires here.
SELECT * FROM patient, orders WHERE patient.id = orders.patient_id;   -- noqa: AM04

-- (4) group prefix: `AM` covers the whole AM** family, so both go quiet.
SELECT * FROM patient, orders WHERE patient.id = orders.patient_id;   -- noqa: AM

-- (5) range form: suppress SF01 from here…
UPDATE patient SET name = 'anon';   -- noqa: disable=SF01
UPDATE orders  SET total = 0;       -- still silent (inside the disabled range)
-- …until it is turned back on.
UPDATE patient SET name = 'anon';   -- noqa: enable=SF01
UPDATE orders  SET total = 0;       -- SF01 fires again here
