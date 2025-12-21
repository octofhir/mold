WITH active AS (SELECT id FROM users WHERE active = true)
SELECT * FROM active;
