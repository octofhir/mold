SELECT * FROM users;
SELECT id, name FROM users WHERE active = true;
SELECT u.id, o.amount FROM users u JOIN orders o ON u.id = o.user_id;
WITH recent AS (
    SELECT id, created_at FROM orders WHERE created_at > NOW() - INTERVAL '7 days'
)
SELECT * FROM recent;
INSERT INTO users (id, name, active) VALUES (1, 'alpha', true);
UPDATE users SET active = false WHERE id = 1;
DELETE FROM orders WHERE amount = 0;
SELECT data->'name'->>'first' FROM users;
SELECT count(*) FROM users WHERE active = true;
SELECT * FROM users WHERE name ILIKE '%test%';
SELECT id FROM users ORDER BY id DESC LIMIT 50;
SELECT u.id, sum(o.amount) FROM users u JOIN orders o ON u.id = o.user_id GROUP BY u.id;
SELECT * FROM users WHERE created_at BETWEEN NOW() - INTERVAL '1 day' AND NOW();
SELECT * FROM users WHERE id IN (SELECT user_id FROM orders WHERE amount > 100);
SELECT * FROM users WHERE EXISTS (SELECT 1 FROM orders WHERE orders.user_id = users.id);
SELECT * FROM users WHERE data->'prefs'->>'theme' = 'dark';
SELECT * FROM users WHERE (active = true AND name <> '');
SELECT * FROM users WHERE id = ANY(ARRAY[1,2,3,4]);
SELECT * FROM users WHERE COALESCE(name, '') <> '';
