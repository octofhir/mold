INSERT INTO users (id, name)
VALUES (1, 'Alice')
ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name
WHERE users.active = true
RETURNING id AS user_id, name;
