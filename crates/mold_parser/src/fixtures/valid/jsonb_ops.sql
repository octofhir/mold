SELECT data->'name'->>'first'
FROM users
WHERE data @> '{"active": true}' AND data ? 'name';
