SELECT u.id, o.amount
FROM users u
LEFT JOIN orders o ON u.id = o.user_id
WHERE o.amount > 10;
