SELECT *
FROM users u
LEFT JOIN LATERAL (
    SELECT * FROM orders o WHERE o.user_id = u.id
) ord ON true;
