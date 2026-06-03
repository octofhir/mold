-- Expression coverage: regex, exponent, NOT precedence, casts, AT TIME ZONE,
-- COLLATE, SIMILAR TO, ISNULL/NOTNULL, array slices.
SELECT name
  FROM users
 WHERE name ~ '^A'
   AND email !~* '@example\.com$'
   AND NOT active = false
   AND created_at AT TIME ZONE 'UTC' > now()
   AND title SIMILAR TO 'Mr%'
   AND deleted_at ISNULL;

SELECT 2 ^ 10 AS power,
       price::numeric(10, 2) AS price,
       measured::double precision AS measured,
       label::character varying AS label,
       ts::timestamp with time zone AS ts,
       tags[1:3] AS first_three,
       tags[2:] AS rest,
       name COLLATE "C" AS sortable
  FROM products;
