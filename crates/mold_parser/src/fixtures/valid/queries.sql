-- Query terms: VALUES, TABLE, parenthesized set operations, grouping sets,
-- TABLESAMPLE, and WITH ORDINALITY.
VALUES (1, 'a'), (2, 'b'), (3, 'c');

TABLE users;

WITH seed (id, name) AS (VALUES (1, 'a'), (2, 'b'))
SELECT * FROM seed;

(SELECT id FROM active_users)
UNION
(SELECT id FROM pending_users)
ORDER BY id
LIMIT 100;

SELECT region, product, sum(amount)
  FROM sales
 GROUP BY GROUPING SETS ((region, product), (region), ());

SELECT region, sum(amount)
  FROM sales
 GROUP BY ROLLUP (region);

SELECT *
  FROM measurements TABLESAMPLE SYSTEM (10) REPEATABLE (42);

SELECT v, n
  FROM unnest(ARRAY['a', 'b', 'c']) WITH ORDINALITY AS t (v, n);
