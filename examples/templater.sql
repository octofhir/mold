-- Linting parameterised app SQL. Placeholders like `:name` are not valid SQL
-- on their own, so enable the templater first. Run from this directory:
--
--     mold lint --config examples/templater.toml examples/templater.sql
--
-- (or drop the [templater] section into your mold.toml). With the templater on,
-- placeholders are substituted before parsing, the statement lints normally,
-- and findings point back at the original text. The placeholders survive
-- `mold fix` untouched.

-- :colon style — psql / many ORMs. `id = :id` lints fine; AM04 still flags `*`.
SELECT * FROM users WHERE id = :id AND name != :name;

-- A placeholder inside a string or comment is left alone: ':id' stays literal.
SELECT ':id' AS tag FROM users WHERE created_at > :since;

-- Native Postgres params ($1, $name) need no templater and are never touched.
SELECT id FROM users WHERE org = $1;
