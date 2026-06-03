---
title: Schema-aware features
description: Live Postgres introspection drives completion and reference checks.
sidebar:
  order: 4
---

With `[database]` configured and a `db`-feature build, banshee introspects
`information_schema` / `pg_catalog` for tables, columns, primary keys, functions,
and **samples JSONB columns** to infer their shape. The result is cached at
`.banshee/schema-cache.json` (keyed by connection + schema, with a TTL), so only
the first run touches the database.

The cached schema drives:

- column / table completion,
- JSONB key completion (`resource->'…'`),
- `RF01` / `RF02` reference checks and "did you mean?" suggestions,
- `AM04`'s `SELECT *` expansion.

Reference checks (`RF*`) only run when a schema is available, so they never
produce false positives offline. The `.banshee/` cache is plain JSON and can be
read without the `db` feature, so embedders stay async-free.
