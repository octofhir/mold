---
title: Quickstart
description: Format and lint your first SQL with banshee.
---

After [installing](/banshee/install/), point banshee at a file, a directory
(walked for `*.sql`), or stdin (`-`).

## Format

```sh
echo "select id,name from users where active=true" | banshee format -
```

```text
 SELECT id, name
   FROM users
  WHERE active = true
```

Use `banshee format --write .` to rewrite files in place, or `--check` (exit code
`1` if anything is unformatted) in CI.

## Lint

```sh
echo "select * from a, b" | banshee lint -
```

```text
warning[AM04]: Avoid SELECT *; list columns explicitly
warning[AM05]: Implicit cross join; use an explicit JOIN clause
```

`banshee fix` applies every available autofix; `banshee explain AM04` prints the
rationale and a before/after example. Browse every rule in the
[rule reference](/banshee/rules/).

## Exit codes

`0` clean · `1` findings or unformatted input · `2` error. That is the CI
contract — see [CI integration](/banshee/guides/ci/).
