---
title: Linting & suppression
description: Run lints, apply fixes, and silence findings inline with noqa.
sidebar:
  order: 2
---

`banshee lint` reports findings; `banshee fix` applies every available autofix.
Output formats: `human` (default, rustc-style), `json`, `github` (PR
annotations), `sarif` (code scanning).

Browse all rules in the [rule reference](/banshee/rules/), or run
`banshee explain <CODE>`.

## Suppressing diagnostics inline

A `-- noqa` comment silences findings without touching `banshee.toml`:

```sql
SELECT * FROM t;        -- noqa             suppress everything on this line
SELECT * FROM t;        -- noqa: AM04       suppress only AM04
SELECT * FROM t;        -- noqa: AM,RF01    a group prefix or explicit codes
```

Range form suppresses until a matching enable (or end of file):

```sql
SELECT * FROM a;        -- noqa: disable=AM04
SELECT * FROM b;        --   (AM04 stays silent here)
SELECT * FROM c;        -- noqa: enable=AM04
```

Use `disable=all` / `enable=all` to gate every rule. An alphabetic token (`AM`)
matches every code in that family; a full code (`AM04`) matches only itself.
Suppression is applied by the engine, so the LSP honours it too.
