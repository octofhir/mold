---
title: Migrate from sqlfluff
description: What changes when you move PostgreSQL linting from sqlfluff to banshee.
sidebar:
  order: 8
---

banshee targets PostgreSQL only, which is the trade that buys the speed (~24×
faster than sqlfluff in formatting) and a real Postgres grammar instead of a
dialect abstraction. If your project is Postgres, the move is usually a win.

## Mapping

| sqlfluff | banshee |
|----------|---------|
| `.sqlfluff` config | `banshee.toml` |
| `sqlfluff lint` | `banshee lint` |
| `sqlfluff fix` | `banshee fix` |
| `sqlfluff format` | `banshee format` |
| `-- noqa: rules` | `-- noqa: CODES` (same convention) |
| Jinja/dbt templating | `[templater]` for `:name` / `?` / `%(name)s` placeholders |

## What you gain

- One binary, no Python environment.
- A migration-safety pack (`MG01`–`MG16`) sqlfluff doesn't have.
- Schema-aware reference checks against a live database.
- An LSP for in-editor diagnostics.

## What's different

- PostgreSQL only — no other dialects.
- Rule codes differ; browse the [rule reference](/banshee/rules/) and map the
  checks you rely on. `banshee rules` lists them all.
