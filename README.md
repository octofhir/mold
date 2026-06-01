# mold

[![CI](https://github.com/octofhir/mold/actions/workflows/ci.yml/badge.svg)](https://github.com/octofhir/mold/actions/workflows/ci.yml)

A PostgreSQL SQL parser, formatter, linter, and language server, written in Rust.

mold builds a **lossless concrete syntax tree** with error recovery, so it keeps
working on incomplete or invalid SQL — the use case editors actually need. On
top of the tree it resolves names, formats, lints (with autofixes), and serves
an LSP. When a database connection is configured it introspects the live schema
to make completion and reference checks accurate.

Scope is deliberately **PostgreSQL only**. There is no dialect-abstraction
layer; the grammar, JSONB/JSONPath support, and lint rules target Postgres.

## Install

```sh
cargo build --release -p mold                  # parser, formatter, linter, LSP
cargo build --release -p mold --features db    # + live schema introspection
```

The binary is `target/release/mold`. The `db` feature pulls in `sqlx`/`tokio`;
leave it off and the binary stays async-free, using a cached schema if present.

## CLI

```
mold format [--write|--check] [files…|-]
mold lint   [--format human|json|github|sarif] [files…|-]
mold fix    [--diff] [files…|-]
mold parse  [--format tree|json] [files…|-]
mold rules                 # list lint rules
mold explain <CODE>        # detailed rule description with examples
mold init                  # scaffold a mold.toml
mold lsp                   # language server over stdio
```

Inputs are files, directories (walked for `*.sql`), or stdin (`-`). Exit codes:
`0` clean, `1` findings / unformatted, `2` error.

```sh
echo "select id,name from users where active=true" | mold format -
#  SELECT id, name
#    FROM users
#   WHERE active = true

echo "select * from a, b" | mold lint -
#  warning[AM04]: Avoid SELECT *; list columns explicitly
#  warning[AM05]: Implicit cross join; use an explicit JOIN clause
#  …
```

Human output is rendered rustc-style (caret underlines, `help:` hints, colors on
a TTY). `--format json|github|sarif` is for CI and code-scanning.

## Configuration

Configuration lives in `mold.toml`, discovered by walking up from the input.
The on-disk schema is decoupled from the engine's internal types.

```toml
[format]
style = "sqlstyle"        # sqlstyle | pgformatter | compact
# keyword-case = "upper"  # plus per-knob overrides (indent, commas, width…)

[lint]
enabled = true
# exclude = ["AM04"]
# [lint.rules.CP01]
# severity = "warning"

[database]
url-env = "DATABASE_URL"  # read the connection string from this env var
schema  = "public"
```

The formatter ships two engines: `sqlstyle` (river alignment, sqlstyle.guide)
and `pgformatter`. `style` selects the engine; shared knobs map across both.

## Linting

| Code | Fixable | Description |
|------|---------|-------------|
| AL01 | yes     | Table alias should be introduced with `AS` |
| AL02 | yes     | Column/expression alias should be introduced with `AS` |
| AL03 | —       | Complex select expression should be aliased with `AS` |
| AL05 | —       | Table alias declared but never used |
| AM02 | —       | Set operators (`UNION`/`EXCEPT`/`INTERSECT`) should state `ALL`/`DISTINCT` |
| AM03 | —       | `ORDER BY` mixes explicit and implicit sort directions |
| AM04 | yes\*   | Avoid `SELECT *`; expand to columns (\*fix needs schema) |
| AM05 | —       | Implicit cross join; use an explicit `JOIN` |
| AM09 | —       | `LIMIT`/`OFFSET` without `ORDER BY` is non-deterministic |
| ST01 | yes     | Redundant `ELSE NULL` in `CASE` |
| ST03 | —       | CTE defined but never used |
| ST05 | —       | Subquery in `FROM`/`JOIN`; prefer a CTE |
| ST07 | —       | Avoid `NATURAL JOIN`; state the join columns |
| ST08 | —       | `DISTINCT ON` without `ORDER BY` is non-deterministic |
| SF01 | —       | `UPDATE` without `WHERE` affects all rows |
| SF02 | —       | `DELETE` without `WHERE` affects all rows |
| SF03 | —       | `INSERT` without an explicit column list |
| JB01 | yes     | Use `->>` when comparing a JSONB value to text |
| CV01 | yes     | Use `<>` instead of `!=` for inequality |
| CV05 | yes     | Compare with NULL using `IS NULL` / `IS NOT NULL` |
| CV06 | yes     | Statements should end with a semicolon |
| CV08 | —       | Prefer `LEFT JOIN` over `RIGHT JOIN` |
| CV10 | yes     | `LIKE` without a wildcard is just `=` |
| CP01 | yes     | Keywords should be upper case |
| CP02 | yes     | Unquoted identifiers should be lower case |
| RF01 | —       | Unknown table/column/alias (needs schema) |
| RF02 | —       | Ambiguous column; qualify it (needs schema) |
| RF03 | —       | Inconsistent column qualification in a single-table query |
| RF06 | yes     | Identifier quoted unnecessarily |

Prefixes: `AL` aliasing, `AM` ambiguity, `ST` structure, `SF` safety, `JB`
JSONB, `CV` convention, `CP` capitalisation, `RF` references.

`mold fix` applies every available autofix; `mold explain <CODE>` prints the
rationale and a before/after example. Reference checks (RF\*) only run when a
schema is available, so they never produce false positives offline.

### Suppressing diagnostics inline

A `noqa` comment silences findings without touching `mold.toml`. It uses the
familiar `-- noqa` convention, so existing suppressions carry over:

```sql
SELECT * FROM t;              -- noqa               suppress everything on this line
SELECT * FROM t;              -- noqa: AM04         suppress only AM04 on this line
SELECT * FROM t;              -- noqa: AM,RF01      a group prefix or explicit codes

-- range form: suppress until a matching enable (or end of file)
SELECT * FROM a;              -- noqa: disable=AM04
SELECT * FROM b;              --   (AM04 stays silent here)
SELECT * FROM c;              -- noqa: enable=AM04
```

Use `disable=all` / `enable=all` to gate every rule. Suppression is applied by
the engine, so the LSP honours it too. An alphabetic token (`AM`) matches every
code in that family; a full code (`AM04`) matches only itself.

## Templating

Application SQL often contains parameter placeholders that are not valid
PostgreSQL on their own (`:name`, `?`, `%(name)s`). Enable the templater to
substitute them before parsing so the statement still lints; findings map back
to the original text and the placeholders survive `mold fix` untouched.

```toml
[templater]
style = "colon"        # colon (:name / :1) | question-mark (?) | percent (%s, %(name)s)
```

Native Postgres parameters (`$1`, `$name`) are already valid and are never
touched, and placeholders inside strings, dollar-quotes and comments are left
alone. See [`examples/templater.sql`](examples/templater.sql).

## Schema-aware features

With `[database]` configured and a `db` build, mold introspects
`information_schema`/`pg_catalog` for tables, columns, primary keys, functions,
and **samples JSONB columns** to infer their shape. The result is cached at
`.mold/schema-cache.json` (keyed by connection + schema, with a TTL), so only
the first run touches the database.

The cached schema drives:
- column / table completion,
- JSONB key completion (`resource->'…'`),
- `RF01`/`RF02` reference checks and "did you mean?" suggestions,
- `AM04`'s `SELECT *` expansion.

A `.mold/` cache is plain JSON and can be read without the `db` feature, so
embedders stay async-free.

## Language server

`mold lsp` speaks LSP over stdio. Implemented:

- diagnostics (parse errors + lint findings, with quick-fix code actions),
- completion (keywords, schema columns/tables, JSONB paths),
- hover (column type and source table),
- document formatting and range formatting,
- document symbols (outline),
- semantic tokens (highlighting),
- signature help.

### Editor setup — VS Code

A local extension lives in [`editors/code`](editors/code). Build `mold`, put it
on your `PATH` (or set `mold.path`), then from `editors/code` run
`npm install && npm run compile` and press **F5** in VS Code to launch it. See
[`editors/code/README.md`](editors/code/README.md) for details. It is not
published to the Marketplace.

## CI integration

`mold lint --format sarif` emits SARIF 2.1.0; upload it to GitHub code scanning:

```yaml
- run: mold lint --format sarif . > mold.sarif
- uses: github/codeql-action/upload-sarif@v3
  with:
    sarif_file: mold.sarif
```

For pre-commit, this repo ships hooks (`mold-format`, `mold-lint`) — see
`.pre-commit-hooks.yaml`.

## Library

The workspace is split into focused crates:

| Crate | Purpose |
|-------|---------|
| `mold_lexer` | Tokenizer |
| `mold_parser` | Recovering parser → lossless CST |
| `mold_syntax` | Syntax kinds, AST, `Parse` container |
| `mold_hir` | Name resolution, scopes, lint rules |
| `mold_completion` | Completion engine and provider traits |
| `mold_format` | Formatter (two engines) and edit diffing |
| `mold_config` | `mold.toml` schema (serde) |
| `mold_schema` | Schema snapshot, `.mold/` cache, live introspection |
| `mold_lsp` | Language server |
| `mold` | Facade crate and CLI |

```rust
let parse = mold::parser::parse("SELECT id, name FROM users");
assert!(parse.errors().is_empty());

let formatted = mold_format::format_sqlstyle("select id,name from users");
println!("{formatted}");
```

Runnable examples:

```sh
cargo run -p mold --example parse_and_format
cargo run -p mold --example completion
cargo run -p mold --example custom_provider
```

## Development

```sh
just test          # cargo test --all
just check         # fmt + clippy + test
just demo-db       # spin up Postgres, introspect, lint a sample (needs Docker)
```

`just demo-db` brings up a seeded Postgres via `docker-compose.yml`, introspects
it (writing `.mold/schema-cache.json`), and runs schema-aware lint plus JSONB
key completion against it end to end.

The project tracks the latest stable Rust toolchain; there is no pinned MSRV.

## Stability

Pre-1.0: the public API of the engine crates may change between minor versions.
The intended public surface is what each crate re-exports from its `lib.rs`
(notably `Parse`/`SyntaxNode` in `mold_syntax`, `parse` in `mold_parser`,
`analyze_query*` / `SchemaProvider` / `Diagnostic` / `RuleCode` in `mold_hir`,
the provider traits in `mold_completion`, and `format*` in `mold_format`).
Items reachable but not part of that surface may change without notice.

## License

MIT
