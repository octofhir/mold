# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); this project tracks
the latest stable Rust toolchain.

## [Unreleased]

### Added

- **Configuration** (`mold_config`): unified `mold.toml` schema covering the
  formatter, linter, completion and database connection, with walk-up
  discovery and an env-var-based connection string.
- **Lint engine** (`mold_hir`): 16 rules across aliasing (AL03, AL05),
  ambiguity (AM02, AM04, AM05, AM09), structure (ST01, ST03, ST05), safety
  (SF01, SF02), JSONB (JB01), convention (CV01, CV05, CV06), capitalisation
  (CP01, CP02) and references (RF01, RF02, RF03). Nine are auto-fixable.
  Diagnostics carry a typed `RuleCode`, an optional `help` line, and fixes.
- **Schema introspection** (`mold_schema`): live Postgres introspection behind
  the `db` feature (tables, columns, primary keys, functions, and sampled JSONB
  column shapes), cached to `.mold/schema-cache.json`; a sync, serde-only
  provider reads the cache without async dependencies.
- **Language server** (`mold_lsp`): diagnostics with quick-fix code actions,
  completion (including JSONB key completion), hover, document & range
  formatting, document symbols, semantic tokens, and signature help.
- **CLI** (`mold`): `format`, `lint`, `fix`, `parse`, `rules`, `explain`,
  `complete`, `schema`, `init`, and `lsp` subcommands. rustc-style diagnostic
  rendering with "did you mean?" hints; `--format human|json|github|sarif`;
  `NO_COLOR`/`--no-color`, `--quiet`, and parallel multi-file processing.
- **Editor**: a local VS Code extension under `editors/code` that runs
  `mold lsp`.
- **CI**: GitHub Actions running fmt, clippy (`-D warnings`) and the test suite,
  plus a `db`-feature job.

### Fixed

- Parser no longer panics on the `!=` operator (a `NE` static-text mismatch).
- pgFormatter engine preserved parentheses (`count(*)`) and implicit-alias
  spacing (`users a`).
- Reference checks (RF\*) are suppressed without a live schema, so offline
  linting produces no false positives.
