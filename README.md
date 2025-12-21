# Mold

Mold is a pure Rust PostgreSQL SQL parser and formatter built for editor tooling.
It is part of the OctoFHIR ecosystem.
It provides a lossless CST parser, semantic analysis helpers, and a completion engine.

## Features
- Lossless CST with recovery for partial or invalid SQL
- PostgreSQL-oriented grammar and JSONB/JSONPath support
- Formatter aligned with sqlstyle.guide
- Completion engine decoupled from LSP types

## Architecture overview
The project is split into focused crates under `crates/`:
- `mold_lexer`: tokenizes SQL input
- `mold_parser`: builds a CST with errors
- `mold_syntax`: syntax kinds and `Parse` container
- `mold_hir`: semantic analysis and name resolution
- `mold_completion`: completion logic and providers
- `mold_format`: SQL formatting
- `mold`: facade crate and CLI entry point

For a deeper design walkthrough, see `ARCHITECTURE.md`.

## Usage
```ignore
let parse = mold::parser::parse("SELECT * FROM users");
if !parse.errors().is_empty() {
    eprintln!("parse errors: {:?}", parse.errors());
}

let formatted = mold_format::format_sqlstyle("select * from users");
println!("{formatted}");
```

## Examples
Run from the repository root:
- `cargo run --example parse_and_format`
- `cargo run --example completion`
- `cargo run --example custom_provider`

## Development
- `cargo test --all`
- `cargo bench -p mold`
- `just fmt`

## License
MIT
