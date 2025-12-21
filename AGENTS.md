# Repository Guidelines

## Project Structure & Module Organization
- `crates/` holds workspace crates: `mold_syntax`, `mold_lexer`, `mold_parser`, `mold_hir`, `mold_completion`, `mold_format`, and the `mold` facade/binary.
- `crates/*/src/` contains library code; parser grammars live under `crates/mold_parser/src/grammar/`.
- Snapshot test fixtures live under `crates/*/src/snapshots/` and `crates/mold_format/tests/snapshots/`.
- `tasks/` documents project milestones and planning notes.
- `ARCHITECTURE.md` explains the core design and key components.

## Build, Test, and Development Commands
Use `just` targets from `justfile` (or run the cargo commands directly):
- `just test` → `cargo test --all` for full workspace tests.
- `just test-update` → runs tests then accepts new snapshots via `cargo insta accept`.
- `just check` → format, clippy, and tests (`cargo fmt`, `cargo clippy`, `cargo test`).
- `just fmt` → `cargo fmt --all` to apply rustfmt.
- `just build-mold` → `cargo build -p mold --release`.
- `just format-sql sql="SELECT 1"` → runs the formatter through the `mold` binary.

## Coding Style & Naming Conventions
- Rust style is idiomatic; prefer `?`, pattern matching, and iterators.
- Avoid `unwrap()` in library code; favor explicit error handling.
- Keep comments minimal and only where behavior is non-obvious.
- Formatting and linting are enforced via `rustfmt` and `clippy`.

## Testing Guidelines
- Tests are run with `cargo test --all`.
- Snapshot testing uses `insta`; update snapshots with `just test-update`.
- Snapshot files are stored under `crates/*/src/snapshots/` or `crates/mold_format/tests/snapshots/`.

## Commit & Pull Request Guidelines
- Commit messages appear to follow a Conventional Commits style (e.g., `chore: ...`); keep the same pattern when possible.
- PRs should describe the change, include rationale, and note any test updates or snapshot changes.
- If behavior changes, add or update tests and mention the relevant command used.

## Security & Configuration Tips
- No secrets should be committed; keep local config out of the repo.
- Favor small, focused changes to core parsing/formatting paths for safer review.
