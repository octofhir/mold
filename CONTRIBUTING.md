# Contributing to mold

mold tracks the latest stable Rust toolchain — there is no MSRV to respect.

## Workflow

```sh
just test        # cargo test --workspace
just check       # cargo fmt --check + clippy -D warnings + test
just fmt         # apply rustfmt
```

CI runs `fmt --check`, `clippy -D warnings`, the full test suite, and a job
with the `db` feature. Match that locally before opening a PR.

Snapshot tests use [`insta`](https://insta.rs). When output changes
intentionally, review and accept with `cargo insta review` (or
`just test-update`).

## Adding a lint rule

1. Add a variant to `RuleCode` in `crates/mold_hir/src/analyze.rs` and its
   string in `RuleCode::as_str`. Follow the prefix convention: `AL` aliasing,
   `AM` ambiguity, `ST` structure, `SF` safety, `JB` JSONB, `CV` convention,
   `CP` capitalisation, `RF` references.
2. Implement the check in `crates/mold_hir/src/lint.rs`. Emit a `Diagnostic`
   with `.with_code(...)`, a range, and—when the fix is mechanical and
   safe—`.with_fix(Fix::new(title, edits))`. Reference checks that need a live
   schema go through the analyzer's provider and are gated accordingly.
3. If the rule belongs to an opt-in pack (capitalisation/convention), wire it
   into the matching `apply_*_lints` dispatcher and the `BuiltinLintPack`.
4. Register it in the CLI catalog `RULES` in
   `crates/mold/src/cli/rules_cmd.rs` (code, fixability, summary, explanation)
   so `mold rules` and `mold explain` stay in sync.
5. Add a unit test in `crates/mold_hir/src/analyze.rs` (positive + negative
   case; assert the fix edit for fixable rules).

## Commit messages

Conventional-commit style (`feat:`, `fix:`, `docs:`, `chore:`, `ci:`). Explain
*why* in the body when it is not obvious from the subject.
