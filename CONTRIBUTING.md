# Contributing to banshee

banshee tracks the latest stable Rust toolchain — there is no MSRV to respect.

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

1. Add a variant to `RuleCode` in `crates/banshee_hir/src/analyze.rs` and its
   string in `RuleCode::as_str`. Follow the prefix convention: `AL` aliasing,
   `AM` ambiguity, `ST` structure, `SF` safety, `JB` JSONB, `CV` convention,
   `CP` capitalisation, `RF` references.
2. Implement the check in `crates/banshee_hir/src/lint.rs`. Emit a `Diagnostic`
   with `.with_code(...)`, a range, and—when the fix is mechanical and
   safe—`.with_fix(Fix::new(title, edits))`. Reference checks that need a live
   schema go through the analyzer's provider and are gated accordingly.
3. If the rule belongs to an opt-in pack (capitalisation/convention), wire it
   into the matching `apply_*_lints` dispatcher and the `BuiltinLintPack`.
4. Register it in the CLI catalog `RULES` in
   `crates/banshee/src/cli/rules_cmd.rs` (code, fixability, summary, explanation)
   so `banshee rules` and `banshee explain` stay in sync.
5. Add a unit test in `crates/banshee_hir/src/analyze.rs` (positive + negative
   case; assert the fix edit for fixable rules).

## Commit messages

Conventional-commit style (`feat:`, `fix:`, `docs:`, `chore:`, `ci:`). Explain
*why* in the body when it is not obvious from the subject.

## Releasing

Releases are cut manually, not on every push to `main`. The whole workspace
shares one version (`version.workspace`), so a single bump moves all crates.

```sh
cargo install cargo-release          # once
cargo release minor                  # dry-run: preview the bump + tag
cargo release minor -x               # bump version, commit, tag vX.Y.Z
git push --follow-tags               # push the commit + tag
```

Pushing the `vX.Y.Z` tag triggers `.github/workflows/release-tag.yml`, which:

1. builds the `banshee` binary for six targets, each with a `.sha256`, and
   publishes a stable GitHub release;
2. publishes all crates to crates.io in dependency order (idempotent — already
   published crates are skipped).

crates.io publishing needs a `CARGO_REGISTRY_TOKEN` repository secret (Settings →
Secrets → Actions) with `publish-update` scope. The first publish of a brand-new
crate is rate-limited by crates.io; subsequent version bumps are not.

The separate rolling `latest` pre-release (`release.yml`) rebuilds bleeding-edge
binaries on every push to `main` and is independent of tagged releases.
