<!-- Briefly describe the change and why it is needed. -->

## Checklist

- [ ] `just check` passes (fmt, clippy `-D warnings`, tests)
- [ ] Snapshots reviewed/accepted if output changed (`cargo insta review`)
- [ ] New lint rule (if any) registered in `rules_cmd::RULES` and covered by a test
- [ ] `CHANGELOG.md` updated under `[Unreleased]`
