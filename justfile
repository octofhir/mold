# Development task runner for mold

# Run all tests
test:
    cargo test --all

# Run tests and accept new snapshots
test-update:
    cargo test --all
    cargo insta accept

# Run pre-commit checks (format, lint, test)
check:
    cargo fmt --all -- --check
    cargo clippy --all -- -D warnings
    cargo test --all

# Format code
fmt:
    cargo fmt --all

# Fix clippy warnings
fix:
    cargo clippy --all --fix --allow-dirty

# Run everything (format, test, accept snapshots)
all:
    cargo fmt --all
    cargo test --all
    cargo insta accept

# Quick development cycle: format, test, accept snapshots
dev:
    cargo fmt --all
    cargo test --all || cargo insta accept && cargo test --all
