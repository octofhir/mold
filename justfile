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

# === CLI ===

# Build the mold binary (release)
build-mold:
    cargo build -p mold --release

# Build with live database introspection (sqlx + tokio)
build-db:
    cargo build -p mold --release --features db

# Format a SQL string (reads via stdin)
format-sql sql:
    echo "{{sql}}" | cargo run -p mold --quiet -- format -

# Lint a SQL string (exits 0 even when findings exist, for convenience)
lint-sql sql:
    echo "{{sql}}" | cargo run -p mold --quiet -- lint - || true

# Apply autofixes to a SQL string
fix-sql sql:
    echo "{{sql}}" | cargo run -p mold --quiet -- fix - || true

# Dump the CST of a SQL string as JSON
parse-sql sql:
    echo "{{sql}}" | cargo run -p mold --quiet -- parse --format json - || true

# List built-in lint rules
rules:
    cargo run -p mold --quiet -- rules

# Run the language server over stdio (with live schema support)
lsp:
    cargo run -p mold --features db -- lsp

# === Postgres-backed demo ===

# Start the demo Postgres (seeded with example tables) and wait for readiness
db-up:
    docker compose up -d --wait

# Stop and remove the demo Postgres (and its volume)
db-down:
    docker compose down -v

# Introspect the live schema, cache it under .mold/, lint a sample (needs Docker)
demo-db: db-up
    DATABASE_URL=postgres://mold:mold@localhost:55432/mold_demo \
        cargo run -p mold --features db -- --config mold.example.toml lint examples/demo.sql || true
    @echo
    @echo "--- JSONB key completion after resource-> (sampled from live rows) ---"
    printf "select resource->'' from patient" | \
        DATABASE_URL=postgres://mold:mold@localhost:55432/mold_demo \
        cargo run -p mold --features db --quiet -- --config mold.example.toml complete --offset 18 - || true
    @echo
    @echo "Schema (incl. JSONB shapes) cached at .mold/schema-cache.json"
