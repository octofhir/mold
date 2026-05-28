//! Resolves a schema provider for the current invocation.
//!
//! Strategy: if a database connection is configured, serve a fresh `.mold/`
//! cache when one exists; otherwise refresh it. Refreshing performs live
//! introspection only when the binary is built with the `db` feature — without
//! it, a stale cache is used if present, and failing that the run proceeds
//! schema-less.

use std::path::PathBuf;

use mold_config::MoldConfig;
use mold_schema::{CachedSchemaProvider, SchemaSnapshot, cache, fingerprint};

/// How long a cached snapshot is considered fresh.
const DEFAULT_TTL_SECS: u64 = 3600;

/// Directory used as the `.mold/` cache root (the current working directory).
pub fn base_dir() -> PathBuf {
    std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
}

/// The connection fingerprint for the configured database, if any.
pub fn config_fingerprint(config: &MoldConfig) -> Option<String> {
    config
        .database
        .resolve_url()
        .map(|url| fingerprint(&url, &config.database.schema))
}

/// Forces a fresh introspection and writes the cache, ignoring any existing
/// snapshot. Returns `None` when no database is configured. Without the `db`
/// feature this errors, since live introspection is unavailable.
pub fn force_refresh(config: &MoldConfig) -> anyhow::Result<Option<SchemaSnapshot>> {
    let db = &config.database;
    let Some(url) = db.resolve_url() else {
        return Ok(None);
    };
    refresh_snapshot(&base_dir(), &url, db)
}

#[cfg(feature = "db")]
fn refresh_snapshot(
    base: &std::path::Path,
    url: &str,
    db: &mold_config::DatabaseSettings,
) -> anyhow::Result<Option<SchemaSnapshot>> {
    use std::time::Duration;

    let runtime = tokio::runtime::Runtime::new()?;
    let snapshot = runtime.block_on(mold_schema::introspect(
        url,
        &db.schema,
        db.pool_size,
        Duration::from_secs(db.connect_timeout_secs),
    ))?;
    cache::save(base, &snapshot)?;
    Ok(Some(snapshot))
}

#[cfg(not(feature = "db"))]
fn refresh_snapshot(
    _base: &std::path::Path,
    _url: &str,
    _db: &mold_config::DatabaseSettings,
) -> anyhow::Result<Option<SchemaSnapshot>> {
    anyhow::bail!("schema refresh requires a build with the `db` feature");
}

/// Resolves a snapshot-backed provider, or `None` for schema-less operation.
pub fn resolve(config: &MoldConfig) -> anyhow::Result<Option<CachedSchemaProvider>> {
    let db = &config.database;
    let Some(url) = db.resolve_url() else {
        return Ok(None);
    };
    let fp = fingerprint(&url, &db.schema);
    let base = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."));

    if let Some(snap) = cache::load_fresh(&base, &fp, DEFAULT_TTL_SECS) {
        return Ok(Some(CachedSchemaProvider::new(snap)));
    }
    refresh(&base, &url, db, &fp)
}

#[cfg(feature = "db")]
fn refresh(
    base: &std::path::Path,
    url: &str,
    db: &mold_config::DatabaseSettings,
    _fp: &str,
) -> anyhow::Result<Option<CachedSchemaProvider>> {
    use std::time::Duration;

    let runtime = tokio::runtime::Runtime::new()?;
    let snapshot = runtime.block_on(mold_schema::introspect(
        url,
        &db.schema,
        db.pool_size,
        Duration::from_secs(db.connect_timeout_secs),
    ))?;
    if let Err(e) = cache::save(base, &snapshot) {
        eprintln!("warning: could not write schema cache: {e}");
    }
    Ok(Some(CachedSchemaProvider::new(snapshot)))
}

#[cfg(not(feature = "db"))]
fn refresh(
    base: &std::path::Path,
    _url: &str,
    _db: &mold_config::DatabaseSettings,
    _fp: &str,
) -> anyhow::Result<Option<CachedSchemaProvider>> {
    // No live introspection in this build; use a stale cache if one exists.
    Ok(cache::load(base).map(CachedSchemaProvider::new))
}
