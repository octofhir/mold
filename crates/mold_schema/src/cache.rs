//! On-disk schema cache stored under a `.mold/` directory.
//!
//! Persisting the snapshot means CLI and LSP startups do not have to re-query
//! the database every time; introspection only runs when the cache is missing,
//! stale, or points at a different connection.

use std::path::{Path, PathBuf};

use crate::snapshot::SchemaSnapshot;

/// Directory name holding mold's cache, relative to the project root.
pub const CACHE_DIR: &str = ".mold";
/// File name of the schema cache within [`CACHE_DIR`].
pub const CACHE_FILE: &str = "schema-cache.json";

/// Errors from cache I/O.
#[derive(Debug)]
pub enum CacheError {
    Io(std::io::Error),
    Serde(serde_json::Error),
}

impl std::fmt::Display for CacheError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CacheError::Io(e) => write!(f, "schema cache I/O error: {e}"),
            CacheError::Serde(e) => write!(f, "schema cache format error: {e}"),
        }
    }
}

impl std::error::Error for CacheError {}

impl From<std::io::Error> for CacheError {
    fn from(e: std::io::Error) -> Self {
        CacheError::Io(e)
    }
}

impl From<serde_json::Error> for CacheError {
    fn from(e: serde_json::Error) -> Self {
        CacheError::Serde(e)
    }
}

/// Returns the cache file path for a project rooted at `base_dir`.
#[must_use]
pub fn cache_path(base_dir: &Path) -> PathBuf {
    base_dir.join(CACHE_DIR).join(CACHE_FILE)
}

/// Loads a cached snapshot, if present and parseable.
///
/// A corrupt or unreadable cache returns `Ok(None)` rather than erroring: a bad
/// cache should trigger a re-fetch, not abort the tool.
pub fn load(base_dir: &Path) -> Option<SchemaSnapshot> {
    let path = cache_path(base_dir);
    let bytes = std::fs::read(&path).ok()?;
    serde_json::from_slice(&bytes).ok()
}

/// Writes a snapshot to the cache, creating `.mold/` as needed.
pub fn save(base_dir: &Path, snapshot: &SchemaSnapshot) -> Result<(), CacheError> {
    let dir = base_dir.join(CACHE_DIR);
    std::fs::create_dir_all(&dir)?;
    let json = serde_json::to_vec_pretty(snapshot)?;
    std::fs::write(dir.join(CACHE_FILE), json)?;
    Ok(())
}

/// Loads the cached snapshot only if it is fresh for the given target.
pub fn load_fresh(
    base_dir: &Path,
    expected_fingerprint: &str,
    ttl_secs: u64,
) -> Option<SchemaSnapshot> {
    load(base_dir).filter(|s| s.is_fresh(expected_fingerprint, ttl_secs))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn save_then_load_roundtrips() {
        let dir = std::env::temp_dir().join(format!("mold_cache_test_{}", crate::snapshot::now_unix()));
        std::fs::create_dir_all(&dir).unwrap();

        let snap = SchemaSnapshot::new("fp", "public");
        save(&dir, &snap).unwrap();

        let loaded = load(&dir).expect("snapshot loads");
        assert_eq!(loaded.fingerprint, "fp");
        assert!(loaded.is_fresh("fp", 600));
        assert!(cache_path(&dir).exists());

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn missing_cache_is_none() {
        let dir = std::env::temp_dir().join("mold_cache_test_does_not_exist_xyz");
        assert!(load(&dir).is_none());
    }
}
