//! Database connection settings.
//!
//! Powers the generic LSP / live-schema features: when a connection is
//! configured, schema introspection can feed completion and lint with real
//! tables, columns and functions.

use serde::{Deserialize, Serialize};

fn default_schema() -> String {
    "public".to_string()
}

fn default_pool_size() -> u32 {
    2
}

fn default_connect_timeout() -> u64 {
    5
}

/// Postgres connection configuration.
///
/// Prefer `url_env` over an inline `url` so connection strings (which carry
/// credentials) never live in a committed config file.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", default)]
pub struct DatabaseSettings {
    /// Inline connection string. Discouraged for anything with credentials.
    pub url: Option<String>,
    /// Name of an environment variable holding the connection string.
    /// Takes precedence over `url` when set and present.
    pub url_env: Option<String>,
    /// Default schema for unqualified names.
    #[serde(default = "default_schema")]
    pub schema: String,
    /// Connection pool size for introspection.
    #[serde(default = "default_pool_size")]
    pub pool_size: u32,
    /// Connection timeout in seconds.
    #[serde(default = "default_connect_timeout")]
    pub connect_timeout_secs: u64,
}

impl Default for DatabaseSettings {
    fn default() -> Self {
        Self {
            url: None,
            url_env: None,
            schema: default_schema(),
            pool_size: default_pool_size(),
            connect_timeout_secs: default_connect_timeout(),
        }
    }
}

impl DatabaseSettings {
    /// Resolves the effective connection string.
    ///
    /// `url_env` wins when set and the variable is present; otherwise the
    /// inline `url` is used. Returns `None` when no connection is configured.
    #[must_use]
    pub fn resolve_url(&self) -> Option<String> {
        if let Some(var) = &self.url_env {
            if let Ok(val) = std::env::var(var) {
                if !val.is_empty() {
                    return Some(val);
                }
            }
        }
        self.url.clone().filter(|u| !u.is_empty())
    }

    /// Whether any connection target is configured.
    #[must_use]
    pub fn is_configured(&self) -> bool {
        self.resolve_url().is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_have_no_connection() {
        let db = DatabaseSettings::default();
        assert_eq!(db.schema, "public");
        assert!(!db.is_configured());
        assert!(db.resolve_url().is_none());
    }

    #[test]
    fn inline_url_resolves() {
        let db = DatabaseSettings {
            url: Some("postgres://localhost/x".into()),
            ..Default::default()
        };
        assert_eq!(db.resolve_url().as_deref(), Some("postgres://localhost/x"));
    }

    #[test]
    fn env_takes_precedence() {
        // Use a unique var name to avoid cross-test contamination.
        let var = "MOLD_TEST_DB_URL_PRECEDENCE";
        unsafe {
            std::env::set_var(var, "postgres://env/db");
        }
        let db = DatabaseSettings {
            url: Some("postgres://inline/db".into()),
            url_env: Some(var.into()),
            ..Default::default()
        };
        assert_eq!(db.resolve_url().as_deref(), Some("postgres://env/db"));
        unsafe {
            std::env::remove_var(var);
        }
    }
}
