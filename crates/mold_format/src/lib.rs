//! CST-based SQL formatter following sqlstyle.guide conventions.
//!
//! This crate provides SQL formatting that preserves the structure of the
//! original code while applying consistent style rules.
//!
//! # Features
//!
//! - River alignment for keyword alignment
//! - Configurable keyword case (UPPER, lower, Preserve)
//! - Leading or trailing comma style
//! - Proper indentation for subqueries and CTEs
//! - Comment preservation
//! - Error node preservation (malformed SQL preserved as-is)
//!
//! # Usage
//!
//! ```ignore
//! use mold_format::{format, FormatConfig};
//!
//! let sql = "select id,name from users where active=true";
//! let formatted = format(sql, &FormatConfig::sqlstyle());
//!
//! println!("{}", formatted);
//! // Output:
//! //     SELECT id
//! //          , name
//! //       FROM users
//! //      WHERE active = TRUE
//! ```

pub mod config;
pub mod format;
pub mod pg_format;
pub mod pg_formatter;
pub mod printer;
pub mod rules;
pub mod validate;

pub use config::{CommaStyle, FormatConfig, FormatStyle, IdentifierCase, IndentStyle, KeywordCase};
pub use format::format;
pub use pg_format::PgPrinter;
pub use pg_formatter::{CaseOption, PgFormatterConfig, PgFormatterError};
pub use printer::Printer;
pub use validate::{
    check_idempotent, count_tokens, format_pgformatter_validated, format_validated, normalize,
    semantically_equal, validate_comprehensive, validate_format, FormatError, FormatResult,
    ValidationReport,
};

/// Formats SQL using sqlstyle.guide defaults.
#[must_use]
pub fn format_sqlstyle(source: &str) -> String {
    format::format(source, &FormatConfig::sqlstyle())
}

/// Formats SQL using compact style.
#[must_use]
pub fn format_compact(source: &str) -> String {
    format::format(source, &FormatConfig::compact())
}

/// Formats SQL using pgFormatter-compatible defaults.
///
/// This uses the default pgFormatter configuration. For custom configuration,
/// use `PgFormatterConfig` directly.
#[must_use]
pub fn format_pgformatter(source: &str) -> String {
    pg_format::format(source, &PgFormatterConfig::default())
}

/// Formats SQL using pgFormatter configuration.
///
/// Allows passing a custom `PgFormatterConfig` for fine-grained control.
#[must_use]
pub fn format_with_pgformatter(source: &str, config: &PgFormatterConfig) -> String {
    pg_format::format(source, config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_select() {
        let sql = "select id, name from users";
        let formatted = format_compact(sql);
        assert!(formatted.contains("SELECT"));
        assert!(formatted.contains("FROM"));
    }

    #[test]
    fn test_format_river_alignment() {
        let sql = "SELECT id, name FROM users WHERE active = true";
        let formatted = format_sqlstyle(sql);
        // River alignment should produce consistent keyword alignment
        assert!(formatted.contains("SELECT"));
        assert!(formatted.contains("FROM"));
        assert!(formatted.contains("WHERE"));
    }

    #[test]
    fn test_format_with_where() {
        let sql = "SELECT * FROM users WHERE id = 1 AND name = 'test'";
        let formatted = format_sqlstyle(sql);
        assert!(formatted.contains("WHERE"));
        assert!(formatted.contains("AND"));
    }

    #[test]
    fn test_format_join() {
        let sql = "SELECT u.id, o.amount FROM users u LEFT JOIN orders o ON u.id = o.user_id";
        let formatted = format_sqlstyle(sql);
        assert!(formatted.contains("LEFT JOIN"));
        assert!(formatted.contains("ON"));
    }

    #[test]
    fn test_format_jsonb() {
        let sql = "SELECT data->'name'->>'first' FROM users";
        let formatted = format_compact(sql);
        // JSONB operators should not have spaces around them
        assert!(formatted.contains("->"));
        assert!(formatted.contains("->>"));
    }

    #[test]
    fn test_format_cte() {
        let sql = "WITH active_users AS (SELECT * FROM users WHERE active = true) SELECT * FROM active_users";
        let formatted = format_sqlstyle(sql);
        assert!(formatted.contains("WITH"));
        assert!(formatted.contains("AS"));
    }

    #[test]
    fn test_idempotent() {
        let sql = "SELECT id, name FROM users WHERE active = true";
        let formatted1 = format_sqlstyle(sql);
        let formatted2 = format_sqlstyle(&formatted1);
        assert_eq!(formatted1, formatted2, "Formatting should be idempotent");
    }

    #[test]
    fn test_config_builder() {
        let config = FormatConfig::sqlstyle()
            .with_keyword_case(KeywordCase::Lower)
            .with_comma_style(CommaStyle::Trailing);

        assert_eq!(config.keyword_case, KeywordCase::Lower);
        assert_eq!(config.comma_style, CommaStyle::Trailing);
    }

    #[test]
    fn test_format_insert() {
        let sql = "INSERT INTO users (id, name) VALUES (1, 'test')";
        let formatted = format_compact(sql);
        assert!(formatted.contains("INSERT"));
        assert!(formatted.contains("INTO"));
        assert!(formatted.contains("VALUES"));
    }

    #[test]
    fn test_format_update() {
        let sql = "UPDATE users SET name = 'test' WHERE id = 1";
        let formatted = format_compact(sql);
        assert!(formatted.contains("UPDATE"));
        assert!(formatted.contains("SET"));
        assert!(formatted.contains("WHERE"));
    }

    #[test]
    fn test_format_delete() {
        let sql = "DELETE FROM users WHERE id = 1";
        let formatted = format_compact(sql);
        assert!(formatted.contains("DELETE"));
        assert!(formatted.contains("FROM"));
        assert!(formatted.contains("WHERE"));
    }

    #[test]
    fn test_format_case() {
        let sql = "SELECT CASE WHEN active THEN 'yes' ELSE 'no' END FROM users";
        let formatted = format_compact(sql);
        assert!(formatted.contains("CASE"));
        assert!(formatted.contains("WHEN"));
        assert!(formatted.contains("THEN"));
        assert!(formatted.contains("ELSE"));
        assert!(formatted.contains("END"));
    }

    #[test]
    fn test_format_subquery() {
        let sql = "SELECT * FROM (SELECT id FROM users) AS u";
        let formatted = format_compact(sql);
        assert!(formatted.contains("SELECT"));
        assert!(formatted.contains("AS"));
    }

    #[test]
    fn test_format_window_function() {
        let sql = "SELECT id, row_number() OVER (PARTITION BY category ORDER BY id) FROM items";
        let formatted = format_compact(sql);
        assert!(formatted.contains("OVER"));
        assert!(formatted.contains("PARTITION"));
        assert!(formatted.contains("ORDER BY"));
    }

    #[test]
    fn test_types_are_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<FormatConfig>();
        assert_send_sync::<KeywordCase>();
        assert_send_sync::<IdentifierCase>();
        assert_send_sync::<IndentStyle>();
        assert_send_sync::<CommaStyle>();
        assert_send_sync::<Printer>();
    }
}
