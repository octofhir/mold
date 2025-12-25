//! pgFormatter-compatible configuration.
//!
//! This module provides a configuration that is compatible with pgFormatter,
//! the popular Perl-based SQL formatter. It supports loading `.pg_format` files
//! and provides all the same options as pgFormatter.
//!
//! # Example
//!
//! ```ignore
//! use mold_format::pg_formatter::PgFormatterConfig;
//!
//! // Use defaults (matches pgFormatter defaults)
//! let config = PgFormatterConfig::default();
//!
//! // Parse from string (always available)
//! let config = PgFormatterConfig::from_str("spaces = 2")?;
//!
//! // Load from file (requires "config-file" feature)
//! #[cfg(feature = "config-file")]
//! let config = PgFormatterConfig::from_file(".pg_format")?;
//!
//! // Auto-discover config file (requires "config-file" feature)
//! #[cfg(feature = "config-file")]
//! let config = PgFormatterConfig::discover().unwrap_or_default();
//! ```

#[cfg(feature = "config-file")]
use std::fs;
#[cfg(feature = "config-file")]
use std::path::{Path, PathBuf};

/// Case transformation options (matches pgFormatter's 0-3 values).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum CaseOption {
    /// Keep original case (0)
    Unchanged = 0,
    /// Convert to lowercase (1)
    Lower = 1,
    /// Convert to UPPERCASE (2)
    #[default]
    Upper = 2,
    /// Capitalize First Letter (3)
    Capitalize = 3,
}

impl CaseOption {
    /// Creates a CaseOption from a numeric value (0-3).
    pub fn from_value(value: u8) -> Option<Self> {
        match value {
            0 => Some(CaseOption::Unchanged),
            1 => Some(CaseOption::Lower),
            2 => Some(CaseOption::Upper),
            3 => Some(CaseOption::Capitalize),
            _ => None,
        }
    }

    /// Transforms text according to the case option.
    pub fn apply(&self, text: &str) -> String {
        match self {
            CaseOption::Unchanged => text.to_string(),
            CaseOption::Lower => text.to_lowercase(),
            CaseOption::Upper => text.to_uppercase(),
            CaseOption::Capitalize => capitalize(text),
        }
    }
}

/// Capitalizes the first letter of each word.
fn capitalize(text: &str) -> String {
    text.split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
                }
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Error type for pgFormatter configuration.
#[derive(Debug, Clone)]
pub enum PgFormatterError {
    /// Failed to read the configuration file.
    IoError(String),
    /// Invalid configuration format.
    ParseError { line: usize, message: String },
    /// Unknown configuration key.
    UnknownKey { line: usize, key: String },
    /// Invalid value for a configuration option.
    InvalidValue { line: usize, key: String, value: String },
}

impl std::fmt::Display for PgFormatterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PgFormatterError::IoError(msg) => write!(f, "IO error: {}", msg),
            PgFormatterError::ParseError { line, message } => {
                write!(f, "Parse error on line {}: {}", line, message)
            }
            PgFormatterError::UnknownKey { line, key } => {
                write!(f, "Unknown key '{}' on line {}", key, line)
            }
            PgFormatterError::InvalidValue { line, key, value } => {
                write!(f, "Invalid value '{}' for key '{}' on line {}", value, key, line)
            }
        }
    }
}

impl std::error::Error for PgFormatterError {}

/// pgFormatter-compatible configuration.
///
/// This struct provides all configuration options available in pgFormatter,
/// with defaults matching pgFormatter's defaults.
#[derive(Debug, Clone)]
pub struct PgFormatterConfig {
    // === Case Options ===
    /// Reserved keyword case (default: Upper, pgFormatter -u)
    pub keyword_case: CaseOption,
    /// Function name case (default: Unchanged, pgFormatter -f)
    pub function_case: CaseOption,
    /// Data type case (default: Lower, pgFormatter -U)
    pub type_case: CaseOption,

    // === Indentation ===
    /// Number of spaces per indent level (default: 4, pgFormatter -s)
    pub spaces: usize,
    /// Use tabs instead of spaces (default: false, pgFormatter -T)
    pub use_tabs: bool,

    // === Comma Placement ===
    /// Place comma at the beginning of the line (default: false, pgFormatter -b)
    pub comma_start: bool,
    /// Place comma at the end of the line (default: true, pgFormatter -e)
    pub comma_end: bool,
    /// Add a newline after each comma (default: false, pgFormatter -B)
    pub comma_break: bool,

    // === Line Wrapping ===
    /// Wrap lines at N characters (default: None, pgFormatter -w)
    pub wrap_limit: Option<usize>,
    /// Wrap lists after N items (default: None, pgFormatter -W)
    pub wrap_after: Option<usize>,
    /// Apply wrap limit to comments (default: false, pgFormatter -C)
    pub wrap_comment: bool,

    // === Content Handling ===
    /// Remove all comments (default: false, pgFormatter -n)
    pub no_comment: bool,
    /// Keep empty lines from the original SQL (default: false, pgFormatter -k)
    pub keep_newline: bool,
    /// Do not add trailing newline at end of output (default: false, pgFormatter -L)
    pub no_extra_line: bool,
    /// Add a newline between statements (default: false, pgFormatter -g)
    pub no_grouping: bool,

    // === Special Options ===
    /// Do not add space before function parentheses (default: false)
    pub no_space_function: bool,
    /// Keep redundant parentheses (default: false)
    pub redundant_parenthesis: bool,
    /// Regex pattern to protect from formatting (default: None, pgFormatter -p)
    pub placeholder: Option<String>,

    // === Extra Options (extensions to pgFormatter) ===
    /// Maximum line width for general formatting (default: 80)
    pub max_width: usize,
    /// Align keywords in a "river" style (default: false)
    pub river_alignment: bool,
}

impl Default for PgFormatterConfig {
    fn default() -> Self {
        Self {
            // Case - match pgFormatter defaults
            keyword_case: CaseOption::Upper,      // -u 2
            function_case: CaseOption::Unchanged, // -f 0
            type_case: CaseOption::Lower,         // -U 1

            // Indentation
            spaces: 4,       // -s 4
            use_tabs: false, // -T

            // Comma
            comma_start: false, // -b
            comma_end: true,    // -e
            comma_break: false, // -B

            // Wrapping
            wrap_limit: None,    // -w
            wrap_after: None,    // -W
            wrap_comment: false, // -C

            // Content
            no_comment: false,    // -n
            keep_newline: false,  // -k
            no_extra_line: false, // -L
            no_grouping: false,   // -g

            // Special
            no_space_function: false,
            redundant_parenthesis: false,
            placeholder: None,

            // Extra options
            max_width: 80,
            river_alignment: false,
        }
    }
}

impl PgFormatterConfig {
    /// Creates a new pgFormatter configuration with default values.
    pub fn new() -> Self {
        Self::default()
    }

    /// Loads configuration from a `.pg_format` file.
    ///
    /// The file format is simple key = value pairs, one per line.
    /// Comments start with `#` and empty lines are ignored.
    ///
    /// # Example file format
    ///
    /// ```text
    /// # pgFormatter configuration
    /// keyword-case = 2
    /// spaces = 4
    /// comma-start = 1
    /// ```
    ///
    /// Requires the `config-file` feature.
    #[cfg(feature = "config-file")]
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, PgFormatterError> {
        let content = fs::read_to_string(path.as_ref())
            .map_err(|e| PgFormatterError::IoError(e.to_string()))?;
        Self::from_str(&content)
    }

    /// Parses configuration from a string.
    pub fn from_str(content: &str) -> Result<Self, PgFormatterError> {
        let mut config = Self::default();

        for (line_num, line) in content.lines().enumerate() {
            let line_num = line_num + 1; // 1-indexed
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Parse key = value
            let parts: Vec<&str> = line.splitn(2, '=').collect();
            if parts.len() != 2 {
                return Err(PgFormatterError::ParseError {
                    line: line_num,
                    message: "Expected 'key = value' format".to_string(),
                });
            }

            let key = parts[0].trim();
            let value = parts[1].trim();

            config.set_option(line_num, key, value)?;
        }

        Ok(config)
    }

    /// Sets a configuration option by name.
    fn set_option(&mut self, line: usize, key: &str, value: &str) -> Result<(), PgFormatterError> {
        // Normalize key (support both hyphen and underscore)
        let key_normalized = key.replace('-', "_").to_lowercase();

        match key_normalized.as_str() {
            // Case options
            "keyword_case" | "ufunctions" | "u" => {
                self.keyword_case = parse_case_option(line, key, value)?;
            }
            "function_case" | "f" => {
                self.function_case = parse_case_option(line, key, value)?;
            }
            "type_case" | "type" | "u_type" => {
                self.type_case = parse_case_option(line, key, value)?;
            }

            // Indentation
            "spaces" | "s" => {
                self.spaces = parse_usize(line, key, value)?;
            }
            "use_tabs" | "tabs" | "t" => {
                self.use_tabs = parse_bool(line, key, value)?;
            }

            // Comma
            "comma_start" | "b" => {
                self.comma_start = parse_bool(line, key, value)?;
            }
            "comma_end" | "e" => {
                self.comma_end = parse_bool(line, key, value)?;
            }
            "comma_break" | "comma" => {
                self.comma_break = parse_bool(line, key, value)?;
            }

            // Wrapping
            "wrap_limit" | "wrap" | "w" => {
                let v = parse_usize(line, key, value)?;
                self.wrap_limit = if v == 0 { None } else { Some(v) };
            }
            "wrap_after" | "wrap_after_num" => {
                let v = parse_usize(line, key, value)?;
                self.wrap_after = if v == 0 { None } else { Some(v) };
            }
            "wrap_comment" | "c" => {
                self.wrap_comment = parse_bool(line, key, value)?;
            }

            // Content
            "no_comment" | "nocomment" | "n" => {
                self.no_comment = parse_bool(line, key, value)?;
            }
            "keep_newline" | "keepnewline" | "k" => {
                self.keep_newline = parse_bool(line, key, value)?;
            }
            "no_extra_line" | "noextraline" | "l" => {
                self.no_extra_line = parse_bool(line, key, value)?;
            }
            "no_grouping" | "nogrouping" | "g" => {
                self.no_grouping = parse_bool(line, key, value)?;
            }

            // Special
            "no_space_function" | "nospacefunc" => {
                self.no_space_function = parse_bool(line, key, value)?;
            }
            "redundant_parenthesis" | "reduntantparen" => {
                self.redundant_parenthesis = parse_bool(line, key, value)?;
            }
            "placeholder" | "p" => {
                self.placeholder = if value.is_empty() {
                    None
                } else {
                    Some(value.to_string())
                };
            }

            // Extra options
            "max_width" | "maxwidth" => {
                self.max_width = parse_usize(line, key, value)?;
            }
            "river_alignment" | "river" => {
                self.river_alignment = parse_bool(line, key, value)?;
            }

            _ => {
                return Err(PgFormatterError::UnknownKey {
                    line,
                    key: key.to_string(),
                });
            }
        }

        Ok(())
    }

    /// Discovers and loads a `.pg_format` configuration file.
    ///
    /// Searches in the following order:
    /// 1. `./.pg_format` (current directory)
    /// 2. `~/.pg_format` (home directory)
    /// 3. `$XDG_CONFIG_HOME/pg_format/pg_format.conf`
    ///
    /// Returns `None` if no configuration file is found.
    ///
    /// Requires the `config-file` feature.
    #[cfg(feature = "config-file")]
    pub fn discover() -> Option<Self> {
        // 1. Current directory
        let cwd_config = PathBuf::from(".pg_format");
        if cwd_config.exists() {
            if let Ok(config) = Self::from_file(&cwd_config) {
                return Some(config);
            }
        }

        // 2. Home directory
        if let Some(home) = dirs::home_dir() {
            let home_config = home.join(".pg_format");
            if home_config.exists() {
                if let Ok(config) = Self::from_file(&home_config) {
                    return Some(config);
                }
            }
        }

        // 3. XDG config directory
        if let Some(config_dir) = dirs::config_dir() {
            let xdg_config = config_dir.join("pg_format").join("pg_format.conf");
            if xdg_config.exists() {
                if let Ok(config) = Self::from_file(&xdg_config) {
                    return Some(config);
                }
            }
        }

        None
    }

    /// Converts this pgFormatter config to a FormatConfig for use with the formatter.
    pub fn to_format_config(&self) -> crate::config::FormatConfig {
        use crate::config::{CommaStyle, IdentifierCase, IndentStyle, KeywordCase};

        let keyword_case = match self.keyword_case {
            CaseOption::Upper => KeywordCase::Upper,
            CaseOption::Lower => KeywordCase::Lower,
            CaseOption::Unchanged | CaseOption::Capitalize => KeywordCase::Preserve,
        };

        let indent = if self.use_tabs {
            IndentStyle::Tabs
        } else {
            IndentStyle::Spaces(self.spaces)
        };

        let comma_style = if self.comma_start {
            CommaStyle::Leading
        } else {
            CommaStyle::Trailing
        };

        crate::config::FormatConfig {
            keyword_case,
            identifier_case: IdentifierCase::Preserve, // pgFormatter doesn't transform identifiers
            indent,
            max_width: self.wrap_limit.unwrap_or(self.max_width),
            river_alignment: self.river_alignment,
            newline_before_logical: true,
            spaces_around_operators: true,
            comma_style,
            parentheses_spacing: false,
            align_select_items: false,
            river_width: 10,
        }
    }

    // === Builder methods ===

    /// Sets the keyword case.
    pub fn with_keyword_case(mut self, case: CaseOption) -> Self {
        self.keyword_case = case;
        self
    }

    /// Sets the function case.
    pub fn with_function_case(mut self, case: CaseOption) -> Self {
        self.function_case = case;
        self
    }

    /// Sets the type case.
    pub fn with_type_case(mut self, case: CaseOption) -> Self {
        self.type_case = case;
        self
    }

    /// Sets the number of spaces for indentation.
    pub fn with_spaces(mut self, spaces: usize) -> Self {
        self.spaces = spaces;
        self
    }

    /// Sets whether to use tabs for indentation.
    pub fn with_use_tabs(mut self, use_tabs: bool) -> Self {
        self.use_tabs = use_tabs;
        self
    }

    /// Sets comma placement at the start of lines.
    pub fn with_comma_start(mut self, comma_start: bool) -> Self {
        self.comma_start = comma_start;
        self.comma_end = !comma_start;
        self
    }

    /// Sets the wrap limit.
    pub fn with_wrap_limit(mut self, limit: Option<usize>) -> Self {
        self.wrap_limit = limit;
        self
    }

    /// Sets whether to keep original empty lines.
    pub fn with_keep_newline(mut self, keep: bool) -> Self {
        self.keep_newline = keep;
        self
    }

    /// Sets whether to remove comments.
    pub fn with_no_comment(mut self, no_comment: bool) -> Self {
        self.no_comment = no_comment;
        self
    }
}

/// Parses a case option value (0-3).
fn parse_case_option(line: usize, key: &str, value: &str) -> Result<CaseOption, PgFormatterError> {
    value
        .parse::<u8>()
        .ok()
        .and_then(CaseOption::from_value)
        .ok_or_else(|| PgFormatterError::InvalidValue {
            line,
            key: key.to_string(),
            value: value.to_string(),
        })
}

/// Parses a boolean value (0/1, true/false, yes/no).
fn parse_bool(line: usize, key: &str, value: &str) -> Result<bool, PgFormatterError> {
    match value.to_lowercase().as_str() {
        "1" | "true" | "yes" | "on" => Ok(true),
        "0" | "false" | "no" | "off" => Ok(false),
        _ => Err(PgFormatterError::InvalidValue {
            line,
            key: key.to_string(),
            value: value.to_string(),
        }),
    }
}

/// Parses a usize value.
fn parse_usize(line: usize, key: &str, value: &str) -> Result<usize, PgFormatterError> {
    value.parse().map_err(|_| PgFormatterError::InvalidValue {
        line,
        key: key.to_string(),
        value: value.to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = PgFormatterConfig::default();
        assert_eq!(config.keyword_case, CaseOption::Upper);
        assert_eq!(config.function_case, CaseOption::Unchanged);
        assert_eq!(config.type_case, CaseOption::Lower);
        assert_eq!(config.spaces, 4);
        assert!(!config.use_tabs);
        assert!(!config.comma_start);
        assert!(config.comma_end);
    }

    #[test]
    fn test_case_option_apply() {
        assert_eq!(CaseOption::Upper.apply("select"), "SELECT");
        assert_eq!(CaseOption::Lower.apply("SELECT"), "select");
        assert_eq!(CaseOption::Unchanged.apply("Select"), "Select");
        assert_eq!(CaseOption::Capitalize.apply("hello world"), "Hello World");
    }

    #[test]
    fn test_case_option_from_value() {
        assert_eq!(CaseOption::from_value(0), Some(CaseOption::Unchanged));
        assert_eq!(CaseOption::from_value(1), Some(CaseOption::Lower));
        assert_eq!(CaseOption::from_value(2), Some(CaseOption::Upper));
        assert_eq!(CaseOption::from_value(3), Some(CaseOption::Capitalize));
        assert_eq!(CaseOption::from_value(4), None);
    }

    #[test]
    fn test_parse_config_string() {
        let content = r#"
# This is a comment
keyword-case = 2
spaces = 2
comma-start = 1
wrap-limit = 100
"#;
        let config = PgFormatterConfig::from_str(content).unwrap();
        assert_eq!(config.keyword_case, CaseOption::Upper);
        assert_eq!(config.spaces, 2);
        assert!(config.comma_start);
        assert_eq!(config.wrap_limit, Some(100));
    }

    #[test]
    fn test_parse_bool_values() {
        assert!(parse_bool(1, "test", "1").unwrap());
        assert!(parse_bool(1, "test", "true").unwrap());
        assert!(parse_bool(1, "test", "yes").unwrap());
        assert!(parse_bool(1, "test", "on").unwrap());
        assert!(!parse_bool(1, "test", "0").unwrap());
        assert!(!parse_bool(1, "test", "false").unwrap());
        assert!(!parse_bool(1, "test", "no").unwrap());
        assert!(!parse_bool(1, "test", "off").unwrap());
    }

    #[test]
    fn test_to_format_config() {
        let pg_config = PgFormatterConfig::default().with_comma_start(true);
        let format_config = pg_config.to_format_config();
        assert_eq!(format_config.comma_style, crate::config::CommaStyle::Leading);
    }

    #[test]
    fn test_builder_pattern() {
        let config = PgFormatterConfig::new()
            .with_keyword_case(CaseOption::Lower)
            .with_spaces(2)
            .with_use_tabs(false)
            .with_comma_start(true);

        assert_eq!(config.keyword_case, CaseOption::Lower);
        assert_eq!(config.spaces, 2);
        assert!(!config.use_tabs);
        assert!(config.comma_start);
        assert!(!config.comma_end);
    }

    #[test]
    fn test_unknown_key_error() {
        let content = "unknown_option = 1";
        let result = PgFormatterConfig::from_str(content);
        assert!(matches!(result, Err(PgFormatterError::UnknownKey { .. })));
    }

    #[test]
    fn test_invalid_value_error() {
        let content = "spaces = not_a_number";
        let result = PgFormatterConfig::from_str(content);
        assert!(matches!(result, Err(PgFormatterError::InvalidValue { .. })));
    }
}
