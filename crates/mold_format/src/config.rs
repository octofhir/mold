//! Format configuration for SQL formatting.

use crate::pg_formatter::PgFormatterConfig;

/// Format style preset.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FormatStyle {
    /// sqlstyle.guide conventions (default)
    #[default]
    SqlStyle,
    /// pgFormatter-compatible style
    PgFormatter,
    /// Compact style with minimal whitespace
    Compact,
}

impl FormatStyle {
    /// Creates a FormatConfig for this style.
    pub fn to_config(self) -> FormatConfig {
        match self {
            FormatStyle::SqlStyle => FormatConfig::sqlstyle(),
            FormatStyle::PgFormatter => PgFormatterConfig::default().to_format_config(),
            FormatStyle::Compact => FormatConfig::compact(),
        }
    }
}

/// Case transformation for SQL keywords.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum KeywordCase {
    /// UPPERCASE keywords (sqlstyle.guide default)
    #[default]
    Upper,
    /// lowercase keywords
    Lower,
    /// Keep original case
    Preserve,
}

/// Case transformation for identifiers.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum IdentifierCase {
    /// lowercase identifiers
    #[default]
    Lower,
    /// Keep original case
    Preserve,
}

/// Indentation style.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndentStyle {
    /// Use spaces for indentation
    Spaces(usize),
    /// Use tabs for indentation
    Tabs,
}

impl Default for IndentStyle {
    fn default() -> Self {
        IndentStyle::Spaces(4)
    }
}

/// Comma placement style.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CommaStyle {
    /// Commas at the end of lines (sqlstyle.guide default)
    #[default]
    Trailing,
    /// Commas at the beginning of lines (some team preferences)
    Leading,
}

/// Configuration for SQL formatting.
#[must_use]
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// How to transform keyword case
    pub keyword_case: KeywordCase,
    /// How to transform identifier case
    pub identifier_case: IdentifierCase,
    /// Indentation style
    pub indent: IndentStyle,
    /// Maximum line width before wrapping
    pub max_width: usize,
    /// Use river alignment (right-align keywords)
    pub river_alignment: bool,
    /// Insert newline before AND/OR in WHERE clauses
    pub newline_before_logical: bool,
    /// Add spaces around operators (=, <, >, etc.)
    pub spaces_around_operators: bool,
    /// Comma placement style
    pub comma_style: CommaStyle,
    /// Add space inside parentheses
    pub parentheses_spacing: bool,
    /// Align items in SELECT list
    pub align_select_items: bool,
    /// Number of spaces for river width (for river alignment)
    pub river_width: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self::sqlstyle()
    }
}

impl FormatConfig {
    /// Creates a new configuration with sqlstyle.guide defaults.
    ///
    /// This follows the conventions from https://www.sqlstyle.guide/
    pub fn sqlstyle() -> Self {
        Self {
            keyword_case: KeywordCase::Upper,
            identifier_case: IdentifierCase::Lower,
            indent: IndentStyle::Spaces(4),
            max_width: 88,
            river_alignment: true,
            newline_before_logical: true,
            spaces_around_operators: true,
            comma_style: CommaStyle::Trailing,
            parentheses_spacing: false,
            align_select_items: true,
            river_width: 10, // Enough for "RETURNING " (longest common keyword)
        }
    }

    /// Creates a compact configuration with minimal whitespace.
    pub fn compact() -> Self {
        Self {
            keyword_case: KeywordCase::Upper,
            identifier_case: IdentifierCase::Preserve,
            indent: IndentStyle::Spaces(2),
            max_width: 120,
            river_alignment: false,
            newline_before_logical: false,
            spaces_around_operators: true,
            comma_style: CommaStyle::Trailing,
            parentheses_spacing: false,
            align_select_items: false,
            river_width: 0,
        }
    }

    /// Builder method for keyword case.
    pub fn with_keyword_case(mut self, case: KeywordCase) -> Self {
        self.keyword_case = case;
        self
    }

    /// Builder method for identifier case.
    pub fn with_identifier_case(mut self, case: IdentifierCase) -> Self {
        self.identifier_case = case;
        self
    }

    /// Builder method for indent style.
    pub fn with_indent(mut self, indent: IndentStyle) -> Self {
        self.indent = indent;
        self
    }

    /// Builder method for max width.
    pub fn with_max_width(mut self, width: usize) -> Self {
        self.max_width = width;
        self
    }

    /// Builder method for river alignment.
    pub fn with_river_alignment(mut self, enabled: bool) -> Self {
        self.river_alignment = enabled;
        self
    }

    /// Builder method for newline before logical operators.
    pub fn with_newline_before_logical(mut self, enabled: bool) -> Self {
        self.newline_before_logical = enabled;
        self
    }

    /// Builder method for spaces around operators.
    pub fn with_spaces_around_operators(mut self, enabled: bool) -> Self {
        self.spaces_around_operators = enabled;
        self
    }

    /// Builder method for comma style.
    pub fn with_comma_style(mut self, style: CommaStyle) -> Self {
        self.comma_style = style;
        self
    }

    /// Builder method for parentheses spacing.
    pub fn with_parentheses_spacing(mut self, enabled: bool) -> Self {
        self.parentheses_spacing = enabled;
        self
    }

    /// Returns the indent string for one level.
    pub fn indent_str(&self) -> String {
        match self.indent {
            IndentStyle::Spaces(n) => " ".repeat(n),
            IndentStyle::Tabs => "\t".to_string(),
        }
    }

    /// Returns the indent size in characters.
    pub fn indent_size(&self) -> usize {
        match self.indent {
            IndentStyle::Spaces(n) => n,
            IndentStyle::Tabs => 4, // Assume tab width of 4
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sqlstyle_defaults() {
        let config = FormatConfig::sqlstyle();
        assert_eq!(config.keyword_case, KeywordCase::Upper);
        assert!(config.river_alignment);
        assert_eq!(config.comma_style, CommaStyle::Trailing);
    }

    #[test]
    fn test_compact_config() {
        let config = FormatConfig::compact();
        assert!(!config.river_alignment);
        assert_eq!(config.comma_style, CommaStyle::Trailing);
    }

    #[test]
    fn test_builder_pattern() {
        let config = FormatConfig::sqlstyle()
            .with_max_width(100)
            .with_river_alignment(false);

        assert_eq!(config.max_width, 100);
        assert!(!config.river_alignment);
    }

    #[test]
    fn test_indent_str() {
        let config = FormatConfig::sqlstyle();
        assert_eq!(config.indent_str(), "    ");

        let config = FormatConfig::compact();
        assert_eq!(config.indent_str(), "  ");
    }
}
