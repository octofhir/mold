//! Serde-facing formatter settings.
//!
//! The on-disk schema is intentionally decoupled from `mold_format`'s internal
//! [`FormatConfig`]. A user selects a base `style` preset and then overrides
//! individual knobs; [`FormatSettings::to_format_config`] resolves the two into
//! the concrete config consumed by the formatter.

use mold_format::{
    CaseOption, CommaStyle as FmtCommaStyle, FormatConfig, FormatStyle as FmtStyle,
    IdentifierCase as FmtIdentCase, IndentStyle as FmtIndent, KeywordCase as FmtKeywordCase,
    PgFormatterConfig,
};
use serde::{Deserialize, Serialize};

/// Base formatter preset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum StylePreset {
    /// sqlstyle.guide conventions with river alignment (default).
    #[default]
    Sqlstyle,
    /// pgFormatter-compatible output.
    Pgformatter,
    /// Compact, minimal-whitespace output.
    Compact,
}

impl From<StylePreset> for FmtStyle {
    fn from(p: StylePreset) -> Self {
        match p {
            StylePreset::Sqlstyle => FmtStyle::SqlStyle,
            StylePreset::Pgformatter => FmtStyle::PgFormatter,
            StylePreset::Compact => FmtStyle::Compact,
        }
    }
}

/// Keyword case policy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum KeywordCase {
    Upper,
    Lower,
    Preserve,
}

impl From<KeywordCase> for FmtKeywordCase {
    fn from(c: KeywordCase) -> Self {
        match c {
            KeywordCase::Upper => FmtKeywordCase::Upper,
            KeywordCase::Lower => FmtKeywordCase::Lower,
            KeywordCase::Preserve => FmtKeywordCase::Preserve,
        }
    }
}

/// Identifier case policy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum IdentifierCase {
    Lower,
    Preserve,
}

impl From<IdentifierCase> for FmtIdentCase {
    fn from(c: IdentifierCase) -> Self {
        match c {
            IdentifierCase::Lower => FmtIdentCase::Lower,
            IdentifierCase::Preserve => FmtIdentCase::Preserve,
        }
    }
}

/// Indentation unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum IndentUnit {
    #[default]
    Space,
    Tab,
}

/// Comma placement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum CommaStyle {
    Trailing,
    Leading,
}

impl From<CommaStyle> for FmtCommaStyle {
    fn from(c: CommaStyle) -> Self {
        match c {
            CommaStyle::Trailing => FmtCommaStyle::Trailing,
            CommaStyle::Leading => FmtCommaStyle::Leading,
        }
    }
}

/// Case policy for pgFormatter-specific knobs (supports capitalize/unchanged
/// that the sqlstyle engine does not).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum CasePolicy {
    Upper,
    Lower,
    Preserve,
    Capitalize,
}

impl From<CasePolicy> for CaseOption {
    fn from(c: CasePolicy) -> Self {
        match c {
            CasePolicy::Upper => CaseOption::Upper,
            CasePolicy::Lower => CaseOption::Lower,
            CasePolicy::Preserve => CaseOption::Unchanged,
            CasePolicy::Capitalize => CaseOption::Capitalize,
        }
    }
}

fn keyword_case_to_pg(c: KeywordCase) -> CaseOption {
    match c {
        KeywordCase::Upper => CaseOption::Upper,
        KeywordCase::Lower => CaseOption::Lower,
        KeywordCase::Preserve => CaseOption::Unchanged,
    }
}

/// pgFormatter-only knobs, used when `style = "pgformatter"`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", default)]
pub struct PgFormatSettings {
    /// Case of function names.
    pub function_case: Option<CasePolicy>,
    /// Case of type names.
    pub type_case: Option<CasePolicy>,
    /// Strip comments from the output.
    pub no_comment: bool,
}

/// Formatter settings: a base preset plus optional per-knob overrides.
///
/// Every override is `Option`: when `None`, the value from the chosen `style`
/// preset is used. This keeps `mold.toml` terse — most users set only `style`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", default)]
pub struct FormatSettings {
    /// Base preset that supplies defaults for any unset field.
    pub style: StylePreset,
    pub keyword_case: Option<KeywordCase>,
    pub identifier_case: Option<IdentifierCase>,
    pub indent_unit: Option<IndentUnit>,
    pub indent_width: Option<usize>,
    pub max_width: Option<usize>,
    pub river_alignment: Option<bool>,
    pub newline_before_logical: Option<bool>,
    pub spaces_around_operators: Option<bool>,
    pub comma_style: Option<CommaStyle>,
    pub parentheses_spacing: Option<bool>,
    pub align_select_items: Option<bool>,
    /// pgFormatter-specific knobs (only consulted when `style = "pgformatter"`).
    pub pgformatter: PgFormatSettings,
}

impl FormatSettings {
    /// Resolves these settings against the base preset into a concrete
    /// [`FormatConfig`] for the formatter.
    pub fn to_format_config(&self) -> FormatConfig {
        let mut cfg = FmtStyle::from(self.style).to_config();

        if let Some(v) = self.keyword_case {
            cfg.keyword_case = v.into();
        }
        if let Some(v) = self.identifier_case {
            cfg.identifier_case = v.into();
        }
        match (self.indent_unit, self.indent_width) {
            (Some(IndentUnit::Tab), _) => cfg.indent = FmtIndent::Tabs,
            (Some(IndentUnit::Space), w) => {
                cfg.indent = FmtIndent::Spaces(w.unwrap_or(4));
            }
            (None, Some(w)) => cfg.indent = FmtIndent::Spaces(w),
            (None, None) => {}
        }
        if let Some(v) = self.max_width {
            cfg.max_width = v;
        }
        if let Some(v) = self.river_alignment {
            cfg.river_alignment = v;
        }
        if let Some(v) = self.newline_before_logical {
            cfg.newline_before_logical = v;
        }
        if let Some(v) = self.spaces_around_operators {
            cfg.spaces_around_operators = v;
        }
        if let Some(v) = self.comma_style {
            cfg.comma_style = v.into();
        }
        if let Some(v) = self.parentheses_spacing {
            cfg.parentheses_spacing = v;
        }
        if let Some(v) = self.align_select_items {
            cfg.align_select_items = v;
        }
        cfg
    }

    /// Resolves these settings into a [`PgFormatterConfig`] for the pgFormatter
    /// engine. Shared knobs (case, indent, commas) are mapped across; the
    /// `pgformatter` subsection supplies engine-specific options.
    #[must_use]
    pub fn to_pg_config(&self) -> PgFormatterConfig {
        let mut cfg = PgFormatterConfig::default();

        if let Some(c) = self.keyword_case {
            cfg.keyword_case = keyword_case_to_pg(c);
        }
        if let Some(c) = self.pgformatter.function_case {
            cfg.function_case = c.into();
        }
        if let Some(c) = self.pgformatter.type_case {
            cfg.type_case = c.into();
        }
        match self.indent_unit {
            Some(IndentUnit::Tab) => cfg.use_tabs = true,
            Some(IndentUnit::Space) | None => {
                if let Some(w) = self.indent_width {
                    cfg.spaces = w;
                }
            }
        }
        match self.comma_style {
            Some(CommaStyle::Leading) => {
                cfg.comma_start = true;
                cfg.comma_end = false;
            }
            Some(CommaStyle::Trailing) => {
                cfg.comma_start = false;
                cfg.comma_end = true;
            }
            None => {}
        }
        cfg.no_comment = self.pgformatter.no_comment;
        cfg
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_is_sqlstyle() {
        let cfg = FormatSettings::default().to_format_config();
        assert_eq!(cfg.keyword_case, FmtKeywordCase::Upper);
        assert!(cfg.river_alignment);
    }

    #[test]
    fn overrides_apply_on_top_of_preset() {
        let s = FormatSettings {
            style: StylePreset::Sqlstyle,
            keyword_case: Some(KeywordCase::Lower),
            indent_unit: Some(IndentUnit::Space),
            indent_width: Some(2),
            ..Default::default()
        };
        let cfg = s.to_format_config();
        assert_eq!(cfg.keyword_case, FmtKeywordCase::Lower);
        assert_eq!(cfg.indent, FmtIndent::Spaces(2));
        // Untouched knob keeps preset value.
        assert!(cfg.river_alignment);
    }

    #[test]
    fn tab_indent_ignores_width() {
        let s = FormatSettings {
            indent_unit: Some(IndentUnit::Tab),
            indent_width: Some(8),
            ..Default::default()
        };
        assert_eq!(s.to_format_config().indent, FmtIndent::Tabs);
    }
}
