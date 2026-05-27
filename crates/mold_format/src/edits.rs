//! Edit-producing formatting.
//!
//! Instead of returning a fully rebuilt string, these helpers diff the source
//! against the formatted output and return the minimal replacement(s). Minimal
//! edits keep the editor cursor stable, compose with other edits (e.g. lint
//! fixes), and enable range formatting.

use text_size::{TextRange, TextSize};

use crate::config::FormatConfig;

/// A single text replacement against the original source offsets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextEdit {
    pub range: TextRange,
    pub new_text: String,
}

/// Formats `source` and returns the minimal edit(s) to apply.
///
/// Returns an empty vector when the source is already formatted.
#[must_use]
pub fn format_edits(source: &str, config: &FormatConfig) -> Vec<TextEdit> {
    let formatted = crate::format::format(source, config);
    minimal_diff(source, &formatted)
}

/// Like [`format_edits`], but only returns edits intersecting `range`.
///
/// The whole document is still formatted (the formatter is whole-statement);
/// edits outside the requested range are dropped so callers can format a
/// selection without rewriting untouched regions.
#[must_use]
pub fn format_range(source: &str, config: &FormatConfig, range: TextRange) -> Vec<TextEdit> {
    format_edits(source, config)
        .into_iter()
        .filter(|e| e.range.start() <= range.end() && range.start() <= e.range.end())
        .collect()
}

/// Computes a single minimal replacement by trimming the common prefix and
/// suffix. Offsets are snapped to UTF-8 char boundaries.
fn minimal_diff(before: &str, after: &str) -> Vec<TextEdit> {
    if before == after {
        return Vec::new();
    }

    let prefix = common_prefix(before, after);
    let suffix = common_suffix(&before[prefix..], &after[prefix..]);

    let before_end = before.len() - suffix;
    let after_end = after.len() - suffix;

    vec![TextEdit {
        range: TextRange::new(
            TextSize::from(prefix as u32),
            TextSize::from(before_end as u32),
        ),
        new_text: after[prefix..after_end].to_string(),
    }]
}

/// Length of the common byte prefix, snapped down to a char boundary in `a`.
fn common_prefix(a: &str, b: &str) -> usize {
    let max = a.len().min(b.len());
    let mut i = 0;
    while i < max && a.as_bytes()[i] == b.as_bytes()[i] {
        i += 1;
    }
    while i > 0 && !a.is_char_boundary(i) {
        i -= 1;
    }
    i
}

/// Length of the common byte suffix, snapped to a char boundary in `a`.
fn common_suffix(a: &str, b: &str) -> usize {
    let max = a.len().min(b.len());
    let mut i = 0;
    while i < max && a.as_bytes()[a.len() - 1 - i] == b.as_bytes()[b.len() - 1 - i] {
        i += 1;
    }
    while i > 0 && !a.is_char_boundary(a.len() - i) {
        i -= 1;
    }
    i
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn already_formatted_yields_no_edits() {
        let cfg = FormatConfig::sqlstyle();
        let formatted = crate::format::format("select 1", &cfg);
        assert!(format_edits(&formatted, &cfg).is_empty());
    }

    #[test]
    fn edit_applies_to_produce_formatted_output() {
        let cfg = FormatConfig::sqlstyle();
        let src = "select id,name from users";
        let edits = format_edits(src, &cfg);
        assert_eq!(edits.len(), 1);

        // Applying the edit reproduces the full formatted string.
        let edit = &edits[0];
        let start = u32::from(edit.range.start()) as usize;
        let end = u32::from(edit.range.end()) as usize;
        let mut out = src.to_string();
        out.replace_range(start..end, &edit.new_text);
        assert_eq!(out, crate::format::format(src, &cfg));
    }

    #[test]
    fn range_outside_change_yields_nothing() {
        let cfg = FormatConfig::sqlstyle();
        let src = "select id,name from users";
        let full = format_edits(src, &cfg);
        let change = full[0].range;
        // A zero-length range far past the edit should be filtered out.
        let far = TextRange::empty(TextSize::from(u32::from(change.end()) + 5));
        assert!(format_range(src, &cfg, far).is_empty());
    }
}
