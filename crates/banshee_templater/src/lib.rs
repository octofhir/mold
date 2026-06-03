//! Placeholder templating.
//!
//! App SQL often carries placeholders that are not valid PostgreSQL on their
//! own — `:name`, `?`, `%(name)s`. This crate rewrites them into a harmless
//! literal so the rest of the statement parses and lints, while keeping a slice
//! map that translates rendered offsets back to the original source. Diagnostic
//! and fix ranges can then be reported against the user's file.
//!
//! Postgres native parameters (`$1`, `$name`) are already valid SQL and are
//! left untouched. Placeholders inside string literals, dollar-quoted strings
//! and comments are ignored.
//!
//! ```
//! use banshee_templater::{PlaceholderStyle, render};
//! let t = render("SELECT id FROM t WHERE name = :name", PlaceholderStyle::Colon);
//! assert_eq!(t.text(), "SELECT id FROM t WHERE name = 1");
//! ```

use serde::{Deserialize, Serialize};
use text_size::{TextRange, TextSize};

/// The placeholder syntax used by the source. Selected via configuration; when
/// unset, no templating runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum PlaceholderStyle {
    /// `:name` or `:1` (psql / many ORMs). `::` casts are left alone.
    Colon,
    /// `?` positional (JDBC/ODBC). Note: clashes with Postgres JSONB `?`.
    QuestionMark,
    /// `%s` and `%(name)s` (Python DB-API).
    Percent,
}

/// The text substituted for each placeholder. A bare integer literal parses
/// wherever a value is expected (`WHERE`, `VALUES`, `LIMIT`, …).
const REPLACEMENT: &str = "1";

/// One contiguous span of the source, mapped to its rendered position.
#[derive(Debug, Clone)]
struct Span {
    src: std::ops::Range<usize>,
    out: std::ops::Range<usize>,
    /// True when this span is a substituted placeholder (source and rendered
    /// text differ and offsets inside it cannot be mapped one-to-one).
    placeholder: bool,
}

/// A rendered SQL string plus the map back to the original source.
#[derive(Debug, Clone)]
pub struct Templated {
    text: String,
    spans: Vec<Span>,
}

impl Templated {
    /// The rendered, parseable SQL.
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Translates a range in the rendered text back to the original source.
    pub fn map_range(&self, rendered: TextRange) -> TextRange {
        let start = self.map_offset(rendered.start().into(), false);
        let end = self.map_offset(rendered.end().into(), true);
        let (start, end) = (start.min(end), start.max(end));
        TextRange::new(TextSize::new(start as u32), TextSize::new(end as u32))
    }

    /// Whether a rendered range overlaps a substituted placeholder. Fixes that
    /// touch a placeholder cannot be applied to the source safely.
    pub fn touches_placeholder(&self, rendered: TextRange) -> bool {
        let s: usize = rendered.start().into();
        let e: usize = rendered.end().into();
        self.spans
            .iter()
            .any(|span| span.placeholder && span.out.start < e.max(s + 1) && s < span.out.end)
    }

    /// Maps a rendered byte offset to a source byte offset. `end_bias` decides
    /// which side of a placeholder a boundary offset snaps to.
    fn map_offset(&self, off: usize, end_bias: bool) -> usize {
        for span in &self.spans {
            if off < span.out.start {
                break;
            }
            if off < span.out.end || (off == span.out.end && span.out.start == span.out.end) {
                return if span.placeholder {
                    if end_bias {
                        span.src.end
                    } else {
                        span.src.start
                    }
                } else {
                    span.src.start + (off - span.out.start)
                };
            }
        }
        // Past the last span: end of source.
        self.spans.last().map(|s| s.src.end).unwrap_or(0)
    }
}

/// Renders `source`, substituting placeholders of `style`.
pub fn render(source: &str, style: PlaceholderStyle) -> Templated {
    let bytes = source.as_bytes();
    let mut spans: Vec<Span> = Vec::new();
    let mut out = String::with_capacity(source.len());
    // Start of the current run of verbatim (literal) text not yet flushed.
    let mut lit_start = 0usize;
    let mut i = 0usize;

    // Flush verbatim source `[lit_start, end)` into the output as a literal span.
    macro_rules! flush_literal {
        ($end:expr) => {{
            let end = $end;
            if end > lit_start {
                let out_start = out.len();
                out.push_str(&source[lit_start..end]);
                spans.push(Span {
                    src: lit_start..end,
                    out: out_start..out.len(),
                    placeholder: false,
                });
            }
        }};
    }

    while i < bytes.len() {
        // Skip over regions where placeholders must be ignored, advancing `i`.
        if let Some(skip_to) = skip_noncode(source, i) {
            i = skip_to;
            continue;
        }

        if let Some(len) = match_placeholder(source, i, style) {
            flush_literal!(i);
            let out_start = out.len();
            out.push_str(REPLACEMENT);
            spans.push(Span {
                src: i..i + len,
                out: out_start..out.len(),
                placeholder: true,
            });
            i += len;
            lit_start = i;
            continue;
        }

        // Advance by a full UTF-8 char so byte offsets stay on boundaries.
        i += utf8_len(bytes[i]);
    }
    flush_literal!(source.len());

    Templated { text: out, spans }
}

/// If `i` begins a string/comment/dollar-quote where placeholders must not be
/// substituted, returns the offset just past it; otherwise `None`.
fn skip_noncode(source: &str, i: usize) -> Option<usize> {
    let b = source.as_bytes();
    match b[i] {
        b'-' if b.get(i + 1) == Some(&b'-') => {
            // Line comment to end of line (newline kept as code).
            let mut j = i + 2;
            while j < b.len() && b[j] != b'\n' {
                j += 1;
            }
            Some(j)
        }
        b'/' if b.get(i + 1) == Some(&b'*') => {
            // Block comment, Postgres allows nesting.
            let mut j = i + 2;
            let mut depth = 1;
            while j < b.len() && depth > 0 {
                if b[j] == b'/' && b.get(j + 1) == Some(&b'*') {
                    depth += 1;
                    j += 2;
                } else if b[j] == b'*' && b.get(j + 1) == Some(&b'/') {
                    depth -= 1;
                    j += 2;
                } else {
                    j += 1;
                }
            }
            Some(j)
        }
        b'\'' => {
            // Single-quoted string; '' is an escaped quote.
            let mut j = i + 1;
            while j < b.len() {
                if b[j] == b'\'' {
                    if b.get(j + 1) == Some(&b'\'') {
                        j += 2;
                        continue;
                    }
                    return Some(j + 1);
                }
                j += 1;
            }
            Some(b.len())
        }
        b'$' => {
            // Dollar-quoted string: $tag$ ... $tag$ (tag may be empty).
            let tag_end = dollar_tag_end(b, i)?;
            let tag = &source[i..tag_end];
            let rest = &source[tag_end..];
            rest.find(tag).map(|pos| tag_end + pos + tag.len())
        }
        _ => None,
    }
}

/// If `b[i] == '$'` opens a dollar-quote tag `$...$`, returns the offset just
/// past the opening tag; otherwise `None`.
fn dollar_tag_end(b: &[u8], i: usize) -> Option<usize> {
    let mut j = i + 1;
    while j < b.len() && (b[j].is_ascii_alphanumeric() || b[j] == b'_') {
        j += 1;
    }
    if b.get(j) == Some(&b'$') {
        Some(j + 1)
    } else {
        None
    }
}

/// If a placeholder of `style` starts at `i`, returns its byte length.
fn match_placeholder(source: &str, i: usize, style: PlaceholderStyle) -> Option<usize> {
    let b = source.as_bytes();
    match style {
        PlaceholderStyle::QuestionMark => (b[i] == b'?').then_some(1),
        PlaceholderStyle::Colon => {
            if b[i] != b':' {
                return None;
            }
            // Leave `::` casts alone (either side being `:`).
            if b.get(i + 1) == Some(&b':') || (i > 0 && b[i - 1] == b':') {
                return None;
            }
            let mut j = i + 1;
            while j < b.len() && (b[j].is_ascii_alphanumeric() || b[j] == b'_') {
                j += 1;
            }
            (j > i + 1).then_some(j - i)
        }
        PlaceholderStyle::Percent => {
            if b[i] != b'%' {
                return None;
            }
            // `%(name)s`
            if b.get(i + 1) == Some(&b'(') {
                let mut j = i + 2;
                while j < b.len() && b[j] != b')' {
                    j += 1;
                }
                if b.get(j) == Some(&b')') && b.get(j + 1) == Some(&b's') {
                    return Some(j + 2 - i);
                }
                return None;
            }
            // `%s` / `%d`
            matches!(b.get(i + 1), Some(&b's') | Some(&b'd')).then_some(2)
        }
    }
}

/// Byte length of a UTF-8 sequence given its lead byte.
fn utf8_len(lead: u8) -> usize {
    match lead {
        0x00..=0x7f => 1,
        0xc0..=0xdf => 2,
        0xe0..=0xef => 3,
        _ => 4,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn src_of(rendered: &str, style: PlaceholderStyle, needle: &str) -> String {
        // Helper: render, find `needle` in rendered, map back, slice source.
        let t = render(rendered, style);
        let pos = t.text().find(needle).unwrap();
        let r = TextRange::new(
            TextSize::new(pos as u32),
            TextSize::new((pos + needle.len()) as u32),
        );
        let mapped = t.map_range(r);
        rendered[usize::from(mapped.start())..usize::from(mapped.end())].to_string()
    }

    #[test]
    fn colon_named() {
        let t = render(
            "SELECT * FROM t WHERE a = :id AND b = :name",
            PlaceholderStyle::Colon,
        );
        assert_eq!(t.text(), "SELECT * FROM t WHERE a = 1 AND b = 1");
    }

    #[test]
    fn colon_skips_cast() {
        let t = render("SELECT a::int FROM t", PlaceholderStyle::Colon);
        assert_eq!(t.text(), "SELECT a::int FROM t");
    }

    #[test]
    fn question_mark() {
        let t = render("SELECT * FROM t LIMIT ?", PlaceholderStyle::QuestionMark);
        assert_eq!(t.text(), "SELECT * FROM t LIMIT 1");
    }

    #[test]
    fn percent_styles() {
        let t = render("VALUES (%s, %(name)s)", PlaceholderStyle::Percent);
        assert_eq!(t.text(), "VALUES (1, 1)");
    }

    #[test]
    fn ignores_placeholder_in_string() {
        let t = render("SELECT ':id' FROM t WHERE a = :id", PlaceholderStyle::Colon);
        assert_eq!(t.text(), "SELECT ':id' FROM t WHERE a = 1");
    }

    #[test]
    fn ignores_placeholder_in_comment() {
        let t = render("SELECT 1 -- :id\nFROM t", PlaceholderStyle::Colon);
        assert_eq!(t.text(), "SELECT 1 -- :id\nFROM t");
    }

    #[test]
    fn ignores_placeholder_in_dollar_quote() {
        let t = render("SELECT $$ :id $$ FROM t", PlaceholderStyle::Colon);
        assert_eq!(t.text(), "SELECT $$ :id $$ FROM t");
    }

    #[test]
    fn native_dollar_params_untouched() {
        let t = render("SELECT * FROM t WHERE a = $1", PlaceholderStyle::Colon);
        assert_eq!(t.text(), "SELECT * FROM t WHERE a = $1");
    }

    #[test]
    fn maps_literal_range_back() {
        // A token after a placeholder must map back to its true source offset.
        assert_eq!(
            src_of("WHERE a = :id AND foo = 2", PlaceholderStyle::Colon, "foo"),
            "foo"
        );
    }

    #[test]
    fn maps_placeholder_range_to_source() {
        let t = render("WHERE a = :id", PlaceholderStyle::Colon);
        let pos = t.text().find('1').unwrap();
        let r = TextRange::new(TextSize::new(pos as u32), TextSize::new(pos as u32 + 1));
        let mapped = t.map_range(r);
        // Maps back onto the `:id` placeholder span.
        assert_eq!(
            &"WHERE a = :id"[mapped.start().into()..mapped.end().into()],
            ":id"
        );
        assert!(t.touches_placeholder(r));
    }
}
