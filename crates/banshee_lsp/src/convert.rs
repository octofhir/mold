//! Conversions between banshee's byte offsets and LSP's UTF-16 positions, and
//! between banshee diagnostics/edits and their LSP equivalents.

use banshee_hir::{Diagnostic as HirDiagnostic, Severity, TextEdit as HirEdit};
use lsp_types::{Diagnostic as LspDiagnostic, DiagnosticSeverity, NumberOrString, Position, Range};
use text_size::TextRange;

/// Precomputed line start offsets for fast offset <-> position mapping.
pub struct LineIndex {
    text: String,
    /// Byte offset of the start of each line.
    line_starts: Vec<u32>,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0u32];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push((i + 1) as u32);
            }
        }
        Self {
            text: text.to_string(),
            line_starts,
        }
    }

    /// Maps a byte offset to an LSP position (UTF-16 column).
    pub fn position(&self, offset: u32) -> Position {
        let line = match self.line_starts.binary_search(&offset) {
            Ok(l) => l,
            Err(next) => next - 1,
        };
        let line_start = self.line_starts[line] as usize;
        let end = (offset as usize).min(self.text.len());
        let col_utf16 = self.text[line_start..end]
            .chars()
            .map(|c| c.len_utf16() as u32)
            .sum();
        Position {
            line: line as u32,
            character: col_utf16,
        }
    }

    /// Maps an LSP position back to a byte offset.
    pub fn offset(&self, pos: Position) -> u32 {
        let line = pos.line as usize;
        if line >= self.line_starts.len() {
            return self.text.len() as u32;
        }
        let line_start = self.line_starts[line] as usize;
        let mut utf16 = 0u32;
        let mut byte = line_start;
        for ch in self.text[line_start..].chars() {
            if utf16 >= pos.character || ch == '\n' {
                break;
            }
            utf16 += ch.len_utf16() as u32;
            byte += ch.len_utf8();
        }
        byte as u32
    }

    /// Maps a byte range to an LSP range.
    pub fn range(&self, range: TextRange) -> Range {
        Range {
            start: self.position(u32::from(range.start())),
            end: self.position(u32::from(range.end())),
        }
    }
}

fn severity(s: Severity) -> DiagnosticSeverity {
    match s {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Info => DiagnosticSeverity::INFORMATION,
        Severity::Hint => DiagnosticSeverity::HINT,
        _ => DiagnosticSeverity::WARNING,
    }
}

/// Converts a banshee diagnostic to its LSP form. Diagnostics without a range are
/// anchored at the document start.
pub fn diagnostic(index: &LineIndex, d: &HirDiagnostic) -> LspDiagnostic {
    let range = d.range.map(|r| index.range(r)).unwrap_or_default();
    let message = match &d.help {
        Some(help) => format!("{}\nhelp: {help}", d.message),
        None => d.message.clone(),
    };
    LspDiagnostic {
        range,
        severity: Some(severity(d.severity)),
        code: d
            .code
            .map(|c| NumberOrString::String(c.as_str().to_string())),
        source: Some("banshee".to_string()),
        message,
        ..Default::default()
    }
}

/// Converts a banshee text edit to an LSP text edit.
pub fn text_edit(index: &LineIndex, edit: &HirEdit) -> lsp_types::TextEdit {
    lsp_types::TextEdit {
        range: index.range(edit.range),
        new_text: edit.new_text.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_roundtrip_ascii() {
        let idx = LineIndex::new("select 1\nfrom t");
        let pos = idx.position(9); // start of "from"
        assert_eq!(
            pos,
            Position {
                line: 1,
                character: 0
            }
        );
        assert_eq!(idx.offset(pos), 9);
    }

    #[test]
    fn handles_multibyte() {
        // 'é' is 2 bytes UTF-8, 1 UTF-16 unit.
        let idx = LineIndex::new("-- é\nx");
        let pos = idx.position(idx.text.find('x').unwrap() as u32);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);
    }
}
