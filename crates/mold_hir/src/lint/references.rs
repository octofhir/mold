//! Reference rules (Core pack): `RF06` unnecessary identifier quoting.

use mold_syntax::{SyntaxKind, keyword_from_str};

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// RF06 — an identifier quoted without need.
pub(super) struct UnnecessaryQuotes;

impl Rule for UnnecessaryQuotes {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Rf06]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for element in root.descendants_with_tokens() {
            let Some(token) = element.as_token() else {
                continue;
            };
            if token.kind() == SyntaxKind::QUOTED_IDENT {
                lint_quoted_ident(token.text(), token.text_range(), analyzer);
            }
        }
    }
}

/// RF06 — `"foo"` is only meaningful when `foo` would not lex as the same
/// identifier: i.e. it contains characters Postgres would fold or reject, or it
/// collides with a keyword. A plain lower-case word can drop its quotes.
fn lint_quoted_ident(text: &str, range: text_size::TextRange, analyzer: &mut Analyzer<'_>) {
    let Some(inner) = text.strip_prefix('"').and_then(|t| t.strip_suffix('"')) else {
        return;
    };
    // Embedded quotes (`""`) make the literal non-trivial; leave it alone.
    if inner.is_empty() || inner.contains('"') {
        return;
    }
    if !is_bare_identifier(inner) {
        return;
    }
    // A keyword must stay quoted to be used as an identifier.
    if keyword_from_str(inner).is_some() {
        return;
    }
    analyzer.emit(
        Diagnostic::warning(format!("Identifier \"{inner}\" does not need quoting"))
            .with_code(RuleCode::Rf06)
            .with_range(range)
            .with_fix(Fix::new(
                "Remove quotes",
                vec![TextEdit::replace(range, inner.to_string())],
            )),
    );
}

/// Whether `s` would lex unchanged as an unquoted identifier: starts with a
/// lower-case letter or `_`, then lower-case letters, digits or `_`.
fn is_bare_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
}
