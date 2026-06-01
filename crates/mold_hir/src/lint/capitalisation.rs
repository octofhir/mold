//! Capitalisation rules (Capitalisation pack): `CP01` keyword case, `CP02`
//! unquoted identifier case. Both are fixable; Postgres folds unquoted
//! identifiers to lower case anyway, so the `CP02` rewrite is safe.

use mold_syntax::SyntaxKind;

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// CP01 (keyword) and CP02 (identifier) capitalisation.
pub(super) struct Capitalisation;

impl Rule for Capitalisation {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cp01, RuleCode::Cp02]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Capitalisation
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for element in root.descendants_with_tokens() {
            let Some(token) = element.as_token() else {
                continue;
            };
            let kind = token.kind();
            let text = token.text();
            let range = token.text_range();

            if kind.is_keyword() {
                let upper = text.to_ascii_uppercase();
                if text == upper {
                    continue;
                }
                analyzer.emit(
                    Diagnostic::warning(format!("Keyword '{text}' should be upper case"))
                        .with_code(RuleCode::Cp01)
                        .with_range(range)
                        .with_fix(Fix::new(
                            format!("Uppercase '{text}'"),
                            vec![TextEdit::replace(range, upper)],
                        )),
                );
            } else if kind == SyntaxKind::IDENT {
                let lower = text.to_ascii_lowercase();
                if text == lower {
                    continue;
                }
                analyzer.emit(
                    Diagnostic::warning(format!("Identifier '{text}' should be lower case"))
                        .with_code(RuleCode::Cp02)
                        .with_range(range)
                        .with_fix(Fix::new(
                            format!("Lowercase '{text}'"),
                            vec![TextEdit::replace(range, lower)],
                        )),
                );
            }
        }
    }
}
