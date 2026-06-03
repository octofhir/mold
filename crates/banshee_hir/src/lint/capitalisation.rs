//! Capitalisation rules (Capitalisation pack): `CP01` keyword case, `CP02`
//! unquoted identifier case. Both are fixable. Each takes a `policy` option
//! (`upper`/`lower`); defaults are `upper` for keywords and `lower` for
//! identifiers, which is also Postgres's own folding for unquoted identifiers.

use banshee_syntax::SyntaxKind;

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// CP01 (keyword) and CP02 (identifier) capitalisation.
pub(super) struct Capitalisation;

/// Whether the configured policy wants upper case.
fn wants_upper(analyzer: &Analyzer<'_>, code: &str, default_upper: bool) -> bool {
    match analyzer.rule_option(code, "policy") {
        Some("upper") => true,
        Some("lower") => false,
        _ => default_upper,
    }
}

impl Rule for Capitalisation {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cp01, RuleCode::Cp02]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Capitalisation
    }
    fn run(&self, root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        let kw_upper = wants_upper(analyzer, "CP01", true);
        let id_upper = wants_upper(analyzer, "CP02", false);

        for element in root.descendants_with_tokens() {
            let Some(token) = element.as_token() else {
                continue;
            };
            let kind = token.kind();
            let text = token.text();
            let range = token.text_range();

            let (code, upper, case_word) = if kind.is_keyword() {
                (RuleCode::Cp01, kw_upper, "Keyword")
            } else if kind == SyntaxKind::IDENT {
                (RuleCode::Cp02, id_upper, "Identifier")
            } else {
                continue;
            };

            let want = if upper {
                text.to_ascii_uppercase()
            } else {
                text.to_ascii_lowercase()
            };
            if text == want {
                continue;
            }
            let case_name = if upper { "upper" } else { "lower" };
            analyzer.emit(
                Diagnostic::warning(format!("{case_word} '{text}' should be {case_name} case"))
                    .with_code(code)
                    .with_range(range)
                    .with_fix(Fix::new(
                        format!("Convert '{text}' to {case_name} case"),
                        vec![TextEdit::replace(range, want)],
                    )),
            );
        }
    }
}
