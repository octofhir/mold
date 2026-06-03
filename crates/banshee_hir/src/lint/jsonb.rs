//! JSONB rules (Jsonb pack): `JB01` comparing a `jsonb` produced by `->`
//! against a text literal, which silently never matches; `->>` should be used.

use banshee_syntax::SyntaxKind;
use banshee_syntax::ast::{AstNode, BinaryExpr, Expr, LiteralKind};

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// JB01 — JSONB scalar compared against text via `->`.
pub(super) struct JsonbTextComparison;

impl Rule for JsonbTextComparison {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Jb01]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Jsonb
    }
    fn run(&self, root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(binary) = BinaryExpr::cast(node.clone()) {
                lint_jsonb_text_comparison_binary(&binary, analyzer);
            }
        }
    }
}

fn lint_jsonb_text_comparison_binary(expr: &BinaryExpr, analyzer: &mut Analyzer<'_>) {
    let is_like = binary_has_operator(expr, &[SyntaxKind::LIKE_KW, SyntaxKind::ILIKE_KW]);
    let is_text_comparison = is_like
        || binary_has_operator(
            expr,
            &[
                SyntaxKind::EQ,
                SyntaxKind::NE,
                SyntaxKind::LT,
                SyntaxKind::LE,
                SyntaxKind::GT,
                SyntaxKind::GE,
            ],
        );

    if !is_text_comparison {
        return;
    }

    let Some(lhs) = expr.lhs() else {
        return;
    };
    let Some(rhs) = expr.rhs() else {
        return;
    };

    let lhs_jsonb = is_jsonb_non_text_expr(&lhs) && is_string_literal_expr(&rhs);
    let rhs_jsonb = is_jsonb_non_text_expr(&rhs) && is_string_literal_expr(&lhs);

    if lhs_jsonb || rhs_jsonb {
        let message = if is_like {
            "Use ->> when matching JSONB scalar against text pattern"
        } else {
            "Use ->> when comparing JSONB scalar to text literal"
        };
        let mut diag = Diagnostic::warning(message)
            .with_code(RuleCode::Jb01)
            .with_range(expr.syntax().text_range());

        // Fix: turn the last `->` on the JSONB side into `->>` so the value is
        // extracted as text.
        let jsonb_side = if lhs_jsonb { &lhs } else { &rhs };
        if let Some(arrow) = last_arrow_token(jsonb_side) {
            diag = diag.with_fix(Fix::new(
                "Replace -> with ->>",
                vec![TextEdit::replace(arrow, "->>")],
            ));
        }
        analyzer.emit(diag);
    }
}

/// Range of the last `->` (non-text arrow) token within an expression.
fn last_arrow_token(expr: &Expr) -> Option<text_size::TextRange> {
    expr.syntax()
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.kind() == SyntaxKind::ARROW)
        .last()
        .map(|t| t.text_range())
}

fn is_jsonb_non_text_expr(expr: &Expr) -> bool {
    let mut has_non_text = false;
    let mut has_text = false;

    for child in expr.syntax().descendants_with_tokens() {
        if let Some(token) = child.as_token() {
            match token.kind() {
                SyntaxKind::ARROW | SyntaxKind::HASH_ARROW => has_non_text = true,
                SyntaxKind::ARROW_TEXT | SyntaxKind::HASH_ARROW_TEXT => has_text = true,
                _ => {}
            }
        }
    }

    has_non_text && !has_text
}

fn is_string_literal_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Literal(lit)
            if matches!(lit.kind(), Some(LiteralKind::String | LiteralKind::DollarString))
    )
}

fn binary_has_operator(expr: &BinaryExpr, kinds: &[SyntaxKind]) -> bool {
    expr.syntax()
        .children_with_tokens()
        .filter_map(|child| child.into_token())
        .any(|token| kinds.contains(&token.kind()))
}
