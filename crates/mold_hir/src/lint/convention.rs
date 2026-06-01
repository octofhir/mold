//! Convention rules (Convention pack): `CV01` `!=`/`<>` spelling, `CV05` NULL
//! comparison, `CV06` missing trailing semicolon.

use mold_syntax::SyntaxKind;
use mold_syntax::ast::{AstNode, BinaryExpr, Expr, LiteralKind};

use super::{Rule, is_statement};
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// CV01 — prefer `<>` over the `!=` spelling.
pub(super) struct NeSpelling;

impl Rule for NeSpelling {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv01]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(binary) = BinaryExpr::cast(node.clone()) {
                lint_ne_spelling(&binary, analyzer);
            }
        }
    }
}

/// CV05 — `=`/`<>` against NULL never matches.
pub(super) struct NullComparison;

impl Rule for NullComparison {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv05]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(binary) = BinaryExpr::cast(node.clone()) {
                lint_null_comparison(&binary, analyzer);
            }
        }
    }
}

/// CV06 — every top-level statement should end with a semicolon.
pub(super) struct MissingSemicolons;

impl Rule for MissingSemicolons {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv06]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        lint_missing_semicolons(root, analyzer);
    }
}

/// CV01 — prefer `<>` over the `!=` spelling.
fn lint_ne_spelling(expr: &BinaryExpr, analyzer: &mut Analyzer<'_>) {
    let Some(op) = expr.op_token() else { return };
    // NULL comparisons are CV05's job; don't fight over the same operator.
    let touches_null = expr.lhs().is_some_and(|e| is_null_literal(&e))
        || expr.rhs().is_some_and(|e| is_null_literal(&e));
    if touches_null {
        return;
    }
    if op.kind() == SyntaxKind::NE && op.text() == "!=" {
        analyzer.emit(
            Diagnostic::warning("Use <> instead of != for inequality")
                .with_code(RuleCode::Cv01)
                .with_range(op.text_range())
                .with_fix(Fix::new(
                    "Replace != with <>",
                    vec![TextEdit::replace(op.text_range(), "<>")],
                )),
        );
    }
}

/// CV05 — `=`/`<>` against NULL never matches; use `IS [NOT] NULL`.
fn lint_null_comparison(expr: &BinaryExpr, analyzer: &mut Analyzer<'_>) {
    let Some(op) = expr.op_token() else { return };
    let replacement = match op.kind() {
        SyntaxKind::EQ => "IS",
        SyntaxKind::NE => "IS NOT",
        _ => return,
    };
    let lhs_null = expr.lhs().is_some_and(|e| is_null_literal(&e));
    let rhs_null = expr.rhs().is_some_and(|e| is_null_literal(&e));
    if !(lhs_null || rhs_null) {
        return;
    }
    let mut diag = Diagnostic::warning("Use IS NULL / IS NOT NULL to compare with NULL")
        .with_code(RuleCode::Cv05)
        .with_range(expr.syntax().text_range());
    // Only rewrite when NULL is on the right (`x = NULL` -> `x IS NULL`).
    if rhs_null {
        diag = diag.with_fix(Fix::new(
            format!("Replace operator with {replacement}"),
            vec![TextEdit::replace(op.text_range(), replacement)],
        ));
    }
    analyzer.emit(diag);
}

fn is_null_literal(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(lit) if lit.kind() == Some(LiteralKind::Null))
}

/// CV06 — every top-level statement should end with a semicolon.
fn lint_missing_semicolons(root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    // A statement is terminated if a SEMICOLON token follows it before the next
    // statement. Walk top-level elements tracking the last unterminated stmt.
    let mut pending: Option<mold_syntax::SyntaxNode> = None;
    for element in root.children_with_tokens() {
        if let Some(node) = element.into_node() {
            if is_statement(node.kind()) {
                if let Some(stmt) = pending.take() {
                    emit_missing_semicolon(&stmt, analyzer);
                }
                pending = Some(node.clone());
            }
        } else if let Some(token) = element.into_token()
            && token.kind() == SyntaxKind::SEMICOLON
        {
            pending = None;
        }
    }
    if let Some(stmt) = pending {
        emit_missing_semicolon(&stmt, analyzer);
    }
}

fn emit_missing_semicolon(stmt: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    // Insert after the last non-trivia token so the `;` does not land past a
    // trailing newline/comment that the statement node happens to include.
    let insert_at = stmt
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| !t.kind().is_trivia())
        .map(|t| t.text_range().end())
        .max()
        .unwrap_or(stmt.text_range().end());
    analyzer.emit(
        Diagnostic::warning("Statement should end with a semicolon")
            .with_code(RuleCode::Cv06)
            .with_range(stmt.text_range())
            .with_fix(Fix::new(
                "Add trailing semicolon",
                vec![TextEdit::replace(
                    text_size::TextRange::empty(insert_at),
                    ";",
                )],
            )),
    );
}
