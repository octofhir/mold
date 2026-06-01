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

/// CV10 — `LIKE` with no wildcard behaves like `=`.
pub(super) struct LikeWithoutWildcard;

impl Rule for LikeWithoutWildcard {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv10]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::LIKE_EXPR {
                lint_like_without_wildcard(node, analyzer);
            }
        }
    }
}

/// CV10 — a plain `LIKE` whose pattern has no `%`/`_` wildcard is just an
/// equality test; `=` says so and lets the planner use an index. Only plain
/// `LIKE` (not `ILIKE`/`NOT LIKE`) is rewritten, since `=` differs otherwise.
fn lint_like_without_wildcard(expr: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let tokens: Vec<_> = expr
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .collect();
    if tokens
        .iter()
        .any(|t| matches!(t.kind(), SyntaxKind::ILIKE_KW | SyntaxKind::NOT_KW))
    {
        return;
    }
    let Some(like) = tokens.iter().find(|t| t.kind() == SyntaxKind::LIKE_KW) else {
        return;
    };
    // The pattern is a string literal somewhere in the right operand.
    let Some(string) = expr
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::STRING)
    else {
        return;
    };
    let text = string.text();
    let inner = text
        .strip_prefix('\'')
        .and_then(|t| t.strip_suffix('\''))
        .unwrap_or(text);
    if inner.contains('%') || inner.contains('_') {
        return;
    }
    analyzer.emit(
        Diagnostic::warning("LIKE without a wildcard is equivalent to =; use = instead")
            .with_code(RuleCode::Cv10)
            .with_range(like.text_range())
            .with_fix(Fix::new(
                "Replace LIKE with =",
                vec![TextEdit::replace(like.text_range(), "=")],
            )),
    );
}

/// CV08 — prefer `LEFT JOIN` over `RIGHT JOIN`.
pub(super) struct RightJoin;

impl Rule for RightJoin {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv08]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::JOIN_EXPR {
                lint_right_join(node, analyzer);
            }
        }
    }
}

/// CV08 — a `RIGHT JOIN` can always be rewritten as a `LEFT JOIN` by swapping
/// the operands, which most readers find easier to follow.
fn lint_right_join(join: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let Some(right_kw) = join
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::RIGHT_KW)
    else {
        return;
    };
    analyzer.emit(
        Diagnostic::warning("Prefer LEFT JOIN over RIGHT JOIN; swap the joined tables")
            .with_code(RuleCode::Cv08)
            .with_range(right_kw.text_range()),
    );
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
