//! Built-in lint rules.
//!
//! Each rule walks the CST (and, where useful, consults the analyzer's resolved
//! scope) and emits [`Diagnostic`]s tagged with a stable rule code. Rules that
//! can mechanically repair the source attach a [`Fix`].
//!
//! Rule code prefixes:
//! - `AM` — ambiguity (`AM04` select-star, `AM05` implicit cross join)
//! - `SF` — safety (`SF01` update-no-where, `SF02` delete-no-where)
//! - `JB` — JSONB usage (`JB01` text comparison via `->`)
//! - `CP` — capitalisation (`CP01` keyword case)

use mold_syntax::SyntaxKind;
use mold_syntax::ast::{
    AstNode, BinaryExpr, DeleteStmt, Expr, FromClause, LiteralKind, SelectItem, SelectStmt,
    UpdateStmt,
};

use crate::analyze::{
    AnalysisOptions, Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit,
};

/// Dispatches every enabled lint pack over the syntax tree.
pub(crate) fn apply_lints(
    root: &mold_syntax::SyntaxNode,
    analyzer: &mut Analyzer<'_>,
    options: &AnalysisOptions,
) {
    if options.has_builtin_pack(BuiltinLintPack::Core) {
        apply_core_lints(root, analyzer);
    }
    if options.has_builtin_pack(BuiltinLintPack::Jsonb) {
        apply_jsonb_lints(root, analyzer);
    }
    if options.has_builtin_pack(BuiltinLintPack::Capitalisation) {
        apply_capitalisation_lints(root, analyzer);
    }
    for pack in &options.external_lint_packs {
        pack.apply(root, analyzer);
    }
}

fn apply_core_lints(root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    for node in root.descendants() {
        if let Some(select) = SelectStmt::cast(node.clone()) {
            lint_select_star(&select, analyzer);
        }
        if let Some(update) = UpdateStmt::cast(node.clone()) {
            lint_update_without_where(&update, analyzer);
        }
        if let Some(delete) = DeleteStmt::cast(node.clone()) {
            lint_delete_without_where(&delete, analyzer);
        }
        if let Some(from) = FromClause::cast(node.clone()) {
            lint_implicit_cross_join(&from, analyzer);
        }
    }
}

/// CP01 — keyword tokens should be upper case (fixable).
/// CP02 — unquoted identifiers should be lower case (fixable). Postgres folds
/// unquoted identifiers to lower case anyway, so the rewrite is safe.
fn apply_capitalisation_lints(root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
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

fn apply_jsonb_lints(root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    for node in root.descendants() {
        if let Some(binary) = BinaryExpr::cast(node.clone()) {
            lint_jsonb_text_comparison_binary(&binary, analyzer);
        }
    }
}

/// AM04 — `SELECT *` hides column count and breaks on schema drift.
fn lint_select_star(stmt: &SelectStmt, analyzer: &mut Analyzer<'_>) {
    for node in stmt.syntax().descendants() {
        if let Some(item) = SelectItem::cast(node.clone())
            && select_item_has_star(&item)
        {
            analyzer.emit(
                Diagnostic::warning("Avoid SELECT *; list columns explicitly")
                    .with_code(RuleCode::Am04)
                    .with_range(item.syntax().text_range()),
            );
        }
    }
}

/// SF01 — `UPDATE` without `WHERE` rewrites every row.
fn lint_update_without_where(stmt: &UpdateStmt, analyzer: &mut Analyzer<'_>) {
    if stmt.where_clause().is_none() {
        analyzer.emit(
            Diagnostic::warning("UPDATE without WHERE affects all rows")
                .with_code(RuleCode::Sf01)
                .with_range(stmt.syntax().text_range()),
        );
    }
}

/// SF02 — `DELETE` without `WHERE` empties the table.
fn lint_delete_without_where(stmt: &DeleteStmt, analyzer: &mut Analyzer<'_>) {
    if stmt.where_clause().is_none() {
        analyzer.emit(
            Diagnostic::warning("DELETE without WHERE affects all rows")
                .with_code(RuleCode::Sf02)
                .with_range(stmt.syntax().text_range()),
        );
    }
}

/// AM05 — comma-separated tables in `FROM` are an implicit (and easy to miss)
/// cross join; prefer an explicit `JOIN`.
fn lint_implicit_cross_join(from: &FromClause, analyzer: &mut Analyzer<'_>) {
    if from.table_refs().count() < 2 {
        return;
    }
    let has_top_level_comma = from
        .syntax()
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .any(|t| t.kind() == SyntaxKind::COMMA);
    if has_top_level_comma {
        analyzer.emit(
            Diagnostic::warning("Implicit cross join; use an explicit JOIN clause")
                .with_code(RuleCode::Am05)
                .with_range(from.syntax().text_range()),
        );
    }
}

/// JB01 — comparing a JSONB value produced by `->` (a `jsonb`) against a text
/// literal silently never matches; `->>` should be used instead.
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

fn select_item_has_star(item: &SelectItem) -> bool {
    for child in item.syntax().descendants_with_tokens() {
        if let Some(token) = child.into_token()
            && token.kind() == SyntaxKind::STAR
        {
            return true;
        }
    }
    false
}

fn binary_has_operator(expr: &BinaryExpr, kinds: &[SyntaxKind]) -> bool {
    expr.syntax()
        .children_with_tokens()
        .filter_map(|child| child.into_token())
        .any(|token| kinds.contains(&token.kind()))
}
