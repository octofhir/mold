//! Structure rules (Core pack): `ST01` redundant `ELSE NULL`, `ST05` subquery
//! used as a table source, `ST03` unused CTE.

use mold_syntax::SyntaxKind;

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// ST01 — redundant `ELSE NULL` in a `CASE`.
pub(super) struct RedundantElseNull;

impl Rule for RedundantElseNull {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::St01]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::CASE_EXPR {
                lint_redundant_else_null(node, analyzer);
            }
        }
    }
}

/// ST05 — subquery used as a table source.
pub(super) struct SubqueryAsTable;

impl Rule for SubqueryAsTable {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::St05]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::TABLE_REF {
                lint_subquery_as_table(node, analyzer);
            }
        }
    }
}

/// ST03 — a CTE declared but never referenced.
pub(super) struct UnusedCtes;

impl Rule for UnusedCtes {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::St03]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        lint_unused_ctes(root, analyzer);
    }
}

/// ST01 — `ELSE NULL` in a `CASE` is redundant (the default result is NULL).
fn lint_redundant_else_null(case: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let Some(else_body) = case.children().find(|c| c.kind() == SyntaxKind::CASE_ELSE) else {
        return;
    };
    // The ELSE body must be exactly a NULL literal.
    let only_null = else_body
        .children()
        .all(|c| c.kind() == SyntaxKind::LITERAL)
        && else_body
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
            .filter(|t| !t.kind().is_trivia())
            .all(|t| t.kind() == SyntaxKind::NULL_KW);
    if !only_null {
        return;
    }
    let Some(else_kw) = case
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::ELSE_KW)
    else {
        return;
    };
    // Delete from `ELSE` through the end of its NULL body.
    let range =
        text_size::TextRange::new(else_kw.text_range().start(), else_body.text_range().end());
    analyzer.emit(
        Diagnostic::warning("Redundant ELSE NULL in CASE expression")
            .with_code(RuleCode::St01)
            .with_range(range)
            .with_fix(Fix::new(
                "Remove ELSE NULL",
                vec![TextEdit::replace(range, "")],
            )),
    );
}

/// ST05 — a subquery used as a table source (a `TABLE_REF` containing a nested
/// `SELECT_STMT`) is better expressed as a CTE.
fn lint_subquery_as_table(table_ref: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let nested = table_ref
        .children()
        .any(|c| c.kind() == SyntaxKind::SELECT_STMT);
    if !nested {
        return;
    }
    analyzer.emit(
        Diagnostic::warning(
            "Subquery in FROM/JOIN; consider extracting it into a CTE for readability",
        )
        .with_code(RuleCode::St05)
        .with_range(table_ref.text_range()),
    );
}

/// ST03 — a CTE that is declared but never referenced.
fn lint_unused_ctes(root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    // Collect referenced table names (every identifier that names a table).
    let referenced: Vec<String> = root
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::TABLE_REF)
        .filter_map(|n| {
            n.descendants_with_tokens()
                .filter_map(|e| e.into_token())
                .find(|t| matches!(t.kind(), SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT))
                .map(|t| t.text().to_ascii_lowercase())
        })
        .collect();

    for cte in root.descendants().filter(|n| n.kind() == SyntaxKind::CTE) {
        let Some(name_tok) = cte
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| matches!(t.kind(), SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT))
        else {
            continue;
        };
        let name = name_tok.text().to_ascii_lowercase();
        if !referenced.contains(&name) {
            analyzer.emit(
                Diagnostic::warning(format!(
                    "CTE '{}' is defined but never used",
                    name_tok.text()
                ))
                .with_code(RuleCode::St03)
                .with_range(name_tok.text_range()),
            );
        }
    }
}
