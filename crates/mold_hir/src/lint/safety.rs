//! Safety rules (Core pack): `SF01` `UPDATE` without `WHERE`, `SF02` `DELETE`
//! without `WHERE`.

use mold_syntax::ast::{AstNode, DeleteStmt, UpdateStmt};

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, RuleCode};

/// SF01 — `UPDATE` without `WHERE`.
pub(super) struct UpdateWithoutWhere;

impl Rule for UpdateWithoutWhere {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Sf01]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(update) = UpdateStmt::cast(node.clone()) {
                lint_update_without_where(&update, analyzer);
            }
        }
    }
}

/// SF02 — `DELETE` without `WHERE`.
pub(super) struct DeleteWithoutWhere;

impl Rule for DeleteWithoutWhere {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Sf02]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(delete) = DeleteStmt::cast(node.clone()) {
                lint_delete_without_where(&delete, analyzer);
            }
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
