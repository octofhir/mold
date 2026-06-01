//! Aliasing and qualification rules (Core pack): `AL03` unaliased complex
//! select item, `AL05` unused table alias, `RF03` mixed column qualification.

use mold_syntax::SyntaxKind;
use mold_syntax::ast::{AstNode, ColumnRef, Expr, SelectItem, SelectStmt};

use super::{Rule, is_statement};
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// AL01 — a table alias should be introduced with `AS`.
pub(super) struct TableAliasAs;

impl Rule for TableAliasAs {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Al01]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::TABLE_REF {
                lint_table_alias_as(node, analyzer);
            }
        }
    }
}

/// AL02 — a column/expression alias should be introduced with `AS`.
pub(super) struct ColumnAliasAs;

impl Rule for ColumnAliasAs {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Al02]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(item) = SelectItem::cast(node.clone()) {
                lint_column_alias_as(&item, analyzer);
            }
        }
    }
}

/// AL03 — a complex select expression should be aliased.
pub(super) struct UnaliasedSelectItem;

impl Rule for UnaliasedSelectItem {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Al03]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(select) = SelectStmt::cast(node.clone()) {
                lint_unaliased_select_items(&select, analyzer);
            }
        }
    }
}

/// AL05 (unused alias) and RF03 (mixed qualification): both ride the same scan
/// of a statement's tables and column qualifiers.
pub(super) struct AliasingAndQualification;

impl Rule for AliasingAndQualification {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Al05, RuleCode::Rf03]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if is_statement(node.kind()) && !matches!(node.kind(), SyntaxKind::INSERT_STMT) {
                lint_aliasing_and_qualification(node, analyzer);
            }
        }
    }
}

/// AL03 — a complex select expression should be aliased so the output column
/// has a stable, named identity.
fn lint_unaliased_select_items(stmt: &SelectStmt, analyzer: &mut Analyzer<'_>) {
    for node in stmt.syntax().descendants() {
        let Some(item) = SelectItem::cast(node.clone()) else {
            continue;
        };
        if has_select_item_alias(&item) {
            continue;
        }
        let Some(expr) = item.expr() else { continue };
        if needs_alias(&expr) {
            analyzer.emit(
                Diagnostic::warning("Complex select expression should be aliased with AS")
                    .with_code(RuleCode::Al03)
                    .with_range(item.syntax().text_range()),
            );
        }
    }
}

fn needs_alias(expr: &Expr) -> bool {
    !matches!(expr, Expr::ColumnRef(_) | Expr::Literal(_))
}

/// AL01 — a `TABLE_REF` carrying an implicit alias (`users u`) reads better as
/// `users AS u`. Fixable by inserting the keyword.
fn lint_table_alias_as(table_ref: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let Some((name, range)) = table_ref_alias(table_ref) else {
        return;
    };
    // Explicit `AS` anywhere in the table ref means the alias is already keyed.
    let has_as = table_ref
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .any(|t| t.kind() == SyntaxKind::AS_KW);
    if has_as {
        return;
    }
    analyzer.emit(
        Diagnostic::warning(format!("Table alias '{name}' should be introduced with AS"))
            .with_code(RuleCode::Al01)
            .with_range(range)
            .with_fix(Fix::new(
                "Insert AS",
                vec![TextEdit::replace(
                    text_size::TextRange::empty(range.start()),
                    "AS ",
                )],
            )),
    );
}

/// AL02 — a `SELECT_ITEM` with an implicit alias (`a b`) reads better as
/// `a AS b`. Fixable by inserting the keyword before the alias.
fn lint_column_alias_as(item: &SelectItem, analyzer: &mut Analyzer<'_>) {
    let direct_tokens: Vec<_> = item
        .syntax()
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .collect();
    // An `AS` already present means the alias is keyed.
    if direct_tokens.iter().any(|t| t.kind() == SyntaxKind::AS_KW) {
        return;
    }
    // A bare trailing identifier directly under the item is the implicit alias
    // (the aliased expression itself lives in a child node, e.g. COLUMN_REF).
    let Some(alias) = direct_tokens
        .iter()
        .find(|t| matches!(t.kind(), SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT))
    else {
        return;
    };
    let range = alias.text_range();
    analyzer.emit(
        Diagnostic::warning(format!(
            "Column alias '{}' should be introduced with AS",
            alias.text()
        ))
        .with_code(RuleCode::Al02)
        .with_range(range)
        .with_fix(Fix::new(
            "Insert AS",
            vec![TextEdit::replace(
                text_size::TextRange::empty(range.start()),
                "AS ",
            )],
        )),
    );
}

/// Whether a `SELECT_ITEM` carries an alias. The grammar represents both the
/// `AS alias` and implicit `expr alias` forms as a trailing `AS_KW` / bare
/// identifier directly under `SELECT_ITEM`, not as a separate `ALIAS` node.
fn has_select_item_alias(item: &SelectItem) -> bool {
    item.syntax()
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .any(|t| {
            matches!(
                t.kind(),
                SyntaxKind::AS_KW | SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT
            )
        })
}

fn lint_aliasing_and_qualification(stmt: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    // Aliases declared in this statement (lowercased name + the alias token range).
    let aliases: Vec<(String, text_size::TextRange)> = stmt
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::TABLE_REF)
        .filter_map(table_ref_alias)
        .map(|(name, range)| (name.to_ascii_lowercase(), range))
        .collect();

    // Qualifiers used by every column reference in the statement.
    let qualifiers: Vec<String> = stmt
        .descendants()
        .filter_map(|n| ColumnRef::cast(n.clone()))
        .filter_map(|c| c.table().map(|t| t.text().to_ascii_lowercase()))
        .collect();

    // AL05: alias declared but no column reference uses it as a qualifier.
    for (name, range) in &aliases {
        if !qualifiers.iter().any(|q| q == name) {
            analyzer.emit(
                Diagnostic::warning(format!("Table alias '{name}' is never used"))
                    .with_code(RuleCode::Al05)
                    .with_range(*range),
            );
        }
    }

    // RF03: a single-table statement either qualifies all columns or none.
    let from = stmt
        .descendants()
        .find(|n| n.kind() == SyntaxKind::FROM_CLAUSE);
    let Some(from) = from else { return };
    let table_count = from
        .children()
        .filter(|c| c.kind() == SyntaxKind::TABLE_REF)
        .count();
    let has_join = from
        .descendants()
        .any(|n| n.kind() == SyntaxKind::JOIN_EXPR);
    if table_count != 1 || has_join {
        return;
    }
    // Only column refs whose nearest enclosing SELECT_STMT is this statement,
    // so subqueries/CTE bodies do not pollute the mixed-qualification check.
    let col_refs: Vec<ColumnRef> = stmt
        .descendants()
        .filter_map(|n| ColumnRef::cast(n.clone()))
        .filter(|c| {
            c.syntax()
                .ancestors()
                .find(|a| is_statement(a.kind()))
                .is_some_and(|a| a.text_range() == stmt.text_range())
        })
        .collect();
    if col_refs.len() < 2 {
        return;
    }
    let qualified = col_refs.iter().filter(|c| c.table().is_some()).count();
    if qualified != 0 && qualified != col_refs.len() {
        analyzer.emit(
            Diagnostic::warning(
                "Mixed column qualification in a single-table query; qualify all or none",
            )
            .with_code(RuleCode::Rf03)
            .with_range(stmt.text_range()),
        );
    }
}

/// Returns the alias `(name, range)` of a `TABLE_REF`, handling both
/// `AS a` (an `ALIAS` node) and the implicit `users a` form (a bare second
/// identifier that isn't part of a dotted qualifier).
fn table_ref_alias(table_ref: &mold_syntax::SyntaxNode) -> Option<(String, text_size::TextRange)> {
    // Explicit `AS alias` produces an ALIAS child whose first identifier is the
    // name. Look for that first.
    if let Some(alias) = table_ref.children().find(|c| c.kind() == SyntaxKind::ALIAS)
        && let Some(token) = alias
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| matches!(t.kind(), SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT))
    {
        return Some((token.text().to_string(), token.text_range()));
    }
    // Implicit alias: count direct IDENT vs DOT tokens. An extra identifier
    // beyond the qualified name (idents > dots + 1) is the alias.
    let ident_info: Vec<(String, text_size::TextRange)> = table_ref
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| matches!(t.kind(), SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT))
        .map(|t| (t.text().to_string(), t.text_range()))
        .collect();
    let dots = table_ref
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.kind() == SyntaxKind::DOT)
        .count();
    if ident_info.len() > dots + 1 {
        ident_info.into_iter().last()
    } else {
        None
    }
}
