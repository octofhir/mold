//! Ambiguity rules (Core pack): `AM04` select-star, `AM05` implicit cross
//! join, `AM02` set-operator modifier, `AM09` limit without order.

use mold_syntax::SyntaxKind;
use mold_syntax::ast::{AstNode, FromClause, SelectItem, SelectStmt, TableRef};

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RelatedInfo, RuleCode, TextEdit};

/// AM04 — `SELECT *`.
pub(super) struct SelectStar;

impl Rule for SelectStar {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Am04]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(select) = SelectStmt::cast(node.clone()) {
                lint_select_star(&select, analyzer);
            }
        }
    }
}

/// AM05 — implicit cross join.
pub(super) struct ImplicitCrossJoin;

impl Rule for ImplicitCrossJoin {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Am05]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(from) = FromClause::cast(node.clone()) {
                lint_implicit_cross_join(&from, analyzer);
            }
        }
    }
}

/// AM02 — set operator without `ALL`/`DISTINCT`.
pub(super) struct SetOpModifier;

impl Rule for SetOpModifier {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Am02]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(select) = SelectStmt::cast(node.clone()) {
                lint_set_op_modifier(&select, analyzer);
            }
        }
    }
}

/// AM09 — `LIMIT`/`OFFSET` without `ORDER BY`.
pub(super) struct LimitWithoutOrder;

impl Rule for LimitWithoutOrder {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Am09]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(select) = SelectStmt::cast(node.clone()) {
                lint_limit_without_order(&select, analyzer);
            }
        }
    }
}

/// AM03 — `ORDER BY` should set a direction on every term or none.
pub(super) struct OrderByDirection;

impl Rule for OrderByDirection {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Am03]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::ORDER_BY_CLAUSE {
                lint_order_by_direction(node, analyzer);
            }
        }
    }
}

/// AM03 — once any `ORDER BY` term is given an explicit `ASC`/`DESC`, leaving
/// the others implicit is ambiguous; state the direction on all of them.
fn lint_order_by_direction(clause: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let items: Vec<_> = clause
        .children()
        .filter(|c| c.kind() == SyntaxKind::ORDER_BY_ITEM)
        .collect();
    if items.len() < 2 {
        return;
    }
    let has_dir = |item: &mold_syntax::SyntaxNode| {
        item.children_with_tokens()
            .filter_map(|e| e.into_token())
            .any(|t| matches!(t.kind(), SyntaxKind::ASC_KW | SyntaxKind::DESC_KW))
    };
    let explicit = items.iter().filter(|i| has_dir(i)).count();
    // Consistent when all or none specify a direction.
    if explicit == 0 || explicit == items.len() {
        return;
    }
    let mut diag = Diagnostic::warning(
        "Inconsistent ORDER BY directions; specify ASC/DESC on every term or none",
    )
    .with_code(RuleCode::Am03)
    .with_range(clause.text_range());
    for item in items.iter().filter(|i| !has_dir(i)) {
        diag = diag.with_related(RelatedInfo {
            message: "this term has no explicit direction".to_string(),
            range: Some(item.text_range()),
        });
    }
    analyzer.emit(diag);
}

fn lint_select_star(stmt: &SelectStmt, analyzer: &mut Analyzer<'_>) {
    let expansion = single_table_column_list(stmt, analyzer);
    for node in stmt.syntax().descendants() {
        if let Some(item) = SelectItem::cast(node.clone())
            && let Some(star) = star_token_range(&item)
        {
            let mut diag = Diagnostic::warning("Avoid SELECT *; list columns explicitly")
                .with_code(RuleCode::Am04)
                .with_range(item.syntax().text_range());
            if let Some(list) = &expansion {
                // Replace just the `*` token so surrounding whitespace is kept.
                diag = diag.with_fix(Fix::new(
                    "Expand * to column list",
                    vec![TextEdit::replace(star, list.clone())],
                ));
            }
            analyzer.emit(diag);
        }
    }
}

/// The explicit column list for a `SELECT` over exactly one known table, or
/// `None` when there are joins/multiple tables or the schema is unknown.
fn single_table_column_list(stmt: &SelectStmt, analyzer: &mut Analyzer<'_>) -> Option<String> {
    let from = stmt.from_clause()?;
    let refs: Vec<TableRef> = from.table_refs().collect();
    if refs.len() != 1 {
        return None;
    }
    if from
        .syntax()
        .descendants()
        .any(|n| n.kind() == SyntaxKind::JOIN_EXPR)
    {
        return None;
    }
    let name = match &refs[0] {
        TableRef::Table(t) => t.name()?.text().to_string(),
        _ => return None,
    };
    let mut columns = analyzer.provider().lookup_columns(None, &name);
    if columns.is_empty() {
        return None;
    }
    columns.sort_by_key(|c| c.ordinal);
    Some(
        columns
            .iter()
            .map(|c| c.name.clone())
            .collect::<Vec<_>>()
            .join(", "),
    )
}

/// AM05 — comma-separated tables in `FROM` are an implicit (and easy to miss)
/// cross join; prefer an explicit `JOIN`.
fn lint_implicit_cross_join(from: &FromClause, analyzer: &mut Analyzer<'_>) {
    if from.table_refs().count() < 2 {
        return;
    }
    // Point the primary span at the offending comma (the implicit join
    // operator) and mark each joined table as related, rustc-style.
    let Some(comma) = from
        .syntax()
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == SyntaxKind::COMMA)
    else {
        return;
    };
    let mut diag = Diagnostic::warning("Implicit cross join; use an explicit JOIN clause")
        .with_code(RuleCode::Am05)
        .with_range(comma.text_range());
    for table in from.table_refs().take(2) {
        diag = diag.with_related(RelatedInfo {
            message: "table joined here".to_string(),
            range: Some(table.syntax().text_range()),
        });
    }
    analyzer.emit(diag);
}

/// AM02 — a set operator should state `ALL` or `DISTINCT` explicitly.
fn lint_set_op_modifier(stmt: &SelectStmt, analyzer: &mut Analyzer<'_>) {
    let tokens: Vec<_> = stmt
        .syntax()
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .collect();
    for (i, token) in tokens.iter().enumerate() {
        if !matches!(
            token.kind(),
            SyntaxKind::UNION_KW | SyntaxKind::EXCEPT_KW | SyntaxKind::INTERSECT_KW
        ) {
            continue;
        }
        let next = tokens[i + 1..]
            .iter()
            .find(|t| !t.kind().is_trivia())
            .map(|t| t.kind());
        if !matches!(next, Some(SyntaxKind::ALL_KW | SyntaxKind::DISTINCT_KW)) {
            analyzer.emit(
                Diagnostic::warning(format!(
                    "'{}' should specify ALL or DISTINCT explicitly",
                    token.text().to_uppercase()
                ))
                .with_code(RuleCode::Am02)
                .with_range(token.text_range()),
            );
        }
    }
}

/// AM09 — `LIMIT`/`OFFSET` without `ORDER BY` returns rows in an arbitrary order.
fn lint_limit_without_order(stmt: &SelectStmt, analyzer: &mut Analyzer<'_>) {
    let s = stmt.syntax();
    let limit = s.children().find(|c| {
        matches!(
            c.kind(),
            SyntaxKind::LIMIT_CLAUSE | SyntaxKind::OFFSET_CLAUSE
        )
    });
    let Some(limit) = limit else { return };
    let has_order = s
        .children()
        .any(|c| c.kind() == SyntaxKind::ORDER_BY_CLAUSE);
    if !has_order {
        analyzer.emit(
            Diagnostic::warning("LIMIT/OFFSET without ORDER BY returns an arbitrary set of rows")
                .with_code(RuleCode::Am09)
                .with_range(limit.text_range()),
        );
    }
}

/// Range of the `*` token in a select item, if it is a star item.
fn star_token_range(item: &SelectItem) -> Option<text_size::TextRange> {
    item.syntax()
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == SyntaxKind::STAR)
        .map(|t| t.text_range())
}
