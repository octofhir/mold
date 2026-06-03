//! Structure rules (Core pack): `ST01` redundant `ELSE NULL`, `ST05` subquery
//! used as a table source, `ST03` unused CTE.

use banshee_syntax::SyntaxKind;

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
    fn run(&self, root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
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
    fn run(&self, root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
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
    fn run(&self, root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        lint_unused_ctes(root, analyzer);
    }
}

/// ST07 — `NATURAL JOIN` relies on implicit shared-name columns.
pub(super) struct NaturalJoin;

impl Rule for NaturalJoin {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::St07]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::JOIN_EXPR {
                lint_natural_join(node, analyzer);
            }
        }
    }
}

/// ST08 — `DISTINCT ON` without `ORDER BY`.
pub(super) struct DistinctOnWithoutOrder;

impl Rule for DistinctOnWithoutOrder {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::St08]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Core
    }
    fn run(&self, root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::SELECT_STMT {
                lint_distinct_on_without_order(node, analyzer);
            }
        }
    }
}

/// ST07 — `NATURAL JOIN` joins on every column that happens to share a name, so
/// adding a column silently changes the join. Spell the condition with `ON`.
fn lint_natural_join(join: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let Some(natural) = join
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::NATURAL_KW)
    else {
        return;
    };
    analyzer.emit(
        Diagnostic::warning("Avoid NATURAL JOIN; state the join columns with ON or USING")
            .with_code(RuleCode::St07)
            .with_range(natural.text_range()),
    );
}

/// ST08 — `DISTINCT ON (...)` keeps one row per group, but which row is
/// arbitrary unless an `ORDER BY` pins it down.
fn lint_distinct_on_without_order(stmt: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let tokens: Vec<_> = stmt
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| !t.kind().is_trivia())
        .collect();
    let distinct = tokens
        .iter()
        .position(|t| t.kind() == SyntaxKind::DISTINCT_KW);
    let Some(d) = distinct else { return };
    // `DISTINCT ON`: the next token after DISTINCT is ON.
    if tokens.get(d + 1).map(|t| t.kind()) != Some(SyntaxKind::ON_KW) {
        return;
    }
    if stmt
        .children()
        .any(|c| c.kind() == SyntaxKind::ORDER_BY_CLAUSE)
    {
        return;
    }
    let range = text_size::TextRange::new(
        tokens[d].text_range().start(),
        tokens[d + 1].text_range().end(),
    );
    analyzer.emit(
        Diagnostic::warning("DISTINCT ON without ORDER BY keeps an arbitrary row from each group")
            .with_code(RuleCode::St08)
            .with_range(range),
    );
}

/// ST01 — `ELSE NULL` in a `CASE` is redundant (the default result is NULL).
fn lint_redundant_else_null(case: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
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
fn lint_subquery_as_table(table_ref: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
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
fn lint_unused_ctes(root: &banshee_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
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
