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
    AstNode, BinaryExpr, ColumnRef, DeleteStmt, Expr, FromClause, LiteralKind, SelectItem,
    SelectStmt, TableRef, UpdateStmt,
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
    if options.has_builtin_pack(BuiltinLintPack::Convention) {
        apply_convention_lints(root, analyzer);
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
        if let Some(select) = SelectStmt::cast(node.clone()) {
            lint_limit_without_order(&select, analyzer);
            lint_set_op_modifier(&select, analyzer);
            lint_aliasing_and_qualification(select.syntax(), analyzer);
            lint_unaliased_select_items(&select, analyzer);
        }
        if let Some(update) = UpdateStmt::cast(node.clone()) {
            lint_aliasing_and_qualification(update.syntax(), analyzer);
        }
        if let Some(delete) = DeleteStmt::cast(node.clone()) {
            lint_aliasing_and_qualification(delete.syntax(), analyzer);
        }
        if node.kind() == SyntaxKind::CASE_EXPR {
            lint_redundant_else_null(node, analyzer);
        }
        if node.kind() == SyntaxKind::TABLE_REF {
            lint_subquery_as_table(node, analyzer);
        }
    }
    lint_unused_ctes(root, analyzer);
}

/// AL05 (unused alias) and RF03 (mixed qualification) live together because
/// they share the same scan of a statement's tables and column qualifiers.
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

/// Convention checks: CV01 (`!=`/`<>` spelling), CV05 (NULL comparison),
/// CV06 (missing trailing semicolon).
fn apply_convention_lints(root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    for node in root.descendants() {
        if let Some(binary) = BinaryExpr::cast(node.clone()) {
            lint_ne_spelling(&binary, analyzer);
            lint_null_comparison(&binary, analyzer);
        }
    }
    lint_missing_semicolons(root, analyzer);
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

fn is_statement(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SELECT_STMT
            | SyntaxKind::INSERT_STMT
            | SyntaxKind::UPDATE_STMT
            | SyntaxKind::DELETE_STMT
    )
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

fn apply_jsonb_lints(root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    for node in root.descendants() {
        if let Some(binary) = BinaryExpr::cast(node.clone()) {
            lint_jsonb_text_comparison_binary(&binary, analyzer);
        }
    }
}

/// AM04 — `SELECT *` hides column count and breaks on schema drift. When the
/// query selects from a single known table, offers a fix expanding `*` to its
/// explicit column list.
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

/// Range of the `*` token in a select item, if it is a star item.
fn star_token_range(item: &SelectItem) -> Option<text_size::TextRange> {
    item.syntax()
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == SyntaxKind::STAR)
        .map(|t| t.text_range())
}

fn binary_has_operator(expr: &BinaryExpr, kinds: &[SyntaxKind]) -> bool {
    expr.syntax()
        .children_with_tokens()
        .filter_map(|child| child.into_token())
        .any(|token| kinds.contains(&token.kind()))
}
