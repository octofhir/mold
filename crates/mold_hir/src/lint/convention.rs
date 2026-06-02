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

/// CV11 — cast style (`::` vs `CAST`) should be consistent within a statement.
pub(super) struct CastStyle;

impl Rule for CastStyle {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv11]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            // Once per top-level statement, so a statement's casts compare only
            // against each other.
            if super::is_statement(node.kind())
                && node.parent().map(|p| p.kind()) == Some(SyntaxKind::SOURCE_FILE)
            {
                lint_cast_style(node, analyzer);
            }
        }
    }
}

/// CV11 — cast style. `prefer` may be `consistent` (default — the first cast's
/// style wins), `shorthand` (require `::`), or `functional` (require `CAST`).
fn lint_cast_style(stmt: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    // (is_shorthand, range) for each cast, in document order.
    let casts: Vec<(bool, text_size::TextRange)> = stmt
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::CAST_EXPR)
        .filter_map(|cast| {
            let mut shorthand = None;
            for t in cast.children_with_tokens().filter_map(|e| e.into_token()) {
                match t.kind() {
                    SyntaxKind::DOUBLE_COLON => shorthand = Some(true),
                    SyntaxKind::CAST_KW => shorthand = Some(false),
                    _ => {}
                }
            }
            shorthand.map(|s| (s, cast.text_range()))
        })
        .collect();
    if casts.is_empty() {
        return;
    }
    let canonical = match analyzer.rule_option("CV11", "prefer") {
        Some("shorthand") => true,
        Some("functional") => false,
        // "consistent" (default): the first cast sets the canonical style, so a
        // lone cast can never be inconsistent.
        _ => {
            if casts.len() < 2 {
                return;
            }
            casts[0].0
        }
    };
    let want = if canonical { "::" } else { "CAST(...)" };
    for (shorthand, range) in &casts {
        if *shorthand != canonical {
            analyzer.emit(
                Diagnostic::warning(format!("Inconsistent cast style; use the {want} form"))
                    .with_code(RuleCode::Cv11)
                    .with_range(*range),
            );
        }
    }
}

/// CV04 — `count(1)`/`count(0)` where `count(*)` is idiomatic.
pub(super) struct CountLiteral;

impl Rule for CountLiteral {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv04]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if node.kind() == SyntaxKind::FUNC_CALL {
                lint_count_literal(node, analyzer);
            }
        }
    }
}

/// CV04 — count-rows spelling. `prefer` may be `star` (default — `count(*)`),
/// `1` (`count(1)`) or `0` (`count(0)`). The non-preferred forms are rewritten.
fn lint_count_literal(func: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
    let is_count = func
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .any(|t| t.kind() == SyntaxKind::COUNT_KW);
    if !is_count {
        return;
    }
    // Reject more than one argument.
    if func
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .any(|t| t.kind() == SyntaxKind::COMMA)
    {
        return;
    }

    // Determine the current argument form and the range that spells it.
    let star = func
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::STAR);
    let (current, range) = if let Some(star) = star {
        ("*", star.text_range())
    } else if let Some(literal) = func.children().find(|c| c.kind() == SyntaxKind::LITERAL) {
        let int_tokens: Vec<_> = literal
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .filter(|t| !t.kind().is_trivia())
            .collect();
        if int_tokens.len() == 1
            && int_tokens[0].kind() == SyntaxKind::INTEGER
            && matches!(int_tokens[0].text(), "0" | "1")
        {
            (int_tokens[0].text(), literal.text_range())
        } else {
            return; // count(id), count(2), … are out of scope.
        }
    } else {
        return;
    };

    let target = match analyzer.rule_option("CV04", "prefer") {
        Some("1") => "1",
        Some("0") => "0",
        _ => "*", // "star" / default
    };
    if current == target {
        return;
    }
    let show = |s: &str| {
        if s == "*" {
            "count(*)".to_string()
        } else {
            format!("count({s})")
        }
    };
    analyzer.emit(
        Diagnostic::warning(format!("Use {} instead of {}", show(target), show(current)))
            .with_code(RuleCode::Cv04)
            .with_range(range)
            .with_fix(Fix::new(
                format!("Replace with {target}"),
                vec![TextEdit::replace(range, target)],
            )),
    );
}

/// CV09 — flag identifiers/keywords listed in the `blocked` option.
pub(super) struct BlockedWords;

impl Rule for BlockedWords {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Cv09]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Convention
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        let blocked = analyzer.rule_option_list("CV09", "blocked");
        if blocked.is_empty() {
            return;
        }
        let mut hits = Vec::new();
        for element in root.descendants_with_tokens() {
            let Some(token) = element.as_token() else {
                continue;
            };
            let kind = token.kind();
            if kind != SyntaxKind::IDENT && kind != SyntaxKind::QUOTED_IDENT && !kind.is_keyword() {
                continue;
            }
            let text = token.text().trim_matches('"');
            if blocked.iter().any(|b| b.eq_ignore_ascii_case(text)) {
                hits.push((text.to_string(), token.text_range()));
            }
        }
        for (word, range) in hits {
            analyzer.emit(
                Diagnostic::warning(format!("Use of blocked word '{word}'"))
                    .with_code(RuleCode::Cv09)
                    .with_range(range),
            );
        }
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
