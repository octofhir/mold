//! UPDATE statement parser.

use crate::parser::Parser;
use mold_syntax::SyntaxKind;

use super::PAREN_RECOVERY;
use super::expressions::expr;
use super::insert::returning_clause;
use super::select::{select_stmt, table_ref};

/// Parse UPDATE statement.
///
/// ```text
/// UPDATE table SET col = expr, ...
/// UPDATE table SET col = expr FROM other_table WHERE ...
/// UPDATE table SET col = expr RETURNING ...
/// ```
pub fn update_stmt(p: &mut Parser<'_>) {
    let m = p.start();

    p.expect(SyntaxKind::UPDATE_KW);

    // Optional ONLY
    p.eat(SyntaxKind::ONLY_KW);

    // Table name
    table_name(p);

    // Optional alias
    if p.eat(SyntaxKind::AS_KW) {
        p.expect(SyntaxKind::IDENT);
    } else if p.at(SyntaxKind::IDENT) && !is_update_keyword(p.current()) {
        p.bump();
    }

    // SET clause
    set_clause(p);

    // FROM clause (PostgreSQL extension)
    if p.at(SyntaxKind::FROM_KW) {
        from_clause(p);
    }

    // WHERE clause
    if p.at(SyntaxKind::WHERE_KW) {
        where_clause(p);
    }

    // RETURNING clause
    if p.at(SyntaxKind::RETURNING_KW) {
        returning_clause(p);
    }

    m.complete(p, SyntaxKind::UPDATE_STMT);
}

fn table_name(p: &mut Parser<'_>) {
    let m = p.start();
    p.expect(SyntaxKind::IDENT);

    // Schema qualification
    if p.at(SyntaxKind::DOT) {
        p.bump();
        p.expect(SyntaxKind::IDENT);
    }

    m.complete(p, SyntaxKind::TABLE_REF);
}

fn set_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.expect(SyntaxKind::SET_KW);

    set_item(p);
    while p.eat(SyntaxKind::COMMA) {
        set_item(p);
    }

    m.complete(p, SyntaxKind::SET_CLAUSE);
}

fn set_item(p: &mut Parser<'_>) {
    let m = p.start();

    // Column name or (col1, col2, ...)
    if p.at(SyntaxKind::L_PAREN) {
        p.bump();
        p.expect(SyntaxKind::IDENT);
        while p.eat(SyntaxKind::COMMA) {
            p.expect(SyntaxKind::IDENT);
        }
        p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
    } else {
        p.expect(SyntaxKind::IDENT);
    }

    p.expect(SyntaxKind::EQ);

    // Value: expr, DEFAULT, or (subquery)
    if p.eat(SyntaxKind::DEFAULT_KW) {
        // DEFAULT
    } else if p.at(SyntaxKind::L_PAREN) && is_subquery_start(p) {
        // Row subquery
        p.bump();
        select_stmt(p);
        p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
    } else {
        expr(p);
    }

    m.complete(p, SyntaxKind::SET_ITEM);
}

fn from_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // FROM

    // Use the full table_ref from select.rs which supports subqueries and joins
    table_ref(p);
    while p.eat(SyntaxKind::COMMA) {
        table_ref(p);
    }

    m.complete(p, SyntaxKind::FROM_CLAUSE);
}

fn where_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // WHERE

    // WHERE CURRENT OF cursor_name (for cursor-based updates)
    if p.eat(SyntaxKind::CURRENT_KW) {
        p.expect(SyntaxKind::OF_KW);
        p.expect(SyntaxKind::IDENT);
    } else {
        expr(p);
    }

    m.complete(p, SyntaxKind::WHERE_CLAUSE);
}

fn is_subquery_start(p: &Parser<'_>) -> bool {
    if !p.at(SyntaxKind::L_PAREN) {
        return false;
    }
    p.nth(1) == SyntaxKind::SELECT_KW || p.nth(1) == SyntaxKind::WITH_KW
}

fn is_update_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SET_KW | SyntaxKind::FROM_KW | SyntaxKind::WHERE_KW | SyntaxKind::RETURNING_KW
    )
}
