pub mod delete;
pub mod expressions;
pub mod insert;
pub mod jsonpath;
pub mod jsonpath_lexer;
pub mod select;
pub mod update;

use crate::parser::Parser;
use mold_syntax::SyntaxKind;

pub fn root(p: &mut Parser<'_>) {
    let m = p.start();

    while !p.at_end() {
        statement(p);
        // Consume optional semicolon
        p.eat(SyntaxKind::SEMICOLON);
    }

    m.complete(p, SyntaxKind::SOURCE_FILE);
}

fn statement(p: &mut Parser<'_>) {
    // WITH can start a CTE for SELECT, INSERT, UPDATE, or DELETE
    if p.at(SyntaxKind::WITH_KW) {
        // Look ahead to determine which statement follows the CTE
        with_statement(p);
        return;
    }

    match p.current() {
        SyntaxKind::SELECT_KW => {
            select::select_stmt(p);
        }
        SyntaxKind::INSERT_KW => {
            insert::insert_stmt(p);
        }
        SyntaxKind::UPDATE_KW => {
            update::update_stmt(p);
        }
        SyntaxKind::DELETE_KW => {
            delete::delete_stmt(p);
        }
        _ => {
            if !p.at_end() {
                p.err_recover(
                    format!("expected statement, found {:?}", p.current()),
                    STMT_RECOVERY,
                );
            }
        }
    }
}

/// Handle WITH clause that can precede SELECT, INSERT, UPDATE, or DELETE.
fn with_statement(p: &mut Parser<'_>) {
    // For now, treat WITH as always leading to SELECT (existing behavior)
    // Data-modifying CTEs will need the CTE to contain INSERT/UPDATE/DELETE
    select::select_stmt(p);
}

use crate::token_set::TokenSet;

/// Recovery set for statement boundaries.
pub const STMT_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::SELECT_KW,
    SyntaxKind::INSERT_KW,
    SyntaxKind::UPDATE_KW,
    SyntaxKind::DELETE_KW,
    SyntaxKind::SEMICOLON,
]);

/// Recovery set for clause boundaries in SELECT statements.
pub const CLAUSE_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::FROM_KW,
    SyntaxKind::WHERE_KW,
    SyntaxKind::GROUP_KW,
    SyntaxKind::HAVING_KW,
    SyntaxKind::ORDER_KW,
    SyntaxKind::LIMIT_KW,
    SyntaxKind::OFFSET_KW,
    SyntaxKind::UNION_KW,
    SyntaxKind::INTERSECT_KW,
    SyntaxKind::EXCEPT_KW,
    SyntaxKind::SEMICOLON,
    SyntaxKind::R_PAREN,
]);

/// Recovery set for expression list boundaries.
pub const EXPR_LIST_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::COMMA,
    SyntaxKind::R_PAREN,
    SyntaxKind::R_BRACKET,
    SyntaxKind::FROM_KW,
    SyntaxKind::WHERE_KW,
    SyntaxKind::GROUP_KW,
    SyntaxKind::ORDER_KW,
    SyntaxKind::SEMICOLON,
]);

/// Recovery set for parenthesized expressions.
pub const PAREN_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::R_PAREN,
    SyntaxKind::COMMA,
    SyntaxKind::SEMICOLON,
]);

/// Recovery set for JOIN clauses.
pub const JOIN_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::JOIN_KW,
    SyntaxKind::LEFT_KW,
    SyntaxKind::RIGHT_KW,
    SyntaxKind::INNER_KW,
    SyntaxKind::FULL_KW,
    SyntaxKind::CROSS_KW,
    SyntaxKind::WHERE_KW,
    SyntaxKind::GROUP_KW,
    SyntaxKind::ORDER_KW,
    SyntaxKind::SEMICOLON,
]);

/// Recovery set for CASE expressions.
pub const CASE_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::WHEN_KW,
    SyntaxKind::THEN_KW,
    SyntaxKind::ELSE_KW,
    SyntaxKind::END_KW,
    SyntaxKind::R_PAREN,
    SyntaxKind::COMMA,
    SyntaxKind::FROM_KW,
    SyntaxKind::WHERE_KW,
    SyntaxKind::SEMICOLON,
]);

/// Recovery set for subquery boundaries.
pub const SUBQUERY_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::SELECT_KW,
    SyntaxKind::FROM_KW,
    SyntaxKind::WHERE_KW,
    SyntaxKind::GROUP_KW,
    SyntaxKind::ORDER_KW,
    SyntaxKind::UNION_KW,
    SyntaxKind::R_PAREN,
    SyntaxKind::SEMICOLON,
]);
