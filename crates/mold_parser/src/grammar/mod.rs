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

const STMT_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::SELECT_KW,
    SyntaxKind::INSERT_KW,
    SyntaxKind::UPDATE_KW,
    SyntaxKind::DELETE_KW,
    SyntaxKind::SEMICOLON,
]);
