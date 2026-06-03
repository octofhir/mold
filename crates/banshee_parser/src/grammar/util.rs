//! Session and utility statements: transaction control, SET / SHOW / RESET,
//! EXPLAIN, COMMENT ON.
//!
//! Option-heavy tails (isolation levels, SET values, COMMENT targets) are
//! consumed positionally; they carry no nested grammar a linter needs.

use crate::parser::Parser;
use banshee_syntax::SyntaxKind;

use super::select::at_ident;

/// BEGIN / START TRANSACTION / COMMIT / END / ROLLBACK / ABORT / SAVEPOINT /
/// RELEASE.
pub fn transaction_stmt(p: &mut Parser<'_>) {
    let m = p.start();

    match p.current() {
        SyntaxKind::SAVEPOINT_KW => {
            p.bump();
            expect_name(p);
        }
        SyntaxKind::RELEASE_KW => {
            p.bump();
            p.eat(SyntaxKind::SAVEPOINT_KW);
            expect_name(p);
        }
        _ => {
            p.bump(); // BEGIN / START / COMMIT / END / ROLLBACK / ABORT
            // Trailing options: TRANSACTION, WORK, ISOLATION LEVEL …, AND CHAIN,
            // TO SAVEPOINT name.
            consume_to_semi(p);
        }
    }

    m.complete(p, SyntaxKind::TRANSACTION_STMT);
}

pub fn set_stmt(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // SET
    consume_to_semi(p);
    m.complete(p, SyntaxKind::SET_STMT);
}

pub fn show_stmt(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // SHOW
    consume_to_semi(p);
    m.complete(p, SyntaxKind::SHOW_STMT);
}

pub fn reset_stmt(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // RESET
    consume_to_semi(p);
    m.complete(p, SyntaxKind::RESET_STMT);
}

pub fn explain_stmt(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // EXPLAIN

    if p.at(SyntaxKind::L_PAREN) {
        // New-style option list: ( ANALYZE, BUFFERS, FORMAT json, … )
        consume_balanced_parens(p);
    } else {
        // Legacy bare options: ANALYZE / VERBOSE. Statements always start with a
        // keyword, so leading identifiers here are options, not the inner query.
        while p.at(SyntaxKind::ANALYZE_KW) || p.at(SyntaxKind::ANALYSE_KW) || at_ident(p) {
            p.bump();
        }
    }

    super::statement(p);
    m.complete(p, SyntaxKind::EXPLAIN_STMT);
}

pub fn comment_stmt(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // COMMENT
    p.expect(SyntaxKind::ON_KW);
    // object type + name … IS { 'text' | NULL }
    consume_to_semi(p);
    m.complete(p, SyntaxKind::COMMENT_STMT);
}

// ---------------------------------------------------------------------------

fn expect_name(p: &mut Parser<'_>) {
    if at_ident(p) {
        p.bump();
    } else {
        p.error("expected name");
    }
}

fn consume_to_semi(p: &mut Parser<'_>) {
    while !p.at_end() && !p.at(SyntaxKind::SEMICOLON) {
        p.bump_any();
    }
}

fn consume_balanced_parens(p: &mut Parser<'_>) {
    if !p.at(SyntaxKind::L_PAREN) {
        return;
    }
    let mut depth = 0i32;
    while !p.at_end() {
        match p.current() {
            SyntaxKind::L_PAREN => depth += 1,
            SyntaxKind::R_PAREN => {
                depth -= 1;
                p.bump();
                if depth == 0 {
                    break;
                }
                continue;
            }
            _ => {}
        }
        p.bump_any();
    }
}
