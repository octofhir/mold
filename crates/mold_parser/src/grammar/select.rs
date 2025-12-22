use crate::parser::Parser;
use mold_syntax::SyntaxKind;

use super::expressions::expr;
use super::{CLAUSE_RECOVERY, JOIN_RECOVERY, PAREN_RECOVERY};

pub fn select_stmt(p: &mut Parser<'_>) {
    let m = p.start();

    // WITH clause (CTEs)
    if p.at(SyntaxKind::WITH_KW) {
        with_clause(p);
    }

    select_core(p);

    // Set operations: UNION, INTERSECT, EXCEPT
    while p.at(SyntaxKind::UNION_KW)
        || p.at(SyntaxKind::INTERSECT_KW)
        || p.at(SyntaxKind::EXCEPT_KW)
    {
        p.bump();
        let _ = p.eat(SyntaxKind::ALL_KW) || p.eat(SyntaxKind::DISTINCT_KW);
        select_core(p);
    }

    // ORDER BY (for the whole query)
    if p.at(SyntaxKind::ORDER_KW) {
        order_by_clause(p);
    }

    // LIMIT / OFFSET
    if p.at(SyntaxKind::LIMIT_KW) {
        limit_clause(p);
    }
    if p.at(SyntaxKind::OFFSET_KW) {
        offset_clause(p);
    }

    // FETCH (SQL standard)
    if p.at(SyntaxKind::FETCH_KW) {
        fetch_clause(p);
    }

    // FOR UPDATE/SHARE
    if p.at(SyntaxKind::FOR_KW) {
        for_clause(p);
    }

    m.complete(p, SyntaxKind::SELECT_STMT);
}

fn select_core(p: &mut Parser<'_>) {
    p.expect_recover(SyntaxKind::SELECT_KW, CLAUSE_RECOVERY);

    // DISTINCT / DISTINCT ON / ALL
    if p.eat(SyntaxKind::DISTINCT_KW) {
        if p.eat(SyntaxKind::ON_KW) {
            p.expect_recover(SyntaxKind::L_PAREN, PAREN_RECOVERY);
            expr(p);
            while p.eat(SyntaxKind::COMMA) {
                expr(p);
            }
            p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
        }
    } else {
        p.eat(SyntaxKind::ALL_KW);
    }

    // Select list
    select_list(p);

    // FROM clause
    if p.at(SyntaxKind::FROM_KW) {
        from_clause(p);
    }

    // WHERE clause
    if p.at(SyntaxKind::WHERE_KW) {
        where_clause(p);
    }

    // GROUP BY clause
    if p.at(SyntaxKind::GROUP_KW) {
        group_by_clause(p);
    }

    // HAVING clause
    if p.at(SyntaxKind::HAVING_KW) {
        having_clause(p);
    }

    // WINDOW clause
    if p.at(SyntaxKind::WINDOW_KW) {
        window_clause(p);
    }
}

fn with_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // WITH
    p.eat(SyntaxKind::RECURSIVE_KW);

    cte(p);
    while p.eat(SyntaxKind::COMMA) {
        cte(p);
    }

    m.complete(p, SyntaxKind::WITH_CLAUSE);
}

fn cte(p: &mut Parser<'_>) {
    let m = p.start();
    p.expect(SyntaxKind::IDENT);

    // Optional column list
    if p.at(SyntaxKind::L_PAREN) {
        p.bump();
        p.expect(SyntaxKind::IDENT);
        while p.eat(SyntaxKind::COMMA) {
            p.expect(SyntaxKind::IDENT);
        }
        p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
    }

    p.expect(SyntaxKind::AS_KW);

    // Optional MATERIALIZED / NOT MATERIALIZED hint
    if p.eat(SyntaxKind::NOT_KW) {
        p.expect(SyntaxKind::MATERIALIZED_KW);
    } else {
        p.eat(SyntaxKind::MATERIALIZED_KW);
    }

    p.expect_recover(SyntaxKind::L_PAREN, PAREN_RECOVERY);

    // CTE body: SELECT, INSERT, UPDATE, or DELETE
    match p.current() {
        SyntaxKind::SELECT_KW | SyntaxKind::WITH_KW => {
            select_stmt(p);
        }
        SyntaxKind::INSERT_KW => {
            super::insert::insert_stmt(p);
        }
        SyntaxKind::UPDATE_KW => {
            super::update::update_stmt(p);
        }
        SyntaxKind::DELETE_KW => {
            super::delete::delete_stmt(p);
        }
        _ => {
            p.error("expected SELECT, INSERT, UPDATE, or DELETE in CTE body");
        }
    }

    p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);

    m.complete(p, SyntaxKind::CTE);
}

fn select_list(p: &mut Parser<'_>) {
    let m = p.start();

    select_item(p);
    while p.eat(SyntaxKind::COMMA) {
        select_item(p);
    }

    m.complete(p, SyntaxKind::SELECT_ITEM_LIST);
}

fn select_item(p: &mut Parser<'_>) {
    let m = p.start();

    expr(p);

    // Optional alias
    if p.eat(SyntaxKind::AS_KW) {
        p.expect(SyntaxKind::IDENT);
    } else if p.at(SyntaxKind::IDENT) && !is_clause_keyword(p.current()) {
        p.bump();
    }

    m.complete(p, SyntaxKind::SELECT_ITEM);
}

fn from_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // FROM

    table_ref(p);
    while p.eat(SyntaxKind::COMMA) {
        table_ref(p);
    }

    m.complete(p, SyntaxKind::FROM_CLAUSE);
}

fn table_ref(p: &mut Parser<'_>) {
    let m = p.start();

    // LATERAL
    p.eat(SyntaxKind::LATERAL_KW);

    // ONLY
    p.eat(SyntaxKind::ONLY_KW);

    // Table or subquery or function call
    if p.at(SyntaxKind::L_PAREN) {
        p.bump();
        if p.at(SyntaxKind::SELECT_KW) || p.at(SyntaxKind::WITH_KW) {
            select_stmt(p);
        } else {
            // Joined table in parens
            table_ref(p);
        }
        p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
    } else {
        // Table name or function call
        p.expect_recover(SyntaxKind::IDENT, JOIN_RECOVERY);

        // Schema qualification
        if p.at(SyntaxKind::DOT) {
            p.bump();
            p.expect(SyntaxKind::IDENT);
        }

        // Function call in FROM clause
        if p.at(SyntaxKind::L_PAREN) {
            p.bump();
            if !p.at(SyntaxKind::R_PAREN) {
                expr(p);
                while p.eat(SyntaxKind::COMMA) {
                    expr(p);
                }
            }
            p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
        }
    }

    // Alias
    if p.eat(SyntaxKind::AS_KW) {
        p.expect(SyntaxKind::IDENT);
        // Column aliases for functions: AS alias(col1, col2)
        if p.at(SyntaxKind::L_PAREN) {
            p.bump();
            p.expect(SyntaxKind::IDENT);
            while p.eat(SyntaxKind::COMMA) {
                p.expect(SyntaxKind::IDENT);
            }
            p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
        }
    } else if p.at(SyntaxKind::IDENT)
        && !is_join_keyword(p.current())
        && !is_clause_keyword(p.current())
    {
        p.bump();
        if p.at(SyntaxKind::L_PAREN) {
            p.bump();
            p.expect(SyntaxKind::IDENT);
            while p.eat(SyntaxKind::COMMA) {
                p.expect(SyntaxKind::IDENT);
            }
            p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
        }
    }

    let mut lhs = m.complete(p, SyntaxKind::TABLE_REF);

    // Joins
    loop {
        if is_join_start(p) {
            lhs = join_expr(p, lhs);
        } else {
            break;
        }
    }
}

fn is_join_start(p: &Parser<'_>) -> bool {
    matches!(
        p.current(),
        SyntaxKind::JOIN_KW
            | SyntaxKind::INNER_KW
            | SyntaxKind::LEFT_KW
            | SyntaxKind::RIGHT_KW
            | SyntaxKind::FULL_KW
            | SyntaxKind::CROSS_KW
            | SyntaxKind::NATURAL_KW
    )
}

fn is_join_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::JOIN_KW
            | SyntaxKind::INNER_KW
            | SyntaxKind::LEFT_KW
            | SyntaxKind::RIGHT_KW
            | SyntaxKind::FULL_KW
            | SyntaxKind::CROSS_KW
            | SyntaxKind::NATURAL_KW
            | SyntaxKind::ON_KW
            | SyntaxKind::USING_KW
    )
}

fn join_expr(
    p: &mut Parser<'_>,
    lhs: crate::parser::CompletedMarker,
) -> crate::parser::CompletedMarker {
    let m = lhs.precede(p);

    // NATURAL
    let natural = p.eat(SyntaxKind::NATURAL_KW);

    // Join type
    let cross = match p.current() {
        SyntaxKind::JOIN_KW | SyntaxKind::INNER_KW => {
            if p.at(SyntaxKind::INNER_KW) {
                p.bump();
            }
            p.expect_recover(SyntaxKind::JOIN_KW, JOIN_RECOVERY);
            false
        }
        SyntaxKind::LEFT_KW | SyntaxKind::RIGHT_KW | SyntaxKind::FULL_KW => {
            p.bump();
            p.eat(SyntaxKind::OUTER_KW);
            p.expect_recover(SyntaxKind::JOIN_KW, JOIN_RECOVERY);
            false
        }
        SyntaxKind::CROSS_KW => {
            p.bump();
            p.expect_recover(SyntaxKind::JOIN_KW, JOIN_RECOVERY);
            true
        }
        _ => {
            p.error("expected JOIN, LEFT/RIGHT/FULL JOIN, CROSS JOIN, or NATURAL JOIN");
            false
        }
    };

    // Right-hand table
    table_ref(p);

    // Join condition (not for CROSS JOIN or NATURAL JOIN)
    if !cross && !natural {
        if p.eat(SyntaxKind::ON_KW) {
            let cm = p.start();
            expr(p);
            cm.complete(p, SyntaxKind::JOIN_CONDITION);
        } else if p.eat(SyntaxKind::USING_KW) {
            let cm = p.start();
            p.expect_recover(SyntaxKind::L_PAREN, PAREN_RECOVERY);
            p.expect(SyntaxKind::IDENT);
            while p.eat(SyntaxKind::COMMA) {
                p.expect(SyntaxKind::IDENT);
            }
            p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
            cm.complete(p, SyntaxKind::JOIN_CONDITION);
        }
    }

    m.complete(p, SyntaxKind::JOIN_EXPR)
}

fn where_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // WHERE
    expr(p);
    m.complete(p, SyntaxKind::WHERE_CLAUSE);
}

fn group_by_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // GROUP
    p.expect(SyntaxKind::BY_KW);

    expr(p);
    while p.eat(SyntaxKind::COMMA) {
        expr(p);
    }

    m.complete(p, SyntaxKind::GROUP_BY_CLAUSE);
}

fn having_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // HAVING
    expr(p);
    m.complete(p, SyntaxKind::HAVING_CLAUSE);
}

fn window_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // WINDOW

    // window_name AS (window_definition)
    p.expect(SyntaxKind::IDENT);
    p.expect(SyntaxKind::AS_KW);
    p.expect_recover(SyntaxKind::L_PAREN, PAREN_RECOVERY);
    // Window specification parsed in expressions.rs
    p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);

    while p.eat(SyntaxKind::COMMA) {
        p.expect(SyntaxKind::IDENT);
        p.expect(SyntaxKind::AS_KW);
        p.expect_recover(SyntaxKind::L_PAREN, PAREN_RECOVERY);
        p.expect_recover(SyntaxKind::R_PAREN, PAREN_RECOVERY);
    }

    m.complete(p, SyntaxKind::WINDOW_SPEC);
}

fn order_by_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // ORDER
    p.expect(SyntaxKind::BY_KW);

    order_by_item(p);
    while p.eat(SyntaxKind::COMMA) {
        order_by_item(p);
    }

    m.complete(p, SyntaxKind::ORDER_BY_CLAUSE);
}

fn order_by_item(p: &mut Parser<'_>) {
    let m = p.start();
    expr(p);
    let _ = p.eat(SyntaxKind::ASC_KW) || p.eat(SyntaxKind::DESC_KW);
    if p.eat(SyntaxKind::NULLS_KW) {
        let _ = p.eat(SyntaxKind::FIRST_KW) || p.eat(SyntaxKind::LAST_KW);
    }
    m.complete(p, SyntaxKind::ORDER_BY_ITEM);
}

fn limit_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // LIMIT

    if p.eat(SyntaxKind::ALL_KW) {
        // LIMIT ALL
    } else {
        expr(p);
    }

    m.complete(p, SyntaxKind::LIMIT_CLAUSE);
}

fn offset_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // OFFSET
    expr(p);
    // Optional ROW/ROWS
    let _ = p.eat(SyntaxKind::ROW_KW) || p.eat(SyntaxKind::ROWS_KW);
    m.complete(p, SyntaxKind::OFFSET_CLAUSE);
}

fn fetch_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // FETCH
    let _ = p.eat(SyntaxKind::FIRST_KW) || p.eat(SyntaxKind::NEXT_KW);
    if !p.at(SyntaxKind::ROW_KW) && !p.at(SyntaxKind::ROWS_KW) {
        expr(p);
    }
    let _ = p.eat(SyntaxKind::ROW_KW) || p.eat(SyntaxKind::ROWS_KW);
    p.eat(SyntaxKind::ONLY_KW);
    m.complete(p, SyntaxKind::FETCH_CLAUSE);
}

fn for_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(); // FOR
    p.bump_any(); // UPDATE/SHARE/etc.
    // Optional OF table_name
    if p.eat(SyntaxKind::OF_KW) {
        p.expect(SyntaxKind::IDENT);
        while p.eat(SyntaxKind::COMMA) {
            p.expect(SyntaxKind::IDENT);
        }
    }
    // Optional NOWAIT/SKIP LOCKED
    p.eat(SyntaxKind::NOWAIT_KW);
    if p.eat(SyntaxKind::SKIP_KW) {
        // SKIP LOCKED - need to add LOCKED_KW if not present
        p.bump_any();
    }
    m.complete(p, SyntaxKind::FOR_CLAUSE);
}

fn is_clause_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::FROM_KW
            | SyntaxKind::WHERE_KW
            | SyntaxKind::GROUP_KW
            | SyntaxKind::HAVING_KW
            | SyntaxKind::ORDER_KW
            | SyntaxKind::LIMIT_KW
            | SyntaxKind::OFFSET_KW
            | SyntaxKind::FETCH_KW
            | SyntaxKind::FOR_KW
            | SyntaxKind::UNION_KW
            | SyntaxKind::INTERSECT_KW
            | SyntaxKind::EXCEPT_KW
            | SyntaxKind::WINDOW_KW
    )
}
