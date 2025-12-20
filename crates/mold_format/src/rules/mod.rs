//! Formatting rules for SQL.
//!
//! This module contains rules for how different SQL constructs should be formatted.

pub mod keywords;

use mold_syntax::SyntaxKind;

/// Returns the keyword text for a syntax kind (for river alignment calculation).
pub fn keyword_text(kind: SyntaxKind) -> Option<&'static str> {
    keywords::keyword_text(kind)
}

/// Returns the river keyword for a clause (right-aligned keywords).
pub fn clause_keyword(kind: SyntaxKind) -> Option<&'static str> {
    match kind {
        SyntaxKind::SELECT_CLAUSE => Some("SELECT"),
        SyntaxKind::FROM_CLAUSE => Some("FROM"),
        SyntaxKind::WHERE_CLAUSE => Some("WHERE"),
        SyntaxKind::GROUP_BY_CLAUSE => Some("GROUP BY"),
        SyntaxKind::HAVING_CLAUSE => Some("HAVING"),
        SyntaxKind::ORDER_BY_CLAUSE => Some("ORDER BY"),
        SyntaxKind::LIMIT_CLAUSE => Some("LIMIT"),
        SyntaxKind::OFFSET_CLAUSE => Some("OFFSET"),
        SyntaxKind::WITH_CLAUSE => Some("WITH"),
        SyntaxKind::RETURNING_CLAUSE => Some("RETURNING"),
        SyntaxKind::SET_CLAUSE => Some("SET"),
        SyntaxKind::VALUES_CLAUSE => Some("VALUES"),
        SyntaxKind::USING_CLAUSE => Some("USING"),
        _ => None,
    }
}

/// Returns the join type keyword for formatting.
pub fn join_keyword(kind: SyntaxKind) -> Option<&'static str> {
    match kind {
        SyntaxKind::INNER_KW => Some("INNER JOIN"),
        SyntaxKind::LEFT_KW => Some("LEFT JOIN"),
        SyntaxKind::RIGHT_KW => Some("RIGHT JOIN"),
        SyntaxKind::FULL_KW => Some("FULL JOIN"),
        SyntaxKind::CROSS_KW => Some("CROSS JOIN"),
        SyntaxKind::NATURAL_KW => Some("NATURAL JOIN"),
        SyntaxKind::JOIN_KW => Some("JOIN"),
        _ => None,
    }
}

/// Returns true if the kind is a clause that starts a new line.
pub fn is_newline_clause(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SELECT_CLAUSE
            | SyntaxKind::FROM_CLAUSE
            | SyntaxKind::WHERE_CLAUSE
            | SyntaxKind::GROUP_BY_CLAUSE
            | SyntaxKind::HAVING_CLAUSE
            | SyntaxKind::ORDER_BY_CLAUSE
            | SyntaxKind::LIMIT_CLAUSE
            | SyntaxKind::OFFSET_CLAUSE
            | SyntaxKind::RETURNING_CLAUSE
            | SyntaxKind::SET_CLAUSE
            | SyntaxKind::VALUES_CLAUSE
            | SyntaxKind::USING_CLAUSE
    )
}

/// Returns true if the kind is a logical operator (AND/OR).
pub fn is_logical_operator(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::AND_KW | SyntaxKind::OR_KW)
}

/// Returns the width needed for river alignment of a clause.
pub fn calculate_river_width(keywords: &[&str]) -> usize {
    keywords.iter().map(|k| k.len()).max().unwrap_or(0)
}

/// Standard SQL clause keywords for river width calculation.
pub const STANDARD_CLAUSE_KEYWORDS: &[&str] = &[
    "SELECT", "FROM", "WHERE", "GROUP BY", "HAVING", "ORDER BY", "LIMIT", "OFFSET", "WITH",
    "RETURNING", "SET", "VALUES", "USING", "LEFT JOIN", "RIGHT JOIN", "INNER JOIN", "FULL JOIN",
    "CROSS JOIN", "ON", "AND", "OR",
];

/// Returns the recommended river width for standard SQL formatting.
pub fn standard_river_width() -> usize {
    // "LEFT JOIN" is 9 chars, "RETURNING" is 9 chars, add 1 for padding
    10
}
