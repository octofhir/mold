//! Keyword formatting rules.

use mold_syntax::SyntaxKind;

/// Returns the text representation of a keyword syntax kind.
pub fn keyword_text(kind: SyntaxKind) -> Option<&'static str> {
    match kind {
        SyntaxKind::SELECT_KW => Some("SELECT"),
        SyntaxKind::FROM_KW => Some("FROM"),
        SyntaxKind::WHERE_KW => Some("WHERE"),
        SyntaxKind::AND_KW => Some("AND"),
        SyntaxKind::OR_KW => Some("OR"),
        SyntaxKind::NOT_KW => Some("NOT"),
        SyntaxKind::AS_KW => Some("AS"),
        SyntaxKind::ON_KW => Some("ON"),
        SyntaxKind::IN_KW => Some("IN"),
        SyntaxKind::IS_KW => Some("IS"),
        SyntaxKind::NULL_KW => Some("NULL"),
        SyntaxKind::TRUE_KW => Some("TRUE"),
        SyntaxKind::FALSE_KW => Some("FALSE"),
        SyntaxKind::LIKE_KW => Some("LIKE"),
        SyntaxKind::ILIKE_KW => Some("ILIKE"),
        SyntaxKind::BETWEEN_KW => Some("BETWEEN"),
        SyntaxKind::EXISTS_KW => Some("EXISTS"),
        SyntaxKind::CASE_KW => Some("CASE"),
        SyntaxKind::WHEN_KW => Some("WHEN"),
        SyntaxKind::THEN_KW => Some("THEN"),
        SyntaxKind::ELSE_KW => Some("ELSE"),
        SyntaxKind::END_KW => Some("END"),
        SyntaxKind::DISTINCT_KW => Some("DISTINCT"),
        SyntaxKind::ALL_KW => Some("ALL"),
        SyntaxKind::JOIN_KW => Some("JOIN"),
        SyntaxKind::INNER_KW => Some("INNER"),
        SyntaxKind::LEFT_KW => Some("LEFT"),
        SyntaxKind::RIGHT_KW => Some("RIGHT"),
        SyntaxKind::FULL_KW => Some("FULL"),
        SyntaxKind::CROSS_KW => Some("CROSS"),
        SyntaxKind::OUTER_KW => Some("OUTER"),
        SyntaxKind::NATURAL_KW => Some("NATURAL"),
        SyntaxKind::USING_KW => Some("USING"),
        SyntaxKind::GROUP_KW => Some("GROUP"),
        SyntaxKind::BY_KW => Some("BY"),
        SyntaxKind::HAVING_KW => Some("HAVING"),
        SyntaxKind::ORDER_KW => Some("ORDER"),
        SyntaxKind::ASC_KW => Some("ASC"),
        SyntaxKind::DESC_KW => Some("DESC"),
        SyntaxKind::NULLS_KW => Some("NULLS"),
        SyntaxKind::FIRST_KW => Some("FIRST"),
        SyntaxKind::LAST_KW => Some("LAST"),
        SyntaxKind::LIMIT_KW => Some("LIMIT"),
        SyntaxKind::OFFSET_KW => Some("OFFSET"),
        SyntaxKind::FETCH_KW => Some("FETCH"),
        SyntaxKind::NEXT_KW => Some("NEXT"),
        SyntaxKind::ROW_KW => Some("ROW"),
        SyntaxKind::ROWS_KW => Some("ROWS"),
        SyntaxKind::ONLY_KW => Some("ONLY"),
        SyntaxKind::WITH_KW => Some("WITH"),
        SyntaxKind::RECURSIVE_KW => Some("RECURSIVE"),
        SyntaxKind::INSERT_KW => Some("INSERT"),
        SyntaxKind::INTO_KW => Some("INTO"),
        SyntaxKind::VALUES_KW => Some("VALUES"),
        SyntaxKind::DEFAULT_KW => Some("DEFAULT"),
        SyntaxKind::UPDATE_KW => Some("UPDATE"),
        SyntaxKind::SET_KW => Some("SET"),
        SyntaxKind::DELETE_KW => Some("DELETE"),
        SyntaxKind::RETURNING_KW => Some("RETURNING"),
        SyntaxKind::CONFLICT_KW => Some("CONFLICT"),
        SyntaxKind::DO_KW => Some("DO"),
        SyntaxKind::NOTHING_KW => Some("NOTHING"),
        SyntaxKind::CREATE_KW => Some("CREATE"),
        SyntaxKind::TABLE_KW => Some("TABLE"),
        SyntaxKind::VIEW_KW => Some("VIEW"),
        SyntaxKind::INDEX_KW => Some("INDEX"),
        SyntaxKind::UNIQUE_KW => Some("UNIQUE"),
        SyntaxKind::PRIMARY_KW => Some("PRIMARY"),
        SyntaxKind::KEY_KW => Some("KEY"),
        SyntaxKind::FOREIGN_KW => Some("FOREIGN"),
        SyntaxKind::REFERENCES_KW => Some("REFERENCES"),
        SyntaxKind::CHECK_KW => Some("CHECK"),
        SyntaxKind::CONSTRAINT_KW => Some("CONSTRAINT"),
        SyntaxKind::DROP_KW => Some("DROP"),
        SyntaxKind::ALTER_KW => Some("ALTER"),
        SyntaxKind::ADD_KW => Some("ADD"),
        SyntaxKind::COLUMN_KW => Some("COLUMN"),
        SyntaxKind::RENAME_KW => Some("RENAME"),
        SyntaxKind::TO_KW => Some("TO"),
        SyntaxKind::CASCADE_KW => Some("CASCADE"),
        SyntaxKind::RESTRICT_KW => Some("RESTRICT"),
        SyntaxKind::IF_KW => Some("IF"),
        SyntaxKind::UNION_KW => Some("UNION"),
        SyntaxKind::INTERSECT_KW => Some("INTERSECT"),
        SyntaxKind::EXCEPT_KW => Some("EXCEPT"),
        SyntaxKind::OVER_KW => Some("OVER"),
        SyntaxKind::PARTITION_KW => Some("PARTITION"),
        SyntaxKind::WINDOW_KW => Some("WINDOW"),
        SyntaxKind::RANGE_KW => Some("RANGE"),
        SyntaxKind::UNBOUNDED_KW => Some("UNBOUNDED"),
        SyntaxKind::PRECEDING_KW => Some("PRECEDING"),
        SyntaxKind::CURRENT_KW => Some("CURRENT"),
        SyntaxKind::FOLLOWING_KW => Some("FOLLOWING"),
        SyntaxKind::FILTER_KW => Some("FILTER"),
        SyntaxKind::LATERAL_KW => Some("LATERAL"),
        SyntaxKind::CAST_KW => Some("CAST"),
        SyntaxKind::COALESCE_KW => Some("COALESCE"),
        SyntaxKind::NULLIF_KW => Some("NULLIF"),
        SyntaxKind::GREATEST_KW => Some("GREATEST"),
        SyntaxKind::LEAST_KW => Some("LEAST"),
        SyntaxKind::ANY_KW => Some("ANY"),
        SyntaxKind::SOME_KW => Some("SOME"),
        SyntaxKind::ARRAY_KW => Some("ARRAY"),
        SyntaxKind::EXPLAIN_KW => Some("EXPLAIN"),
        SyntaxKind::ANALYZE_KW => Some("ANALYZE"),
        SyntaxKind::TRUNCATE_KW => Some("TRUNCATE"),
        _ => None,
    }
}

/// Returns true if the keyword is a reserved keyword that should always be uppercase.
pub fn is_reserved_keyword(kind: SyntaxKind) -> bool {
    kind.is_keyword()
}

/// Returns true if the keyword is a clause-starting keyword.
pub fn is_clause_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SELECT_KW
            | SyntaxKind::FROM_KW
            | SyntaxKind::WHERE_KW
            | SyntaxKind::GROUP_KW
            | SyntaxKind::HAVING_KW
            | SyntaxKind::ORDER_KW
            | SyntaxKind::LIMIT_KW
            | SyntaxKind::OFFSET_KW
            | SyntaxKind::WITH_KW
            | SyntaxKind::SET_KW
            | SyntaxKind::VALUES_KW
            | SyntaxKind::RETURNING_KW
            | SyntaxKind::USING_KW
    )
}

/// Returns true if the keyword should have a space before it.
pub fn needs_space_before(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AND_KW
            | SyntaxKind::OR_KW
            | SyntaxKind::AS_KW
            | SyntaxKind::ON_KW
            | SyntaxKind::BETWEEN_KW
            | SyntaxKind::LIKE_KW
            | SyntaxKind::ILIKE_KW
            | SyntaxKind::IN_KW
            | SyntaxKind::IS_KW
            | SyntaxKind::JOIN_KW
            | SyntaxKind::INNER_KW
            | SyntaxKind::LEFT_KW
            | SyntaxKind::RIGHT_KW
            | SyntaxKind::FULL_KW
            | SyntaxKind::CROSS_KW
            | SyntaxKind::NATURAL_KW
            | SyntaxKind::OUTER_KW
            // Clause keywords - need space before when appearing in subqueries
            | SyntaxKind::SELECT_KW
            | SyntaxKind::FROM_KW
            | SyntaxKind::WHERE_KW
            | SyntaxKind::GROUP_KW
            | SyntaxKind::HAVING_KW
            | SyntaxKind::ORDER_KW
            | SyntaxKind::LIMIT_KW
            | SyntaxKind::OFFSET_KW
            // Direction keywords - need space before
            | SyntaxKind::ASC_KW
            | SyntaxKind::DESC_KW
            | SyntaxKind::NULLS_KW
    )
}

/// Returns true if the keyword should have a space after it.
pub fn needs_space_after(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SELECT_KW
            | SyntaxKind::FROM_KW
            | SyntaxKind::WHERE_KW
            | SyntaxKind::AND_KW
            | SyntaxKind::OR_KW
            | SyntaxKind::AS_KW
            | SyntaxKind::ON_KW
            | SyntaxKind::NOT_KW
            | SyntaxKind::IN_KW
            | SyntaxKind::IS_KW
            | SyntaxKind::BETWEEN_KW
            | SyntaxKind::LIKE_KW
            | SyntaxKind::ILIKE_KW
            | SyntaxKind::JOIN_KW
            | SyntaxKind::INNER_KW
            | SyntaxKind::LEFT_KW
            | SyntaxKind::RIGHT_KW
            | SyntaxKind::FULL_KW
            | SyntaxKind::CROSS_KW
            | SyntaxKind::NATURAL_KW
            | SyntaxKind::OUTER_KW
            | SyntaxKind::GROUP_KW
            | SyntaxKind::ORDER_KW
            | SyntaxKind::BY_KW
            | SyntaxKind::HAVING_KW
            | SyntaxKind::LIMIT_KW
            | SyntaxKind::OFFSET_KW
            | SyntaxKind::CASE_KW
            | SyntaxKind::WHEN_KW
            | SyntaxKind::THEN_KW
            | SyntaxKind::ELSE_KW
            | SyntaxKind::SET_KW
            | SyntaxKind::VALUES_KW
            | SyntaxKind::INSERT_KW
            | SyntaxKind::INTO_KW
            | SyntaxKind::UPDATE_KW
            | SyntaxKind::DELETE_KW
            | SyntaxKind::RETURNING_KW
            | SyntaxKind::USING_KW
            | SyntaxKind::WITH_KW
            | SyntaxKind::RECURSIVE_KW
            | SyntaxKind::CREATE_KW
            | SyntaxKind::TABLE_KW
            | SyntaxKind::VIEW_KW
            | SyntaxKind::INDEX_KW
            | SyntaxKind::DROP_KW
            | SyntaxKind::ALTER_KW
            | SyntaxKind::ADD_KW
            | SyntaxKind::COLUMN_KW
            | SyntaxKind::RENAME_KW
            | SyntaxKind::TO_KW
            | SyntaxKind::UNION_KW
            | SyntaxKind::INTERSECT_KW
            | SyntaxKind::EXCEPT_KW
            | SyntaxKind::OVER_KW
            | SyntaxKind::PARTITION_KW
            | SyntaxKind::FILTER_KW
            | SyntaxKind::LATERAL_KW
            | SyntaxKind::CAST_KW
            | SyntaxKind::EXPLAIN_KW
            | SyntaxKind::ANALYZE_KW
            | SyntaxKind::TRUNCATE_KW
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_text() {
        assert_eq!(keyword_text(SyntaxKind::SELECT_KW), Some("SELECT"));
        assert_eq!(keyword_text(SyntaxKind::FROM_KW), Some("FROM"));
        assert_eq!(keyword_text(SyntaxKind::IDENT), None);
    }

    #[test]
    fn test_is_clause_keyword() {
        assert!(is_clause_keyword(SyntaxKind::SELECT_KW));
        assert!(is_clause_keyword(SyntaxKind::FROM_KW));
        assert!(!is_clause_keyword(SyntaxKind::AND_KW));
    }
}
