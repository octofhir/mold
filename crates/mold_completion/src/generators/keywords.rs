//! Keyword completion generator.

use crate::types::{CompletionItem, CompletionItemKind};

/// Generates keyword completion items.
pub fn complete_keywords(expected: Option<&[String]>, prefix: Option<&str>) -> Vec<CompletionItem> {
    let keywords = match expected {
        Some(exp) if !exp.is_empty() => exp.to_vec(),
        _ => all_keywords(),
    };

    let prefix_upper = prefix.map(|p| p.to_uppercase());

    keywords
        .into_iter()
        .filter(|kw| {
            prefix_upper
                .as_ref()
                .is_none_or(|p| kw.to_uppercase().starts_with(p))
        })
        .map(|kw| {
            let sort_key = keyword_sort_key(&kw);
            CompletionItem::new(CompletionItemKind::Keyword, kw.clone())
                .with_sort_key(sort_key)
                .with_insert_text(format!("{} ", kw))
        })
        .collect()
}

/// Returns the sort key for a keyword.
fn keyword_sort_key(keyword: &str) -> String {
    // Prioritize commonly used keywords
    let priority = match keyword.to_uppercase().as_str() {
        "SELECT" | "FROM" | "WHERE" | "AND" | "OR" | "JOIN" | "ON" => 0,
        "LEFT" | "RIGHT" | "INNER" | "OUTER" | "ORDER" | "BY" | "GROUP" => 1,
        "INSERT" | "UPDATE" | "DELETE" | "INTO" | "VALUES" | "SET" => 2,
        "CREATE" | "ALTER" | "DROP" | "TABLE" | "INDEX" | "VIEW" => 3,
        _ => 5,
    };
    format!("{}_{}", priority, keyword.to_lowercase())
}

/// Returns all SQL keywords.
fn all_keywords() -> Vec<String> {
    vec![
        // DML keywords
        "SELECT".to_string(),
        "FROM".to_string(),
        "WHERE".to_string(),
        "AND".to_string(),
        "OR".to_string(),
        "NOT".to_string(),
        "IN".to_string(),
        "EXISTS".to_string(),
        "BETWEEN".to_string(),
        "LIKE".to_string(),
        "ILIKE".to_string(),
        "IS".to_string(),
        "NULL".to_string(),
        "TRUE".to_string(),
        "FALSE".to_string(),
        // Joins
        "JOIN".to_string(),
        "LEFT".to_string(),
        "RIGHT".to_string(),
        "INNER".to_string(),
        "OUTER".to_string(),
        "FULL".to_string(),
        "CROSS".to_string(),
        "NATURAL".to_string(),
        "ON".to_string(),
        "USING".to_string(),
        // Ordering and grouping
        "ORDER".to_string(),
        "BY".to_string(),
        "ASC".to_string(),
        "DESC".to_string(),
        "NULLS".to_string(),
        "FIRST".to_string(),
        "LAST".to_string(),
        "GROUP".to_string(),
        "HAVING".to_string(),
        "LIMIT".to_string(),
        "OFFSET".to_string(),
        "FETCH".to_string(),
        // Set operations
        "UNION".to_string(),
        "INTERSECT".to_string(),
        "EXCEPT".to_string(),
        "ALL".to_string(),
        "DISTINCT".to_string(),
        // INSERT
        "INSERT".to_string(),
        "INTO".to_string(),
        "VALUES".to_string(),
        "DEFAULT".to_string(),
        "RETURNING".to_string(),
        // UPDATE
        "UPDATE".to_string(),
        "SET".to_string(),
        // DELETE
        "DELETE".to_string(),
        // WITH/CTE
        "WITH".to_string(),
        "RECURSIVE".to_string(),
        "AS".to_string(),
        // DDL
        "CREATE".to_string(),
        "ALTER".to_string(),
        "DROP".to_string(),
        "TABLE".to_string(),
        "INDEX".to_string(),
        "VIEW".to_string(),
        "MATERIALIZED".to_string(),
        "SCHEMA".to_string(),
        "DATABASE".to_string(),
        "SEQUENCE".to_string(),
        "FUNCTION".to_string(),
        "PROCEDURE".to_string(),
        "TRIGGER".to_string(),
        "TYPE".to_string(),
        // Constraints
        "PRIMARY".to_string(),
        "KEY".to_string(),
        "FOREIGN".to_string(),
        "REFERENCES".to_string(),
        "UNIQUE".to_string(),
        "CHECK".to_string(),
        "CONSTRAINT".to_string(),
        "CASCADE".to_string(),
        "RESTRICT".to_string(),
        // Expressions
        "CASE".to_string(),
        "WHEN".to_string(),
        "THEN".to_string(),
        "ELSE".to_string(),
        "END".to_string(),
        "CAST".to_string(),
        "COALESCE".to_string(),
        "NULLIF".to_string(),
        "GREATEST".to_string(),
        "LEAST".to_string(),
        // Window functions
        "OVER".to_string(),
        "PARTITION".to_string(),
        "ROWS".to_string(),
        "RANGE".to_string(),
        "UNBOUNDED".to_string(),
        "PRECEDING".to_string(),
        "FOLLOWING".to_string(),
        "CURRENT".to_string(),
        "ROW".to_string(),
        // JSONB operators (as keywords for completion)
        "JSONB".to_string(),
        "JSON".to_string(),
        // Transaction
        "BEGIN".to_string(),
        "COMMIT".to_string(),
        "ROLLBACK".to_string(),
        "SAVEPOINT".to_string(),
        // Locking
        "FOR".to_string(),
        "SHARE".to_string(),
        "NOWAIT".to_string(),
        "SKIP".to_string(),
        "LOCKED".to_string(),
        // Conflict handling
        "CONFLICT".to_string(),
        "DO".to_string(),
        "NOTHING".to_string(),
        // Misc
        "LATERAL".to_string(),
        "ONLY".to_string(),
        "IF".to_string(),
        "TEMPORARY".to_string(),
        "TEMP".to_string(),
        "UNLOGGED".to_string(),
        "CONCURRENTLY".to_string(),
        "EXPLAIN".to_string(),
        "ANALYZE".to_string(),
        "VERBOSE".to_string(),
    ]
}

/// Returns statement-starting keywords.
pub fn statement_keywords() -> Vec<CompletionItem> {
    let keywords = [
        ("SELECT", "Query data from tables"),
        ("INSERT", "Insert new rows"),
        ("UPDATE", "Modify existing rows"),
        ("DELETE", "Remove rows"),
        ("WITH", "Common Table Expression (CTE)"),
        ("CREATE", "Create database objects"),
        ("ALTER", "Modify database objects"),
        ("DROP", "Remove database objects"),
        ("TRUNCATE", "Remove all rows from a table"),
        ("EXPLAIN", "Show query execution plan"),
        ("BEGIN", "Start a transaction"),
        ("COMMIT", "Commit a transaction"),
        ("ROLLBACK", "Rollback a transaction"),
    ];

    keywords
        .iter()
        .map(|(kw, doc)| {
            CompletionItem::new(CompletionItemKind::Keyword, *kw)
                .with_documentation(doc.to_string())
                .with_sort_key(format!("0_{}", kw.to_lowercase()))
                .with_insert_text(format!("{} ", kw))
        })
        .collect()
}

/// Returns keywords that follow SELECT.
pub fn after_select_keywords() -> Vec<CompletionItem> {
    vec![
        CompletionItem::new(CompletionItemKind::Keyword, "DISTINCT")
            .with_documentation("Remove duplicate rows")
            .with_insert_text("DISTINCT "),
        CompletionItem::new(CompletionItemKind::Keyword, "ALL")
            .with_documentation("Include all rows (default)")
            .with_insert_text("ALL "),
        CompletionItem::new(CompletionItemKind::Keyword, "*")
            .with_documentation("Select all columns")
            .with_insert_text("* "),
    ]
}

/// Returns keywords that follow FROM.
pub fn after_from_keywords() -> Vec<CompletionItem> {
    vec![
        CompletionItem::new(CompletionItemKind::Keyword, "ONLY")
            .with_documentation("Exclude child tables")
            .with_insert_text("ONLY "),
        CompletionItem::new(CompletionItemKind::Keyword, "LATERAL")
            .with_documentation("Lateral subquery")
            .with_insert_text("LATERAL "),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complete_keywords_all() {
        let items = complete_keywords(None, None);
        assert!(!items.is_empty());
        assert!(items.iter().any(|i| i.label == "SELECT"));
        assert!(items.iter().any(|i| i.label == "FROM"));
    }

    #[test]
    fn test_complete_keywords_with_prefix() {
        let items = complete_keywords(None, Some("SEL"));
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "SELECT");
    }

    #[test]
    fn test_complete_keywords_expected() {
        let expected = vec!["FROM".to_string(), "WHERE".to_string()];
        let items = complete_keywords(Some(&expected), None);
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn test_statement_keywords() {
        let items = statement_keywords();
        assert!(items.iter().any(|i| i.label == "SELECT"));
        assert!(items.iter().any(|i| i.label == "INSERT"));
    }

    #[test]
    fn test_keyword_sort_order() {
        let items = complete_keywords(None, None);
        let select_item = items.iter().find(|i| i.label == "SELECT").unwrap();
        let create_item = items.iter().find(|i| i.label == "CREATE").unwrap();

        // SELECT should sort before CREATE
        assert!(select_item.effective_sort_key() < create_item.effective_sort_key());
    }
}
