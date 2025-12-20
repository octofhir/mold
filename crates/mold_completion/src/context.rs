//! Completion context detection.
//!
//! Analyzes the cursor position in a parse tree to determine
//! what kind of completions are appropriate.

use mold_syntax::{Parse, SyntaxKind, SyntaxNode, SyntaxToken};
use text_size::TextSize;

use crate::types::CompletionContext;

/// Detects the completion context at a given offset in the source.
pub fn detect_context(parse: &Parse, offset: TextSize) -> CompletionContext {
    let root = parse.syntax();

    // Find the token at or before the offset
    let token = find_token_at_offset(&root, offset);

    // Find the node containing the offset
    let node = find_node_at_offset(&root, offset);

    match (token.as_ref(), node.as_ref()) {
        (Some(token), Some(node)) => detect_context_from_token_and_node(token, node, offset),
        (Some(token), None) => detect_context_from_token(token, offset),
        (None, Some(node)) => detect_context_from_node(node, offset),
        (None, None) => detect_statement_start(&root, offset),
    }
}

/// Finds the token at or immediately before the given offset.
fn find_token_at_offset(root: &SyntaxNode, offset: TextSize) -> Option<SyntaxToken> {
    // Try to find a token that contains the offset
    let mut best_token: Option<SyntaxToken> = None;

    for token in root
        .descendants_with_tokens()
        .filter_map(|it| it.into_token())
    {
        let range = token.text_range();
        if range.contains(offset) || range.end() == offset {
            return Some(token.clone());
        }
        if range.end() <= offset {
            best_token = Some(token.clone());
        }
    }

    best_token
}

/// Finds the deepest node containing the offset.
fn find_node_at_offset(root: &SyntaxNode, offset: TextSize) -> Option<SyntaxNode> {
    root.descendants()
        .filter(|n| n.text_range().contains(offset))
        .last()
        .cloned()
}

/// Detects context using both token and node information.
fn detect_context_from_token_and_node(
    token: &SyntaxToken,
    node: &SyntaxNode,
    offset: TextSize,
) -> CompletionContext {
    let token_kind = token.kind();
    let node_kind = node.kind();

    // Check for JSONB access context
    if is_jsonb_context(token_kind, node_kind) {
        return detect_jsonb_context(token, node);
    }

    // Check for function argument context
    if node_kind == SyntaxKind::ARG_LIST || node_kind == SyntaxKind::FUNC_CALL {
        if let Some(ctx) = detect_function_arg_context(node) {
            return ctx;
        }
    }

    // Check by parent node type
    match node_kind {
        SyntaxKind::SELECT_ITEM | SyntaxKind::SELECT_ITEM_LIST | SyntaxKind::SELECT_CLAUSE => {
            detect_select_column_context(node)
        }
        SyntaxKind::FROM_CLAUSE | SyntaxKind::TABLE_REF | SyntaxKind::TABLE_NAME => {
            detect_table_name_context(token)
        }
        SyntaxKind::WHERE_CLAUSE => detect_where_context(node),
        SyntaxKind::JOIN_CONDITION => detect_join_condition_context(node),
        SyntaxKind::ORDER_BY_CLAUSE | SyntaxKind::ORDER_BY_ITEM => detect_order_by_context(node),
        SyntaxKind::COLUMN_REF | SyntaxKind::QUALIFIED_NAME => {
            detect_column_ref_context(token, node)
        }
        SyntaxKind::TYPE_NAME => CompletionContext::TypeName,
        _ => detect_from_token(token, offset),
    }
}

/// Checks if we're in a JSONB access context.
fn is_jsonb_context(token_kind: SyntaxKind, node_kind: SyntaxKind) -> bool {
    token_kind.is_jsonb_operator()
        || node_kind == SyntaxKind::JSONB_ACCESS_EXPR
        || node_kind == SyntaxKind::JSONB_PATH_EXPR
}

/// Detects JSONB completion context.
fn detect_jsonb_context(token: &SyntaxToken, node: &SyntaxNode) -> CompletionContext {
    // Extract the base column and current path from the JSONB expression
    let (column, path) = extract_jsonb_path(node);

    if token.kind() == SyntaxKind::ARROW || token.kind() == SyntaxKind::ARROW_TEXT {
        // Just after -> or ->>
        CompletionContext::JsonbField { column, path }
    } else {
        CompletionContext::JsonbField { column, path }
    }
}

/// Extracts the column name and path from a JSONB access expression.
fn extract_jsonb_path(node: &SyntaxNode) -> (String, Vec<String>) {
    let mut column = String::new();
    let mut path = Vec::new();

    // Walk up to find the complete JSONB chain
    let mut current = node.clone();
    loop {
        if current.kind() == SyntaxKind::JSONB_ACCESS_EXPR {
            // Extract the key from this access
            for child in current.children_with_tokens() {
                if let Some(token) = child.as_token() {
                    if token.kind() == SyntaxKind::STRING {
                        let text = token.text().to_string();
                        // Remove quotes
                        let key = text.trim_matches('\'').to_string();
                        path.insert(0, key);
                    }
                }
            }
        } else if current.kind() == SyntaxKind::COLUMN_REF {
            // This is the base column
            column = extract_column_name(&current);
            break;
        }

        if let Some(parent) = current.parent() {
            current = parent.clone();
        } else {
            break;
        }
    }

    (column, path)
}

/// Extracts a column name from a COLUMN_REF node.
fn extract_column_name(node: &SyntaxNode) -> String {
    for child in node.children_with_tokens() {
        if let Some(token) = child.as_token() {
            if token.kind() == SyntaxKind::IDENT || token.kind() == SyntaxKind::QUOTED_IDENT {
                return token.text().to_string();
            }
        }
    }
    String::new()
}

/// Detects function argument context.
fn detect_function_arg_context(node: &SyntaxNode) -> Option<CompletionContext> {
    // Find the function name
    let func_call = if node.kind() == SyntaxKind::FUNC_CALL {
        node.clone()
    } else {
        node.ancestors()
            .find(|n| n.kind() == SyntaxKind::FUNC_CALL)
            .cloned()?
    };

    let mut function_name = String::new();
    for child in func_call.children_with_tokens() {
        if let Some(token) = child.as_token() {
            if token.kind() == SyntaxKind::IDENT {
                function_name = token.text().to_string();
                break;
            }
        }
    }

    if function_name.is_empty() {
        return None;
    }

    // Count the argument position
    let arg_list = func_call
        .children()
        .find(|c| c.kind() == SyntaxKind::ARG_LIST)
        .cloned()?;

    let mut arg_index = 0;
    for child in arg_list.children_with_tokens() {
        if let Some(token) = child.as_token() {
            if token.kind() == SyntaxKind::COMMA {
                arg_index += 1;
            }
        }
    }

    Some(CompletionContext::FunctionArg {
        function: function_name,
        arg_index,
    })
}

/// Detects SELECT column context.
fn detect_select_column_context(node: &SyntaxNode) -> CompletionContext {
    // Find tables from FROM clause
    let tables = find_tables_in_scope(node);
    CompletionContext::SelectColumn { tables }
}

/// Detects table name context.
fn detect_table_name_context(token: &SyntaxToken) -> CompletionContext {
    // Check if there's a schema prefix
    let schema = find_schema_prefix(token);
    CompletionContext::TableName { schema }
}

/// Finds a schema prefix before the current token.
fn find_schema_prefix(token: &SyntaxToken) -> Option<String> {
    // Look for pattern: IDENT DOT <cursor>
    let mut prev = token.prev_token();
    while let Some(t) = prev {
        if t.kind() == SyntaxKind::DOT {
            // Look for the schema name before the dot
            if let Some(ident) = t.prev_token() {
                if ident.kind() == SyntaxKind::IDENT || ident.kind() == SyntaxKind::QUOTED_IDENT {
                    return Some(ident.text().to_string());
                }
            }
        } else if !t.kind().is_trivia() {
            break;
        }
        prev = t.prev_token();
    }
    None
}

/// Detects WHERE clause context.
fn detect_where_context(node: &SyntaxNode) -> CompletionContext {
    let tables = find_tables_in_scope(node);
    CompletionContext::WhereCondition { tables }
}

/// Detects JOIN condition context.
fn detect_join_condition_context(node: &SyntaxNode) -> CompletionContext {
    // Find the JOIN_EXPR and extract table names
    let (left_table, right_table) = find_join_tables(node);
    CompletionContext::JoinCondition {
        left_table,
        right_table,
    }
}

/// Finds the tables involved in a JOIN.
fn find_join_tables(node: &SyntaxNode) -> (String, String) {
    let join_expr = node.ancestors().find(|n| n.kind() == SyntaxKind::JOIN_EXPR);

    if let Some(join) = join_expr {
        let mut tables = Vec::new();
        for child in join.children() {
            if child.kind() == SyntaxKind::TABLE_REF || child.kind() == SyntaxKind::TABLE_NAME {
                if let Some(name) = extract_table_name(&child) {
                    tables.push(name);
                }
            }
        }
        if tables.len() >= 2 {
            return (tables[0].clone(), tables[1].clone());
        }
    }

    (String::new(), String::new())
}

/// Extracts a table name from a TABLE_REF or TABLE_NAME node.
fn extract_table_name(node: &SyntaxNode) -> Option<String> {
    // Look for alias first
    for child in node.children() {
        if child.kind() == SyntaxKind::ALIAS {
            for token in child.children_with_tokens().filter_map(|c| c.into_token()) {
                if token.kind() == SyntaxKind::IDENT || token.kind() == SyntaxKind::QUOTED_IDENT {
                    return Some(token.text().to_string());
                }
            }
        }
    }

    // Fall back to table name
    for token in node.children_with_tokens().filter_map(|c| c.into_token()) {
        if token.kind() == SyntaxKind::IDENT || token.kind() == SyntaxKind::QUOTED_IDENT {
            return Some(token.text().to_string());
        }
    }

    // Check nested TABLE_NAME
    for child in node.children() {
        if child.kind() == SyntaxKind::TABLE_NAME {
            return extract_table_name(&child);
        }
    }

    None
}

/// Detects ORDER BY context.
fn detect_order_by_context(node: &SyntaxNode) -> CompletionContext {
    let tables = find_tables_in_scope(node);
    CompletionContext::ColumnRef {
        table: None,
        tables,
    }
}

/// Detects column reference context.
fn detect_column_ref_context(token: &SyntaxToken, node: &SyntaxNode) -> CompletionContext {
    // Check if there's a table qualifier
    let table = find_table_qualifier(token);
    let tables = find_tables_in_scope(node);

    CompletionContext::ColumnRef { table, tables }
}

/// Finds a table qualifier before the current position.
fn find_table_qualifier(token: &SyntaxToken) -> Option<String> {
    // Same as find_schema_prefix - look for IDENT DOT pattern
    find_schema_prefix(token)
}

/// Finds all tables in scope for the given node.
fn find_tables_in_scope(node: &SyntaxNode) -> Vec<String> {
    let mut tables = Vec::new();

    // Find the nearest SELECT statement
    let select = node
        .ancestors()
        .find(|n| n.kind() == SyntaxKind::SELECT_STMT);

    if let Some(select) = select {
        // Find FROM clause
        for child in select.children() {
            if child.kind() == SyntaxKind::FROM_CLAUSE {
                collect_tables_from_clause(&child, &mut tables);
            }
        }

        // Also check for CTEs in WITH clause
        for child in select.children() {
            if child.kind() == SyntaxKind::WITH_CLAUSE {
                collect_ctes(&child, &mut tables);
            }
        }
    }

    tables
}

/// Collects table names from a FROM clause.
fn collect_tables_from_clause(from_clause: &SyntaxNode, tables: &mut Vec<String>) {
    for child in from_clause.descendants() {
        if child.kind() == SyntaxKind::TABLE_REF || child.kind() == SyntaxKind::TABLE_NAME {
            if let Some(name) = extract_table_name(&child) {
                if !tables.contains(&name) {
                    tables.push(name);
                }
            }
        }
    }
}

/// Collects CTE names from a WITH clause.
fn collect_ctes(with_clause: &SyntaxNode, tables: &mut Vec<String>) {
    for child in with_clause.descendants() {
        if child.kind() == SyntaxKind::CTE {
            for token in child.children_with_tokens().filter_map(|c| c.into_token()) {
                if token.kind() == SyntaxKind::IDENT {
                    let name = token.text().to_string();
                    if !tables.contains(&name) {
                        tables.push(name);
                    }
                    break;
                }
            }
        }
    }
}

/// Detects context from token alone.
fn detect_context_from_token(token: &SyntaxToken, offset: TextSize) -> CompletionContext {
    detect_from_token(token, offset)
}

/// Detects context from node alone.
fn detect_context_from_node(node: &SyntaxNode, _offset: TextSize) -> CompletionContext {
    match node.kind() {
        SyntaxKind::SELECT_STMT => CompletionContext::Statement,
        SyntaxKind::SELECT_CLAUSE => detect_select_column_context(node),
        SyntaxKind::FROM_CLAUSE => CompletionContext::TableName { schema: None },
        SyntaxKind::WHERE_CLAUSE => detect_where_context(node),
        _ => CompletionContext::Unknown,
    }
}

/// Detects context based on the token at cursor.
fn detect_from_token(token: &SyntaxToken, offset: TextSize) -> CompletionContext {
    let kind = token.kind();
    let range = token.text_range();

    // If we're at the end of a token, consider what comes next
    let at_token_end = range.end() == offset;

    // Check for incomplete keyword
    if kind == SyntaxKind::IDENT && at_token_end {
        // Could be a partial keyword
        return CompletionContext::Keyword {
            expected: get_expected_keywords(token),
        };
    }

    match kind {
        SyntaxKind::SELECT_KW => CompletionContext::SelectColumn { tables: Vec::new() },
        SyntaxKind::FROM_KW => CompletionContext::TableName { schema: None },
        SyntaxKind::WHERE_KW | SyntaxKind::AND_KW | SyntaxKind::OR_KW => {
            CompletionContext::WhereCondition { tables: Vec::new() }
        }
        SyntaxKind::JOIN_KW
        | SyntaxKind::LEFT_KW
        | SyntaxKind::RIGHT_KW
        | SyntaxKind::INNER_KW
        | SyntaxKind::OUTER_KW => CompletionContext::TableName { schema: None },
        SyntaxKind::ON_KW => CompletionContext::JoinCondition {
            left_table: String::new(),
            right_table: String::new(),
        },
        SyntaxKind::ORDER_KW => CompletionContext::Keyword {
            expected: vec!["BY".to_string()],
        },
        SyntaxKind::GROUP_KW => CompletionContext::Keyword {
            expected: vec!["BY".to_string()],
        },
        SyntaxKind::DOT => {
            // After a dot, we need column completion
            if let Some(table) = find_table_qualifier(token) {
                CompletionContext::ColumnRef {
                    table: Some(table),
                    tables: Vec::new(),
                }
            } else {
                CompletionContext::Unknown
            }
        }
        SyntaxKind::ARROW | SyntaxKind::ARROW_TEXT => CompletionContext::JsonbField {
            column: String::new(),
            path: Vec::new(),
        },
        _ if kind.is_keyword() => {
            // After a keyword, suggest what typically follows
            CompletionContext::Keyword {
                expected: get_keywords_after(kind),
            }
        }
        _ => CompletionContext::Unknown,
    }
}

/// Detects statement start context.
fn detect_statement_start(root: &SyntaxNode, _offset: TextSize) -> CompletionContext {
    // At the start or in an empty file
    if root.children().count() == 0 {
        return CompletionContext::Statement;
    }

    CompletionContext::Statement
}

/// Gets expected keywords based on context.
fn get_expected_keywords(token: &SyntaxToken) -> Vec<String> {
    let text = token.text().to_uppercase();

    // Suggest keywords that start with the current text
    let keywords = [
        "SELECT",
        "FROM",
        "WHERE",
        "AND",
        "OR",
        "JOIN",
        "LEFT",
        "RIGHT",
        "INNER",
        "OUTER",
        "ON",
        "ORDER",
        "BY",
        "GROUP",
        "HAVING",
        "LIMIT",
        "OFFSET",
        "INSERT",
        "UPDATE",
        "DELETE",
        "INTO",
        "VALUES",
        "SET",
        "CREATE",
        "DROP",
        "ALTER",
        "TABLE",
        "INDEX",
        "VIEW",
        "WITH",
        "AS",
        "DISTINCT",
        "ALL",
        "UNION",
        "INTERSECT",
        "EXCEPT",
        "CASE",
        "WHEN",
        "THEN",
        "ELSE",
        "END",
        "NULL",
        "NOT",
        "IN",
        "EXISTS",
        "BETWEEN",
        "LIKE",
        "ILIKE",
        "IS",
        "TRUE",
        "FALSE",
        "ASC",
        "DESC",
        "NULLS",
        "FIRST",
        "LAST",
        "RETURNING",
        "CASCADE",
        "RESTRICT",
    ];

    keywords
        .iter()
        .filter(|kw| kw.starts_with(&text))
        .map(|s| s.to_string())
        .collect()
}

/// Gets keywords that typically follow a given keyword.
fn get_keywords_after(kind: SyntaxKind) -> Vec<String> {
    match kind {
        SyntaxKind::SELECT_KW => vec!["DISTINCT".to_string(), "ALL".to_string(), "*".to_string()],
        SyntaxKind::FROM_KW => vec![],
        SyntaxKind::WHERE_KW => vec!["NOT".to_string(), "EXISTS".to_string()],
        SyntaxKind::ORDER_KW => vec!["BY".to_string()],
        SyntaxKind::GROUP_KW => vec!["BY".to_string()],
        SyntaxKind::INSERT_KW => vec!["INTO".to_string()],
        SyntaxKind::UPDATE_KW => vec![],
        SyntaxKind::DELETE_KW => vec!["FROM".to_string()],
        SyntaxKind::CREATE_KW => vec![
            "TABLE".to_string(),
            "INDEX".to_string(),
            "VIEW".to_string(),
            "MATERIALIZED".to_string(),
        ],
        SyntaxKind::DROP_KW => vec!["TABLE".to_string(), "INDEX".to_string(), "VIEW".to_string()],
        SyntaxKind::LEFT_KW | SyntaxKind::RIGHT_KW | SyntaxKind::FULL_KW => {
            vec!["OUTER".to_string(), "JOIN".to_string()]
        }
        SyntaxKind::INNER_KW | SyntaxKind::CROSS_KW | SyntaxKind::NATURAL_KW => {
            vec!["JOIN".to_string()]
        }
        SyntaxKind::OUTER_KW => vec!["JOIN".to_string()],
        _ => vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_types() {
        // Basic type checks
        let ctx = CompletionContext::Statement;
        assert!(matches!(ctx, CompletionContext::Statement));

        let ctx = CompletionContext::SelectColumn {
            tables: vec!["users".to_string()],
        };
        if let CompletionContext::SelectColumn { tables } = ctx {
            assert_eq!(tables.len(), 1);
        }
    }

    #[test]
    fn test_extract_column_name() {
        // Would need a real syntax node to test
    }

    #[test]
    fn test_get_expected_keywords() {
        // Create a mock token scenario
        // For now, just test the keyword list function
        let keywords = get_keywords_after(SyntaxKind::SELECT_KW);
        assert!(keywords.contains(&"DISTINCT".to_string()));
    }

    #[test]
    fn test_context_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<CompletionContext>();
    }
}
