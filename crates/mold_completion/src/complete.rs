//! Main completion logic.
//!
//! This module provides the main entry point for completion requests.

use mold_syntax::Parse;
use text_size::TextSize;

use crate::context::detect_context;
use crate::generators::{
    complete_columns, complete_functions, complete_jsonb_paths, complete_jsonpath,
    complete_keywords, complete_tables,
};
use crate::providers::{FunctionProvider, SchemaProvider};
use crate::types::{CompletionContext, CompletionItem, CompletionResult};

/// A completion request.
pub struct CompletionRequest<'a> {
    /// The source code being edited.
    pub source: &'a str,

    /// The cursor offset in the source.
    pub offset: TextSize,

    /// The parsed syntax tree (if available).
    pub parse: Option<&'a Parse>,

    /// Optional schema provider for table/column information.
    pub schema_provider: Option<&'a dyn SchemaProvider>,

    /// Optional function provider for function information.
    pub function_provider: Option<&'a dyn FunctionProvider>,

    /// Maximum number of items to return (0 = unlimited).
    pub limit: usize,
}

impl<'a> CompletionRequest<'a> {
    /// Creates a new completion request.
    pub fn new(source: &'a str, offset: TextSize) -> Self {
        Self {
            source,
            offset,
            parse: None,
            schema_provider: None,
            function_provider: None,
            limit: 100,
        }
    }

    /// Sets the parse tree.
    pub fn with_parse(mut self, parse: &'a Parse) -> Self {
        self.parse = Some(parse);
        self
    }

    /// Sets the schema provider.
    pub fn with_schema_provider(mut self, provider: &'a dyn SchemaProvider) -> Self {
        self.schema_provider = Some(provider);
        self
    }

    /// Sets the function provider.
    pub fn with_function_provider(mut self, provider: &'a dyn FunctionProvider) -> Self {
        self.function_provider = Some(provider);
        self
    }

    /// Sets the item limit.
    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }
}

/// Performs completion at the specified position.
pub fn complete(request: CompletionRequest<'_>) -> CompletionResult {
    // Detect the completion context
    let context = match request.parse {
        Some(parse) => detect_context(parse, request.offset),
        None => detect_context_from_text(request.source, request.offset),
    };

    // Generate completion items based on context
    let mut items = generate_completions(&context, &request);

    // Apply limit
    let is_incomplete = if request.limit > 0 && items.len() > request.limit {
        items.truncate(request.limit);
        true
    } else {
        false
    };

    // Sort items by sort key
    items.sort_by(|a, b| a.effective_sort_key().cmp(b.effective_sort_key()));

    CompletionResult {
        context,
        items,
        is_incomplete,
    }
}

/// Detects context from raw text when no parse tree is available.
fn detect_context_from_text(source: &str, offset: TextSize) -> CompletionContext {
    let offset_usize: usize = offset.into();
    let text_before = &source[..offset_usize.min(source.len())];

    // Find the last significant token
    let text_trimmed = text_before.trim_end();
    let last_word = text_trimmed
        .rsplit(|c: char| c.is_whitespace() || c == ',' || c == '(' || c == ')')
        .next()
        .unwrap_or("");

    // Detect context based on preceding keywords
    let upper = text_trimmed.to_uppercase();

    if upper.ends_with("SELECT") || upper.ends_with("SELECT DISTINCT") {
        return CompletionContext::SelectColumn { tables: Vec::new() };
    }

    if upper.ends_with("FROM") || upper.ends_with("JOIN") {
        return CompletionContext::TableName { schema: None };
    }

    if upper.ends_with("WHERE") || upper.ends_with("AND") || upper.ends_with("OR") {
        return CompletionContext::WhereCondition { tables: Vec::new() };
    }

    if upper.ends_with("ON") {
        return CompletionContext::JoinCondition {
            left_table: String::new(),
            right_table: String::new(),
        };
    }

    if upper.ends_with("ORDER BY") || upper.ends_with("GROUP BY") {
        return CompletionContext::ColumnRef {
            table: None,
            tables: Vec::new(),
        };
    }

    // Check for qualified name (has a dot)
    if last_word.contains('.') {
        let parts: Vec<&str> = last_word.split('.').collect();
        if parts.len() == 2 {
            let table = parts[0].to_string();
            return CompletionContext::ColumnRef {
                table: Some(table),
                tables: Vec::new(),
            };
        }
    }

    // Check for JSONB operator
    if text_trimmed.ends_with("->") || text_trimmed.ends_with("->>") {
        return CompletionContext::JsonbField {
            column: String::new(),
            path: Vec::new(),
        };
    }

    // Default: statement start or keyword
    if text_before.trim().is_empty() {
        CompletionContext::Statement
    } else {
        CompletionContext::Unknown
    }
}

/// Generates completion items based on context.
fn generate_completions(
    context: &CompletionContext,
    request: &CompletionRequest<'_>,
) -> Vec<CompletionItem> {
    match context {
        CompletionContext::Statement => {
            // Statement start keywords
            crate::generators::keywords::statement_keywords()
        }

        CompletionContext::SelectColumn { tables } => {
            let mut items = Vec::new();

            // Add columns from tables in scope
            items.extend(complete_columns(
                request.schema_provider,
                None,
                tables,
                None,
            ));

            // Add table names for qualified references
            items.extend(complete_tables(request.schema_provider, None, None));

            // Add functions
            items.extend(complete_functions(request.function_provider, None));

            // Add keywords like DISTINCT, CASE, etc.
            items.extend(crate::generators::keywords::after_select_keywords());

            items
        }

        CompletionContext::TableName { schema } => {
            let mut items = complete_tables(request.schema_provider, schema.as_deref(), None);

            // If no schema specified, also suggest schemas
            if schema.is_none() {
                items.extend(crate::generators::tables::complete_schemas(
                    request.schema_provider,
                ));
            }

            // Add keywords like LATERAL, ONLY
            items.extend(crate::generators::keywords::after_from_keywords());

            items
        }

        CompletionContext::ColumnRef { table, tables } => {
            match table {
                Some(t) => {
                    // Qualified column reference
                    complete_columns(request.schema_provider, Some(t), &[], None)
                }
                None => {
                    // Unqualified - show all columns in scope
                    let mut items = complete_columns(request.schema_provider, None, tables, None);

                    // Also add table names for qualification
                    for table_name in tables {
                        items.push(CompletionItem::new(
                            crate::types::CompletionItemKind::Table,
                            table_name,
                        ));
                    }

                    items
                }
            }
        }

        CompletionContext::JoinCondition {
            left_table,
            right_table,
        } => {
            let mut items = Vec::new();
            let tables = vec![left_table.clone(), right_table.clone()];

            // Suggest columns from both tables
            items.extend(complete_columns(
                request.schema_provider,
                None,
                &tables,
                None,
            ));

            // Add table aliases for qualified references
            if !left_table.is_empty() {
                items.push(CompletionItem::new(
                    crate::types::CompletionItemKind::Alias,
                    left_table,
                ));
            }
            if !right_table.is_empty() {
                items.push(CompletionItem::new(
                    crate::types::CompletionItemKind::Alias,
                    right_table,
                ));
            }

            items
        }

        CompletionContext::WhereCondition { tables } => {
            let mut items = Vec::new();

            // Add columns from tables in scope
            items.extend(complete_columns(
                request.schema_provider,
                None,
                tables,
                None,
            ));

            // Add functions for expressions
            items.extend(complete_functions(request.function_provider, None));

            // Add operators and keywords
            items.extend(complete_keywords(
                Some(&[
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
                ]),
                None,
            ));

            items
        }

        CompletionContext::JsonbField { column, path } => {
            let mut items = Vec::new();

            // Get JSONB path completions from schema
            items.extend(complete_jsonb_paths(
                request.schema_provider,
                None,
                column,
                path,
            ));

            // Add JSONB operators
            items.extend(crate::generators::jsonb::complete_jsonb_operators());

            items
        }

        CompletionContext::JsonPath { current_path } => complete_jsonpath(current_path),

        CompletionContext::FunctionArg {
            function,
            arg_index,
        } => {
            // Get function signature and suggest based on expected type
            if let Some(func_info) = crate::generators::functions::get_function_signature(
                request.function_provider,
                function,
            ) {
                if *arg_index < func_info.args.len() {
                    // Could suggest values based on argument type
                }
            }

            // Default to column/expression completion
            Vec::new()
        }

        CompletionContext::TypeName => {
            // SQL type names
            let types = [
                "integer",
                "bigint",
                "smallint",
                "text",
                "varchar",
                "char",
                "boolean",
                "timestamp",
                "timestamptz",
                "date",
                "time",
                "timetz",
                "interval",
                "numeric",
                "decimal",
                "real",
                "double precision",
                "json",
                "jsonb",
                "uuid",
                "bytea",
                "serial",
                "bigserial",
            ];

            types
                .iter()
                .map(|t| CompletionItem::new(crate::types::CompletionItemKind::Type, *t))
                .collect()
        }

        CompletionContext::Keyword { expected } => complete_keywords(Some(expected), None),

        CompletionContext::Unknown => {
            // Fallback: suggest keywords
            complete_keywords(None, None)
        }
    }
}

/// Gets the word being typed at the cursor position.
pub fn get_prefix_at_offset(source: &str, offset: TextSize) -> Option<String> {
    let offset_usize: usize = offset.into();
    if offset_usize > source.len() {
        return None;
    }

    let text_before = &source[..offset_usize];

    // Find the start of the current word
    let word_start = text_before
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);

    let prefix = &text_before[word_start..];
    if prefix.is_empty() {
        None
    } else {
        Some(prefix.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_completion_request() {
        let request = CompletionRequest::new("SELECT ", TextSize::new(7)).with_limit(50);

        assert_eq!(request.limit, 50);
        assert!(request.schema_provider.is_none());
    }

    #[test]
    fn test_get_prefix_at_offset() {
        assert_eq!(
            get_prefix_at_offset("SELECT us", TextSize::new(9)),
            Some("us".to_string())
        );
        assert_eq!(get_prefix_at_offset("SELECT ", TextSize::new(7)), None);
        assert_eq!(
            get_prefix_at_offset("SELECT users.na", TextSize::new(15)),
            Some("na".to_string())
        );
    }

    #[test]
    fn test_detect_context_from_text() {
        let ctx = detect_context_from_text("SELECT ", TextSize::new(7));
        assert!(matches!(ctx, CompletionContext::SelectColumn { .. }));

        let ctx = detect_context_from_text("SELECT * FROM ", TextSize::new(14));
        assert!(matches!(ctx, CompletionContext::TableName { .. }));

        let ctx = detect_context_from_text("SELECT * FROM users WHERE ", TextSize::new(26));
        assert!(matches!(ctx, CompletionContext::WhereCondition { .. }));
    }

    #[test]
    fn test_complete_without_providers() {
        let request = CompletionRequest::new("", TextSize::new(0));
        let result = complete(request);

        // Should return statement keywords
        assert!(!result.items.is_empty());
        assert!(result.items.iter().any(|i| i.label == "SELECT"));
    }

    #[test]
    fn test_complete_statement() {
        let request = CompletionRequest::new("", TextSize::new(0));
        let result = complete(request);

        assert!(matches!(result.context, CompletionContext::Statement));
        assert!(result.items.iter().any(|i| i.label == "SELECT"));
        assert!(result.items.iter().any(|i| i.label == "INSERT"));
    }

    #[test]
    fn test_complete_after_from() {
        let request = CompletionRequest::new("SELECT * FROM ", TextSize::new(14));
        let result = complete(request);

        assert!(matches!(
            result.context,
            CompletionContext::TableName { .. }
        ));
    }

    #[test]
    fn test_result_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<CompletionResult>();
    }
}
