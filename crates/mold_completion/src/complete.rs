//! Main completion logic.
//!
//! This module provides the main entry point for completion requests.

use mold_syntax::Parse;
use text_size::TextSize;

/// Finds the valid UTF-8 char boundary at or before the given byte offset.
///
/// If the offset is already on a char boundary, returns it unchanged.
/// Otherwise, moves backwards to find the nearest valid boundary.
/// This prevents panics when slicing strings with multi-byte UTF-8 characters.
#[inline]
fn floor_char_boundary(s: &str, mut offset: usize) -> usize {
    if offset >= s.len() {
        return s.len();
    }
    // Move backwards until we find a char boundary
    while offset > 0 && !s.is_char_boundary(offset) {
        offset -= 1;
    }
    offset
}

/// Safely slices a string up to the given byte offset.
///
/// Returns the slice `&s[..offset]` but adjusted to the nearest valid
/// UTF-8 char boundary if `offset` falls in the middle of a multi-byte character.
#[inline]
fn safe_slice_to(s: &str, offset: usize) -> &str {
    let safe_offset = floor_char_boundary(s, offset);
    &s[..safe_offset]
}

use crate::context::{detect_context, find_cte_columns};
use crate::generators::{
    complete_columns, complete_functions, complete_jsonb_paths, complete_jsonpath,
    complete_keywords, complete_tables, get_jsonb_arg_completion, JsonbArgCompletion,
};
use crate::providers::{FunctionProvider, SchemaProvider};
use crate::triggers::{detect_jsonb_trigger, detect_jsonpath_in_string};
use crate::types::{CompletionContext, CompletionItem, CompletionResult};

/// A completion request.
#[must_use]
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
    let mut context = match request.parse {
        Some(parse) => detect_context(parse, request.offset),
        None => detect_context_from_text(request.source, request.offset),
    };

    if matches!(context, CompletionContext::Unknown) {
        context = detect_context_from_text(request.source, request.offset);
    }

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
    let text_before = safe_slice_to(source, offset_usize);

    // Check for JSONPath inside string literals first (highest priority)
    if let Some(trigger) = detect_jsonpath_in_string(text_before, text_before.len()) {
        return CompletionContext::JsonPath {
            current_path: trigger.path,
        };
    }

    // Check for JSONB operator triggers
    if let Some(trigger) = detect_jsonb_trigger(text_before, text_before.len()) {
        let (table, column, path) = extract_jsonb_base_from_text(text_before, trigger.position);
        return CompletionContext::JsonbField {
            table,
            column,
            path,
        };
    }

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

    if let Some(prefix) = table_context_prefix(source, offset) {
        return CompletionContext::TableName {
            schema: prefix.schema,
        };
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
            let tables = resolve_tables_in_scope(tables, request.source, request.offset);
            let prefix = get_prefix_at_offset(request.source, request.offset);

            // Add columns from tables in scope
            items.extend(complete_columns(
                request.schema_provider,
                None,
                &tables,
                prefix.as_deref(),
            ));

            // Add table names for qualified references
            items.extend(complete_tables(request.schema_provider, None, None));

            // Add functions
            items.extend(complete_functions(
                request.function_provider,
                prefix.as_deref(),
            ));

            // Add keywords like DISTINCT, CASE, etc.
            items.extend(crate::generators::keywords::after_select_keywords());

            items
        }

        CompletionContext::TableName { schema } => {
            let prefix = table_context_prefix(request.source, request.offset);
            let schema = prefix
                .as_ref()
                .and_then(|value| value.schema.as_deref())
                .or(schema.as_deref());
            let table_prefix = prefix
                .as_ref()
                .and_then(|value| value.table_prefix.as_deref());

            let mut items = complete_tables(request.schema_provider, schema, table_prefix);

            // If no schema specified, also suggest schemas
            if schema.is_none() && table_prefix.is_none() {
                items.extend(crate::generators::tables::complete_schemas(
                    request.schema_provider,
                ));
            }

            // Add keywords like LATERAL, ONLY
            items.extend(crate::generators::keywords::after_from_keywords());

            items
        }

        CompletionContext::ColumnRef { table, tables } => {
            let prefix = get_prefix_at_offset(request.source, request.offset);
            match table {
                Some(t) => {
                    // Qualified column reference
                    let mut items =
                        complete_columns(request.schema_provider, Some(t), &[], prefix.as_deref());

                    // If no columns from schema provider, check if it's a CTE
                    if items.is_empty()
                        && let Some(parse) = request.parse
                    {
                        let cte_columns = find_cte_columns(&parse.syntax(), t);
                        for col in cte_columns {
                            if prefix
                                .as_ref()
                                .is_none_or(|p| col.to_lowercase().starts_with(&p.to_lowercase()))
                            {
                                items.push(CompletionItem::new(
                                    crate::types::CompletionItemKind::Column,
                                    &col,
                                ));
                            }
                        }
                    }

                    // Fallback: try tables in scope
                    if items.is_empty() && !t.contains('.') {
                        let tables =
                            resolve_tables_in_scope(tables, request.source, request.offset);
                        if tables.len() == 1 {
                            items = complete_columns(
                                request.schema_provider,
                                Some(&tables[0]),
                                &[],
                                prefix.as_deref(),
                            );
                        }
                    }

                    items
                }
                None => {
                    // Unqualified - show all columns in scope
                    let tables = resolve_tables_in_scope(tables, request.source, request.offset);
                    let mut items =
                        complete_columns(request.schema_provider, None, &tables, prefix.as_deref());

                    // Add functions for expressions (jsonb_*, etc.)
                    items.extend(complete_functions(
                        request.function_provider,
                        prefix.as_deref(),
                    ));

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
            let prefix = get_prefix_at_offset(request.source, request.offset);
            items.extend(complete_columns(
                request.schema_provider,
                None,
                &tables,
                prefix.as_deref(),
            ));

            // Add functions for expressions (jsonb_*, etc.)
            items.extend(complete_functions(
                request.function_provider,
                prefix.as_deref(),
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
            let tables = resolve_tables_in_scope(tables, request.source, request.offset);
            let prefix = get_prefix_at_offset(request.source, request.offset);

            // Add columns from tables in scope
            items.extend(complete_columns(
                request.schema_provider,
                None,
                &tables,
                prefix.as_deref(),
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

        CompletionContext::JsonbField {
            table,
            column,
            path,
        } => {
            let mut items = Vec::new();

            // Use table from context if available, otherwise try to resolve from scope
            let resolved_table = match table {
                Some(t) => Some(t.clone()),
                None => {
                    let mut tables = resolve_tables_in_scope(&[], request.source, request.offset);
                    if tables.is_empty() {
                        tables = extract_tables_from_text_full(request.source);
                    }
                    tables.first().cloned()
                }
            };
            let table_ref = resolved_table.as_deref();

            // The path includes the current accessor being typed.
            // For `resource->'name'->''` the path is ["name", ""].
            // We need to pop the last element (the current accessor) and use it as prefix.
            // The remaining path is used for schema lookup.
            let mut path = path.clone();
            let current_accessor = path.pop().unwrap_or_default();

            // Get JSONB path completions from schema at the parent path
            items.extend(complete_jsonb_paths(
                request.schema_provider,
                table_ref,
                column,
                &path,
            ));

            // Add JSONB operators
            items.extend(crate::generators::jsonb::complete_jsonb_operators());

            // Filter completions by what the user has typed so far
            if !current_accessor.is_empty() {
                let prefix_lower = current_accessor.to_lowercase();
                items.retain(|item| {
                    if item.kind != crate::types::CompletionItemKind::JsonbPath {
                        return true;
                    }
                    item.label.to_lowercase().starts_with(&prefix_lower)
                });
            }

            items
        }

        CompletionContext::JsonPath { current_path } => complete_jsonpath(current_path),

        CompletionContext::FunctionArg {
            function,
            arg_index,
        } => {
            let mut items = Vec::new();
            let tables = resolve_tables_in_scope(&[], request.source, request.offset);
            let prefix = get_prefix_at_offset(request.source, request.offset);

            // Check if this is a JSONB function with special completion
            if let Some(arg_completion) = get_jsonb_arg_completion(function, *arg_index) {
                match arg_completion {
                    JsonbArgCompletion::JsonbColumn => {
                        // Suggest JSONB columns from tables in scope
                        items.extend(
                            complete_columns(
                                request.schema_provider,
                                None,
                                &tables,
                                prefix.as_deref(),
                            )
                            .into_iter()
                            .filter(|item| {
                                // Filter to only JSONB columns if we have type info
                                item.detail
                                    .as_ref()
                                    .is_none_or(|d| d.to_lowercase().contains("jsonb"))
                            }),
                        );
                        // If no JSONB-specific columns, show all columns
                        if items.is_empty() {
                            items.extend(complete_columns(
                                request.schema_provider,
                                None,
                                &tables,
                                prefix.as_deref(),
                            ));
                        }
                    }
                    JsonbArgCompletion::PathKeys => {
                        // Suggest field names as strings
                        // For now, suggest common field names or get from schema
                        // This would need the first argument's column info to look up JSONB schema
                        items.extend(crate::generators::jsonb::complete_common_field_names(
                            prefix.as_deref(),
                        ));
                    }
                    JsonbArgCompletion::JsonPath => {
                        // Suggest JSONPath syntax
                        items.extend(complete_jsonpath("$"));
                    }
                    JsonbArgCompletion::None => {
                        // Default to column/expression completion
                        items.extend(complete_columns(
                            request.schema_provider,
                            None,
                            &tables,
                            prefix.as_deref(),
                        ));
                    }
                }
            } else {
                // Non-JSONB function: default to column/expression completion
                items.extend(complete_columns(
                    request.schema_provider,
                    None,
                    &tables,
                    prefix.as_deref(),
                ));
            }

            items
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

        CompletionContext::WindowClause { tables } => {
            let mut items = Vec::new();

            // Add window clause keywords first
            items.extend(crate::generators::keywords::window_clause_keywords());

            // Add columns from tables in scope for PARTITION BY / ORDER BY
            let tables = resolve_tables_in_scope(tables, request.source, request.offset);
            let prefix = get_prefix_at_offset(request.source, request.offset);
            items.extend(complete_columns(
                request.schema_provider,
                None,
                &tables,
                prefix.as_deref(),
            ));

            items
        }

        CompletionContext::WindowPartition { tables } => {
            let mut items = Vec::new();
            let tables = resolve_tables_in_scope(tables, request.source, request.offset);
            let prefix = get_prefix_at_offset(request.source, request.offset);

            // Add columns from tables in scope
            items.extend(complete_columns(
                request.schema_provider,
                None,
                &tables,
                prefix.as_deref(),
            ));

            // Add table names for qualified references
            for table_name in &tables {
                items.push(CompletionItem::new(
                    crate::types::CompletionItemKind::Table,
                    table_name,
                ));
            }

            items
        }

        CompletionContext::WindowOrderDirection => {
            // Suggest sort direction keywords
            crate::generators::keywords::window_order_direction_keywords()
        }

        CompletionContext::WindowFrame => {
            // Suggest frame specification keywords
            crate::generators::keywords::window_frame_keywords()
        }

        CompletionContext::Unknown => {
            // Fallback: suggest keywords
            complete_keywords(None, None)
        }
    }
}

fn resolve_tables_in_scope(tables: &[String], source: &str, offset: TextSize) -> Vec<String> {
    if !tables.is_empty() {
        return tables.to_vec();
    }

    extract_tables_from_text(source, offset)
}

#[derive(Clone, Debug)]
struct TableContextPrefix {
    schema: Option<String>,
    table_prefix: Option<String>,
}

impl TableContextPrefix {
    fn new(schema: Option<String>, table_prefix: Option<String>) -> Self {
        Self {
            schema,
            table_prefix,
        }
    }

    fn parse(token: &str) -> Self {
        if let Some((schema, table)) = token.split_once('.') {
            let schema = schema.trim();
            let table = table.trim();
            return Self {
                schema: if schema.is_empty() {
                    None
                } else {
                    Some(schema.to_string())
                },
                table_prefix: if table.is_empty() {
                    None
                } else {
                    Some(table.to_string())
                },
            };
        }

        Self {
            schema: None,
            table_prefix: if token.is_empty() {
                None
            } else {
                Some(token.to_string())
            },
        }
    }
}

fn table_context_prefix(source: &str, offset: TextSize) -> Option<TableContextPrefix> {
    let offset: usize = offset.into();
    if offset == 0 || offset > source.len() {
        return None;
    }

    let prefix = safe_slice_to(source, offset).trim_end();
    if prefix.is_empty() {
        return None;
    }

    let (last_token, last_start) = last_word(prefix)?;
    let last_upper = last_token.to_uppercase();

    if is_table_keyword(&last_upper) {
        return Some(TableContextPrefix::new(None, None));
    }

    let prev = prefix[..last_start].trim_end();
    let (prev_word, _) = last_word(prev)?;
    let prev_upper = prev_word.to_uppercase();

    if is_table_keyword(&prev_upper) {
        Some(TableContextPrefix::parse(last_token))
    } else {
        None
    }
}

fn last_word(text: &str) -> Option<(&str, usize)> {
    let mut end = text.len();
    for (idx, ch) in text.char_indices().rev() {
        if ch.is_alphanumeric() || ch == '_' || ch == '.' {
            end = idx + ch.len_utf8();
            break;
        }
    }

    let mut start = end;
    for (idx, ch) in text[..end].char_indices().rev() {
        if ch.is_alphanumeric() || ch == '_' || ch == '.' {
            start = idx;
            continue;
        }
        start = idx + ch.len_utf8();
        break;
    }

    if start >= end {
        None
    } else {
        Some((&text[start..end], start))
    }
}

fn is_table_keyword(word: &str) -> bool {
    matches!(word, "FROM" | "JOIN" | "UPDATE" | "INTO")
}

fn extract_tables_from_text(source: &str, offset: TextSize) -> Vec<String> {
    let offset: usize = offset.into();
    let prefix = source.get(..offset).unwrap_or(source);

    extract_tables_from_text_slice(prefix)
}

fn extract_tables_from_text_full(source: &str) -> Vec<String> {
    extract_tables_from_text_slice(source)
}

fn extract_tables_from_text_slice(text: &str) -> Vec<String> {
    let mut tables = Vec::new();
    let mut expecting_table = false;

    for token in tokenize_sql_words(text) {
        let upper = token.to_uppercase();
        if matches!(upper.as_str(), "FROM" | "JOIN" | "UPDATE" | "INTO") {
            expecting_table = true;
            continue;
        }

        if expecting_table && !token.is_empty() {
            if !tables.contains(&token) {
                tables.push(token);
            }
            expecting_table = false;
        }
    }

    tables
}

fn tokenize_sql_words(text: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();

    for ch in text.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' || ch == '.' {
            current.push(ch);
        } else if !current.is_empty() {
            tokens.push(current.clone());
            current.clear();
        }
    }

    if !current.is_empty() {
        tokens.push(current);
    }

    tokens
}

/// Extracts the base column (table.column or just column) and path from text before a JSONB operator.
///
/// For `resource->'name'->` at position before the last `->`, this extracts:
/// - table: None (or from context)
/// - column: "resource"
/// - path: ["name"]
fn extract_jsonb_base_from_text(
    text: &str,
    operator_pos: usize,
) -> (Option<String>, String, Vec<String>) {
    let before_op = &text[..operator_pos];

    // Parse backwards from the operator to find the column reference and any preceding path accessors
    let mut column = String::new();
    let mut table: Option<String> = None;
    let mut path = Vec::new();

    // Find all JSONB accessor chains before this operator
    // E.g., "resource->'name'" -> base: resource, path: [name]
    let mut in_accessor = false;
    let mut accessor_value = String::new();

    // Scan backwards through the text
    let chars: Vec<char> = before_op.chars().collect();
    let mut i = chars.len();

    while i > 0 {
        i -= 1;
        let ch = chars[i];

        if in_accessor {
            // We're inside a quoted string or looking for the quote
            if ch == '\'' {
                // End of quoted accessor (reading backwards)
                path.insert(0, accessor_value.clone());
                accessor_value.clear();
                in_accessor = false;
            } else {
                accessor_value.insert(0, ch);
            }
        } else if ch == '\'' {
            // Start of a quoted accessor (reading backwards)
            in_accessor = true;
        } else if ch == '>' || ch == '#' || ch == '-' || ch == '@' || ch == '?' || ch == '<' {
            // Part of a JSONB operator, skip
            continue;
        } else if ch.is_whitespace() {
            // End of expression
            if !column.is_empty() {
                break;
            }
        } else if ch.is_alphanumeric() || ch == '_' {
            // Part of an identifier
            column.insert(0, ch);
        } else if ch == '.' {
            // Qualified reference (table.column)
            if !column.is_empty() {
                // What we collected so far is the column, now collect the table
                let col = column.clone();
                column.clear();
                // Continue to collect table name
                while i > 0 {
                    i -= 1;
                    let tch = chars[i];
                    if tch.is_alphanumeric() || tch == '_' {
                        column.insert(0, tch);
                    } else {
                        break;
                    }
                }
                table = Some(column.clone());
                column = col;
                break;
            }
        } else {
            // Other character, end of expression
            if !column.is_empty() {
                break;
            }
        }
    }

    (table, column, path)
}

/// Gets the word being typed at the cursor position.
pub fn get_prefix_at_offset(source: &str, offset: TextSize) -> Option<String> {
    let offset_usize: usize = offset.into();
    if offset_usize > source.len() {
        return None;
    }

    // Use safe slicing to handle multi-byte UTF-8 characters
    let text_before = safe_slice_to(source, offset_usize);

    // Find the start of the current word using char_indices for UTF-8 safety
    let mut word_start = 0;
    for (idx, ch) in text_before.char_indices().rev() {
        if !ch.is_alphanumeric() && ch != '_' {
            // Found a non-word character, word starts after it
            word_start = idx + ch.len_utf8();
            break;
        }
    }

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

    #[test]
    fn test_utf8_safety_cyrillic() {
        // Test with Cyrillic text - "SELECT * а" where 'а' is a 2-byte UTF-8 character
        let source = "SELECT * а";
        // Offset 10 is in the middle of the Cyrillic 'а' (bytes 9..11)
        let request = CompletionRequest::new(source, TextSize::new(10));
        // Should not panic
        let _result = complete(request);
    }

    #[test]
    fn test_utf8_safety_emoji() {
        // Test with emoji - 4-byte UTF-8 character
        let source = "SELECT * 😀";
        // Try various offsets that might land in the middle of the emoji
        for offset in 9..14 {
            let request = CompletionRequest::new(source, TextSize::new(offset as u32));
            // Should not panic
            let _result = complete(request);
        }
    }

    #[test]
    fn test_utf8_safety_chinese() {
        // Test with Chinese characters - 3-byte UTF-8
        let source = "SELECT 名字 FROM";
        for offset in 0..source.len() {
            let request = CompletionRequest::new(source, TextSize::new(offset as u32));
            // Should not panic
            let _result = complete(request);
        }
    }

    #[test]
    fn test_get_prefix_with_cyrillic() {
        // "привет" is Russian for "hello" - each character is 2 bytes
        let source = "SELECT прив";
        let prefix = get_prefix_at_offset(source, TextSize::new(source.len() as u32));
        assert_eq!(prefix, Some("прив".to_string()));
    }

    #[test]
    fn test_floor_char_boundary() {
        let s = "аб"; // Two Cyrillic letters, 4 bytes total
        assert_eq!(floor_char_boundary(s, 0), 0); // Start
        assert_eq!(floor_char_boundary(s, 1), 0); // Middle of 'а', floor to 0
        assert_eq!(floor_char_boundary(s, 2), 2); // Start of 'б'
        assert_eq!(floor_char_boundary(s, 3), 2); // Middle of 'б', floor to 2
        assert_eq!(floor_char_boundary(s, 4), 4); // End
        assert_eq!(floor_char_boundary(s, 100), 4); // Beyond end, clamp to len
    }

    // JSONB trigger tests

    #[test]
    fn test_jsonb_trigger_arrow() {
        let source = "SELECT resource->";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
        if let CompletionContext::JsonbField { column, .. } = ctx {
            assert_eq!(column, "resource");
        }
    }

    #[test]
    fn test_jsonb_trigger_double_arrow() {
        let source = "SELECT data->>";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
        if let CompletionContext::JsonbField { column, .. } = ctx {
            assert_eq!(column, "data");
        }
    }

    #[test]
    fn test_jsonb_trigger_hash_arrow() {
        let source = "SELECT data#>";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
    }

    #[test]
    fn test_jsonb_trigger_with_path() {
        let source = "SELECT resource->'name'->";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
        if let CompletionContext::JsonbField { column, path, .. } = ctx {
            assert_eq!(column, "resource");
            assert_eq!(path, vec!["name"]);
        }
    }

    #[test]
    fn test_jsonb_trigger_qualified_column() {
        let source = "SELECT patient.resource->";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
        if let CompletionContext::JsonbField { table, column, .. } = ctx {
            assert_eq!(table, Some("patient".to_string()));
            assert_eq!(column, "resource");
        }
    }

    #[test]
    fn test_jsonb_trigger_contains() {
        let source = "SELECT * WHERE data@>";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
    }

    #[test]
    fn test_jsonb_trigger_key_exists() {
        let source = "SELECT * WHERE data?";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
    }

    #[test]
    fn test_jsonb_trigger_jsonpath() {
        let source = "SELECT * WHERE data@?";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonbField { .. }));
    }

    #[test]
    fn test_extract_jsonb_base_simple() {
        let (table, column, path) = extract_jsonb_base_from_text("resource->", 8);
        assert_eq!(table, None);
        assert_eq!(column, "resource");
        assert!(path.is_empty());
    }

    #[test]
    fn test_extract_jsonb_base_with_path() {
        let (table, column, path) = extract_jsonb_base_from_text("resource->'name'->", 16);
        assert_eq!(table, None);
        assert_eq!(column, "resource");
        assert_eq!(path, vec!["name"]);
    }

    #[test]
    fn test_extract_jsonb_base_qualified() {
        let (table, column, path) = extract_jsonb_base_from_text("p.resource->", 10);
        assert_eq!(table, Some("p".to_string()));
        assert_eq!(column, "resource");
        assert!(path.is_empty());
    }

    #[test]
    fn test_extract_jsonb_base_nested_path() {
        let (table, column, path) =
            extract_jsonb_base_from_text("data->'address'->'city'->", 23);
        assert_eq!(table, None);
        assert_eq!(column, "data");
        assert_eq!(path, vec!["address", "city"]);
    }

    // JSONPath string context tests

    #[test]
    fn test_jsonpath_context_at_root() {
        let source = "resource @? '$";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonPath { .. }));
        if let CompletionContext::JsonPath { current_path } = ctx {
            assert_eq!(current_path, "$");
        }
    }

    #[test]
    fn test_jsonpath_context_after_dot() {
        let source = "resource @? '$.";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonPath { .. }));
        if let CompletionContext::JsonPath { current_path } = ctx {
            assert_eq!(current_path, "$.");
        }
    }

    #[test]
    fn test_jsonpath_context_with_member() {
        let source = "resource @? '$.name";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonPath { .. }));
        if let CompletionContext::JsonPath { current_path } = ctx {
            assert_eq!(current_path, "$.name");
        }
    }

    #[test]
    fn test_jsonpath_context_array_bracket() {
        let source = "resource @? '$.items[";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonPath { .. }));
        if let CompletionContext::JsonPath { current_path } = ctx {
            assert_eq!(current_path, "$.items[");
        }
    }

    #[test]
    fn test_jsonpath_context_filter() {
        let source = "resource @? '$.items[?(@.";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonPath { .. }));
        if let CompletionContext::JsonPath { current_path } = ctx {
            assert_eq!(current_path, "$.items[?(@.");
        }
    }

    #[test]
    fn test_jsonpath_context_in_function() {
        let source = "jsonb_path_query(resource, '$.name";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(matches!(ctx, CompletionContext::JsonPath { .. }));
        if let CompletionContext::JsonPath { current_path } = ctx {
            assert_eq!(current_path, "$.name");
        }
    }

    #[test]
    fn test_jsonpath_context_closed_string() {
        // Closed string should NOT trigger JSONPath context
        let source = "resource @? '$.name'";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        // Should not be JSONPath context since string is closed
        assert!(!matches!(ctx, CompletionContext::JsonPath { .. }));
    }

    #[test]
    fn test_jsonpath_context_not_jsonpath() {
        // Regular string should NOT trigger JSONPath context
        let source = "SELECT 'hello";
        let ctx = detect_context_from_text(source, TextSize::new(source.len() as u32));
        assert!(!matches!(ctx, CompletionContext::JsonPath { .. }));
    }
}
