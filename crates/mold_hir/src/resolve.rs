//! Name resolution functions for semantic analysis.
//!
//! This module provides functions to resolve names in various contexts:
//! - Table names in FROM clauses
//! - Column references in expressions
//! - CTE references
//! - Qualified names (schema.table.column)
//! - Table and column validation with diagnostics

use std::sync::Arc;

use text_size::TextRange;

use crate::analyze::{SchemaProvider, suggest_similar};
use crate::binding::{
    ColumnBinding, CteBinding, DataType, Resolution, ResolvedColumn, TableBinding,
};
use crate::scope::Scope;

/// Result of resolving a qualified name (e.g., schema.table.column).
#[derive(Clone, Debug)]
pub struct QualifiedResolution {
    /// The catalog/database name, if specified.
    pub catalog: Option<String>,
    /// The schema name, if specified.
    pub schema: Option<String>,
    /// The table name, if specified.
    pub table: Option<String>,
    /// The column name.
    pub column: String,
}

impl QualifiedResolution {
    /// Parses a qualified name from its parts.
    pub fn from_parts(parts: &[String]) -> Self {
        match parts.len() {
            1 => Self {
                catalog: None,
                schema: None,
                table: None,
                column: parts[0].clone(),
            },
            2 => Self {
                catalog: None,
                schema: None,
                table: Some(parts[0].clone()),
                column: parts[1].clone(),
            },
            3 => Self {
                catalog: None,
                schema: Some(parts[0].clone()),
                table: Some(parts[1].clone()),
                column: parts[2].clone(),
            },
            4 => Self {
                catalog: Some(parts[0].clone()),
                schema: Some(parts[1].clone()),
                table: Some(parts[2].clone()),
                column: parts[3].clone(),
            },
            _ => Self {
                catalog: None,
                schema: None,
                table: None,
                column: parts.last().cloned().unwrap_or_default(),
            },
        }
    }

    /// Returns true if this is a qualified reference (has table or schema).
    pub fn is_qualified(&self) -> bool {
        self.table.is_some() || self.schema.is_some() || self.catalog.is_some()
    }
}

/// Resolves a table reference in the given scope.
///
/// Handles:
/// - Simple table names: `users`
/// - Schema-qualified names: `public.users`
/// - CTE references: looks up CTEs before physical tables
pub fn resolve_table(
    name: &str,
    schema: Option<&str>,
    scope: &Arc<Scope>,
) -> Resolution<Arc<TableBinding>> {
    // If schema is specified, we need to match both
    if let Some(schema_name) = schema {
        // Look for a table with matching schema
        for table in scope.tables() {
            if let crate::binding::TableSource::Table {
                schema: Some(ref s),
                name: ref n,
            } = table.source
                && s.eq_ignore_ascii_case(schema_name)
                && n.eq_ignore_ascii_case(name)
            {
                return Resolution::Resolved(Arc::clone(table));
            }
        }
        // Check parent scope
        if let Some(parent) = scope.parent() {
            return resolve_table(name, schema, parent);
        }
        return Resolution::Unresolved;
    }

    // Unqualified name - delegate to scope's resolution
    scope.resolve_table(name)
}

/// Resolves a column reference in the given scope.
///
/// Handles:
/// - Simple column names: `id`
/// - Table-qualified names: `users.id`
/// - Schema-qualified names: `public.users.id`
pub fn resolve_column(parts: &[String], scope: &Arc<Scope>) -> Resolution<ResolvedColumn> {
    let qualified = QualifiedResolution::from_parts(parts);

    let table_name = qualified.table.as_deref();

    match scope.resolve_column(&qualified.column, table_name) {
        Resolution::Resolved((table, column)) => {
            Resolution::Resolved(ResolvedColumn { table, column })
        }
        Resolution::Ambiguous(matches) => Resolution::Ambiguous(
            matches
                .into_iter()
                .map(|(table, column)| ResolvedColumn { table, column })
                .collect(),
        ),
        Resolution::Unresolved => Resolution::Unresolved,
    }
}

/// Resolves a CTE reference in the given scope.
pub fn resolve_cte(name: &str, scope: &Arc<Scope>) -> Resolution<Arc<CteBinding>> {
    scope.resolve_cte(name)
}

/// Resolves a column or alias in ORDER BY context.
///
/// ORDER BY can reference:
/// 1. Ordinal positions: `ORDER BY 1`
/// 2. SELECT aliases: `ORDER BY alias_name`
/// 3. Column references: `ORDER BY column_name`
pub fn resolve_order_by(
    reference: &str,
    select_columns: &[ColumnBinding],
    scope: &Arc<Scope>,
) -> Resolution<ColumnBinding> {
    scope.resolve_order_by(reference, select_columns)
}

/// Information about a JSONB column for path completion.
#[derive(Clone, Debug)]
pub struct JsonbColumnInfo {
    /// The column binding.
    pub column: ColumnBinding,

    /// Known keys at the top level of this JSONB column.
    /// Empty if schema information is not available.
    pub known_keys: Vec<String>,

    /// Nested structure information, if available.
    pub structure: Option<JsonbStructure>,
}

/// Structure information for a JSONB column.
#[derive(Clone, Debug)]
pub struct JsonbStructure {
    /// Type of the JSONB value at this level.
    pub value_type: JsonbValueType,

    /// Child keys/indices, if this is an object or array.
    pub children: Vec<JsonbChild>,
}

/// Type of a JSONB value.
#[non_exhaustive]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JsonbValueType {
    Object,
    Array,
    String,
    Number,
    Boolean,
    Null,
    Unknown,
}

/// A child element in a JSONB structure.
#[derive(Clone, Debug)]
pub struct JsonbChild {
    /// Key name (for objects) or index (for arrays as string).
    pub key: String,

    /// Nested structure, if known.
    pub structure: Option<JsonbStructure>,
}

/// Finds JSONB columns in scope for path completion.
pub fn find_jsonb_columns(scope: &Arc<Scope>) -> Vec<JsonbColumnInfo> {
    let mut result = Vec::new();

    for table in scope.tables() {
        for column in &table.columns {
            if let Some(DataType::Jsonb) = &column.data_type {
                result.push(JsonbColumnInfo {
                    column: column.clone(),
                    known_keys: Vec::new(),
                    structure: None,
                });
            }
        }
    }

    // Also check parent scope
    if let Some(parent) = scope.parent() {
        result.extend(find_jsonb_columns(parent));
    }

    result
}

/// Determines the result type of a JSONB access expression.
///
/// - `->` returns JSONB
/// - `->>` returns TEXT
/// - `#>` returns JSONB
/// - `#>>` returns TEXT
pub fn jsonb_access_result_type(extracts_text: bool) -> DataType {
    if extracts_text {
        DataType::Text
    } else {
        DataType::Jsonb
    }
}

// =============================================================================
// Table and Column Validation
// =============================================================================

/// Result of validating a table reference.
#[derive(Clone, Debug)]
pub struct TableValidation {
    /// Whether the table exists.
    pub exists: bool,
    /// Whether the schema exists (if schema was specified).
    pub schema_exists: bool,
    /// Suggestions for similar table names ("did you mean").
    pub suggestions: Vec<String>,
    /// The source range of the table reference.
    pub range: Option<TextRange>,
}

impl TableValidation {
    /// Creates a validation result for an existing table.
    pub fn found(range: Option<TextRange>) -> Self {
        Self {
            exists: true,
            schema_exists: true,
            suggestions: Vec::new(),
            range,
        }
    }

    /// Creates a validation result for a missing table.
    pub fn not_found(suggestions: Vec<String>, range: Option<TextRange>) -> Self {
        Self {
            exists: false,
            schema_exists: true,
            suggestions,
            range,
        }
    }

    /// Creates a validation result for a missing schema.
    pub fn schema_not_found(suggestions: Vec<String>, range: Option<TextRange>) -> Self {
        Self {
            exists: false,
            schema_exists: false,
            suggestions,
            range,
        }
    }
}

/// Result of validating a column reference.
#[derive(Clone, Debug)]
pub enum ColumnValidation {
    /// Column was found in the specified table.
    Found { table: String, column: String },
    /// Column not found in the specified table (with alias).
    UnknownColumn {
        table: String,
        alias: String,
        column: String,
        suggestions: Vec<String>,
        range: Option<TextRange>,
    },
    /// The table alias/qualifier was not found.
    UnknownAlias {
        alias: String,
        available_aliases: Vec<String>,
        range: Option<TextRange>,
    },
    /// Unqualified column not found in any table.
    NotFound {
        column: String,
        range: Option<TextRange>,
    },
    /// Ambiguous column - found in multiple tables.
    Ambiguous {
        column: String,
        tables: Vec<(String, String)>, // (alias, table_name)
        range: Option<TextRange>,
    },
}

impl ColumnValidation {
    /// Returns true if the column was successfully resolved.
    pub fn is_valid(&self) -> bool {
        matches!(self, ColumnValidation::Found { .. })
    }

    /// Returns an error message if validation failed.
    pub fn error_message(&self) -> Option<String> {
        match self {
            ColumnValidation::Found { .. } => None,
            ColumnValidation::UnknownColumn {
                table,
                alias,
                column,
                suggestions,
                ..
            } => {
                let suggestion_text = if !suggestions.is_empty() {
                    format!(". Did you mean: {}?", suggestions.join(", "))
                } else {
                    String::new()
                };
                Some(format!(
                    "Column '{}' not found in table '{}' (alias '{}'){}",
                    column, table, alias, suggestion_text
                ))
            }
            ColumnValidation::UnknownAlias {
                alias,
                available_aliases,
                ..
            } => {
                let available = if available_aliases.is_empty() {
                    "none".to_string()
                } else {
                    available_aliases.join(", ")
                };
                Some(format!(
                    "Unknown table alias '{}'. Available aliases: {}",
                    alias, available
                ))
            }
            ColumnValidation::NotFound { column, .. } => Some(format!(
                "Column '{}' not found in any table in scope",
                column
            )),
            ColumnValidation::Ambiguous { column, tables, .. } => {
                let table_list = tables
                    .iter()
                    .map(|(alias, table)| {
                        if alias == table {
                            table.clone()
                        } else {
                            format!("{} ({})", alias, table)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(format!(
                    "Ambiguous column '{}' - found in multiple tables: {}. Use table alias to qualify.",
                    column, table_list
                ))
            }
        }
    }

    /// Returns the source range if available.
    pub fn range(&self) -> Option<TextRange> {
        match self {
            ColumnValidation::Found { .. } => None,
            ColumnValidation::UnknownColumn { range, .. } => *range,
            ColumnValidation::UnknownAlias { range, .. } => *range,
            ColumnValidation::NotFound { range, .. } => *range,
            ColumnValidation::Ambiguous { range, .. } => *range,
        }
    }
}

/// Validates a table reference against the schema provider.
pub fn validate_table_reference(
    schema: Option<&str>,
    name: &str,
    provider: &dyn SchemaProvider,
    range: Option<TextRange>,
) -> TableValidation {
    // Check schema exists first (if specified)
    if let Some(schema_name) = schema
        && !provider.schema_exists(schema_name)
    {
        let suggestions = provider.suggest_schemas(schema_name);
        return TableValidation::schema_not_found(suggestions, range);
    }

    // Check table exists
    if provider.table_exists(schema, name) {
        TableValidation::found(range)
    } else {
        let suggestions = provider.suggest_tables(schema, name);
        TableValidation::not_found(suggestions, range)
    }
}

/// Validates a column reference against the scope.
///
/// Handles both qualified (t.column) and unqualified (column) references.
pub fn validate_column_reference(
    qualifier: Option<&str>,
    column: &str,
    scope: &Arc<Scope>,
    _provider: &dyn SchemaProvider,
    range: Option<TextRange>,
) -> ColumnValidation {
    match qualifier {
        Some(q) => {
            // Qualified reference: t.column or schema.table.column
            match scope.resolve_table(q) {
                Resolution::Resolved(table) => {
                    // Check if column exists in this table
                    if table
                        .columns
                        .iter()
                        .any(|c| c.name.eq_ignore_ascii_case(column))
                    {
                        ColumnValidation::Found {
                            table: table.original_name.clone(),
                            column: column.to_string(),
                        }
                    } else {
                        // Column not found - get suggestions
                        let col_names: Vec<String> =
                            table.columns.iter().map(|c| c.name.clone()).collect();
                        let suggestions = suggest_similar(column, &col_names, 3);
                        ColumnValidation::UnknownColumn {
                            table: table.original_name.clone(),
                            alias: q.to_string(),
                            column: column.to_string(),
                            suggestions,
                            range,
                        }
                    }
                }
                Resolution::Ambiguous(_) => {
                    // Multiple tables match the qualifier - shouldn't happen with aliases
                    let available: Vec<String> = scope
                        .tables()
                        .map(|t| t.reference_name().to_string())
                        .collect();
                    ColumnValidation::UnknownAlias {
                        alias: q.to_string(),
                        available_aliases: available,
                        range,
                    }
                }
                Resolution::Unresolved => {
                    let available: Vec<String> = scope
                        .tables()
                        .map(|t| t.reference_name().to_string())
                        .collect();
                    ColumnValidation::UnknownAlias {
                        alias: q.to_string(),
                        available_aliases: available,
                        range,
                    }
                }
            }
        }
        None => {
            // Unqualified reference: search all tables
            let mut found_in: Vec<(String, String)> = Vec::new();

            for table in scope.tables() {
                if table
                    .columns
                    .iter()
                    .any(|c| c.name.eq_ignore_ascii_case(column))
                {
                    found_in.push((
                        table.reference_name().to_string(),
                        table.original_name.clone(),
                    ));
                }
            }

            match found_in.len() {
                0 => ColumnValidation::NotFound {
                    column: column.to_string(),
                    range,
                },
                1 => ColumnValidation::Found {
                    table: found_in[0].1.clone(),
                    column: column.to_string(),
                },
                _ => ColumnValidation::Ambiguous {
                    column: column.to_string(),
                    tables: found_in,
                    range,
                },
            }
        }
    }
}

// =============================================================================
// JSONB Chain Validation
// =============================================================================

/// Error when validating a JSONB access chain.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JsonbChainError {
    /// Attempted to access a key/index on a text value (after ->>).
    AccessAfterTextExtract {
        /// The position in the chain where text was extracted.
        text_extract_at: usize,
        /// The key that was attempted to access after text extraction.
        attempted_key: String,
    },
    /// Key not found in known JSONB structure.
    KeyNotFound {
        /// The path up to this point.
        path: Vec<String>,
        /// The key that wasn't found.
        key: String,
        /// Available keys at this level.
        available_keys: Vec<String>,
    },
    /// Attempted array access with non-numeric key.
    NonNumericArrayAccess {
        /// The path up to this point.
        path: Vec<String>,
        /// The non-numeric key that was used.
        key: String,
    },
    /// Attempted to access a field on a scalar value.
    AccessOnScalar {
        /// The path up to this point.
        path: Vec<String>,
        /// The scalar type.
        scalar_type: JsonbValueType,
        /// The key that was attempted.
        key: String,
    },
    /// The base column is not a JSONB type.
    NotJsonbColumn {
        /// The column name.
        column: String,
    },
}

impl JsonbChainError {
    /// Returns an error message for this error.
    pub fn error_message(&self) -> String {
        match self {
            JsonbChainError::AccessAfterTextExtract {
                text_extract_at,
                attempted_key,
            } => {
                format!(
                    "Cannot access key '{}' after ->> operator at position {}. The ->> operator extracts text, not JSONB.",
                    attempted_key, text_extract_at
                )
            }
            JsonbChainError::KeyNotFound {
                path,
                key,
                available_keys,
            } => {
                let path_str = if path.is_empty() {
                    "root".to_string()
                } else {
                    path.join(".")
                };
                let suggestions = if available_keys.is_empty() {
                    String::new()
                } else {
                    format!(". Available keys: {}", available_keys.join(", "))
                };
                format!(
                    "Key '{}' not found at path '{}'{}",
                    key, path_str, suggestions
                )
            }
            JsonbChainError::NonNumericArrayAccess { path, key } => {
                let path_str = if path.is_empty() {
                    "root".to_string()
                } else {
                    path.join(".")
                };
                format!(
                    "Cannot use non-numeric key '{}' to access array at path '{}'",
                    key, path_str
                )
            }
            JsonbChainError::AccessOnScalar {
                path,
                scalar_type,
                key,
            } => {
                let path_str = if path.is_empty() {
                    "root".to_string()
                } else {
                    path.join(".")
                };
                format!(
                    "Cannot access key '{}' on {:?} value at path '{}'",
                    key, scalar_type, path_str
                )
            }
            JsonbChainError::NotJsonbColumn { column } => {
                format!("Column '{}' is not a JSONB type", column)
            }
        }
    }
}

/// Result of validating a JSONB chain.
#[derive(Clone, Debug)]
pub struct JsonbChainValidation {
    /// Whether the chain is valid.
    pub valid: bool,
    /// Any errors found.
    pub errors: Vec<JsonbChainError>,
    /// The inferred result type of the chain.
    pub result_type: JsonbValueType,
}

impl JsonbChainValidation {
    /// Creates a successful validation.
    pub fn success(result_type: JsonbValueType) -> Self {
        Self {
            valid: true,
            errors: Vec::new(),
            result_type,
        }
    }

    /// Creates a failed validation with errors.
    pub fn failure(errors: Vec<JsonbChainError>) -> Self {
        Self {
            valid: false,
            errors,
            result_type: JsonbValueType::Unknown,
        }
    }
}

/// Validates a JSONB access chain.
///
/// Checks that:
/// 1. No access after text extraction (->>)
/// 2. Keys exist in known structure (if structure is provided)
/// 3. Array access uses numeric indices
/// 4. No access on scalar values
pub fn validate_jsonb_chain(
    segments: &[(String, bool)], // (key, extracts_text)
    structure: Option<&JsonbStructure>,
) -> JsonbChainValidation {
    let mut errors = Vec::new();
    let mut path: Vec<String> = Vec::new();
    let mut last_extracted_text = false;
    let mut text_extract_position = 0;
    let mut current_structure = structure;

    for (i, (key, extracts_text)) in segments.iter().enumerate() {
        // Check if previous access extracted text
        if last_extracted_text {
            errors.push(JsonbChainError::AccessAfterTextExtract {
                text_extract_at: text_extract_position,
                attempted_key: key.clone(),
            });
            // Continue checking for more errors
        }

        // Validate against known structure if available
        if let Some(struct_info) = current_structure {
            match &struct_info.value_type {
                JsonbValueType::Object => {
                    // Check if key exists in object
                    let child = struct_info.children.iter().find(|c| c.key == *key);
                    if let Some(child) = child {
                        current_structure = child.structure.as_ref();
                    } else {
                        let available: Vec<String> =
                            struct_info.children.iter().map(|c| c.key.clone()).collect();
                        if !available.is_empty() {
                            errors.push(JsonbChainError::KeyNotFound {
                                path: path.clone(),
                                key: key.clone(),
                                available_keys: available,
                            });
                        }
                        current_structure = None;
                    }
                }
                JsonbValueType::Array => {
                    // For arrays, key should be numeric
                    if key.parse::<usize>().is_err() && key != "*" {
                        errors.push(JsonbChainError::NonNumericArrayAccess {
                            path: path.clone(),
                            key: key.clone(),
                        });
                    }
                    // After array access, move to array element type
                    if let Some(first_child) = struct_info.children.first() {
                        current_structure = first_child.structure.as_ref();
                    } else {
                        current_structure = None;
                    }
                }
                scalar_type @ (JsonbValueType::String
                | JsonbValueType::Number
                | JsonbValueType::Boolean
                | JsonbValueType::Null) => {
                    errors.push(JsonbChainError::AccessOnScalar {
                        path: path.clone(),
                        scalar_type: scalar_type.clone(),
                        key: key.clone(),
                    });
                    current_structure = None;
                }
                JsonbValueType::Unknown => {
                    // Unknown type - can't validate further
                    current_structure = None;
                }
            }
        }

        path.push(key.clone());
        if *extracts_text {
            last_extracted_text = true;
            text_extract_position = i;
        }
    }

    if errors.is_empty() {
        // Determine result type
        let result_type = if last_extracted_text {
            JsonbValueType::String // ->> extracts as text
        } else {
            current_structure
                .map(|s| s.value_type.clone())
                .unwrap_or(JsonbValueType::Unknown)
        };
        JsonbChainValidation::success(result_type)
    } else {
        JsonbChainValidation::failure(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scope::ScopeBuilder;

    #[test]
    fn test_qualified_resolution_parsing() {
        let single = QualifiedResolution::from_parts(&["id".to_string()]);
        assert_eq!(single.column, "id");
        assert!(single.table.is_none());
        assert!(!single.is_qualified());

        let double = QualifiedResolution::from_parts(&["users".to_string(), "id".to_string()]);
        assert_eq!(double.column, "id");
        assert_eq!(double.table.as_deref(), Some("users"));
        assert!(double.is_qualified());

        let triple = QualifiedResolution::from_parts(&[
            "public".to_string(),
            "users".to_string(),
            "id".to_string(),
        ]);
        assert_eq!(triple.column, "id");
        assert_eq!(triple.table.as_deref(), Some("users"));
        assert_eq!(triple.schema.as_deref(), Some("public"));
        assert!(triple.is_qualified());
    }

    #[test]
    fn test_resolve_column_simple() {
        let scope = ScopeBuilder::new()
            .add_table(
                TableBinding::table(None, "users".to_string())
                    .with_columns(vec![ColumnBinding::new("id".to_string(), 0)]),
            )
            .build();

        let result = resolve_column(&["id".to_string()], &scope);
        assert!(result.ok().is_some());
    }

    #[test]
    fn test_resolve_column_qualified() {
        let scope = ScopeBuilder::new()
            .add_table(
                TableBinding::table(None, "users".to_string())
                    .with_columns(vec![ColumnBinding::new("id".to_string(), 0)]),
            )
            .build();

        let result = resolve_column(&["users".to_string(), "id".to_string()], &scope);
        assert!(result.ok().is_some());
    }

    #[test]
    fn test_jsonb_access_result_type() {
        assert_eq!(jsonb_access_result_type(false), DataType::Jsonb);
        assert_eq!(jsonb_access_result_type(true), DataType::Text);
    }
}
