//! Name resolution functions for semantic analysis.
//!
//! This module provides functions to resolve names in various contexts:
//! - Table names in FROM clauses
//! - Column references in expressions
//! - CTE references
//! - Qualified names (schema.table.column)

use std::sync::Arc;

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
            {
                if s.eq_ignore_ascii_case(schema_name) && n.eq_ignore_ascii_case(name) {
                    return Resolution::Resolved(Arc::clone(table));
                }
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
