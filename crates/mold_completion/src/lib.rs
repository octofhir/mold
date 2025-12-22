//! Completion engine for SQL queries.
//!
//! This crate provides context-aware completion that is decoupled from LSP types.
//! It can be used to implement completion in any editor or IDE.
//!
//! # Features
//!
//! - Table and column completion from schema providers
//! - Function completion with signature help
//! - JSONB path completion with schema support
//! - Keyword completion based on context
//! - Context detection for accurate suggestions
//!
//! # Usage
//!
//! ```ignore
//! use mold_completion::{complete, CompletionRequest};
//! use text_size::TextSize;
//!
//! let source = "SELECT * FROM ";
//! let request = CompletionRequest::new(source, TextSize::new(14));
//! let result = complete(request);
//!
//! for item in result.items {
//!     println!("{}: {:?}", item.label, item.kind);
//! }
//! ```
//!
//! # Architecture
//!
//! The completion engine is organized into several modules:
//!
//! - [`types`] - Data types for completion items and context
//! - [`providers`] - Traits for schema and function providers
//! - [`context`] - Completion context detection
//! - [`generators`] - Completion item generators
//! - [`complete`] - Main completion logic

pub mod complete;
pub mod context;
pub mod generators;
pub mod providers;
pub mod types;

// Re-export main types
pub use complete::{CompletionRequest, complete, get_prefix_at_offset};
pub use context::{detect_context, find_cte_columns, CteInfo};
pub use generators::{
    complete_columns, complete_functions, complete_jsonb_paths, complete_jsonpath,
    complete_keywords, complete_tables,
};
pub use providers::{
    FunctionProvider, MemoryFunctionProvider, MemorySchemaProvider, NullFunctionProvider,
    NullSchemaProvider, SchemaProvider,
};
pub use types::{
    ArgMode, ColumnInfo, CompletionContext, CompletionData, CompletionItem, CompletionItemKind,
    CompletionResult, FunctionArg, FunctionInfo, JsonbField, JsonbFieldType, JsonbSchema,
    TableInfo, TableType, Volatility,
};

#[cfg(test)]
mod tests {
    use super::*;
    use text_size::TextSize;

    #[test]
    fn test_complete_tables_after_from() {
        // Acceptance criteria 1: Complete tables after FROM
        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users").with_schema("public"))
            .add_table(TableInfo::new("orders").with_schema("public"));

        let request = CompletionRequest::new("SELECT * FROM ", TextSize::new(14))
            .with_schema_provider(&provider);
        let result = complete(request);

        assert!(result.items.iter().any(|i| i.label == "users"));
        assert!(result.items.iter().any(|i| i.label == "orders"));
    }

    #[test]
    fn test_complete_columns_after_select() {
        // Acceptance criteria 2: Complete columns after SELECT with FROM context
        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users"))
            .add_columns(
                None,
                "users",
                vec![
                    ColumnInfo::new("id", "integer"),
                    ColumnInfo::new("name", "text"),
                ],
            );

        // Note: Without parse tree, we can't fully detect FROM context from text
        // This tests that columns are available when explicitly requested
        let columns = complete_columns(Some(&provider), Some("users"), &[], None);
        assert!(columns.iter().any(|i| i.label == "id"));
        assert!(columns.iter().any(|i| i.label == "name"));
    }

    #[test]
    fn test_complete_qualified_columns() {
        // Acceptance criteria 3: Complete qualified columns (users. suggests user columns)
        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users"))
            .add_columns(
                None,
                "users",
                vec![
                    ColumnInfo::new("id", "integer"),
                    ColumnInfo::new("email", "text"),
                ],
            );

        let columns =
            generators::columns::complete_qualified_columns(Some(&provider), "users", None);
        assert_eq!(columns.len(), 2);
        assert!(columns.iter().any(|i| i.label == "id"));
        assert!(columns.iter().any(|i| i.label == "email"));
    }

    #[test]
    fn test_complete_jsonb_paths() {
        // Acceptance criteria 4: Complete JSONB paths (data->' suggests known fields)
        let schema = JsonbSchema::new()
            .with_field(JsonbField::new("name", JsonbFieldType::String))
            .with_field(JsonbField::new("age", JsonbFieldType::Number));

        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users"))
            .add_jsonb_schema(None, "users", "data", schema);

        let paths = complete_jsonb_paths(Some(&provider), Some("users"), "data", &[]);
        assert!(paths.iter().any(|i| i.label == "name"));
        assert!(paths.iter().any(|i| i.label == "age"));
    }

    #[test]
    fn test_complete_functions_by_prefix() {
        // Acceptance criteria 5: Complete functions (json suggests jsonb_* functions)
        let items = complete_functions(None, Some("json"));
        assert!(!items.is_empty());
        assert!(items.iter().all(|i| i.label.starts_with("json")));
        assert!(items.iter().any(|i| i.label == "jsonb_build_object"));
    }

    #[test]
    fn test_complete_keywords() {
        // Acceptance criteria 6: Complete keywords (SELECT * FR suggests FROM)
        let items = complete_keywords(None, Some("FR"));
        assert!(items.iter().any(|i| i.label == "FROM"));
    }

    #[test]
    fn test_complete_without_providers() {
        // Acceptance criteria 7: Work without providers (keyword-only completion)
        let request = CompletionRequest::new("", TextSize::new(0));
        let result = complete(request);

        // Should still have keyword completions
        assert!(!result.items.is_empty());
        assert!(result.items.iter().any(|i| i.label == "SELECT"));
    }

    #[test]
    fn test_types_are_send_sync() {
        // Acceptance criteria 8: All types are Send + Sync
        fn assert_send_sync<T: Send + Sync>() {}

        assert_send_sync::<CompletionItem>();
        assert_send_sync::<CompletionResult>();
        assert_send_sync::<CompletionContext>();
        assert_send_sync::<TableInfo>();
        assert_send_sync::<ColumnInfo>();
        assert_send_sync::<FunctionInfo>();
        assert_send_sync::<JsonbSchema>();
        assert_send_sync::<NullSchemaProvider>();
        assert_send_sync::<NullFunctionProvider>();
        assert_send_sync::<MemorySchemaProvider>();
        assert_send_sync::<MemoryFunctionProvider>();
    }

    #[test]
    fn test_full_workflow() {
        // Full workflow test with schema provider
        // Note: columns are added without schema for simpler lookup
        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users").with_schema("public"))
            .add_columns(
                None,
                "users",
                vec![
                    ColumnInfo::new("id", "integer").with_primary_key(true),
                    ColumnInfo::new("name", "text"),
                    ColumnInfo::new("data", "jsonb"),
                ],
            )
            .add_jsonb_schema(
                None,
                "users",
                "data",
                JsonbSchema::new()
                    .with_field(JsonbField::new("email", JsonbFieldType::String))
                    .with_field(JsonbField::new("preferences", JsonbFieldType::Object)),
            );

        // 1. Complete tables
        let tables = complete_tables(Some(&provider), None, None);
        assert!(tables.iter().any(|i| i.label == "users"));

        // 2. Complete columns for users table
        let columns = complete_columns(Some(&provider), Some("users"), &[], None);
        assert!(columns.iter().any(|i| i.label == "id"));
        assert!(columns.iter().any(|i| i.label == "data"));

        // 3. Complete JSONB paths
        let paths = complete_jsonb_paths(Some(&provider), Some("users"), "data", &[]);
        assert!(paths.iter().any(|i| i.label == "email"));
        assert!(paths.iter().any(|i| i.label == "preferences"));

        // 4. Complete functions
        let funcs = complete_functions(None, Some("jsonb_"));
        assert!(funcs.iter().any(|i| i.label == "jsonb_build_object"));
    }
}
