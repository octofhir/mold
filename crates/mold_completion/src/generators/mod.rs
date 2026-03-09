//! Completion generators.
//!
//! Each generator produces completion items for a specific context.

pub mod columns;
pub mod functions;
pub mod jsonb;
pub mod keywords;
pub mod tables;

pub use columns::complete_columns;
pub use functions::{JsonbArgCompletion, complete_functions, get_jsonb_arg_completion};
pub use jsonb::{complete_jsonb_paths, complete_jsonpath};
pub use keywords::complete_keywords;
pub use tables::complete_tables;

pub(crate) fn split_table_reference(table: &str) -> (Option<&str>, &str) {
    let mut parts = table
        .split('.')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();
    if parts.is_empty() {
        return (None, table);
    }

    let table_name = parts.pop().unwrap_or(table);
    let schema = parts.pop();
    (schema, table_name)
}

pub(crate) fn resolve_table_lookup(
    provider: &dyn crate::providers::SchemaProvider,
    table: &str,
) -> (Option<String>, String) {
    let (schema, table_name) = split_table_reference(table);
    if let Some(schema) = schema {
        return (Some(schema.to_string()), table_name.to_string());
    }

    if let Some(info) = provider.table(None, table_name) {
        return (info.schema, info.name);
    }

    (None, table_name.to_string())
}
