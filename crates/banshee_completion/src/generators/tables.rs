//! Table completion generator.

use crate::providers::SchemaProvider;
use crate::types::{CompletionData, CompletionItem, CompletionItemKind, TableType};

/// Generates table completion items.
pub fn complete_tables(
    provider: Option<&dyn SchemaProvider>,
    schema: Option<&str>,
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    let Some(provider) = provider else {
        return Vec::new();
    };

    let tables = match (schema, prefix) {
        (Some(s), Some(p)) => provider
            .tables_in_schema(s)
            .into_iter()
            .filter(|t| t.name.to_lowercase().starts_with(&p.to_lowercase()))
            .collect(),
        (Some(s), None) => provider.tables_in_schema(s),
        (None, Some(p)) => provider.tables_by_prefix(p),
        (None, None) => provider.tables(),
    };

    tables
        .into_iter()
        .map(|table| {
            let kind = match table.table_type {
                TableType::Table => CompletionItemKind::Table,
                TableType::View | TableType::MaterializedView => CompletionItemKind::View,
                TableType::ForeignTable => CompletionItemKind::Table,
                TableType::Cte => CompletionItemKind::Alias,
            };

            let detail = match &table.schema {
                Some(s) => format!("{}.{}", s, table.name),
                None => table.name.clone(),
            };

            // Sort key: prioritize tables in public schema
            let sort_key = if table.schema.as_deref() == Some("public") {
                format!("0_{}", table.name.to_lowercase())
            } else {
                format!("1_{}", table.name.to_lowercase())
            };

            CompletionItem::new(kind, &table.name)
                .with_detail(detail)
                .with_sort_key(sort_key)
                .with_data(CompletionData::Table {
                    schema: table.schema.clone(),
                    name: table.name.clone(),
                })
                .with_documentation(table.description.clone().unwrap_or_default())
        })
        .collect()
}

/// Generates schema completion items.
pub fn complete_schemas(provider: Option<&dyn SchemaProvider>) -> Vec<CompletionItem> {
    let Some(provider) = provider else {
        return Vec::new();
    };

    provider
        .schemas()
        .into_iter()
        .map(|schema| {
            CompletionItem::new(CompletionItemKind::Schema, &schema)
                .with_insert_text(format!("{}.", schema))
                .with_sort_key(if schema == "public" {
                    format!("0_{}", schema)
                } else {
                    format!("1_{}", schema)
                })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MemorySchemaProvider;
    use crate::types::TableInfo;

    #[test]
    fn test_complete_tables_empty() {
        let items = complete_tables(None, None, None);
        assert!(items.is_empty());
    }

    #[test]
    fn test_complete_tables_with_provider() {
        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users").with_schema("public"))
            .add_table(TableInfo::new("orders").with_schema("public"))
            .add_table(TableInfo::new("products").with_schema("inventory"));

        let items = complete_tables(Some(&provider), None, None);
        assert_eq!(items.len(), 3);

        // Check that public tables are sorted first
        let users_item = items.iter().find(|i| i.label == "users").unwrap();
        let products_item = items.iter().find(|i| i.label == "products").unwrap();
        assert!(users_item.effective_sort_key() < products_item.effective_sort_key());
    }

    #[test]
    fn test_complete_tables_with_prefix() {
        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users"))
            .add_table(TableInfo::new("user_roles"))
            .add_table(TableInfo::new("orders"));

        let items = complete_tables(Some(&provider), None, Some("user"));
        assert_eq!(items.len(), 2);
        assert!(items.iter().all(|i| i.label.starts_with("user")));
    }

    #[test]
    fn test_complete_tables_with_schema() {
        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users").with_schema("public"))
            .add_table(TableInfo::new("products").with_schema("inventory"));

        let items = complete_tables(Some(&provider), Some("inventory"), None);
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "products");
    }
}
