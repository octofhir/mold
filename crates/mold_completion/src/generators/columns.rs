//! Column completion generator.

use crate::providers::SchemaProvider;
use crate::types::{CompletionData, CompletionItem, CompletionItemKind};

/// Generates column completion items.
///
/// If `table` is specified, returns columns for that table only.
/// Otherwise, returns columns from all tables in `tables_in_scope`.
pub fn complete_columns(
    provider: Option<&dyn SchemaProvider>,
    table: Option<&str>,
    tables_in_scope: &[String],
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    let Some(provider) = provider else {
        return Vec::new();
    };

    let mut items = Vec::new();

    if let Some(table_name) = table {
        // Columns for a specific table
        let columns = match prefix {
            Some(p) => provider.columns_by_prefix(None, table_name, p),
            None => provider.columns(None, table_name),
        };

        for col in columns {
            items.push(create_column_item(
                &col.name,
                &col.data_type,
                Some(table_name),
            ));
        }
    } else {
        // Columns from all tables in scope
        for table_name in tables_in_scope {
            let columns = provider.columns(None, table_name);
            for col in columns {
                let matches_prefix = match prefix {
                    Some(p) => col.name.to_lowercase().starts_with(&p.to_lowercase()),
                    None => true,
                };

                if matches_prefix {
                    items.push(create_column_item(
                        &col.name,
                        &col.data_type,
                        Some(table_name),
                    ));
                }
            }
        }
    }

    // Sort by name
    items.sort_by(|a, b| a.label.to_lowercase().cmp(&b.label.to_lowercase()));

    items
}

/// Creates a completion item for a column.
fn create_column_item(name: &str, data_type: &str, table: Option<&str>) -> CompletionItem {
    let detail = if data_type.is_empty() {
        table.unwrap_or("").to_string()
    } else {
        format!("{} ({})", table.unwrap_or(""), data_type)
    };

    // Sort key: primary key columns first, then alphabetically
    let sort_key = format!("1_{}", name.to_lowercase());

    CompletionItem::new(CompletionItemKind::Column, name)
        .with_detail(detail)
        .with_sort_key(sort_key)
        .with_data(CompletionData::Column {
            table: table.map(String::from),
            name: name.to_string(),
            data_type: Some(data_type.to_string()),
        })
}

/// Generates qualified column completions (table.column).
pub fn complete_qualified_columns(
    provider: Option<&dyn SchemaProvider>,
    table: &str,
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    let Some(provider) = provider else {
        return Vec::new();
    };

    let columns = match prefix {
        Some(p) => provider.columns_by_prefix(None, table, p),
        None => provider.columns(None, table),
    };

    columns
        .into_iter()
        .map(|col| {
            CompletionItem::new(CompletionItemKind::Column, &col.name)
                .with_detail(col.data_type.clone())
                .with_insert_text(col.name.clone())
                .with_data(CompletionData::Column {
                    table: Some(table.to_string()),
                    name: col.name,
                    data_type: Some(col.data_type),
                })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MemorySchemaProvider;
    use crate::types::{ColumnInfo, TableInfo};

    fn create_test_provider() -> MemorySchemaProvider {
        MemorySchemaProvider::new()
            .add_table(TableInfo::new("users"))
            .add_columns(
                None,
                "users",
                vec![
                    ColumnInfo::new("id", "integer"),
                    ColumnInfo::new("name", "text"),
                    ColumnInfo::new("email", "text"),
                ],
            )
            .add_table(TableInfo::new("orders"))
            .add_columns(
                None,
                "orders",
                vec![
                    ColumnInfo::new("id", "integer"),
                    ColumnInfo::new("user_id", "integer"),
                    ColumnInfo::new("amount", "numeric"),
                ],
            )
    }

    #[test]
    fn test_complete_columns_empty() {
        let items = complete_columns(None, None, &[], None);
        assert!(items.is_empty());
    }

    #[test]
    fn test_complete_columns_for_table() {
        let provider = create_test_provider();
        let items = complete_columns(Some(&provider), Some("users"), &[], None);
        assert_eq!(items.len(), 3);
        assert!(items.iter().any(|i| i.label == "id"));
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "email"));
    }

    #[test]
    fn test_complete_columns_all_in_scope() {
        let provider = create_test_provider();
        let tables = vec!["users".to_string(), "orders".to_string()];
        let items = complete_columns(Some(&provider), None, &tables, None);
        assert_eq!(items.len(), 6);
    }

    #[test]
    fn test_complete_columns_with_prefix() {
        let provider = create_test_provider();
        let tables = vec!["users".to_string(), "orders".to_string()];
        let items = complete_columns(Some(&provider), None, &tables, Some("id"));
        assert_eq!(items.len(), 2); // id from users, id from orders
    }

    #[test]
    fn test_complete_qualified_columns() {
        let provider = create_test_provider();
        let items = complete_qualified_columns(Some(&provider), "users", None);
        assert_eq!(items.len(), 3);
    }
}
