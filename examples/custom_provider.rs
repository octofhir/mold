//! Drive completion with a custom schema provider.

use mold_completion::providers::SchemaProvider;
use mold_completion::types::JsonbSchema;
use mold_completion::{
    ColumnInfo, CompletionRequest, CompletionResult, MemorySchemaProvider, TableInfo, complete,
};
use text_size::TextSize;

/// A provider that delegates to an in-memory store. Replace the body of these
/// methods to source schema from a catalog, a cache, or a live database.
struct CustomProvider {
    inner: MemorySchemaProvider,
}

impl CustomProvider {
    fn new() -> Self {
        let inner = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users"))
            .add_columns(
                None,
                "users",
                vec![
                    ColumnInfo::new("id", "integer"),
                    ColumnInfo::new("email", "text"),
                ],
            );
        Self { inner }
    }
}

impl SchemaProvider for CustomProvider {
    fn tables(&self) -> Vec<TableInfo> {
        self.inner.tables()
    }

    fn columns(&self, schema: Option<&str>, table: &str) -> Vec<ColumnInfo> {
        self.inner.columns(schema, table)
    }

    fn jsonb_schema(
        &self,
        _schema: Option<&str>,
        _table: &str,
        _column: &str,
    ) -> Option<JsonbSchema> {
        None
    }
}

fn main() {
    let provider = CustomProvider::new();
    let source = "SELECT id, email FROM ";
    let request = CompletionRequest::new(source, TextSize::new(source.len() as u32))
        .with_schema_provider(&provider);
    let CompletionResult { items, .. } = complete(request);

    for item in items {
        println!("{:?}  {}", item.kind, item.label);
    }
}
