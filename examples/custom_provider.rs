use mold_completion::{
    complete, ColumnInfo, CompletionRequest, CompletionResult, MemorySchemaProvider, SchemaProvider,
    TableInfo,
};
use text_size::TextSize;

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
                    ColumnInfo::new("id", "int"),
                    ColumnInfo::new("email", "text"),
                ],
            );
        Self { inner }
    }
}

impl SchemaProvider for CustomProvider {
    fn tables(&self, schema: Option<&str>) -> Vec<TableInfo> {
        self.inner.tables(schema)
    }

    fn columns(&self, schema: Option<&str>, table: &str) -> Vec<ColumnInfo> {
        self.inner.columns(schema, table)
    }

    fn has_schema(&self, schema: &str) -> bool {
        self.inner.has_schema(schema)
    }
}

fn main() {
    let provider = CustomProvider::new();
    let source = "SELECT ";
    let request = CompletionRequest::new(source, TextSize::new(source.len() as u32))
        .with_schema_provider(&provider);
    let CompletionResult { items, .. } = complete(request);

    for item in items {
        println!("{}", item.label);
    }
}
