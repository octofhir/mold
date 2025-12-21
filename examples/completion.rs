use mold_completion::{complete, CompletionRequest, MemorySchemaProvider, TableInfo};
use text_size::TextSize;

fn main() {
    let provider = MemorySchemaProvider::new().add_table(TableInfo::new("users"));
    let source = "SELECT * FROM ";
    let request = CompletionRequest::new(source, TextSize::new(source.len() as u32))
        .with_schema_provider(&provider);
    let result = complete(request);

    for item in result.items {
        println!("{}", item.label);
    }
}
