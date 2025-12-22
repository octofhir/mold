//! JSONB path completion generator.

use crate::providers::SchemaProvider;
use crate::types::{CompletionData, CompletionItem, CompletionItemKind, JsonbFieldType};

/// Generates JSONB path completion items.
pub fn complete_jsonb_paths(
    provider: Option<&dyn SchemaProvider>,
    table: Option<&str>,
    column: &str,
    path: &[String],
) -> Vec<CompletionItem> {
    let Some(provider) = provider else {
        return Vec::new();
    };

    let table_name = table.unwrap_or("");
    let schema = provider.jsonb_schema(None, table_name, column);

    let Some(schema) = schema else {
        return Vec::new();
    };

    let fields = schema.fields_at_path(path);

    fields
        .into_iter()
        .map(|field| {
            let type_label = match field.field_type {
                JsonbFieldType::Object => "object",
                JsonbFieldType::Array => "array",
                JsonbFieldType::String => "string",
                JsonbFieldType::Number => "number",
                JsonbFieldType::Boolean => "boolean",
                JsonbFieldType::Null => "null",
                JsonbFieldType::Unknown => "unknown",
            };

            let mut full_path = path.to_vec();
            full_path.push(field.name.clone());

            // Don't wrap in quotes - the user/editor controls the quoting context.
            // If user is typing inside quotes (e.g., data->'nam|'), we just need the name.
            // If outside quotes (e.g., data->|), the editor should handle insertion appropriately.
            CompletionItem::new(CompletionItemKind::JsonbPath, &field.name)
                .with_detail(type_label.to_string())
                .with_sort_key(format!("0_{}", field.name.to_lowercase()))
                .with_documentation(field.description.clone().unwrap_or_default())
                .with_data(CompletionData::JsonbPath {
                    base_column: column.to_string(),
                    path: full_path,
                })
        })
        .collect()
}

/// Returns true if a JSONB key needs quoting (special chars or starts with digit).
/// Note: For JSONB string keys, PostgreSQL always requires quotes.
/// This function is useful for determining if extra escaping is needed.
#[cfg(test)]
fn needs_quoting(key: &str) -> bool {
    // Keys need quoting if they contain special characters or start with a digit
    if key.is_empty() {
        return true;
    }

    let first = key.chars().next().unwrap();
    if first.is_ascii_digit() {
        return true;
    }

    key.chars().any(|c| !c.is_ascii_alphanumeric() && c != '_')
}

/// Generates JSONPath expression completion items.
pub fn complete_jsonpath(current_path: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Determine what to suggest based on current path
    if current_path.is_empty() || current_path == "$" {
        // At the root - suggest common patterns
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "$")
                .with_detail("Root element")
                .with_documentation("Start of JSONPath expression"),
        );
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "$.key")
                .with_detail("Member access")
                .with_insert_text("$.")
                .with_documentation("Access a member by name"),
        );
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "$[*]")
                .with_detail("Array wildcard")
                .with_documentation("All array elements"),
        );
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "$[0]")
                .with_detail("Array index")
                .with_insert_text("$[0]")
                .with_documentation("First array element"),
        );
    } else if current_path.ends_with('.') {
        // After a dot - suggest member access patterns
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "*")
                .with_detail("Wildcard")
                .with_documentation("All members"),
        );
    } else if current_path.ends_with('[') {
        // Inside array access
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "*]")
                .with_detail("All elements")
                .with_documentation("Wildcard for all array elements"),
        );
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "0]")
                .with_detail("First element")
                .with_documentation("First array element (0-indexed)"),
        );
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "last]")
                .with_detail("Last element")
                .with_documentation("Last array element"),
        );
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "0 to 9]")
                .with_detail("Slice")
                .with_insert_text("0 to 9]")
                .with_documentation("Array slice (range)"),
        );
    }

    // Always suggest common operations
    items.extend(jsonpath_operations());

    items
}

/// Returns common JSONPath operations as completion items.
fn jsonpath_operations() -> Vec<CompletionItem> {
    vec![
        // Accessor patterns
        CompletionItem::new(CompletionItemKind::Snippet, ".key")
            .with_detail("Member access")
            .with_insert_text("."),
        CompletionItem::new(CompletionItemKind::Snippet, "[index]")
            .with_detail("Array access")
            .with_insert_text("["),
        CompletionItem::new(CompletionItemKind::Snippet, ".*")
            .with_detail("Wildcard member")
            .with_insert_text(".*"),
        CompletionItem::new(CompletionItemKind::Snippet, "[*]")
            .with_detail("Wildcard array")
            .with_insert_text("[*]"),
        CompletionItem::new(CompletionItemKind::Snippet, "..")
            .with_detail("Recursive descent")
            .with_documentation("Search all descendants"),
        // Filter
        CompletionItem::new(CompletionItemKind::Snippet, "?(@.key == value)")
            .with_detail("Filter")
            .with_insert_text("?(@.")
            .with_documentation("Filter array elements"),
        // Methods
        CompletionItem::new(CompletionItemKind::Function, ".size()")
            .with_detail("Array/object size")
            .with_documentation("Returns the size of an array or object"),
        CompletionItem::new(CompletionItemKind::Function, ".type()")
            .with_detail("Value type")
            .with_documentation("Returns the type of the value"),
        CompletionItem::new(CompletionItemKind::Function, ".double()")
            .with_detail("Convert to double")
            .with_documentation("Converts a string to a double"),
        CompletionItem::new(CompletionItemKind::Function, ".ceiling()")
            .with_detail("Ceiling")
            .with_documentation("Rounds up to nearest integer"),
        CompletionItem::new(CompletionItemKind::Function, ".floor()")
            .with_detail("Floor")
            .with_documentation("Rounds down to nearest integer"),
        CompletionItem::new(CompletionItemKind::Function, ".abs()")
            .with_detail("Absolute value")
            .with_documentation("Returns the absolute value"),
        CompletionItem::new(CompletionItemKind::Function, ".keyvalue()")
            .with_detail("Key-value pairs")
            .with_documentation("Converts object to key-value pairs"),
    ]
}

/// Generates completions for JSONB operators.
pub fn complete_jsonb_operators() -> Vec<CompletionItem> {
    vec![
        CompletionItem::new(CompletionItemKind::Operator, "->")
            .with_detail("Get JSON element")
            .with_documentation("Extract JSON element by key or index"),
        CompletionItem::new(CompletionItemKind::Operator, "->>")
            .with_detail("Get JSON element as text")
            .with_documentation("Extract JSON element as text"),
        CompletionItem::new(CompletionItemKind::Operator, "#>")
            .with_detail("Get JSON at path")
            .with_documentation("Extract JSON at specified path"),
        CompletionItem::new(CompletionItemKind::Operator, "#>>")
            .with_detail("Get JSON at path as text")
            .with_documentation("Extract JSON at path as text"),
        CompletionItem::new(CompletionItemKind::Operator, "@>")
            .with_detail("Contains")
            .with_documentation("Does left contain right?"),
        CompletionItem::new(CompletionItemKind::Operator, "<@")
            .with_detail("Contained by")
            .with_documentation("Is left contained in right?"),
        CompletionItem::new(CompletionItemKind::Operator, "?")
            .with_detail("Key exists")
            .with_documentation("Does key exist in JSON?"),
        CompletionItem::new(CompletionItemKind::Operator, "?|")
            .with_detail("Any key exists")
            .with_documentation("Do any of these keys exist?"),
        CompletionItem::new(CompletionItemKind::Operator, "?&")
            .with_detail("All keys exist")
            .with_documentation("Do all of these keys exist?"),
        CompletionItem::new(CompletionItemKind::Operator, "||")
            .with_detail("Concatenate")
            .with_documentation("Concatenate two JSONB values"),
        CompletionItem::new(CompletionItemKind::Operator, "-")
            .with_detail("Delete key/index")
            .with_documentation("Delete key or array element"),
        CompletionItem::new(CompletionItemKind::Operator, "#-")
            .with_detail("Delete at path")
            .with_documentation("Delete element at path"),
        CompletionItem::new(CompletionItemKind::Operator, "@?")
            .with_detail("JSONPath exists")
            .with_documentation("Does JSONPath return any items?"),
        CompletionItem::new(CompletionItemKind::Operator, "@@")
            .with_detail("JSONPath match")
            .with_documentation("JSONPath predicate check"),
    ]
    .into_iter()
    .enumerate()
    .map(|(index, item)| {
        item.with_sort_key(format!("9_{index:02}"))
    })
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MemorySchemaProvider;
    use crate::types::{JsonbField, JsonbSchema, TableInfo};

    #[test]
    fn test_complete_jsonb_paths_no_schema() {
        let items = complete_jsonb_paths(None, Some("users"), "data", &[]);
        assert!(items.is_empty());
    }

    #[test]
    fn test_complete_jsonb_paths_with_schema() {
        let schema = JsonbSchema::new()
            .with_field(JsonbField::new("name", JsonbFieldType::String))
            .with_field(
                JsonbField::new("address", JsonbFieldType::Object).with_nested(
                    JsonbSchema::new()
                        .with_field(JsonbField::new("street", JsonbFieldType::String))
                        .with_field(JsonbField::new("city", JsonbFieldType::String)),
                ),
            );

        let provider = MemorySchemaProvider::new()
            .add_table(TableInfo::new("users"))
            .add_jsonb_schema(None, "users", "data", schema);

        // Root level
        let items = complete_jsonb_paths(Some(&provider), Some("users"), "data", &[]);
        assert_eq!(items.len(), 2);
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "address"));

        // Nested level
        let items = complete_jsonb_paths(
            Some(&provider),
            Some("users"),
            "data",
            &["address".to_string()],
        );
        assert_eq!(items.len(), 2);
        assert!(items.iter().any(|i| i.label == "street"));
        assert!(items.iter().any(|i| i.label == "city"));
    }

    #[test]
    fn test_complete_jsonpath() {
        let items = complete_jsonpath("");
        assert!(!items.is_empty());
        assert!(items.iter().any(|i| i.label == "$"));

        let items = complete_jsonpath("$.");
        assert!(!items.is_empty());

        let items = complete_jsonpath("$[");
        assert!(items.iter().any(|i| i.label == "*]"));
    }

    #[test]
    fn test_complete_jsonb_operators() {
        let items = complete_jsonb_operators();
        assert!(items.iter().any(|i| i.label == "->"));
        assert!(items.iter().any(|i| i.label == "->>"));
        assert!(items.iter().any(|i| i.label == "@>"));
    }

    #[test]
    fn test_needs_quoting() {
        assert!(!needs_quoting("name"));
        assert!(!needs_quoting("user_id"));
        assert!(needs_quoting("123"));
        assert!(needs_quoting("with-dash"));
        assert!(needs_quoting("has space"));
        assert!(needs_quoting(""));
    }
}
