//! JSONB path completion generator.

use crate::providers::SchemaProvider;
use crate::types::{
    CompletionData, CompletionItem, CompletionItemKind, JsonbField, JsonbFieldType, JsonbSchema,
};

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
    complete_jsonpath_with_schema(current_path, None)
}

/// Generates JSONPath expression completion items with optional schema awareness.
///
/// When schema is provided, suggests field names from the known structure.
pub fn complete_jsonpath_with_schema(
    current_path: &str,
    schema: Option<&JsonbSchema>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Parse the current path to determine context
    let parsed = parse_jsonpath_position(current_path);

    match parsed.context {
        JsonPathContext::Root => {
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
        }
        JsonPathContext::MemberAccess => {
            // After a dot - suggest field names from schema
            if let Some(schema) = schema {
                let fields = schema.fields_at_path(&parsed.path);
                for field in fields {
                    items.push(create_jsonpath_field_item(field, &parsed.path));
                }
            }
            // Also suggest wildcard
            items.push(
                CompletionItem::new(CompletionItemKind::Snippet, "*")
                    .with_detail("Wildcard")
                    .with_documentation("All members"),
            );
        }
        JsonPathContext::ArrayAccess => {
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
        JsonPathContext::FilterPredicate => {
            // Inside filter predicate ?(@.
            items.extend(complete_jsonpath_filter(&parsed.filter_path, schema));
        }
        JsonPathContext::General => {
            // General context - suggest common operations
        }
    }

    // Always suggest common operations
    items.extend(jsonpath_operations());

    items
}

/// Creates a completion item for a JSONB field in JSONPath context.
fn create_jsonpath_field_item(field: &JsonbField, current_path: &[String]) -> CompletionItem {
    let type_label = match field.field_type {
        JsonbFieldType::Object => "object",
        JsonbFieldType::Array => "array",
        JsonbFieldType::String => "string",
        JsonbFieldType::Number => "number",
        JsonbFieldType::Boolean => "boolean",
        JsonbFieldType::Null => "null",
        JsonbFieldType::Unknown => "unknown",
    };

    let mut full_path = current_path.to_vec();
    full_path.push(field.name.clone());

    CompletionItem::new(CompletionItemKind::JsonbPath, &field.name)
        .with_detail(type_label.to_string())
        .with_sort_key(format!("0_{}", field.name.to_lowercase()))
        .with_documentation(field.description.clone().unwrap_or_default())
}

/// Context for JSONPath completion.
#[derive(Debug, Clone, PartialEq, Eq)]
enum JsonPathContext {
    /// At the root or empty path
    Root,
    /// After a dot (member access)
    MemberAccess,
    /// Inside array brackets
    ArrayAccess,
    /// Inside a filter predicate ?(@.
    FilterPredicate,
    /// General context
    General,
}

/// Parsed JSONPath position for completion.
#[derive(Debug, Clone)]
struct JsonPathPosition {
    /// Current context
    context: JsonPathContext,
    /// Path segments traversed so far
    path: Vec<String>,
    /// Path inside filter predicate (if applicable)
    filter_path: String,
}

/// Parses a JSONPath string to determine completion context.
fn parse_jsonpath_position(path: &str) -> JsonPathPosition {
    let path = path.trim();

    if path.is_empty() || path == "$" {
        return JsonPathPosition {
            context: JsonPathContext::Root,
            path: Vec::new(),
            filter_path: String::new(),
        };
    }

    // Check for filter predicate context
    if let Some(filter_start) = path.rfind("?(@") {
        let filter_content = &path[filter_start + 3..];
        return JsonPathPosition {
            context: JsonPathContext::FilterPredicate,
            path: extract_jsonpath_segments(path),
            filter_path: filter_content.to_string(),
        };
    }

    // Check for array access context
    if path.ends_with('[') {
        return JsonPathPosition {
            context: JsonPathContext::ArrayAccess,
            path: extract_jsonpath_segments(path),
            filter_path: String::new(),
        };
    }

    // Check for member access context
    if path.ends_with('.') {
        return JsonPathPosition {
            context: JsonPathContext::MemberAccess,
            path: extract_jsonpath_segments(path),
            filter_path: String::new(),
        };
    }

    JsonPathPosition {
        context: JsonPathContext::General,
        path: extract_jsonpath_segments(path),
        filter_path: String::new(),
    }
}

/// Extracts path segments from a JSONPath string.
fn extract_jsonpath_segments(path: &str) -> Vec<String> {
    let mut segments = Vec::new();
    let mut current = String::new();
    let mut in_brackets = false;

    for c in path.chars() {
        match c {
            '$' => continue,
            '.' if !in_brackets => {
                if !current.is_empty() {
                    segments.push(current.clone());
                    current.clear();
                }
            }
            '[' => {
                if !current.is_empty() {
                    segments.push(current.clone());
                    current.clear();
                }
                in_brackets = true;
            }
            ']' => {
                if !current.is_empty() {
                    segments.push(current.clone());
                    current.clear();
                }
                in_brackets = false;
            }
            '?' | '@' | '(' | ')' => {
                // Skip filter syntax characters
            }
            _ => {
                current.push(c);
            }
        }
    }

    if !current.is_empty() {
        segments.push(current);
    }

    segments
}

/// Generates completions for filter predicates inside ?(@.
pub fn complete_jsonpath_filter(
    filter_path: &str,
    schema: Option<&JsonbSchema>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // If we're right after ?(@, suggest field names
    if filter_path.is_empty() || filter_path.ends_with('.') {
        if let Some(schema) = schema {
            for field in &schema.fields {
                // Suggest field with appropriate comparison based on type
                let comparison = match field.field_type {
                    JsonbFieldType::String => " == \"value\"",
                    JsonbFieldType::Number => " == 0",
                    JsonbFieldType::Boolean => " == true",
                    _ => ".exists()",
                };

                items.push(
                    CompletionItem::new(CompletionItemKind::Snippet, &field.name)
                        .with_detail(format!("{:?}", field.field_type))
                        .with_insert_text(format!("{}{}", field.name, comparison))
                        .with_documentation(format!("Filter by {} field", field.name)),
                );
            }
        }

        // Always suggest common filter patterns
        items.push(
            CompletionItem::new(CompletionItemKind::Snippet, "@.field == value")
                .with_detail("Equality filter")
                .with_insert_text("@.")
                .with_documentation("Filter where field equals value"),
        );
    } else {
        // After typing some filter content, suggest comparison operators
        items.extend(jsonpath_filter_operators());
    }

    items
}

/// Returns JSONPath filter comparison operators.
fn jsonpath_filter_operators() -> Vec<CompletionItem> {
    vec![
        CompletionItem::new(CompletionItemKind::Operator, "==")
            .with_detail("Equals")
            .with_documentation("Equality comparison"),
        CompletionItem::new(CompletionItemKind::Operator, "!=")
            .with_detail("Not equals")
            .with_documentation("Inequality comparison"),
        CompletionItem::new(CompletionItemKind::Operator, ">")
            .with_detail("Greater than")
            .with_documentation("Greater than comparison"),
        CompletionItem::new(CompletionItemKind::Operator, ">=")
            .with_detail("Greater or equal")
            .with_documentation("Greater than or equal comparison"),
        CompletionItem::new(CompletionItemKind::Operator, "<")
            .with_detail("Less than")
            .with_documentation("Less than comparison"),
        CompletionItem::new(CompletionItemKind::Operator, "<=")
            .with_detail("Less or equal")
            .with_documentation("Less than or equal comparison"),
        CompletionItem::new(CompletionItemKind::Operator, "&&")
            .with_detail("Logical AND")
            .with_documentation("Combine conditions with AND"),
        CompletionItem::new(CompletionItemKind::Operator, "||")
            .with_detail("Logical OR")
            .with_documentation("Combine conditions with OR"),
        CompletionItem::new(CompletionItemKind::Snippet, "like_regex")
            .with_detail("Regex match")
            .with_insert_text("like_regex \"pattern\"")
            .with_documentation("Match against regular expression"),
        CompletionItem::new(CompletionItemKind::Snippet, "starts with")
            .with_detail("Prefix match")
            .with_insert_text("starts with \"prefix\"")
            .with_documentation("Check if string starts with prefix"),
        CompletionItem::new(CompletionItemKind::Function, ".exists()")
            .with_detail("Key exists")
            .with_documentation("Check if key exists"),
        CompletionItem::new(CompletionItemKind::Function, ".type()")
            .with_detail("Type check")
            .with_insert_text(".type() == \"string\"")
            .with_documentation("Check value type"),
        CompletionItem::new(CompletionItemKind::Function, ".size()")
            .with_detail("Array size")
            .with_insert_text(".size() > 0")
            .with_documentation("Check array size"),
    ]
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

/// Generates completions for common JSONB field names.
///
/// These are suggested when typing path keys in functions like `jsonb_extract_path`.
pub fn complete_common_field_names(prefix: Option<&str>) -> Vec<CompletionItem> {
    // Common field names found in JSONB documents
    let common_fields = [
        ("id", "Unique identifier"),
        ("name", "Name field"),
        ("type", "Type field"),
        ("status", "Status field"),
        ("value", "Value field"),
        ("data", "Data field"),
        ("items", "Array of items"),
        ("meta", "Metadata"),
        ("metadata", "Metadata object"),
        ("attributes", "Attributes object"),
        ("properties", "Properties object"),
        ("config", "Configuration"),
        ("settings", "Settings object"),
        ("options", "Options object"),
        ("tags", "Tags array"),
        ("labels", "Labels object"),
        ("description", "Description text"),
        ("title", "Title text"),
        ("content", "Content field"),
        ("created_at", "Creation timestamp"),
        ("updated_at", "Update timestamp"),
        ("timestamp", "Timestamp field"),
        ("version", "Version field"),
        ("enabled", "Enabled flag"),
        ("active", "Active flag"),
        ("count", "Count field"),
        ("total", "Total field"),
        ("amount", "Amount field"),
        ("price", "Price field"),
        ("quantity", "Quantity field"),
        ("user_id", "User identifier"),
        ("parent_id", "Parent identifier"),
        ("ref", "Reference"),
        ("reference", "Reference field"),
        ("source", "Source field"),
        ("target", "Target field"),
        ("key", "Key field"),
        ("code", "Code field"),
        ("message", "Message field"),
        ("error", "Error field"),
        ("result", "Result field"),
        ("response", "Response field"),
        ("request", "Request field"),
        ("payload", "Payload field"),
        ("body", "Body field"),
        ("header", "Header field"),
        ("headers", "Headers object"),
        ("params", "Parameters"),
        ("parameters", "Parameters object"),
        ("args", "Arguments"),
        ("arguments", "Arguments object"),
        ("context", "Context object"),
        ("env", "Environment"),
        ("environment", "Environment object"),
        // FHIR-specific common fields
        ("resourceType", "FHIR resource type"),
        ("resource", "FHIR resource"),
        ("extension", "FHIR extension"),
        ("identifier", "FHIR identifier"),
        ("coding", "FHIR coding"),
        ("system", "FHIR system URL"),
        ("display", "FHIR display text"),
        ("text", "FHIR text"),
        ("div", "FHIR narrative div"),
        ("url", "URL field"),
        ("valueString", "FHIR string value"),
        ("valueCode", "FHIR code value"),
        ("valueBoolean", "FHIR boolean value"),
        ("valueInteger", "FHIR integer value"),
        ("valueDecimal", "FHIR decimal value"),
        ("valueDateTime", "FHIR datetime value"),
        ("valueCoding", "FHIR coding value"),
        ("valueCodeableConcept", "FHIR codeable concept value"),
        ("valueReference", "FHIR reference value"),
        ("entry", "Bundle entry"),
        ("fullUrl", "Bundle entry URL"),
        ("link", "Link field"),
        ("address", "Address object"),
        ("telecom", "Telecom array"),
        ("contact", "Contact info"),
    ];

    let prefix_lower = prefix.map(|p| p.to_lowercase());

    common_fields
        .iter()
        .filter(|(name, _)| {
            prefix_lower
                .as_ref()
                .is_none_or(|p| name.to_lowercase().starts_with(p))
        })
        .map(|(name, desc)| {
            // Suggest as a quoted string since these are path key arguments
            let insert_text = format!("'{}'", name);
            CompletionItem::new(CompletionItemKind::JsonbPath, *name)
                .with_detail("field name")
                .with_insert_text(insert_text)
                .with_documentation(desc.to_string())
                .with_sort_key(format!("1_{}", name.to_lowercase()))
        })
        .collect()
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
    .map(|(index, item)| item.with_sort_key(format!("9_{index:02}")))
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

    #[test]
    fn test_complete_jsonpath_with_schema() {
        let schema = JsonbSchema::new()
            .with_field(JsonbField::new("name", JsonbFieldType::String))
            .with_field(JsonbField::new("age", JsonbFieldType::Number))
            .with_field(
                JsonbField::new("address", JsonbFieldType::Object).with_nested(
                    JsonbSchema::new()
                        .with_field(JsonbField::new("street", JsonbFieldType::String))
                        .with_field(JsonbField::new("city", JsonbFieldType::String)),
                ),
            );

        // After dot, should suggest schema fields
        let items = complete_jsonpath_with_schema("$.", Some(&schema));
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "age"));
        assert!(items.iter().any(|i| i.label == "address"));
    }

    #[test]
    fn test_complete_jsonpath_filter() {
        let schema = JsonbSchema::new()
            .with_field(JsonbField::new("name", JsonbFieldType::String))
            .with_field(JsonbField::new("active", JsonbFieldType::Boolean));

        // Filter context should suggest fields with type-appropriate comparisons
        let items = complete_jsonpath_filter("", Some(&schema));
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "active"));

        // After typing partial filter, suggest operators
        let items = complete_jsonpath_filter("name", None);
        assert!(items.iter().any(|i| i.label == "=="));
        assert!(items.iter().any(|i| i.label == "!="));
    }

    #[test]
    fn test_parse_jsonpath_position() {
        let pos = parse_jsonpath_position("");
        assert_eq!(pos.context, JsonPathContext::Root);

        let pos = parse_jsonpath_position("$.");
        assert_eq!(pos.context, JsonPathContext::MemberAccess);

        let pos = parse_jsonpath_position("$[");
        assert_eq!(pos.context, JsonPathContext::ArrayAccess);

        let pos = parse_jsonpath_position("$.items[*]?(@");
        assert_eq!(pos.context, JsonPathContext::FilterPredicate);
    }

    #[test]
    fn test_extract_jsonpath_segments() {
        let segments = extract_jsonpath_segments("$.name.first");
        assert_eq!(segments, vec!["name", "first"]);

        let segments = extract_jsonpath_segments("$.items[0].value");
        assert_eq!(segments, vec!["items", "0", "value"]);

        let segments = extract_jsonpath_segments("$[*]");
        assert_eq!(segments, vec!["*"]);
    }

    #[test]
    fn test_complete_common_field_names() {
        // Should return common field names
        let items = complete_common_field_names(None);
        assert!(!items.is_empty());
        assert!(items.iter().any(|i| i.label == "id"));
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "data"));
    }

    #[test]
    fn test_complete_common_field_names_with_prefix() {
        // Should filter by prefix
        let items = complete_common_field_names(Some("val"));
        assert!(!items.is_empty());
        assert!(items.iter().all(|i| i.label.to_lowercase().starts_with("val")));
        assert!(items.iter().any(|i| i.label == "value"));
    }

    #[test]
    fn test_complete_common_field_names_fhir() {
        // Should include FHIR-specific fields
        let items = complete_common_field_names(None);
        assert!(items.iter().any(|i| i.label == "resourceType"));
        assert!(items.iter().any(|i| i.label == "extension"));
        assert!(items.iter().any(|i| i.label == "identifier"));
    }

    #[test]
    fn test_complete_common_field_names_insert_text() {
        // Should include quoted insert text
        let items = complete_common_field_names(Some("id"));
        let id_item = items.iter().find(|i| i.label == "id").unwrap();
        assert_eq!(id_item.insert_text.as_deref(), Some("'id'"));
    }
}
