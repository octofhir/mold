//! Function completion generator.

use crate::providers::FunctionProvider;
use crate::types::{CompletionData, CompletionItem, CompletionItemKind, FunctionInfo};

/// Generates function completion items.
pub fn complete_functions(
    provider: Option<&dyn FunctionProvider>,
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    let Some(provider) = provider else {
        return builtin_functions(prefix);
    };

    let functions = match prefix {
        Some(p) => provider.functions_by_prefix(p),
        None => provider.functions(),
    };

    let mut items: Vec<_> = functions.into_iter().map(create_function_item).collect();

    // Also include built-in functions
    items.extend(builtin_functions(prefix));

    // Sort by name
    items.sort_by(|a, b| a.label.to_lowercase().cmp(&b.label.to_lowercase()));

    // Deduplicate by label
    items.dedup_by(|a, b| a.label.to_lowercase() == b.label.to_lowercase());

    items
}

/// Creates a completion item for a function.
fn create_function_item(func: FunctionInfo) -> CompletionItem {
    let signature = func.signature();
    let insert_text = format!("{}(", func.name);

    // Sort key: frequently used functions first
    let sort_key = if is_common_function(&func.name) {
        format!("0_{}", func.name.to_lowercase())
    } else {
        format!("1_{}", func.name.to_lowercase())
    };

    CompletionItem::new(CompletionItemKind::Function, &func.name)
        .with_detail(signature.clone())
        .with_insert_text(insert_text)
        .with_sort_key(sort_key)
        .with_data(CompletionData::Function {
            schema: func.schema.clone(),
            name: func.name.clone(),
            signature: Some(signature),
        })
        .with_documentation(func.description.clone().unwrap_or_default())
}

/// Returns true if this is a commonly used function.
fn is_common_function(name: &str) -> bool {
    let common = [
        "count",
        "sum",
        "avg",
        "min",
        "max",
        "coalesce",
        "nullif",
        "now",
        "current_timestamp",
        "extract",
        "cast",
        "to_char",
        "to_date",
        "to_timestamp",
        "array_agg",
        "string_agg",
        "jsonb_build_object",
        "jsonb_agg",
        "row_number",
        "rank",
        "dense_rank",
        "lag",
        "lead",
        "first_value",
        "last_value",
    ];
    common.iter().any(|c| c.eq_ignore_ascii_case(name))
}

/// Returns built-in SQL functions.
fn builtin_functions(prefix: Option<&str>) -> Vec<CompletionItem> {
    let functions = [
        // Aggregate functions
        ("count", "count(expression) -> bigint", "Count rows"),
        ("sum", "sum(expression) -> numeric", "Sum of values"),
        ("avg", "avg(expression) -> numeric", "Average of values"),
        ("min", "min(expression) -> same as input", "Minimum value"),
        ("max", "max(expression) -> same as input", "Maximum value"),
        (
            "array_agg",
            "array_agg(expression) -> array",
            "Aggregate into array",
        ),
        (
            "string_agg",
            "string_agg(expression, delimiter) -> text",
            "Aggregate into string",
        ),
        ("bool_and", "bool_and(expression) -> boolean", "Logical AND"),
        ("bool_or", "bool_or(expression) -> boolean", "Logical OR"),
        // JSON/JSONB functions
        (
            "jsonb_build_object",
            "jsonb_build_object(key, value, ...) -> jsonb",
            "Build JSONB object",
        ),
        (
            "jsonb_build_array",
            "jsonb_build_array(value, ...) -> jsonb",
            "Build JSONB array",
        ),
        (
            "jsonb_agg",
            "jsonb_agg(expression) -> jsonb",
            "Aggregate into JSONB array",
        ),
        (
            "jsonb_object_agg",
            "jsonb_object_agg(key, value) -> jsonb",
            "Aggregate into JSONB object",
        ),
        (
            "jsonb_extract_path",
            "jsonb_extract_path(jsonb, path...) -> jsonb",
            "Extract JSONB path",
        ),
        (
            "jsonb_extract_path_text",
            "jsonb_extract_path_text(jsonb, path...) -> text",
            "Extract JSONB path as text",
        ),
        (
            "jsonb_set",
            "jsonb_set(jsonb, path, value) -> jsonb",
            "Set JSONB value at path",
        ),
        (
            "jsonb_insert",
            "jsonb_insert(jsonb, path, value) -> jsonb",
            "Insert into JSONB",
        ),
        (
            "jsonb_pretty",
            "jsonb_pretty(jsonb) -> text",
            "Pretty-print JSONB",
        ),
        (
            "jsonb_typeof",
            "jsonb_typeof(jsonb) -> text",
            "Return JSONB type",
        ),
        (
            "jsonb_array_elements",
            "jsonb_array_elements(jsonb) -> setof jsonb",
            "Expand JSONB array",
        ),
        (
            "jsonb_array_elements_text",
            "jsonb_array_elements_text(jsonb) -> setof text",
            "Expand JSONB array as text",
        ),
        (
            "jsonb_object_keys",
            "jsonb_object_keys(jsonb) -> setof text",
            "Return JSONB object keys",
        ),
        (
            "jsonb_each",
            "jsonb_each(jsonb) -> setof (key, value)",
            "Expand JSONB to key-value pairs",
        ),
        (
            "jsonb_each_text",
            "jsonb_each_text(jsonb) -> setof (key, value)",
            "Expand JSONB to text key-value pairs",
        ),
        (
            "jsonb_strip_nulls",
            "jsonb_strip_nulls(jsonb) -> jsonb",
            "Remove null fields",
        ),
        ("to_jsonb", "to_jsonb(value) -> jsonb", "Convert to JSONB"),
        // Window functions
        (
            "row_number",
            "row_number() -> bigint",
            "Row number in partition",
        ),
        ("rank", "rank() -> bigint", "Rank with gaps"),
        ("dense_rank", "dense_rank() -> bigint", "Rank without gaps"),
        ("ntile", "ntile(n) -> integer", "Divide into n buckets"),
        (
            "lag",
            "lag(value, offset, default) -> same as input",
            "Previous row value",
        ),
        (
            "lead",
            "lead(value, offset, default) -> same as input",
            "Next row value",
        ),
        (
            "first_value",
            "first_value(value) -> same as input",
            "First value in window",
        ),
        (
            "last_value",
            "last_value(value) -> same as input",
            "Last value in window",
        ),
        (
            "nth_value",
            "nth_value(value, n) -> same as input",
            "Nth value in window",
        ),
        // String functions
        ("concat", "concat(text, ...) -> text", "Concatenate strings"),
        (
            "concat_ws",
            "concat_ws(separator, text, ...) -> text",
            "Concatenate with separator",
        ),
        ("length", "length(text) -> integer", "String length"),
        ("lower", "lower(text) -> text", "Convert to lowercase"),
        ("upper", "upper(text) -> text", "Convert to uppercase"),
        ("trim", "trim(text) -> text", "Remove whitespace"),
        ("ltrim", "ltrim(text) -> text", "Remove leading whitespace"),
        ("rtrim", "rtrim(text) -> text", "Remove trailing whitespace"),
        (
            "substring",
            "substring(text, start, length) -> text",
            "Extract substring",
        ),
        (
            "replace",
            "replace(text, from, to) -> text",
            "Replace occurrences",
        ),
        (
            "split_part",
            "split_part(text, delimiter, n) -> text",
            "Split and return part",
        ),
        (
            "regexp_replace",
            "regexp_replace(text, pattern, replacement) -> text",
            "Regex replace",
        ),
        (
            "regexp_matches",
            "regexp_matches(text, pattern) -> text[]",
            "Regex matches",
        ),
        // Date/time functions
        (
            "now",
            "now() -> timestamp with time zone",
            "Current timestamp",
        ),
        (
            "current_timestamp",
            "current_timestamp -> timestamp with time zone",
            "Current timestamp",
        ),
        ("current_date", "current_date -> date", "Current date"),
        (
            "current_time",
            "current_time -> time with time zone",
            "Current time",
        ),
        (
            "extract",
            "extract(field from source) -> numeric",
            "Extract date/time field",
        ),
        (
            "date_trunc",
            "date_trunc(field, source) -> timestamp",
            "Truncate to specified precision",
        ),
        (
            "date_part",
            "date_part(field, source) -> double precision",
            "Extract date/time part",
        ),
        (
            "age",
            "age(timestamp, timestamp) -> interval",
            "Calculate age",
        ),
        (
            "to_char",
            "to_char(timestamp, format) -> text",
            "Format as string",
        ),
        ("to_date", "to_date(text, format) -> date", "Parse date"),
        (
            "to_timestamp",
            "to_timestamp(text, format) -> timestamp",
            "Parse timestamp",
        ),
        // Conditional functions
        (
            "coalesce",
            "coalesce(value, ...) -> same as first non-null",
            "Return first non-null",
        ),
        (
            "nullif",
            "nullif(value1, value2) -> same as input",
            "Return null if equal",
        ),
        (
            "greatest",
            "greatest(value, ...) -> same as input",
            "Return greatest value",
        ),
        (
            "least",
            "least(value, ...) -> same as input",
            "Return smallest value",
        ),
        // Math functions
        ("abs", "abs(numeric) -> numeric", "Absolute value"),
        ("ceil", "ceil(numeric) -> numeric", "Round up"),
        ("floor", "floor(numeric) -> numeric", "Round down"),
        (
            "round",
            "round(numeric, scale) -> numeric",
            "Round to scale",
        ),
        (
            "trunc",
            "trunc(numeric, scale) -> numeric",
            "Truncate to scale",
        ),
        ("sqrt", "sqrt(numeric) -> double precision", "Square root"),
        (
            "power",
            "power(base, exponent) -> double precision",
            "Raise to power",
        ),
        ("mod", "mod(dividend, divisor) -> numeric", "Modulo"),
        ("random", "random() -> double precision", "Random value 0-1"),
        // Array functions
        (
            "array_length",
            "array_length(array, dimension) -> integer",
            "Array length",
        ),
        (
            "array_append",
            "array_append(array, element) -> array",
            "Append to array",
        ),
        (
            "array_prepend",
            "array_prepend(element, array) -> array",
            "Prepend to array",
        ),
        (
            "array_cat",
            "array_cat(array, array) -> array",
            "Concatenate arrays",
        ),
        (
            "array_position",
            "array_position(array, element) -> integer",
            "Find element position",
        ),
        (
            "array_remove",
            "array_remove(array, element) -> array",
            "Remove element",
        ),
        (
            "unnest",
            "unnest(array) -> setof element",
            "Expand array to rows",
        ),
        // Type conversion
        ("cast", "cast(value AS type) -> type", "Convert type"),
    ];

    let prefix_lower = prefix.map(|p| p.to_lowercase());

    functions
        .iter()
        .filter(|(name, _, _)| {
            prefix_lower
                .as_ref()
                .is_none_or(|p| name.to_lowercase().starts_with(p))
        })
        .map(|(name, signature, doc)| {
            let sort_key = if is_common_function(name) {
                format!("0_{}", name)
            } else {
                format!("1_{}", name)
            };

            CompletionItem::new(CompletionItemKind::Function, *name)
                .with_detail(signature.to_string())
                .with_insert_text(format!("{}(", name))
                .with_sort_key(sort_key)
                .with_documentation(doc.to_string())
                .with_data(CompletionData::Function {
                    schema: None,
                    name: name.to_string(),
                    signature: Some(signature.to_string()),
                })
        })
        .collect()
}

/// Returns function signature help for a specific function.
pub fn get_function_signature(
    provider: Option<&dyn FunctionProvider>,
    name: &str,
) -> Option<FunctionInfo> {
    if let Some(provider) = provider {
        if let Some(func) = provider.function(None, name) {
            return Some(func);
        }
    }

    // Check built-in functions
    get_builtin_function_info(name)
}

/// Returns info for a built-in function.
fn get_builtin_function_info(name: &str) -> Option<FunctionInfo> {
    use crate::types::FunctionArg;

    let name_lower = name.to_lowercase();
    match name_lower.as_str() {
        "count" => {
            Some(FunctionInfo::new("count", "bigint").with_arg(FunctionArg::new("expression")))
        }
        "sum" => Some(FunctionInfo::new("sum", "numeric").with_arg(FunctionArg::new("expression"))),
        "avg" => Some(FunctionInfo::new("avg", "numeric").with_arg(FunctionArg::new("expression"))),
        "min" | "max" => Some(
            FunctionInfo::new(&name_lower, "same as input")
                .with_arg(FunctionArg::new("expression")),
        ),
        "coalesce" => Some(
            FunctionInfo::new("coalesce", "same as first non-null")
                .with_arg(FunctionArg::new("value").with_name("value1"))
                .with_arg(FunctionArg::new("value").with_name("value2")),
        ),
        "nullif" => Some(
            FunctionInfo::new("nullif", "same as input")
                .with_arg(FunctionArg::new("value").with_name("value1"))
                .with_arg(FunctionArg::new("value").with_name("value2")),
        ),
        "jsonb_build_object" => Some(
            FunctionInfo::new("jsonb_build_object", "jsonb")
                .with_arg(FunctionArg::new("any").with_name("key"))
                .with_arg(FunctionArg::new("any").with_name("value")),
        ),
        "jsonb_extract_path" => Some(
            FunctionInfo::new("jsonb_extract_path", "jsonb")
                .with_arg(FunctionArg::new("jsonb").with_name("from_json"))
                .with_arg(
                    FunctionArg::new("text")
                        .with_name("path_elems")
                        .with_mode(crate::types::ArgMode::Variadic),
                ),
        ),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MemoryFunctionProvider;

    #[test]
    fn test_complete_functions_builtin() {
        let items = complete_functions(None, None);
        assert!(!items.is_empty());
        assert!(items.iter().any(|i| i.label == "count"));
        assert!(items.iter().any(|i| i.label == "jsonb_build_object"));
    }

    #[test]
    fn test_complete_functions_with_prefix() {
        let items = complete_functions(None, Some("json"));
        assert!(!items.is_empty());
        assert!(items.iter().all(|i| i.label.starts_with("json")));
    }

    #[test]
    fn test_complete_functions_with_provider() {
        let provider = MemoryFunctionProvider::new()
            .add_function(FunctionInfo::new("my_custom_func", "integer"));

        let items = complete_functions(Some(&provider), None);
        assert!(items.iter().any(|i| i.label == "my_custom_func"));
        // Should also include built-ins
        assert!(items.iter().any(|i| i.label == "count"));
    }

    #[test]
    fn test_get_function_signature() {
        let info = get_function_signature(None, "count");
        assert!(info.is_some());
        let func = info.unwrap();
        assert_eq!(func.name, "count");
        assert!(!func.args.is_empty());
    }
}
