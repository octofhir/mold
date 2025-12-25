//! Function completion generator.

use crate::providers::FunctionProvider;
use crate::types::{CompletionData, CompletionItem, CompletionItemKind, FunctionInfo};

/// Default search path for schema-qualified display decisions.
const DEFAULT_SEARCH_PATH: &[&str] = &["pg_catalog", "public"];

/// Generates function completion items.
pub fn complete_functions(
    provider: Option<&dyn FunctionProvider>,
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    complete_functions_with_options(provider, prefix, DEFAULT_SEARCH_PATH)
}

/// Generates function completion items with custom search path.
pub fn complete_functions_with_options(
    provider: Option<&dyn FunctionProvider>,
    prefix: Option<&str>,
    search_path: &[&str],
) -> Vec<CompletionItem> {
    let Some(provider) = provider else {
        return builtin_functions(prefix);
    };

    let functions = match prefix {
        Some(p) if !p.is_empty() => {
            // Try fuzzy matching
            let all_functions = provider.functions();
            filter_functions_fuzzy(&all_functions, p)
        }
        Some(p) => provider.functions_by_prefix(p),
        None => provider.functions(),
    };

    let mut items: Vec<_> = functions
        .into_iter()
        .map(|f| create_function_item_with_options(f, search_path))
        .collect();

    // Also include built-in functions
    items.extend(builtin_functions(prefix));

    // Sort by relevance (prefix matches first, then fuzzy matches)
    let prefix_lower = prefix.map(|p| p.to_lowercase());
    items.sort_by(|a, b| {
        let a_relevance = compute_relevance(&a.label, prefix_lower.as_deref());
        let b_relevance = compute_relevance(&b.label, prefix_lower.as_deref());
        a_relevance.cmp(&b_relevance).then(a.label.cmp(&b.label))
    });

    // Deduplicate by label
    items.dedup_by(|a, b| a.label.to_lowercase() == b.label.to_lowercase());

    items
}

/// Filters functions using fuzzy matching.
fn filter_functions_fuzzy(functions: &[FunctionInfo], prefix: &str) -> Vec<FunctionInfo> {
    let prefix_lower = prefix.to_lowercase();

    functions
        .iter()
        .filter(|f| {
            let name_lower = f.name.to_lowercase();
            // Prefix match
            name_lower.starts_with(&prefix_lower)
                // Contains match
                || name_lower.contains(&prefix_lower)
                // Abbreviation match (e.g., "jep" matches "jsonb_extract_path")
                || matches_abbreviation(&name_lower, &prefix_lower)
        })
        .cloned()
        .collect()
}

/// Checks if the pattern matches the name as an abbreviation.
///
/// Examples:
/// - `jep` matches `jsonb_extract_path`
/// - `jba` matches `jsonb_build_array`
/// - `cb` matches `concat_ws`
pub fn matches_abbreviation(name: &str, pattern: &str) -> bool {
    if pattern.is_empty() || name.is_empty() {
        return false;
    }

    let mut pattern_chars = pattern.chars().peekable();
    let mut current_pattern = pattern_chars.next();
    let mut at_word_start = true;

    for c in name.chars() {
        let Some(pattern_char) = current_pattern else {
            // Pattern fully matched
            return true;
        };

        // Match at word boundaries (start of name, after underscore)
        if at_word_start && c.eq_ignore_ascii_case(&pattern_char) {
            current_pattern = pattern_chars.next();
        }

        at_word_start = c == '_';
    }

    // Pattern is fully matched if we consumed all pattern chars
    current_pattern.is_none()
}

/// Computes relevance score for sorting (lower is better).
fn compute_relevance(name: &str, prefix: Option<&str>) -> u32 {
    let Some(prefix) = prefix else {
        return if is_common_function(name) { 0 } else { 1 };
    };

    let name_lower = name.to_lowercase();
    let prefix_lower = prefix.to_lowercase();

    // Common function bonus
    let base = if is_common_function(name) { 0 } else { 100 };

    if name_lower.starts_with(&prefix_lower) {
        // Exact prefix match - best
        base
    } else if name_lower.contains(&prefix_lower) {
        // Contains match - good
        base + 10
    } else if matches_abbreviation(&name_lower, &prefix_lower) {
        // Abbreviation match - acceptable
        base + 20
    } else {
        // No match
        base + 50
    }
}

/// Creates a completion item for a function with custom search path.
///
/// If the function's schema is not in the search path, the label will be schema-qualified.
fn create_function_item_with_options(func: FunctionInfo, search_path: &[&str]) -> CompletionItem {
    let signature = func.signature();

    // Determine if we need to qualify with schema
    let needs_qualification = func
        .schema
        .as_ref()
        .is_some_and(|s| !search_path.iter().any(|sp| sp.eq_ignore_ascii_case(s)));

    let label = if needs_qualification {
        format!(
            "{}.{}",
            func.schema.as_deref().unwrap_or("public"),
            func.name
        )
    } else {
        func.name.clone()
    };

    let insert_text = format!("{}(", label);

    // Sort key: frequently used functions first
    let sort_key = if is_common_function(&func.name) {
        format!("0_{}", func.name.to_lowercase())
    } else {
        format!("1_{}", func.name.to_lowercase())
    };

    CompletionItem::new(CompletionItemKind::Function, &label)
        .with_detail(signature.clone())
        .with_insert_text(insert_text)
        .with_sort_key(sort_key)
        .with_filter_text(func.name.clone()) // Always filter on just the function name
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
        // PostgreSQL 12+ JSONPath functions
        (
            "jsonb_path_exists",
            "jsonb_path_exists(target jsonb, path jsonpath) -> boolean",
            "Check if JSONPath returns any items",
        ),
        (
            "jsonb_path_match",
            "jsonb_path_match(target jsonb, path jsonpath) -> boolean",
            "Check JSONPath predicate result",
        ),
        (
            "jsonb_path_query",
            "jsonb_path_query(target jsonb, path jsonpath) -> setof jsonb",
            "Extract values matching JSONPath",
        ),
        (
            "jsonb_path_query_array",
            "jsonb_path_query_array(target jsonb, path jsonpath) -> jsonb",
            "Extract values as JSONB array",
        ),
        (
            "jsonb_path_query_first",
            "jsonb_path_query_first(target jsonb, path jsonpath) -> jsonb",
            "Extract first value matching JSONPath",
        ),
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
    if let Some(provider) = provider
        && let Some(func) = provider.function(None, name)
    {
        return Some(func);
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
        "jsonb_extract_path_text" => Some(
            FunctionInfo::new("jsonb_extract_path_text", "text")
                .with_arg(FunctionArg::new("jsonb").with_name("from_json"))
                .with_arg(
                    FunctionArg::new("text")
                        .with_name("path_elems")
                        .with_mode(crate::types::ArgMode::Variadic),
                ),
        ),
        "jsonb_path_exists" => Some(
            FunctionInfo::new("jsonb_path_exists", "boolean")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("jsonpath").with_name("path")),
        ),
        "jsonb_path_match" => Some(
            FunctionInfo::new("jsonb_path_match", "boolean")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("jsonpath").with_name("path")),
        ),
        "jsonb_path_query" => Some(
            FunctionInfo::new("jsonb_path_query", "setof jsonb")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("jsonpath").with_name("path")),
        ),
        "jsonb_path_query_array" => Some(
            FunctionInfo::new("jsonb_path_query_array", "jsonb")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("jsonpath").with_name("path")),
        ),
        "jsonb_path_query_first" => Some(
            FunctionInfo::new("jsonb_path_query_first", "jsonb")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("jsonpath").with_name("path")),
        ),
        "jsonb_set" => Some(
            FunctionInfo::new("jsonb_set", "jsonb")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("text[]").with_name("path"))
                .with_arg(FunctionArg::new("jsonb").with_name("new_value")),
        ),
        "jsonb_insert" => Some(
            FunctionInfo::new("jsonb_insert", "jsonb")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("text[]").with_name("path"))
                .with_arg(FunctionArg::new("jsonb").with_name("new_value")),
        ),
        "jsonb_delete_path" => Some(
            FunctionInfo::new("jsonb_delete_path", "jsonb")
                .with_arg(FunctionArg::new("jsonb").with_name("target"))
                .with_arg(FunctionArg::new("text[]").with_name("path")),
        ),
        _ => None,
    }
}

/// Describes what kind of completion to provide for a JSONB function argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsonbArgCompletion {
    /// Suggest JSONB columns from tables in scope
    JsonbColumn,
    /// Suggest field names as path keys (for jsonb_extract_path, etc.)
    PathKeys,
    /// Trigger JSONPath completion (for jsonb_path_* functions)
    JsonPath,
    /// No special completion (e.g., value argument in jsonb_set)
    None,
}

/// Returns the type of completion to provide for a JSONB function argument.
///
/// Returns `None` if the function is not a JSONB function.
pub fn get_jsonb_arg_completion(function: &str, arg_index: usize) -> Option<JsonbArgCompletion> {
    let func_lower = function.to_lowercase();

    match func_lower.as_str() {
        // Functions with VARIADIC path keys
        "jsonb_extract_path" | "jsonb_extract_path_text" => match arg_index {
            0 => Some(JsonbArgCompletion::JsonbColumn),
            _ => Some(JsonbArgCompletion::PathKeys),
        },
        // Functions with JSONPath argument
        "jsonb_path_exists" | "jsonb_path_match" | "jsonb_path_query"
        | "jsonb_path_query_array" | "jsonb_path_query_first" => match arg_index {
            0 => Some(JsonbArgCompletion::JsonbColumn),
            1 => Some(JsonbArgCompletion::JsonPath),
            _ => Some(JsonbArgCompletion::None),
        },
        // Functions with text[] path argument
        "jsonb_set" | "jsonb_insert" => match arg_index {
            0 => Some(JsonbArgCompletion::JsonbColumn),
            1 => Some(JsonbArgCompletion::PathKeys),
            _ => Some(JsonbArgCompletion::None),
        },
        "jsonb_delete_path" => match arg_index {
            0 => Some(JsonbArgCompletion::JsonbColumn),
            1 => Some(JsonbArgCompletion::PathKeys),
            _ => Some(JsonbArgCompletion::None),
        },
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

    #[test]
    fn test_matches_abbreviation() {
        // Positive cases
        assert!(matches_abbreviation("jsonb_extract_path", "jep"));
        assert!(matches_abbreviation("jsonb_build_array", "jba"));
        assert!(matches_abbreviation("jsonb_build_object", "jbo"));
        assert!(matches_abbreviation("array_agg", "aa"));
        assert!(matches_abbreviation("string_agg", "sa"));
        assert!(matches_abbreviation("row_number", "rn"));
        assert!(matches_abbreviation("dense_rank", "dr"));

        // Case insensitive
        assert!(matches_abbreviation("jsonb_extract_path", "JEP"));
        assert!(matches_abbreviation("JSONB_EXTRACT_PATH", "jep"));

        // Negative cases
        assert!(!matches_abbreviation("count", "xyz"));
        assert!(!matches_abbreviation("jsonb_extract_path", "jxp"));
        assert!(!matches_abbreviation("", "a"));
        assert!(!matches_abbreviation("count", ""));
    }

    #[test]
    fn test_fuzzy_matching() {
        let functions = vec![
            FunctionInfo::new("jsonb_extract_path", "jsonb"),
            FunctionInfo::new("jsonb_build_array", "jsonb"),
            FunctionInfo::new("jsonb_build_object", "jsonb"),
            FunctionInfo::new("count", "bigint"),
        ];

        // Prefix match
        let results = filter_functions_fuzzy(&functions, "jsonb");
        assert_eq!(results.len(), 3);

        // Contains match
        let results = filter_functions_fuzzy(&functions, "build");
        assert_eq!(results.len(), 2);

        // Abbreviation match
        let results = filter_functions_fuzzy(&functions, "jep");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "jsonb_extract_path");

        // No match
        let results = filter_functions_fuzzy(&functions, "xyz");
        assert!(results.is_empty());
    }

    #[test]
    fn test_schema_qualified_display() {
        // Function in search path - should not be qualified
        let func_in_path = FunctionInfo::new("my_func", "integer").with_schema("public");
        let item = create_function_item_with_options(func_in_path, &["public", "pg_catalog"]);
        assert_eq!(item.label, "my_func");

        // Function not in search path - should be qualified
        let func_not_in_path = FunctionInfo::new("special_func", "text").with_schema("my_schema");
        let item = create_function_item_with_options(func_not_in_path, &["public", "pg_catalog"]);
        assert_eq!(item.label, "my_schema.special_func");

        // Filter text should still be just the function name
        assert_eq!(item.filter_text.as_deref(), Some("special_func"));
    }

    #[test]
    fn test_compute_relevance() {
        // Common function prefix match - best
        assert_eq!(compute_relevance("count", Some("cou")), 0);

        // Common function contains match
        assert_eq!(compute_relevance("count", Some("oun")), 10);

        // jsonb_build_object is in common list, so base is 0
        assert_eq!(compute_relevance("jsonb_build_object", Some("jsonb")), 0);

        // Non-common function abbreviation match
        // jsonb_extract_path_text is not in common list, so base is 100
        assert_eq!(compute_relevance("my_custom_func", Some("mcf")), 120);
    }

    // JSONB function argument completion tests

    #[test]
    fn test_jsonb_arg_completion_extract_path() {
        // First arg: JSONB column
        let result = get_jsonb_arg_completion("jsonb_extract_path", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        // Second arg onwards: path keys (variadic)
        let result = get_jsonb_arg_completion("jsonb_extract_path", 1);
        assert_eq!(result, Some(JsonbArgCompletion::PathKeys));

        let result = get_jsonb_arg_completion("jsonb_extract_path", 2);
        assert_eq!(result, Some(JsonbArgCompletion::PathKeys));

        let result = get_jsonb_arg_completion("jsonb_extract_path", 5);
        assert_eq!(result, Some(JsonbArgCompletion::PathKeys));
    }

    #[test]
    fn test_jsonb_arg_completion_extract_path_text() {
        let result = get_jsonb_arg_completion("jsonb_extract_path_text", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        let result = get_jsonb_arg_completion("jsonb_extract_path_text", 1);
        assert_eq!(result, Some(JsonbArgCompletion::PathKeys));
    }

    #[test]
    fn test_jsonb_arg_completion_path_query() {
        // First arg: JSONB column
        let result = get_jsonb_arg_completion("jsonb_path_query", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        // Second arg: JSONPath
        let result = get_jsonb_arg_completion("jsonb_path_query", 1);
        assert_eq!(result, Some(JsonbArgCompletion::JsonPath));

        // Third arg: no special completion
        let result = get_jsonb_arg_completion("jsonb_path_query", 2);
        assert_eq!(result, Some(JsonbArgCompletion::None));
    }

    #[test]
    fn test_jsonb_arg_completion_path_exists() {
        let result = get_jsonb_arg_completion("jsonb_path_exists", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        let result = get_jsonb_arg_completion("jsonb_path_exists", 1);
        assert_eq!(result, Some(JsonbArgCompletion::JsonPath));
    }

    #[test]
    fn test_jsonb_arg_completion_path_match() {
        let result = get_jsonb_arg_completion("jsonb_path_match", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        let result = get_jsonb_arg_completion("jsonb_path_match", 1);
        assert_eq!(result, Some(JsonbArgCompletion::JsonPath));
    }

    #[test]
    fn test_jsonb_arg_completion_set() {
        // First arg: JSONB column
        let result = get_jsonb_arg_completion("jsonb_set", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        // Second arg: path array
        let result = get_jsonb_arg_completion("jsonb_set", 1);
        assert_eq!(result, Some(JsonbArgCompletion::PathKeys));

        // Third arg: new value, no special completion
        let result = get_jsonb_arg_completion("jsonb_set", 2);
        assert_eq!(result, Some(JsonbArgCompletion::None));
    }

    #[test]
    fn test_jsonb_arg_completion_insert() {
        let result = get_jsonb_arg_completion("jsonb_insert", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        let result = get_jsonb_arg_completion("jsonb_insert", 1);
        assert_eq!(result, Some(JsonbArgCompletion::PathKeys));

        let result = get_jsonb_arg_completion("jsonb_insert", 2);
        assert_eq!(result, Some(JsonbArgCompletion::None));
    }

    #[test]
    fn test_jsonb_arg_completion_delete_path() {
        let result = get_jsonb_arg_completion("jsonb_delete_path", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        let result = get_jsonb_arg_completion("jsonb_delete_path", 1);
        assert_eq!(result, Some(JsonbArgCompletion::PathKeys));
    }

    #[test]
    fn test_jsonb_arg_completion_case_insensitive() {
        // Should work case-insensitively
        let result = get_jsonb_arg_completion("JSONB_EXTRACT_PATH", 0);
        assert_eq!(result, Some(JsonbArgCompletion::JsonbColumn));

        let result = get_jsonb_arg_completion("Jsonb_Path_Query", 1);
        assert_eq!(result, Some(JsonbArgCompletion::JsonPath));
    }

    #[test]
    fn test_jsonb_arg_completion_non_jsonb_function() {
        // Non-JSONB functions should return None
        let result = get_jsonb_arg_completion("count", 0);
        assert_eq!(result, None);

        let result = get_jsonb_arg_completion("sum", 0);
        assert_eq!(result, None);

        let result = get_jsonb_arg_completion("coalesce", 0);
        assert_eq!(result, None);
    }

    #[test]
    fn test_builtin_function_info_jsonb_path_query() {
        let info = get_function_signature(None, "jsonb_path_query");
        assert!(info.is_some());
        let func = info.unwrap();
        assert_eq!(func.name, "jsonb_path_query");
        assert_eq!(func.args.len(), 2);
        assert_eq!(func.args[0].data_type, "jsonb");
        assert_eq!(func.args[1].data_type, "jsonpath");
    }

    #[test]
    fn test_builtin_function_info_jsonb_set() {
        let info = get_function_signature(None, "jsonb_set");
        assert!(info.is_some());
        let func = info.unwrap();
        assert_eq!(func.name, "jsonb_set");
        assert_eq!(func.args.len(), 3);
        assert_eq!(func.args[0].data_type, "jsonb");
        assert_eq!(func.args[1].data_type, "text[]");
        assert_eq!(func.args[2].data_type, "jsonb");
    }
}
