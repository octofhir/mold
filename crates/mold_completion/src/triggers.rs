//! JSONB operator trigger detection for auto-completion.
//!
//! This module detects when the cursor is positioned after a JSONB operator
//! and should trigger field/path completions.

/// JSONB operator sequences that trigger completions.
/// Sorted by length descending to match longer operators first (e.g., `->>` before `->`).
pub const JSONB_TRIGGER_SEQUENCES: &[&str] = &[
    // 3-character operators
    "#>>",
    // 2-character operators
    "->>",
    "#>",
    "@>",
    "<@",
    "?|",
    "?&",
    "@?",
    "@@",
    "->",
    // 1-character operators
    "?",
];

/// Information about a detected JSONB trigger.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonbTrigger {
    /// The operator that triggered completion.
    pub operator: String,
    /// The byte position where the operator starts.
    pub position: usize,
}

impl JsonbTrigger {
    /// Create a new JSONB trigger.
    pub fn new(operator: impl Into<String>, position: usize) -> Self {
        Self {
            operator: operator.into(),
            position,
        }
    }

    /// Returns the kind of completion to suggest based on the operator.
    pub fn completion_kind(&self) -> JsonbCompletionKind {
        match self.operator.as_str() {
            "->" | "->>" => JsonbCompletionKind::FieldName,
            "#>" | "#>>" => JsonbCompletionKind::PathArray,
            "@>" | "<@" => JsonbCompletionKind::JsonbPattern,
            "?" => JsonbCompletionKind::KeyName,
            "?|" | "?&" => JsonbCompletionKind::KeyArray,
            "@?" | "@@" => JsonbCompletionKind::JsonPath,
            _ => JsonbCompletionKind::FieldName,
        }
    }
}

/// The kind of completion to suggest after a JSONB operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsonbCompletionKind {
    /// Suggest field names (for `->` and `->>`)
    FieldName,
    /// Suggest path arrays (for `#>` and `#>>`)
    PathArray,
    /// Suggest JSONB patterns (for `@>` and `<@`)
    JsonbPattern,
    /// Suggest key names (for `?`)
    KeyName,
    /// Suggest key arrays (for `?|` and `?&`)
    KeyArray,
    /// Suggest JSONPath expressions (for `@?` and `@@`)
    JsonPath,
}

/// Check if the text before an offset ends with a JSONB operator.
///
/// Returns `Some(JsonbTrigger)` if a trigger sequence is found, `None` otherwise.
///
/// # Example
///
/// ```
/// use mold_completion::triggers::detect_jsonb_trigger;
///
/// let text = "SELECT resource->";
/// let trigger = detect_jsonb_trigger(text, text.len());
/// assert!(trigger.is_some());
/// assert_eq!(trigger.unwrap().operator, "->");
/// ```
pub fn detect_jsonb_trigger(text: &str, offset: usize) -> Option<JsonbTrigger> {
    if offset > text.len() {
        return None;
    }

    let before = &text[..offset];

    // Check each trigger sequence (already sorted by length descending)
    for seq in JSONB_TRIGGER_SEQUENCES {
        if before.ends_with(seq) {
            return Some(JsonbTrigger::new(*seq, offset - seq.len()));
        }
    }

    None
}

/// Check if a single character is the start of a potential JSONB trigger.
///
/// This is useful for LSP trigger character configuration.
pub fn is_jsonb_trigger_char(c: char) -> bool {
    matches!(c, '-' | '#' | '@' | '<' | '?' | '>')
}

/// Get the list of characters that can start a JSONB operator.
///
/// These are useful for configuring LSP trigger characters.
pub fn jsonb_trigger_chars() -> Vec<char> {
    vec!['-', '#', '@', '<', '?', '>']
}

/// Information about a detected JSONPath trigger inside a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonPathTrigger {
    /// The full JSONPath expression from the start of the string.
    pub path: String,
    /// The position of the string start (opening quote) in the source.
    pub string_start: usize,
    /// The cursor offset within the JSONPath (relative to `$`).
    pub cursor_in_path: usize,
}

impl JsonPathTrigger {
    /// Create a new JSONPath trigger.
    pub fn new(path: String, string_start: usize, cursor_in_path: usize) -> Self {
        Self {
            path,
            string_start,
            cursor_in_path,
        }
    }
}

/// Detects if we're inside a JSONPath string literal.
///
/// Returns `Some(JsonPathTrigger)` if:
/// 1. The cursor is inside an unclosed string literal
/// 2. The string content starts with `$` (JSONPath root)
///
/// # Example
///
/// ```
/// use mold_completion::triggers::detect_jsonpath_in_string;
///
/// let text = "resource @? '$.name";
/// let trigger = detect_jsonpath_in_string(text, text.len());
/// assert!(trigger.is_some());
/// let trigger = trigger.unwrap();
/// assert_eq!(trigger.path, "$.name");
/// ```
pub fn detect_jsonpath_in_string(text: &str, offset: usize) -> Option<JsonPathTrigger> {
    if offset > text.len() {
        return None;
    }

    let before = &text[..offset];

    // Find if we're inside an unclosed string
    let (in_string, string_start) = find_string_context(before);
    if !in_string {
        return None;
    }

    // Extract string content from start to cursor
    let string_content = &before[string_start + 1..]; // Skip the opening quote

    // Check if it's a JSONPath (starts with $)
    if !string_content.starts_with('$') {
        return None;
    }

    Some(JsonPathTrigger::new(
        string_content.to_string(),
        string_start,
        string_content.len(),
    ))
}

/// Finds if the cursor is inside an unclosed string literal.
/// Returns (is_in_string, string_start_position).
fn find_string_context(text: &str) -> (bool, usize) {
    let mut in_string = false;
    let mut string_start = 0;
    let mut chars = text.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        match c {
            '\'' => {
                if in_string {
                    // Check for escaped quote ('')
                    if chars.peek().map(|(_, nc)| *nc) == Some('\'') {
                        chars.next(); // Skip the escaped quote
                        continue;
                    }
                    in_string = false;
                } else {
                    in_string = true;
                    string_start = i;
                }
            }
            _ => {}
        }
    }

    (in_string, string_start)
}

/// Checks if the character should trigger JSONPath completion inside a string.
///
/// Inside a JSONPath string, these characters should trigger completions:
/// - `$` - Start of JSONPath
/// - `.` - Member access
/// - `[` - Array access
/// - `@` - Current item in filter
pub fn is_jsonpath_trigger_char(c: char) -> bool {
    matches!(c, '$' | '.' | '[' | '@')
}

/// Get all trigger characters for JSONPath inside strings.
pub fn jsonpath_trigger_chars() -> Vec<char> {
    vec!['$', '.', '[', '@']
}

// ============================================================================
// Keyword Triggers
// ============================================================================

/// Keywords that trigger table completions.
/// Sorted by length descending for proper matching of multi-word keywords.
pub const TABLE_KEYWORDS: &[&str] = &[
    // Multi-word keywords first (longer matches)
    "DELETE FROM",
    "INSERT INTO",
    "NATURAL JOIN",
    "CROSS JOIN",
    "RIGHT OUTER JOIN",
    "LEFT OUTER JOIN",
    "FULL OUTER JOIN",
    "RIGHT JOIN",
    "LEFT JOIN",
    "FULL JOIN",
    "INNER JOIN",
    // Single-word keywords
    "UPDATE",
    "INTO",
    "FROM",
    "JOIN",
];

/// Keywords that trigger column completions.
/// Sorted by length descending for proper matching of multi-word keywords.
pub const COLUMN_KEYWORDS: &[&str] = &[
    // Multi-word keywords first
    "PARTITION BY",
    "ORDER BY",
    "GROUP BY",
    "HAVING",
    // Single-word keywords
    "SELECT",
    "WHERE",
    "SET",
    "AND",
    "OR",
    "ON",
];

/// The kind of completion to suggest after a keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeywordTriggerKind {
    /// Suggest table names
    Table,
    /// Suggest column names/expressions
    Column,
    /// Suggest columns from both tables in a join
    JoinCondition,
    /// Suggest columns for SET clause (specific table)
    SetColumn,
}

/// Information about a detected keyword trigger.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeywordTrigger {
    /// The keyword that triggered completion.
    pub keyword: String,
    /// The kind of completion to suggest.
    pub kind: KeywordTriggerKind,
    /// The byte position where the keyword ends.
    pub position: usize,
}

impl KeywordTrigger {
    /// Create a new keyword trigger.
    pub fn new(keyword: impl Into<String>, kind: KeywordTriggerKind, position: usize) -> Self {
        Self {
            keyword: keyword.into(),
            kind,
            position,
        }
    }
}

/// Detects if the cursor is positioned after a SQL keyword that should trigger completions.
///
/// Returns `Some(KeywordTrigger)` if a trigger keyword is found, `None` otherwise.
///
/// # Example
///
/// ```ignore
/// use mold_completion::triggers::detect_keyword_trigger;
///
/// let text = "SELECT * FROM ";
/// let trigger = detect_keyword_trigger(text, text.len());
/// assert!(trigger.is_some());
/// assert_eq!(trigger.unwrap().keyword, "FROM");
/// ```
pub fn detect_keyword_trigger(text: &str, offset: usize) -> Option<KeywordTrigger> {
    if offset > text.len() {
        return None;
    }

    let before = &text[..offset];
    let trimmed = before.trim_end();

    // Need at least some content after the keyword (space/newline was typed)
    // and text shouldn't end with the keyword (still typing it)
    if before.len() == trimmed.len() {
        return None;
    }

    let upper = trimmed.to_uppercase();

    // Check table keywords first (longer multi-word patterns)
    for kw in TABLE_KEYWORDS {
        if upper.ends_with(kw) {
            // Make sure it's a word boundary (not part of another word)
            let prefix_len = upper.len() - kw.len();
            if prefix_len == 0 || !upper.chars().nth(prefix_len - 1).is_some_and(|c| c.is_alphanumeric()) {
                return Some(KeywordTrigger::new(*kw, KeywordTriggerKind::Table, trimmed.len()));
            }
        }
    }

    // Check column keywords
    for kw in COLUMN_KEYWORDS {
        if upper.ends_with(kw) {
            let prefix_len = upper.len() - kw.len();
            if prefix_len == 0 || !upper.chars().nth(prefix_len - 1).is_some_and(|c| c.is_alphanumeric()) {
                let kind = match *kw {
                    "ON" => KeywordTriggerKind::JoinCondition,
                    "SET" => KeywordTriggerKind::SetColumn,
                    _ => KeywordTriggerKind::Column,
                };
                return Some(KeywordTrigger::new(*kw, kind, trimmed.len()));
            }
        }
    }

    None
}

/// Returns true if DELETE alone was typed (without FROM).
/// DELETE alone should not trigger table completions.
pub fn is_incomplete_delete(text: &str) -> bool {
    let upper = text.to_uppercase();
    let trimmed = upper.trim_end();
    trimmed.ends_with("DELETE") && !trimmed.ends_with("DELETE FROM")
}

/// Returns true if INSERT alone was typed (without INTO).
/// INSERT alone should not trigger table completions.
pub fn is_incomplete_insert(text: &str) -> bool {
    let upper = text.to_uppercase();
    let trimmed = upper.trim_end();
    trimmed.ends_with("INSERT") && !trimmed.ends_with("INSERT INTO")
}

/// Get the list of keywords that trigger completions.
pub fn keyword_trigger_chars() -> Vec<char> {
    // Keywords trigger on space/newline after the keyword
    vec![' ', '\n', '\t']
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_arrow() {
        let text = "SELECT data->";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "->");
        // "SELECT data" is 11 chars, "->" starts at index 11
        assert_eq!(trigger.position, 11);
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::FieldName);
    }

    #[test]
    fn test_detect_double_arrow() {
        let text = "SELECT data->>";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "->>");
        // "SELECT data" is 11 chars, "->>" starts at index 11
        assert_eq!(trigger.position, 11);
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::FieldName);
    }

    #[test]
    fn test_detect_hash_arrow() {
        let text = "SELECT data#>";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "#>");
        assert_eq!(trigger.position, 11);
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::PathArray);
    }

    #[test]
    fn test_detect_hash_double_arrow() {
        let text = "SELECT data#>>";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "#>>");
        assert_eq!(trigger.position, 11);
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::PathArray);
    }

    #[test]
    fn test_detect_contains() {
        let text = "SELECT * WHERE data@>";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "@>");
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::JsonbPattern);
    }

    #[test]
    fn test_detect_contained_by() {
        let text = "SELECT * WHERE data<@";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "<@");
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::JsonbPattern);
    }

    #[test]
    fn test_detect_key_exists() {
        let text = "SELECT * WHERE data?";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "?");
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::KeyName);
    }

    #[test]
    fn test_detect_any_key_exists() {
        let text = "SELECT * WHERE data?|";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "?|");
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::KeyArray);
    }

    #[test]
    fn test_detect_all_keys_exist() {
        let text = "SELECT * WHERE data?&";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "?&");
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::KeyArray);
    }

    #[test]
    fn test_detect_jsonpath_exists() {
        let text = "SELECT * WHERE data@?";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "@?");
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::JsonPath);
    }

    #[test]
    fn test_detect_jsonpath_match() {
        let text = "SELECT * WHERE data@@";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.operator, "@@");
        assert_eq!(trigger.completion_kind(), JsonbCompletionKind::JsonPath);
    }

    #[test]
    fn test_no_trigger() {
        let text = "SELECT data";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_none());
    }

    #[test]
    fn test_trigger_mid_text() {
        let text = "SELECT data->'name' FROM table";
        // Cursor right after -> (index 13, since "SELECT data->" is 13 chars)
        let trigger = detect_jsonb_trigger(text, 13);
        assert!(trigger.is_some());
        assert_eq!(trigger.unwrap().operator, "->");
    }

    #[test]
    fn test_chained_operators() {
        let text = "SELECT data->'a'->>";
        let trigger = detect_jsonb_trigger(text, text.len());
        assert!(trigger.is_some());
        assert_eq!(trigger.unwrap().operator, "->>");
    }

    #[test]
    fn test_trigger_chars() {
        assert!(is_jsonb_trigger_char('-'));
        assert!(is_jsonb_trigger_char('>'));
        assert!(is_jsonb_trigger_char('#'));
        assert!(is_jsonb_trigger_char('@'));
        assert!(is_jsonb_trigger_char('<'));
        assert!(is_jsonb_trigger_char('?'));
        assert!(!is_jsonb_trigger_char('a'));
        assert!(!is_jsonb_trigger_char(' '));
    }

    #[test]
    fn test_offset_beyond_text() {
        let text = "SELECT data";
        let trigger = detect_jsonb_trigger(text, text.len() + 10);
        assert!(trigger.is_none());
    }

    // JSONPath string trigger tests

    #[test]
    fn test_jsonpath_in_string_basic() {
        let text = "resource @? '$.name";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.path, "$.name");
    }

    #[test]
    fn test_jsonpath_in_string_at_root() {
        let text = "resource @? '$";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.path, "$");
    }

    #[test]
    fn test_jsonpath_in_string_after_dot() {
        let text = "resource @? '$.";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.path, "$.");
    }

    #[test]
    fn test_jsonpath_in_string_array() {
        let text = "resource @? '$.items[";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.path, "$.items[");
    }

    #[test]
    fn test_jsonpath_in_string_filter() {
        let text = "resource @? '$.items[?(@.";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.path, "$.items[?(@.");
    }

    #[test]
    fn test_jsonpath_in_function() {
        let text = "jsonb_path_query(resource, '$.name";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.path, "$.name");
    }

    #[test]
    fn test_jsonpath_not_in_string() {
        let text = "resource @? $.name";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_none());
    }

    #[test]
    fn test_jsonpath_in_closed_string() {
        let text = "resource @? '$.name'";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_none()); // String is closed
    }

    #[test]
    fn test_jsonpath_non_jsonpath_string() {
        let text = "SELECT 'hello";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_none()); // Not a JSONPath (doesn't start with $)
    }

    #[test]
    fn test_jsonpath_with_escaped_quotes() {
        let text = "resource @? '$.name''s";
        let trigger = detect_jsonpath_in_string(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        // The escaped quote should be handled correctly
        assert_eq!(trigger.path, "$.name''s");
    }

    #[test]
    fn test_jsonpath_trigger_chars() {
        assert!(is_jsonpath_trigger_char('$'));
        assert!(is_jsonpath_trigger_char('.'));
        assert!(is_jsonpath_trigger_char('['));
        assert!(is_jsonpath_trigger_char('@'));
        assert!(!is_jsonpath_trigger_char('a'));
        assert!(!is_jsonpath_trigger_char(' '));
    }

    #[test]
    fn test_find_string_context() {
        // Not in string
        let (in_str, _) = find_string_context("SELECT data");
        assert!(!in_str);

        // In string
        let (in_str, pos) = find_string_context("SELECT '$.name");
        assert!(in_str);
        assert_eq!(pos, 7); // Position of opening quote

        // Closed string
        let (in_str, _) = find_string_context("SELECT '$.name'");
        assert!(!in_str);

        // Multiple strings, cursor in second
        let (in_str, pos) = find_string_context("'first' AND '$.second");
        assert!(in_str);
        assert_eq!(pos, 12); // Position of second opening quote
    }

    // Keyword trigger tests

    #[test]
    fn test_keyword_trigger_from() {
        let text = "SELECT * FROM ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "FROM");
        assert_eq!(trigger.kind, KeywordTriggerKind::Table);
    }

    #[test]
    fn test_keyword_trigger_join() {
        let text = "SELECT * FROM users JOIN ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "JOIN");
        assert_eq!(trigger.kind, KeywordTriggerKind::Table);
    }

    #[test]
    fn test_keyword_trigger_left_join() {
        let text = "SELECT * FROM users LEFT JOIN ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "LEFT JOIN");
        assert_eq!(trigger.kind, KeywordTriggerKind::Table);
    }

    #[test]
    fn test_keyword_trigger_inner_join() {
        let text = "SELECT * FROM users INNER JOIN ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "INNER JOIN");
        assert_eq!(trigger.kind, KeywordTriggerKind::Table);
    }

    #[test]
    fn test_keyword_trigger_update() {
        let text = "UPDATE ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "UPDATE");
        assert_eq!(trigger.kind, KeywordTriggerKind::Table);
    }

    #[test]
    fn test_keyword_trigger_into() {
        let text = "INSERT INTO ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "INSERT INTO");
        assert_eq!(trigger.kind, KeywordTriggerKind::Table);
    }

    #[test]
    fn test_keyword_trigger_delete_from() {
        let text = "DELETE FROM ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "DELETE FROM");
        assert_eq!(trigger.kind, KeywordTriggerKind::Table);
    }

    #[test]
    fn test_keyword_trigger_select() {
        let text = "SELECT ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "SELECT");
        assert_eq!(trigger.kind, KeywordTriggerKind::Column);
    }

    #[test]
    fn test_keyword_trigger_where() {
        let text = "SELECT * FROM users WHERE ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "WHERE");
        assert_eq!(trigger.kind, KeywordTriggerKind::Column);
    }

    #[test]
    fn test_keyword_trigger_and() {
        let text = "SELECT * FROM users WHERE id = 1 AND ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "AND");
        assert_eq!(trigger.kind, KeywordTriggerKind::Column);
    }

    #[test]
    fn test_keyword_trigger_or() {
        let text = "SELECT * FROM users WHERE id = 1 OR ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "OR");
        assert_eq!(trigger.kind, KeywordTriggerKind::Column);
    }

    #[test]
    fn test_keyword_trigger_on() {
        let text = "SELECT * FROM users JOIN orders ON ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "ON");
        assert_eq!(trigger.kind, KeywordTriggerKind::JoinCondition);
    }

    #[test]
    fn test_keyword_trigger_set() {
        let text = "UPDATE users SET ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "SET");
        assert_eq!(trigger.kind, KeywordTriggerKind::SetColumn);
    }

    #[test]
    fn test_keyword_trigger_order_by() {
        let text = "SELECT * FROM users ORDER BY ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "ORDER BY");
        assert_eq!(trigger.kind, KeywordTriggerKind::Column);
    }

    #[test]
    fn test_keyword_trigger_group_by() {
        let text = "SELECT * FROM users GROUP BY ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        let trigger = trigger.unwrap();
        assert_eq!(trigger.keyword, "GROUP BY");
        assert_eq!(trigger.kind, KeywordTriggerKind::Column);
    }

    #[test]
    fn test_keyword_trigger_no_space() {
        // No space after keyword - still typing
        let text = "SELECT * FROM";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_none());
    }

    #[test]
    fn test_keyword_trigger_delete_alone() {
        // DELETE alone should not trigger table completion
        let text = "DELETE ";
        let trigger = detect_keyword_trigger(text, text.len());
        // DELETE alone is not in TABLE_KEYWORDS
        assert!(trigger.is_none());
    }

    #[test]
    fn test_keyword_trigger_insert_alone() {
        // INSERT alone should not trigger table completion
        let text = "INSERT ";
        let trigger = detect_keyword_trigger(text, text.len());
        // INSERT alone is not in TABLE_KEYWORDS
        assert!(trigger.is_none());
    }

    #[test]
    fn test_keyword_trigger_case_insensitive() {
        let text = "select * from ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        assert_eq!(trigger.unwrap().keyword, "FROM");
    }

    #[test]
    fn test_keyword_trigger_newline() {
        let text = "SELECT * FROM\n";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_some());
        assert_eq!(trigger.unwrap().keyword, "FROM");
    }

    #[test]
    fn test_keyword_trigger_not_part_of_word() {
        // AFROM should not trigger (not a word boundary)
        let text = "SELECTAFROM ";
        let trigger = detect_keyword_trigger(text, text.len());
        assert!(trigger.is_none());
    }

    #[test]
    fn test_incomplete_delete() {
        assert!(is_incomplete_delete("DELETE "));
        assert!(!is_incomplete_delete("DELETE FROM "));
        assert!(!is_incomplete_delete("SELECT "));
    }

    #[test]
    fn test_incomplete_insert() {
        assert!(is_incomplete_insert("INSERT "));
        assert!(!is_incomplete_insert("INSERT INTO "));
        assert!(!is_incomplete_insert("SELECT "));
    }
}
