//! Query analysis entry point.
//!
//! This module provides the main entry point for semantic analysis of SQL queries.

use std::sync::Arc;

use text_size::TextRange;

use crate::binding::{ColumnBinding, CteBinding, DataType, Resolution, TableBinding, TableSource};
use crate::scope::{Scope, ScopeBuilder};

/// The result of analyzing a query.
#[derive(Clone, Debug)]
pub struct Analysis {
    /// The root scope of the analyzed query.
    pub scope: Arc<Scope>,

    /// Resolved column references in the query.
    pub columns: Vec<ResolvedReference>,

    /// Resolved table references in the query.
    pub tables: Vec<ResolvedTableRef>,

    /// Diagnostics (errors, warnings) from analysis.
    pub diagnostics: Vec<Diagnostic>,

    /// The output columns of the query (for SELECT).
    pub output_columns: Vec<ColumnBinding>,
}

impl Analysis {
    /// Creates an empty analysis result.
    pub fn empty() -> Self {
        Self {
            scope: ScopeBuilder::new().build(),
            columns: Vec::new(),
            tables: Vec::new(),
            diagnostics: Vec::new(),
            output_columns: Vec::new(),
        }
    }

    /// Returns true if the analysis has errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    /// Returns all error diagnostics.
    pub fn errors(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
    }

    /// Returns all warning diagnostics.
    pub fn warnings(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
    }
}

/// A resolved column reference in the query.
#[derive(Clone, Debug)]
pub struct ResolvedReference {
    /// The range in the source where the reference appears.
    pub range: TextRange,

    /// The resolved table binding.
    pub table: Arc<TableBinding>,

    /// The resolved column binding.
    pub column: ColumnBinding,

    /// Whether this reference was ambiguous (resolved to first match).
    pub was_ambiguous: bool,
}

/// A resolved table reference in the query.
#[derive(Clone, Debug)]
pub struct ResolvedTableRef {
    /// The range in the source where the reference appears.
    pub range: TextRange,

    /// The resolved table binding.
    pub binding: Arc<TableBinding>,
}

/// A diagnostic message from analysis.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    /// The severity of the diagnostic.
    pub severity: Severity,

    /// The diagnostic message.
    pub message: String,

    /// The source range associated with this diagnostic.
    pub range: Option<TextRange>,

    /// Additional related information.
    pub related: Vec<RelatedInfo>,
}

impl Diagnostic {
    /// Creates an error diagnostic.
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            range: None,
            related: Vec::new(),
        }
    }

    /// Creates a warning diagnostic.
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            range: None,
            related: Vec::new(),
        }
    }

    /// Creates an info diagnostic.
    pub fn info(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Info,
            message: message.into(),
            range: None,
            related: Vec::new(),
        }
    }

    /// Sets the source range for this diagnostic.
    pub fn with_range(mut self, range: TextRange) -> Self {
        self.range = Some(range);
        self
    }

    /// Adds related information to this diagnostic.
    pub fn with_related(mut self, info: RelatedInfo) -> Self {
        self.related.push(info);
        self
    }
}

/// Severity level of a diagnostic.
#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// Related information for a diagnostic.
#[derive(Clone, Debug)]
pub struct RelatedInfo {
    /// The message.
    pub message: String,

    /// The source range.
    pub range: Option<TextRange>,
}

/// Schema provider trait for looking up table and column information.
///
/// Implement this trait to provide schema information to the analyzer.
pub trait SchemaProvider: Send + Sync {
    /// Looks up a table by name.
    fn lookup_table(&self, schema: Option<&str>, name: &str) -> Option<TableInfo>;

    /// Looks up columns for a table.
    fn lookup_columns(&self, schema: Option<&str>, table: &str) -> Vec<ColumnInfo>;

    /// Checks if a table exists in the schema.
    fn table_exists(&self, schema: Option<&str>, name: &str) -> bool {
        self.lookup_table(schema, name).is_some()
    }

    /// Checks if a schema exists.
    fn schema_exists(&self, _schema: &str) -> bool {
        true // Default: assume schema exists
    }

    /// Returns all available table names for suggestions.
    fn all_table_names(&self) -> Vec<String> {
        Vec::new()
    }

    /// Returns all available schema names for suggestions.
    fn all_schema_names(&self) -> Vec<String> {
        Vec::new()
    }

    /// Suggests similar table names (for "did you mean" messages).
    fn suggest_tables(&self, _schema: Option<&str>, name: &str) -> Vec<String> {
        suggest_similar(name, &self.all_table_names(), 3)
    }

    /// Suggests similar schema names (for "did you mean" messages).
    fn suggest_schemas(&self, name: &str) -> Vec<String> {
        suggest_similar(name, &self.all_schema_names(), 2)
    }

    /// Suggests similar column names for a table.
    fn suggest_columns(&self, schema: Option<&str>, table: &str, name: &str) -> Vec<String> {
        let columns = self.lookup_columns(schema, table);
        let col_names: Vec<String> = columns.into_iter().map(|c| c.name).collect();
        suggest_similar(name, &col_names, 3)
    }

    /// Returns valid JSONB field names for a given column at a specific path.
    ///
    /// This is used to validate JSONB path expressions like `resource->'name'->'given'`.
    /// The path is the list of keys accessed so far (e.g., `["name"]` when validating `given`).
    ///
    /// Returns `None` if the column has no JSONB schema, or `Some(fields)` with valid
    /// field names at that path.
    fn lookup_jsonb_fields(
        &self,
        _schema: Option<&str>,
        _table: &str,
        _column: &str,
        _path: &[&str],
    ) -> Option<Vec<String>> {
        None // Default: no JSONB schema information
    }

    /// Checks if a JSONB field exists at a given path.
    fn jsonb_field_exists(
        &self,
        schema: Option<&str>,
        table: &str,
        column: &str,
        path: &[&str],
        field: &str,
    ) -> Option<bool> {
        self.lookup_jsonb_fields(schema, table, column, path)
            .map(|fields| fields.iter().any(|f| f.eq_ignore_ascii_case(field)))
    }

    /// Checks if a JSONB field at a given path is an array type.
    ///
    /// Returns `Some(true)` if the field is an array, `Some(false)` if not,
    /// or `None` if the field type cannot be determined.
    fn jsonb_field_is_array(
        &self,
        _schema: Option<&str>,
        _table: &str,
        _column: &str,
        _path: &[&str],
    ) -> Option<bool> {
        None // Default: unknown field type
    }
}

/// Suggests similar strings using prefix matching and Levenshtein distance.
///
/// Priority order:
/// 1. Prefix matches (needle is a prefix of the candidate)
/// 2. Levenshtein distance within max_distance
///
/// This ensures that typing 'pa' suggests 'patient' before 'app'.
pub fn suggest_similar(needle: &str, haystack: &[String], max_distance: usize) -> Vec<String> {
    let needle_lower = needle.to_lowercase();

    // Collect matches with their scores
    // Score: (is_prefix_match, levenshtein_distance)
    // Lower is better for both components
    let mut matches: Vec<(String, bool, usize)> = haystack
        .iter()
        .filter_map(|s| {
            let s_lower = s.to_lowercase();
            let is_prefix = s_lower.starts_with(&needle_lower);
            let dist = levenshtein_distance(&needle_lower, &s_lower);

            // Include if it's a prefix match OR within levenshtein distance
            if is_prefix || dist <= max_distance {
                Some((s.clone(), is_prefix, dist))
            } else {
                None
            }
        })
        .collect();

    // Sort: prefix matches first, then by levenshtein distance
    matches.sort_by(|(_, is_prefix_a, dist_a), (_, is_prefix_b, dist_b)| {
        // Prefix matches come first (true > false, so reverse)
        match (is_prefix_a, is_prefix_b) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            _ => dist_a.cmp(dist_b),
        }
    });

    matches.into_iter().take(3).map(|(s, _, _)| s).collect()
}

/// Simple Levenshtein distance implementation.
fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row: Vec<usize> = vec![0; b_len + 1];

    for i in 1..=a_len {
        curr_row[0] = i;
        for j in 1..=b_len {
            let cost = if a_chars[i - 1] == b_chars[j - 1] {
                0
            } else {
                1
            };
            curr_row[j] = (prev_row[j] + 1)
                .min(curr_row[j - 1] + 1)
                .min(prev_row[j - 1] + cost);
        }
        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// Information about a table from the schema provider.
#[derive(Clone, Debug)]
pub struct TableInfo {
    /// The schema name.
    pub schema: Option<String>,

    /// The table name.
    pub name: String,

    /// The table type (table, view, etc.).
    pub table_type: TableType,
}

/// Type of a database table.
#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TableType {
    Table,
    View,
    MaterializedView,
    ForeignTable,
}

/// Information about a column from the schema provider.
#[derive(Clone, Debug)]
pub struct ColumnInfo {
    /// The column name.
    pub name: String,

    /// The data type.
    pub data_type: DataType,

    /// Whether the column is nullable.
    pub nullable: bool,

    /// The ordinal position (0-indexed).
    pub ordinal: usize,
}

/// A null schema provider that returns no information.
#[derive(Clone, Debug, Default)]
pub struct NullSchemaProvider;

impl SchemaProvider for NullSchemaProvider {
    fn lookup_table(&self, _schema: Option<&str>, _name: &str) -> Option<TableInfo> {
        None
    }

    fn lookup_columns(&self, _schema: Option<&str>, _table: &str) -> Vec<ColumnInfo> {
        Vec::new()
    }
}

/// The query analyzer.
pub struct Analyzer<'a> {
    /// The schema provider for table/column lookups.
    provider: &'a dyn SchemaProvider,

    /// Collected diagnostics during analysis.
    diagnostics: Vec<Diagnostic>,

    /// Resolved column references.
    resolved_columns: Vec<ResolvedReference>,

    /// Resolved table references.
    resolved_tables: Vec<ResolvedTableRef>,
}

impl<'a> Analyzer<'a> {
    /// Creates a new analyzer with the given schema provider.
    pub fn new(provider: &'a dyn SchemaProvider) -> Self {
        Self {
            provider,
            diagnostics: Vec::new(),
            resolved_columns: Vec::new(),
            resolved_tables: Vec::new(),
        }
    }

    /// Creates an analyzer with no schema information.
    pub fn without_schema() -> Analyzer<'static> {
        static NULL_PROVIDER: NullSchemaProvider = NullSchemaProvider;
        Analyzer::new(&NULL_PROVIDER)
    }

    /// Adds an error diagnostic.
    pub fn error(&mut self, message: impl Into<String>, range: Option<TextRange>) {
        let mut diag = Diagnostic::error(message);
        if let Some(r) = range {
            diag = diag.with_range(r);
        }
        self.diagnostics.push(diag);
    }

    /// Adds a warning diagnostic.
    pub fn warning(&mut self, message: impl Into<String>, range: Option<TextRange>) {
        let mut diag = Diagnostic::warning(message);
        if let Some(r) = range {
            diag = diag.with_range(r);
        }
        self.diagnostics.push(diag);
    }

    /// Builds a table binding from schema information.
    pub fn build_table_binding(
        &self,
        schema: Option<&str>,
        name: &str,
        alias: Option<&str>,
    ) -> TableBinding {
        let columns = self
            .provider
            .lookup_columns(schema, name)
            .into_iter()
            .map(|c| {
                ColumnBinding::new(c.name, c.ordinal)
                    .with_type(c.data_type)
                    .with_nullable(c.nullable)
            })
            .collect();

        let mut binding =
            TableBinding::table(schema.map(String::from), name.to_string()).with_columns(columns);

        if let Some(a) = alias {
            binding = binding.with_alias(a.to_string());
        }

        binding
    }

    /// Builds a CTE binding.
    pub fn build_cte_binding(
        &self,
        name: &str,
        columns: Vec<String>,
        is_recursive: bool,
    ) -> CteBinding {
        let mut binding = CteBinding::new(name.to_string()).with_columns(columns);
        if is_recursive {
            binding = binding.recursive();
        }
        binding
    }

    /// Records a resolved column reference.
    pub fn record_column_reference(
        &mut self,
        range: TextRange,
        table: Arc<TableBinding>,
        column: ColumnBinding,
        was_ambiguous: bool,
    ) {
        self.resolved_columns.push(ResolvedReference {
            range,
            table,
            column,
            was_ambiguous,
        });
    }

    /// Records a resolved table reference.
    pub fn record_table_reference(&mut self, range: TextRange, binding: Arc<TableBinding>) {
        self.resolved_tables
            .push(ResolvedTableRef { range, binding });
    }

    /// Completes the analysis and returns the result.
    pub fn finish(self, scope: Arc<Scope>, output_columns: Vec<ColumnBinding>) -> Analysis {
        Analysis {
            scope,
            columns: self.resolved_columns,
            tables: self.resolved_tables,
            diagnostics: self.diagnostics,
            output_columns,
        }
    }
}

/// Analyzes a FROM clause and builds the scope.
pub fn analyze_from_clause(
    tables: Vec<TableBinding>,
    ctes: Vec<CteBinding>,
    parent_scope: Option<Arc<Scope>>,
) -> Arc<Scope> {
    let mut builder = match parent_scope {
        Some(parent) => ScopeBuilder::with_parent(parent),
        None => ScopeBuilder::new(),
    };

    // Add CTEs first (they shadow physical tables)
    for cte in ctes {
        builder = builder.add_cte(cte);
    }

    // Add tables from FROM clause
    for table in tables {
        builder = builder.add_table(table);
    }

    builder.build()
}

/// Analyzes a LATERAL subquery and builds the scope.
///
/// LATERAL allows referencing tables that appear earlier in the FROM clause.
pub fn analyze_lateral_scope(
    preceding_tables: Vec<String>,
    subquery_tables: Vec<TableBinding>,
    parent_scope: Arc<Scope>,
) -> Arc<Scope> {
    let mut builder = ScopeBuilder::with_parent(parent_scope).lateral();

    for table_name in preceding_tables {
        builder = builder.add_lateral_table(table_name);
    }

    for table in subquery_tables {
        builder = builder.add_table(table);
    }

    builder.build()
}

/// Expands SELECT * to include all columns from all tables in scope.
pub fn expand_star(scope: &Arc<Scope>) -> Vec<ColumnBinding> {
    let mut columns = Vec::new();
    let mut ordinal = 0;

    for table in scope.tables() {
        for col in &table.columns {
            columns.push(ColumnBinding {
                name: col.name.clone(),
                table: Some(table.reference_name().to_string()),
                data_type: col.data_type.clone(),
                ordinal,
                range: None,
                nullable: col.nullable,
            });
            ordinal += 1;
        }
    }

    columns
}

/// Expands SELECT table.* to include all columns from a specific table.
pub fn expand_table_star(
    table_name: &str,
    scope: &Arc<Scope>,
    start_ordinal: usize,
) -> Resolution<Vec<ColumnBinding>> {
    match scope.resolve_table(table_name) {
        Resolution::Resolved(table) => {
            let columns = table
                .columns
                .iter()
                .enumerate()
                .map(|(i, col)| ColumnBinding {
                    name: col.name.clone(),
                    table: Some(table.reference_name().to_string()),
                    data_type: col.data_type.clone(),
                    ordinal: start_ordinal + i,
                    range: None,
                    nullable: col.nullable,
                })
                .collect();
            Resolution::Resolved(columns)
        }
        Resolution::Ambiguous(_) => Resolution::Ambiguous(vec![]),
        Resolution::Unresolved => Resolution::Unresolved,
    }
}

// =============================================================================
// Semantic Query Analysis
// =============================================================================

use mold_syntax::Parse;
use mold_syntax::ast::{AstNode, ColumnRef, JsonbAccessExpr, TableName};

use crate::resolve::{validate_column_reference, validate_table_reference};

/// Analyzes a parsed SQL query for semantic errors.
///
/// This function walks the AST and validates:
/// - Table references (checks if tables exist)
/// - Column references (checks if columns exist and are unambiguous)
/// - JSONB access chains (validates path structure)
///
/// Returns an Analysis containing diagnostics for any errors found.
pub fn analyze_query(parse: &Parse, provider: &dyn SchemaProvider) -> Analysis {
    let mut analyzer = Analyzer::new(provider);

    // Walk the syntax tree to find table and column references
    let root = parse.syntax();

    // First pass: collect table bindings from FROM clauses
    let mut scope_builder = ScopeBuilder::new();

    for node in root.descendants() {
        // Find table names in FROM clauses
        if let Some(table_name) = TableName::cast(node.clone()) {
            let name = table_name.name().map(|t| t.text().to_string());
            let schema = table_name.schema().map(|t| t.text().to_string());
            let alias = table_name
                .alias()
                .and_then(|a| a.name())
                .map(|t| t.text().to_string());
            let range = table_name.syntax().text_range();

            if let Some(ref name) = name {
                // Validate the table exists
                let validation =
                    validate_table_reference(schema.as_deref(), name, provider, Some(range));

                if !validation.exists {
                    let message = if !validation.schema_exists {
                        format!(
                            "Schema '{}' does not exist",
                            schema.as_deref().unwrap_or("")
                        )
                    } else if !validation.suggestions.is_empty() {
                        format!(
                            "Table '{}' does not exist. Did you mean: {}?",
                            name,
                            validation.suggestions.join(", ")
                        )
                    } else {
                        format!("Table '{}' does not exist", name)
                    };
                    analyzer.error(message, Some(range));
                } else {
                    // Build table binding for scope
                    let binding =
                        analyzer.build_table_binding(schema.as_deref(), name, alias.as_deref());
                    scope_builder = scope_builder.add_table(binding);
                }
            }
        }
    }

    let scope = scope_builder.build();

    // Second pass: validate column references
    for node in root.descendants() {
        if let Some(col_ref) = ColumnRef::cast(node.clone()) {
            let column = col_ref.column().map(|t| t.text().to_string());
            let table_qualifier = col_ref.table().map(|t| t.text().to_string());
            let range = col_ref.syntax().text_range();

            if let Some(ref column) = column {
                let validation = validate_column_reference(
                    table_qualifier.as_deref(),
                    column,
                    &scope,
                    provider,
                    Some(range),
                );

                match validation {
                    crate::resolve::ColumnValidation::Found { .. } => {}
                    crate::resolve::ColumnValidation::UnknownColumn {
                        column,
                        table,
                        suggestions,
                        range,
                        ..
                    } => {
                        let message = if !suggestions.is_empty() {
                            format!(
                                "Column '{}' not found in table '{}'. Did you mean: {}?",
                                column,
                                table,
                                suggestions.join(", ")
                            )
                        } else {
                            format!("Column '{}' not found in table '{}'", column, table)
                        };
                        analyzer.error(message, range);
                    }
                    crate::resolve::ColumnValidation::UnknownAlias {
                        alias,
                        available_aliases,
                        range,
                    } => {
                        let message = if available_aliases.is_empty() {
                            format!("Unknown table alias '{}'", alias)
                        } else {
                            format!(
                                "Unknown table alias '{}'. Available: {}",
                                alias,
                                available_aliases.join(", ")
                            )
                        };
                        analyzer.error(message, range);
                    }
                    crate::resolve::ColumnValidation::NotFound { column, range } => {
                        analyzer.error(
                            format!("Column '{}' not found in any table in scope", column),
                            range,
                        );
                    }
                    crate::resolve::ColumnValidation::Ambiguous {
                        column,
                        tables,
                        range,
                    } => {
                        let table_names: Vec<String> =
                            tables.iter().map(|(alias, _)| alias.clone()).collect();
                        analyzer.error(
                            format!(
                                "Column '{}' is ambiguous. Found in tables: {}. Use table qualifier.",
                                column,
                                table_names.join(", ")
                            ),
                            range,
                        );
                    }
                }
            }
        }
    }

    // Third pass: validate JSONB access chains
    for node in root.descendants() {
        if let Some(jsonb_access) = JsonbAccessExpr::cast(node.clone())
            && let Some(chain) = jsonb_access.extract_chain()
        {
            let range = jsonb_access.syntax().text_range();

            // Validate that the base column is of type JSONB/JSON
            let table_qualifier = chain.table_qualifier.as_deref();
            let column_name = &chain.column;

            // Try to find the column type in scope
            let column_type = scope
                .resolve_column(column_name, table_qualifier)
                .ok()
                .and_then(|(_, col)| col.data_type.clone());

            if let Some(ref data_type) = column_type {
                // Check if it's a JSONB/JSON type
                let is_jsonb_type = matches!(data_type, DataType::Jsonb | DataType::Json);

                if !is_jsonb_type {
                    let type_name = data_type.to_string();
                    let op_token = jsonb_access
                        .operator_token()
                        .map(|t| t.text().to_string())
                        .unwrap_or_else(|| "->".to_string());

                    let message = if let Some(table) = table_qualifier {
                        format!(
                            "JSONB operator '{}' cannot be applied to column '{}.{}' of type '{}'",
                            op_token, table, column_name, type_name
                        )
                    } else {
                        format!(
                            "JSONB operator '{}' cannot be applied to column '{}' of type '{}'",
                            op_token, column_name, type_name
                        )
                    };

                    analyzer.error(message, Some(range));
                }
            }

            // Validate the chain structure
            let segments: Vec<(String, bool)> = chain
                .segments
                .iter()
                .map(|s| (s.key.clone(), s.extracts_text))
                .collect();

            let validation = crate::resolve::validate_jsonb_chain(&segments, None);

            for error in validation.errors {
                analyzer.error(error.error_message(), Some(range));
            }

            // Validate JSONB path segments against FHIR schema (if available)
            // Get the table name for JSONB field lookup
            let table_name = scope
                .resolve_column(column_name, table_qualifier)
                .ok()
                .and_then(|(table_binding, _)| {
                    // Get the original table name (not alias)
                    match &table_binding.source {
                        TableSource::Table { name, .. } => Some(name.clone()),
                        _ => Some(table_binding.original_name.clone()),
                    }
                });

            if let Some(ref table) = table_name {
                let path_keys: Vec<&str> = chain.path_keys();

                // Validate each path segment
                for (i, key) in path_keys.iter().enumerate() {
                    let parent_path: Vec<&str> = path_keys[..i].to_vec();

                    // Check if this is a numeric index (array access)
                    let is_numeric_index = key.parse::<usize>().is_ok();

                    if is_numeric_index {
                        // Check if the parent field is actually an array
                        if let Some(false) =
                            provider.jsonb_field_is_array(None, table, column_name, &parent_path)
                        {
                            let path_str = if parent_path.is_empty() {
                                column_name.to_string()
                            } else {
                                format!("{}.{}", column_name, parent_path.join("."))
                            };

                            analyzer.error(
                                format!(
                                    "Cannot use array index '{}' on non-array property '{}'",
                                    key, path_str
                                ),
                                Some(range),
                            );
                        }
                    } else {
                        // Check if this field exists at the current path
                        if let Some(false) =
                            provider.jsonb_field_exists(None, table, column_name, &parent_path, key)
                        {
                            let path_str = if parent_path.is_empty() {
                                column_name.to_string()
                            } else {
                                format!("{}.{}", column_name, parent_path.join("."))
                            };

                            analyzer.error(
                                format!("Unknown property '{}' in '{}'", key, path_str),
                                Some(range),
                            );
                        }
                    }
                }
            }
        }
    }

    analyzer.finish(scope, Vec::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_analysis() {
        let analysis = Analysis::empty();
        assert!(!analysis.has_errors());
        assert_eq!(analysis.errors().count(), 0);
    }

    #[test]
    fn test_diagnostic_creation() {
        let error = Diagnostic::error("test error");
        assert_eq!(error.severity, Severity::Error);
        assert_eq!(error.message, "test error");

        let warning = Diagnostic::warning("test warning");
        assert_eq!(warning.severity, Severity::Warning);
    }

    #[test]
    fn test_analysis_with_errors() {
        let mut analysis = Analysis::empty();
        analysis.diagnostics.push(Diagnostic::error("error 1"));
        analysis.diagnostics.push(Diagnostic::warning("warning 1"));

        assert!(analysis.has_errors());
        assert_eq!(analysis.errors().count(), 1);
        assert_eq!(analysis.warnings().count(), 1);
    }

    #[test]
    fn test_from_clause_analysis() {
        let tables = vec![
            TableBinding::table(None, "users".to_string()),
            TableBinding::table(None, "orders".to_string()),
        ];

        let scope = analyze_from_clause(tables, vec![], None);

        assert!(scope.resolve_table("users").ok().is_some());
        assert!(scope.resolve_table("orders").ok().is_some());
        assert!(scope.resolve_table("products").is_unresolved());
    }

    #[test]
    fn test_cte_analysis() {
        let ctes = vec![CteBinding::new("my_cte".to_string())];

        let scope = analyze_from_clause(vec![], ctes, None);

        assert!(scope.resolve_cte("my_cte").ok().is_some());
        // CTE is also available as a table
        assert!(scope.resolve_table("my_cte").ok().is_some());
    }

    #[test]
    fn test_expand_star() {
        let scope = ScopeBuilder::new()
            .add_table(
                TableBinding::table(None, "users".to_string()).with_columns(vec![
                    ColumnBinding::new("id".to_string(), 0),
                    ColumnBinding::new("name".to_string(), 1),
                ]),
            )
            .add_table(
                TableBinding::table(None, "orders".to_string())
                    .with_columns(vec![ColumnBinding::new("order_id".to_string(), 0)]),
            )
            .build();

        let columns = expand_star(&scope);
        assert_eq!(columns.len(), 3);
    }

    #[test]
    fn test_expand_table_star() {
        let scope = ScopeBuilder::new()
            .add_table(
                TableBinding::table(None, "users".to_string()).with_columns(vec![
                    ColumnBinding::new("id".to_string(), 0),
                    ColumnBinding::new("name".to_string(), 1),
                ]),
            )
            .build();

        let columns = expand_table_star("users", &scope, 0);
        let cols = columns.ok().unwrap();
        assert_eq!(cols.len(), 2);
        assert_eq!(cols[0].name, "id");
        assert_eq!(cols[1].name, "name");
    }

    #[test]
    fn test_analyzer_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Analysis>();
        assert_send_sync::<Diagnostic>();
    }

    #[test]
    fn test_suggest_similar_prefers_prefix_matches() {
        // The user types 'pa' - should suggest 'patient' first, not 'app'
        let tables = vec![
            "app".to_string(),
            "goal".to_string(),
            "flag".to_string(),
            "patient".to_string(),
            "practitioner".to_string(),
        ];

        let suggestions = suggest_similar("pa", &tables, 3);

        // patient and practitioner should come first (prefix matches)
        assert!(!suggestions.is_empty());
        assert!(
            suggestions[0] == "patient" || suggestions[0] == "practitioner",
            "Expected prefix match first, got: {:?}",
            suggestions
        );
    }

    #[test]
    fn test_suggest_similar_levenshtein_fallback() {
        // When no prefix matches, use levenshtein distance
        let tables = vec![
            "users".to_string(),
            "orders".to_string(),
            "products".to_string(),
        ];

        let suggestions = suggest_similar("usrs", &tables, 3);

        // 'users' is closest by levenshtein (distance 1)
        assert_eq!(suggestions.first(), Some(&"users".to_string()));
    }

    #[test]
    fn test_suggest_similar_case_insensitive() {
        let tables = vec!["Patient".to_string(), "Observation".to_string()];

        let suggestions = suggest_similar("pa", &tables, 3);

        assert_eq!(suggestions.first(), Some(&"Patient".to_string()));
    }
}
