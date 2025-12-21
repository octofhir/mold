//! Query analysis entry point.
//!
//! This module provides the main entry point for semantic analysis of SQL queries.

use std::sync::Arc;

use text_size::TextRange;

use crate::binding::{ColumnBinding, CteBinding, DataType, Resolution, TableBinding};
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
}
