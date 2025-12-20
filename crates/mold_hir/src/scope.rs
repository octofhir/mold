//! Scope management for semantic analysis.
//!
//! Scopes form a hierarchy that tracks available names at each point in a query.
//! PostgreSQL has specific scoping rules:
//! - CTEs are visible in the entire query
//! - FROM clause introduces table bindings
//! - LATERAL allows referencing preceding tables
//! - Subqueries create child scopes

use std::collections::HashMap;
use std::sync::Arc;

use crate::binding::{ColumnBinding, CteBinding, Resolution, TableBinding};

/// A scope representing the available names at a point in a query.
///
/// Scopes are immutable and form a linked list through the `parent` field.
/// This allows efficient sharing of parent scopes across child queries.
#[derive(Clone, Debug)]
pub struct Scope {
    /// Tables available in this scope, keyed by their reference name.
    tables: HashMap<String, Arc<TableBinding>>,

    /// CTEs available in this scope, keyed by name.
    ctes: HashMap<String, Arc<CteBinding>>,

    /// Column aliases from SELECT clause (for ORDER BY resolution).
    select_aliases: HashMap<String, ColumnBinding>,

    /// Parent scope for name lookup chain.
    parent: Option<Arc<Scope>>,

    /// Whether LATERAL scoping is active (can reference preceding tables).
    is_lateral: bool,

    /// Tables that are visible for LATERAL references (preceding tables in FROM).
    lateral_tables: Vec<String>,
}

impl Scope {
    /// Creates a new empty root scope.
    pub fn new() -> Self {
        Self {
            tables: HashMap::new(),
            ctes: HashMap::new(),
            select_aliases: HashMap::new(),
            parent: None,
            is_lateral: false,
            lateral_tables: Vec::new(),
        }
    }

    /// Creates a child scope with this scope as parent.
    pub fn child(self: &Arc<Self>) -> ScopeBuilder {
        ScopeBuilder::with_parent(Arc::clone(self))
    }

    /// Returns the parent scope, if any.
    pub fn parent(&self) -> Option<&Arc<Scope>> {
        self.parent.as_ref()
    }

    /// Returns all tables in this scope (not including parent scopes).
    pub fn tables(&self) -> impl Iterator<Item = &Arc<TableBinding>> {
        self.tables.values()
    }

    /// Returns all CTEs in this scope (not including parent scopes).
    pub fn ctes(&self) -> impl Iterator<Item = &Arc<CteBinding>> {
        self.ctes.values()
    }

    /// Resolves a table name in this scope and parent scopes.
    ///
    /// Uses PostgreSQL's case-insensitive matching for unquoted identifiers.
    pub fn resolve_table(&self, name: &str) -> Resolution<Arc<TableBinding>> {
        let name_lower = normalize_name(name);

        // First, check local tables
        if let Some(table) = self.tables.get(&name_lower) {
            return Resolution::Resolved(Arc::clone(table));
        }

        // Check CTEs (CTEs shadow physical tables)
        if self.ctes.contains_key(&name_lower) {
            // CTE exists but we return it as a table binding
            // The caller should have created a TableBinding from the CTE
            if let Some(table) = self.tables.get(&name_lower) {
                return Resolution::Resolved(Arc::clone(table));
            }
        }

        // Check parent scope
        if let Some(parent) = &self.parent {
            return parent.resolve_table(name);
        }

        Resolution::Unresolved
    }

    /// Resolves a CTE name in this scope and parent scopes.
    pub fn resolve_cte(&self, name: &str) -> Resolution<Arc<CteBinding>> {
        let name_lower = normalize_name(name);

        if let Some(cte) = self.ctes.get(&name_lower) {
            return Resolution::Resolved(Arc::clone(cte));
        }

        if let Some(parent) = &self.parent {
            return parent.resolve_cte(name);
        }

        Resolution::Unresolved
    }

    /// Resolves a column reference.
    ///
    /// If `table` is specified, looks only in that table.
    /// Otherwise, searches all tables in scope.
    pub fn resolve_column(
        &self,
        column: &str,
        table: Option<&str>,
    ) -> Resolution<(Arc<TableBinding>, ColumnBinding)> {
        let column_lower = normalize_name(column);

        if let Some(table_name) = table {
            // Qualified reference: table.column
            match self.resolve_table(table_name) {
                Resolution::Resolved(table_binding) => {
                    if let Some(col) = table_binding.find_column(&column_lower) {
                        let col = col.clone();
                        return Resolution::Resolved((table_binding, col));
                    }
                    Resolution::Unresolved
                }
                Resolution::Ambiguous(tables) => {
                    // Multiple tables with same name - check if column exists in any
                    let matches: Vec<_> = tables
                        .into_iter()
                        .filter_map(|t| {
                            let col = t.find_column(&column_lower)?.clone();
                            Some((t, col))
                        })
                        .collect();

                    match matches.len() {
                        0 => Resolution::Unresolved,
                        1 => Resolution::Resolved(matches.into_iter().next().unwrap()),
                        _ => Resolution::Ambiguous(matches),
                    }
                }
                Resolution::Unresolved => Resolution::Unresolved,
            }
        } else {
            // Unqualified reference: search all tables
            self.resolve_unqualified_column(&column_lower)
        }
    }

    /// Resolves an unqualified column name across all tables in scope.
    fn resolve_unqualified_column(
        &self,
        column: &str,
    ) -> Resolution<(Arc<TableBinding>, ColumnBinding)> {
        let mut matches = Vec::new();

        // Search local tables
        for table in self.tables.values() {
            if let Some(col) = table.find_column(column) {
                matches.push((Arc::clone(table), col.clone()));
            }
        }

        // If LATERAL, also search lateral-visible tables
        // (This is handled by the scope structure - lateral tables are in scope)

        // Search parent scope if no local match
        if matches.is_empty() {
            if let Some(parent) = &self.parent {
                return parent.resolve_unqualified_column(column);
            }
        }

        match matches.len() {
            0 => Resolution::Unresolved,
            1 => Resolution::Resolved(matches.into_iter().next().unwrap()),
            _ => Resolution::Ambiguous(matches),
        }
    }

    /// Resolves a SELECT alias (for ORDER BY).
    pub fn resolve_select_alias(&self, name: &str) -> Option<&ColumnBinding> {
        let name_lower = normalize_name(name);
        self.select_aliases.get(&name_lower)
    }

    /// Resolves an ORDER BY item which can be:
    /// 1. A column reference
    /// 2. A SELECT alias
    /// 3. An ordinal position (1-based index)
    pub fn resolve_order_by(
        &self,
        name: &str,
        select_columns: &[ColumnBinding],
    ) -> Resolution<ColumnBinding> {
        // First, try as ordinal position
        if let Ok(ordinal) = name.parse::<usize>() {
            if ordinal >= 1 && ordinal <= select_columns.len() {
                return Resolution::Resolved(select_columns[ordinal - 1].clone());
            }
        }

        // Try as SELECT alias
        if let Some(alias) = self.resolve_select_alias(name) {
            return Resolution::Resolved(alias.clone());
        }

        // Try as column reference
        match self.resolve_column(name, None) {
            Resolution::Resolved((_, col)) => Resolution::Resolved(col),
            Resolution::Ambiguous(matches) => {
                Resolution::Ambiguous(matches.into_iter().map(|(_, c)| c).collect())
            }
            Resolution::Unresolved => Resolution::Unresolved,
        }
    }

    /// Returns true if this scope or any parent has LATERAL enabled.
    pub fn is_lateral(&self) -> bool {
        self.is_lateral || self.parent.as_ref().is_some_and(|p| p.is_lateral())
    }

    /// Returns the tables visible for LATERAL references.
    pub fn lateral_tables(&self) -> &[String] {
        &self.lateral_tables
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for constructing scopes.
#[derive(Debug)]
pub struct ScopeBuilder {
    tables: HashMap<String, Arc<TableBinding>>,
    ctes: HashMap<String, Arc<CteBinding>>,
    select_aliases: HashMap<String, ColumnBinding>,
    parent: Option<Arc<Scope>>,
    is_lateral: bool,
    lateral_tables: Vec<String>,
}

impl ScopeBuilder {
    /// Creates a new scope builder for a root scope.
    pub fn new() -> Self {
        Self {
            tables: HashMap::new(),
            ctes: HashMap::new(),
            select_aliases: HashMap::new(),
            parent: None,
            is_lateral: false,
            lateral_tables: Vec::new(),
        }
    }

    /// Creates a new scope builder with a parent scope.
    pub fn with_parent(parent: Arc<Scope>) -> Self {
        Self {
            tables: HashMap::new(),
            ctes: HashMap::new(),
            select_aliases: HashMap::new(),
            parent: Some(parent),
            is_lateral: false,
            lateral_tables: Vec::new(),
        }
    }

    /// Adds a table binding to this scope.
    pub fn add_table(mut self, binding: TableBinding) -> Self {
        let name = normalize_name(&binding.name);
        self.tables.insert(name, Arc::new(binding));
        self
    }

    /// Adds a CTE binding to this scope.
    pub fn add_cte(mut self, binding: CteBinding) -> Self {
        let name = normalize_name(&binding.name);
        // Also add as a table binding for resolution
        let table_binding = TableBinding::cte(binding.name.clone()).with_columns(
            binding
                .columns
                .iter()
                .enumerate()
                .map(|(i, c)| ColumnBinding::new(c.clone(), i))
                .collect(),
        );
        self.tables.insert(name.clone(), Arc::new(table_binding));
        self.ctes.insert(name, Arc::new(binding));
        self
    }

    /// Adds a SELECT alias for ORDER BY resolution.
    pub fn add_select_alias(mut self, name: String, binding: ColumnBinding) -> Self {
        let name_lower = normalize_name(&name);
        self.select_aliases.insert(name_lower, binding);
        self
    }

    /// Enables LATERAL scoping for this scope.
    pub fn lateral(mut self) -> Self {
        self.is_lateral = true;
        self
    }

    /// Adds a table to the LATERAL-visible set.
    pub fn add_lateral_table(mut self, name: String) -> Self {
        self.lateral_tables.push(normalize_name(&name));
        self
    }

    /// Builds the scope.
    pub fn build(self) -> Arc<Scope> {
        Arc::new(Scope {
            tables: self.tables,
            ctes: self.ctes,
            select_aliases: self.select_aliases,
            parent: self.parent,
            is_lateral: self.is_lateral,
            lateral_tables: self.lateral_tables,
        })
    }
}

impl Default for ScopeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Normalizes a name for case-insensitive lookup.
///
/// PostgreSQL treats unquoted identifiers as lowercase.
/// Quoted identifiers preserve case.
fn normalize_name(name: &str) -> String {
    // If the name starts and ends with quotes, preserve case
    if name.starts_with('"') && name.ends_with('"') && name.len() > 2 {
        // Remove quotes but preserve case
        name[1..name.len() - 1].to_string()
    } else {
        // Lowercase for unquoted identifiers
        name.to_lowercase()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_name() {
        assert_eq!(normalize_name("Users"), "users");
        assert_eq!(normalize_name("USERS"), "users");
        assert_eq!(normalize_name("\"Users\""), "Users");
        assert_eq!(normalize_name("\"USERS\""), "USERS");
    }

    #[test]
    fn test_empty_scope() {
        let scope = ScopeBuilder::new().build();
        assert!(scope.resolve_table("users").is_unresolved());
        assert!(scope.resolve_cte("my_cte").is_unresolved());
    }

    #[test]
    fn test_table_resolution() {
        let scope = ScopeBuilder::new()
            .add_table(TableBinding::table(None, "users".to_string()))
            .build();

        // Case-insensitive match
        assert!(scope.resolve_table("users").ok().is_some());
        assert!(scope.resolve_table("USERS").ok().is_some());
        assert!(scope.resolve_table("Users").ok().is_some());

        // Non-existent table
        assert!(scope.resolve_table("orders").is_unresolved());
    }

    #[test]
    fn test_table_alias() {
        let scope = ScopeBuilder::new()
            .add_table(TableBinding::table(None, "users".to_string()).with_alias("u".to_string()))
            .build();

        // Alias should be resolvable
        assert!(scope.resolve_table("u").ok().is_some());

        // Original name NOT resolvable when aliased (PostgreSQL behavior)
        // Note: In our implementation, we use the alias as the key
        assert!(scope.resolve_table("users").is_unresolved());
    }

    #[test]
    fn test_cte_resolution() {
        let scope = ScopeBuilder::new()
            .add_cte(CteBinding::new("my_cte".to_string()))
            .build();

        assert!(scope.resolve_cte("my_cte").ok().is_some());
        assert!(scope.resolve_cte("MY_CTE").ok().is_some());
        assert!(scope.resolve_cte("other").is_unresolved());
    }

    #[test]
    fn test_parent_scope_resolution() {
        let parent = ScopeBuilder::new()
            .add_table(TableBinding::table(None, "users".to_string()))
            .build();

        let child = parent
            .child()
            .add_table(TableBinding::table(None, "orders".to_string()))
            .build();

        // Child can resolve its own tables
        assert!(child.resolve_table("orders").ok().is_some());

        // Child can resolve parent tables
        assert!(child.resolve_table("users").ok().is_some());

        // Parent cannot resolve child tables
        assert!(parent.resolve_table("orders").is_unresolved());
    }

    #[test]
    fn test_column_resolution() {
        let scope = ScopeBuilder::new()
            .add_table(
                TableBinding::table(None, "users".to_string()).with_columns(vec![
                    ColumnBinding::new("id".to_string(), 0),
                    ColumnBinding::new("name".to_string(), 1),
                ]),
            )
            .build();

        // Qualified reference
        let result = scope.resolve_column("id", Some("users"));
        assert!(result.ok().is_some());

        // Unqualified reference
        let result = scope.resolve_column("id", None);
        assert!(result.ok().is_some());

        // Non-existent column
        let result = scope.resolve_column("email", None);
        assert!(result.is_unresolved());
    }

    #[test]
    fn test_ambiguous_column() {
        let scope = ScopeBuilder::new()
            .add_table(
                TableBinding::table(None, "users".to_string())
                    .with_alias("u".to_string())
                    .with_columns(vec![ColumnBinding::new("id".to_string(), 0)]),
            )
            .add_table(
                TableBinding::table(None, "orders".to_string())
                    .with_alias("o".to_string())
                    .with_columns(vec![ColumnBinding::new("id".to_string(), 0)]),
            )
            .build();

        // Unqualified "id" is ambiguous
        let result = scope.resolve_column("id", None);
        assert!(result.is_ambiguous());

        // Qualified references are not ambiguous
        assert!(scope.resolve_column("id", Some("u")).ok().is_some());
        assert!(scope.resolve_column("id", Some("o")).ok().is_some());
    }

    #[test]
    fn test_scope_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Scope>();
        assert_send_sync::<ScopeBuilder>();
    }
}
