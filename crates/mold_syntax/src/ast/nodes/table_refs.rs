//! Table reference AST nodes.
//!
//! This module provides typed wrappers for table reference nodes used in
//! FROM clauses, JOINs, and other contexts.

use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

use super::super::support;
use super::super::traits::AstNode;
use super::clauses::{Alias, JoinExpr};
use super::expressions::FuncCall;
use super::statements::SelectStmt;

/// Enum representing all table reference types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TableRef {
    /// A simple or qualified table name.
    Table(TableName),
    /// A subquery used as a table source.
    Subquery(SubqueryRef),
    /// A function call used as a table source.
    Function(FunctionRef),
    /// A JOIN expression.
    Join(JoinExpr),
}

impl AstNode for TableRef {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::TABLE_REF
                | SyntaxKind::TABLE_NAME
                | SyntaxKind::SUBQUERY
                | SyntaxKind::FUNC_CALL
                | SyntaxKind::JOIN_EXPR
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::TABLE_NAME => Some(TableRef::Table(TableName(node))),
            SyntaxKind::TABLE_REF => {
                // TABLE_REF is a wrapper that may contain different types
                if let Some(table_name) = support::child::<TableName>(&node) {
                    return Some(TableRef::Table(table_name));
                }
                if let Some(subquery) = support::child::<SubqueryRef>(&node) {
                    return Some(TableRef::Subquery(subquery));
                }
                if let Some(func_ref) = support::child::<FunctionRef>(&node) {
                    return Some(TableRef::Function(func_ref));
                }
                if let Some(join) = support::child::<JoinExpr>(&node) {
                    return Some(TableRef::Join(join));
                }
                // If TABLE_REF contains a table name directly, wrap it
                Some(TableRef::Table(TableName(node)))
            }
            SyntaxKind::SUBQUERY => Some(TableRef::Subquery(SubqueryRef(node))),
            SyntaxKind::FUNC_CALL => Some(TableRef::Function(FunctionRef(node))),
            SyntaxKind::JOIN_EXPR => Some(TableRef::Join(JoinExpr(node))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            TableRef::Table(it) => it.syntax(),
            TableRef::Subquery(it) => it.syntax(),
            TableRef::Function(it) => it.syntax(),
            TableRef::Join(it) => it.syntax(),
        }
    }
}

impl TableRef {
    /// Returns the alias for this table reference, if any.
    pub fn alias(&self) -> Option<Alias> {
        support::child(self.syntax())
    }
}

// =============================================================================
// Table Name
// =============================================================================

/// A table name reference, optionally qualified with schema.
///
/// Structure: `[[catalog.]schema.]table`
///
/// Examples:
/// - `users`
/// - `public.users`
/// - `mydb.public.users`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TableName(pub(crate) SyntaxNode);

impl AstNode for TableName {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::TABLE_NAME || kind == SyntaxKind::TABLE_REF
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl TableName {
    /// Returns the table name (the last identifier in the chain).
    pub fn name(&self) -> Option<SyntaxToken> {
        let idents: Vec<_> = self.name_parts().collect();
        idents.last().cloned()
    }

    /// Returns the schema name if present (the second-to-last identifier).
    pub fn schema(&self) -> Option<SyntaxToken> {
        let idents: Vec<_> = self.name_parts().collect();
        if idents.len() >= 2 {
            idents.get(idents.len() - 2).cloned()
        } else {
            None
        }
    }

    /// Returns the catalog name if present (the third-to-last identifier).
    pub fn catalog(&self) -> Option<SyntaxToken> {
        let idents: Vec<_> = self.name_parts().collect();
        if idents.len() >= 3 {
            idents.get(idents.len() - 3).cloned()
        } else {
            None
        }
    }

    /// Returns an iterator over all name parts in order.
    pub fn name_parts(&self) -> impl Iterator<Item = SyntaxToken> {
        support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
    }

    /// Returns the alias for this table, if any.
    pub fn alias(&self) -> Option<Alias> {
        support::child(&self.0)
    }

    /// Returns true if ONLY is specified (for inheritance).
    pub fn is_only(&self) -> bool {
        support::token(&self.0, SyntaxKind::ONLY_KW).is_some()
    }

    /// Returns the full qualified name as a string.
    pub fn full_name(&self) -> String {
        self.name_parts()
            .map(|t| t.text().to_string())
            .collect::<Vec<_>>()
            .join(".")
    }
}

// =============================================================================
// Subquery Reference
// =============================================================================

/// A subquery used as a table source.
///
/// Structure: `(SELECT ...) [AS] alias`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SubqueryRef(pub(crate) SyntaxNode);

impl AstNode for SubqueryRef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SUBQUERY
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SubqueryRef {
    /// Returns the subquery SELECT statement.
    pub fn query(&self) -> Option<SelectStmt> {
        support::child(&self.0)
    }

    /// Returns the alias for this subquery.
    pub fn alias(&self) -> Option<Alias> {
        support::child(&self.0)
    }

    /// Returns true if LATERAL is specified.
    pub fn is_lateral(&self) -> bool {
        support::token(&self.0, SyntaxKind::LATERAL_KW).is_some()
    }
}

// =============================================================================
// Function Reference
// =============================================================================

/// A function call used as a table source (table-valued function).
///
/// Structure: `function_call [AS] alias`
///
/// Examples:
/// - `generate_series(1, 10) AS n`
/// - `unnest(array[1,2,3]) AS x`
/// - `jsonb_each(data) AS (key, value)`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionRef(pub(crate) SyntaxNode);

impl AstNode for FunctionRef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::FUNC_CALL
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl FunctionRef {
    /// Returns the function call.
    pub fn function(&self) -> Option<FuncCall> {
        FuncCall::cast(self.0.clone())
    }

    /// Returns the alias for this function reference.
    pub fn alias(&self) -> Option<Alias> {
        support::child(&self.0)
    }

    /// Returns true if LATERAL is specified.
    pub fn is_lateral(&self) -> bool {
        support::token(&self.0, SyntaxKind::LATERAL_KW).is_some()
    }

    /// Returns true if WITH ORDINALITY is specified.
    pub fn with_ordinality(&self) -> bool {
        // Check for WITH keyword followed by specific context
        // In PostgreSQL, this adds a row number column
        support::token(&self.0, SyntaxKind::WITH_KW).is_some()
    }
}

// =============================================================================
// Qualified Name (for generic qualified identifiers)
// =============================================================================

/// A qualified name (schema.table.column or similar).
///
/// This is a generic representation for any dot-separated identifier chain.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QualifiedName(pub(crate) SyntaxNode);

impl AstNode for QualifiedName {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::QUALIFIED_NAME
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl QualifiedName {
    /// Returns an iterator over name parts.
    pub fn parts(&self) -> impl Iterator<Item = SyntaxToken> {
        support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
    }

    /// Returns the first part (typically schema or table).
    pub fn first(&self) -> Option<SyntaxToken> {
        self.parts().next()
    }

    /// Returns the last part (typically the column or table name).
    pub fn last(&self) -> Option<SyntaxToken> {
        self.parts().last()
    }

    /// Returns the full qualified name as a string.
    pub fn full_name(&self) -> String {
        self.parts()
            .map(|t| t.text().to_string())
            .collect::<Vec<_>>()
            .join(".")
    }
}
