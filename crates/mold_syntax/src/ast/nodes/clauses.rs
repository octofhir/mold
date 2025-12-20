//! Clause AST nodes.
//!
//! This module provides typed wrappers for SQL clause nodes such as
//! WITH, FROM, WHERE, GROUP BY, HAVING, ORDER BY, LIMIT, etc.

use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

use super::super::support;
use super::super::traits::AstNode;
use super::expressions::Expr;
use super::table_refs::TableRef;

// =============================================================================
// WITH Clause (Common Table Expressions)
// =============================================================================

/// A WITH clause containing Common Table Expressions (CTEs).
///
/// Structure: `WITH [RECURSIVE] cte [, cte ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WithClause(pub(crate) SyntaxNode);

impl AstNode for WithClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::WITH_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl WithClause {
    /// Returns true if this is a recursive CTE.
    pub fn is_recursive(&self) -> bool {
        support::token(&self.0, SyntaxKind::RECURSIVE_KW).is_some()
    }

    /// Returns an iterator over all CTEs in this WITH clause.
    pub fn ctes(&self) -> impl Iterator<Item = Cte> {
        support::children(&self.0)
    }
}

/// A Common Table Expression (CTE).
///
/// Structure: `name [(columns)] AS (query)`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Cte(pub(crate) SyntaxNode);

impl AstNode for Cte {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CTE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Cte {
    /// Returns the CTE name.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::IDENT)
            .or_else(|| support::token(&self.0, SyntaxKind::QUOTED_IDENT))
    }

    /// Returns the optional column list.
    pub fn columns(&self) -> impl Iterator<Item = SyntaxToken> {
        // Column names are IDENT tokens inside the CTE definition
        support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
    }

    /// Returns the CTE query (the SELECT inside the parentheses).
    pub fn query(&self) -> Option<super::statements::SelectStmt> {
        support::child(&self.0)
    }
}

// =============================================================================
// SELECT Clause
// =============================================================================

/// The SELECT clause specifying columns/expressions to retrieve.
///
/// Structure: `SELECT [ALL|DISTINCT] select_item [, select_item ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SelectClause(pub(crate) SyntaxNode);

impl AstNode for SelectClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SELECT_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SelectClause {
    /// Returns true if DISTINCT is specified.
    pub fn is_distinct(&self) -> bool {
        support::token(&self.0, SyntaxKind::DISTINCT_KW).is_some()
    }

    /// Returns true if ALL is specified.
    pub fn is_all(&self) -> bool {
        support::token(&self.0, SyntaxKind::ALL_KW).is_some()
    }

    /// Returns an iterator over select items.
    pub fn items(&self) -> impl Iterator<Item = SelectItem> {
        support::children(&self.0)
    }
}

/// A single item in a SELECT clause (column, expression, or *).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SelectItem(pub(crate) SyntaxNode);

impl AstNode for SelectItem {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SELECT_ITEM
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SelectItem {
    /// Returns the expression for this select item.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the alias if present.
    pub fn alias(&self) -> Option<Alias> {
        support::child(&self.0)
    }

    /// Returns true if this is a star expression (*).
    pub fn is_star(&self) -> bool {
        support::token(&self.0, SyntaxKind::STAR).is_some()
    }
}

// =============================================================================
// FROM Clause
// =============================================================================

/// The FROM clause specifying source tables.
///
/// Structure: `FROM table_ref [, table_ref ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FromClause(pub(crate) SyntaxNode);

impl AstNode for FromClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::FROM_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl FromClause {
    /// Returns an iterator over table references.
    pub fn table_refs(&self) -> impl Iterator<Item = TableRef> {
        support::children(&self.0)
    }
}

// =============================================================================
// JOIN Clause
// =============================================================================

/// A JOIN expression within a FROM clause.
///
/// Structure: `table_ref [join_type] JOIN table_ref [ON condition | USING (columns)]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JoinExpr(pub(crate) SyntaxNode);

impl AstNode for JoinExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JOIN_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The type of a JOIN operation.
///
/// PostgreSQL join types:
/// - `INNER JOIN` (or just `JOIN`) - returns matching rows from both tables
/// - `LEFT [OUTER] JOIN` - returns all rows from left table, matched rows from right
/// - `RIGHT [OUTER] JOIN` - returns all rows from right table, matched rows from left
/// - `FULL [OUTER] JOIN` - returns all rows from both tables
/// - `CROSS JOIN` - cartesian product
/// - `NATURAL JOIN` - implicit join on matching column names
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JoinType {
    /// INNER JOIN - only matching rows
    Inner,
    /// LEFT [OUTER] JOIN - all left rows, matching right rows
    Left,
    /// RIGHT [OUTER] JOIN - all right rows, matching left rows
    Right,
    /// FULL [OUTER] JOIN - all rows from both sides
    Full,
    /// CROSS JOIN - cartesian product
    Cross,
    /// NATURAL [LEFT|RIGHT|FULL|INNER] JOIN - implicit join on common columns
    Natural,
}

impl JoinType {
    /// Returns true if this is an outer join (LEFT, RIGHT, or FULL).
    pub fn is_outer(&self) -> bool {
        matches!(self, JoinType::Left | JoinType::Right | JoinType::Full)
    }

    /// Returns true if this is an inner join.
    pub fn is_inner(&self) -> bool {
        matches!(self, JoinType::Inner)
    }

    /// Returns true if this is a cross join (cartesian product).
    pub fn is_cross(&self) -> bool {
        matches!(self, JoinType::Cross)
    }

    /// Returns true if this is a natural join.
    pub fn is_natural(&self) -> bool {
        matches!(self, JoinType::Natural)
    }
}

impl JoinExpr {
    /// Returns the left side of the join.
    pub fn left(&self) -> Option<TableRef> {
        support::child(&self.0)
    }

    /// Returns the right side of the join.
    pub fn right(&self) -> Option<TableRef> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the join type.
    ///
    /// Detects LEFT, RIGHT, FULL, CROSS, NATURAL, and INNER joins.
    /// If no explicit type is specified, defaults to INNER.
    pub fn join_type(&self) -> JoinType {
        // Check for NATURAL first as it can combine with other types
        let is_natural = support::token(&self.0, SyntaxKind::NATURAL_KW).is_some();

        if support::token(&self.0, SyntaxKind::LEFT_KW).is_some() {
            if is_natural {
                JoinType::Natural // NATURAL LEFT JOIN
            } else {
                JoinType::Left
            }
        } else if support::token(&self.0, SyntaxKind::RIGHT_KW).is_some() {
            if is_natural {
                JoinType::Natural // NATURAL RIGHT JOIN
            } else {
                JoinType::Right
            }
        } else if support::token(&self.0, SyntaxKind::FULL_KW).is_some() {
            if is_natural {
                JoinType::Natural // NATURAL FULL JOIN
            } else {
                JoinType::Full
            }
        } else if support::token(&self.0, SyntaxKind::CROSS_KW).is_some() {
            JoinType::Cross
        } else if support::token(&self.0, SyntaxKind::NATURAL_KW).is_some() {
            JoinType::Natural
        } else {
            JoinType::Inner
        }
    }

    /// Returns the JOIN condition.
    pub fn condition(&self) -> Option<JoinCondition> {
        support::child(&self.0)
    }

    /// Returns true if the OUTER keyword is explicitly present.
    ///
    /// In PostgreSQL, `LEFT JOIN` and `LEFT OUTER JOIN` are equivalent,
    /// but this method allows you to detect the explicit syntax used.
    pub fn has_outer_keyword(&self) -> bool {
        support::token(&self.0, SyntaxKind::OUTER_KW).is_some()
    }

    /// Returns true if the INNER keyword is explicitly present.
    ///
    /// In PostgreSQL, `JOIN` and `INNER JOIN` are equivalent.
    pub fn has_inner_keyword(&self) -> bool {
        support::token(&self.0, SyntaxKind::INNER_KW).is_some()
    }

    /// Returns true if this is an outer join (LEFT, RIGHT, or FULL).
    ///
    /// Convenience method that checks the join type.
    pub fn is_outer_join(&self) -> bool {
        self.join_type().is_outer()
    }

    /// Returns true if this is an inner join.
    pub fn is_inner_join(&self) -> bool {
        self.join_type().is_inner()
    }

    /// Returns true if this is a left join (LEFT [OUTER] JOIN).
    pub fn is_left_join(&self) -> bool {
        matches!(self.join_type(), JoinType::Left)
    }

    /// Returns true if this is a right join (RIGHT [OUTER] JOIN).
    pub fn is_right_join(&self) -> bool {
        matches!(self.join_type(), JoinType::Right)
    }

    /// Returns true if this is a full join (FULL [OUTER] JOIN).
    pub fn is_full_join(&self) -> bool {
        matches!(self.join_type(), JoinType::Full)
    }

    /// Returns true if this is a cross join (CROSS JOIN).
    pub fn is_cross_join(&self) -> bool {
        matches!(self.join_type(), JoinType::Cross)
    }

    /// Returns true if this is a natural join (NATURAL JOIN).
    pub fn is_natural_join(&self) -> bool {
        support::token(&self.0, SyntaxKind::NATURAL_KW).is_some()
    }
}

/// A JOIN condition (ON or USING).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JoinCondition(pub(crate) SyntaxNode);

impl AstNode for JoinCondition {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JOIN_CONDITION
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JoinCondition {
    /// Returns true if this is an ON condition.
    pub fn is_on(&self) -> bool {
        support::token(&self.0, SyntaxKind::ON_KW).is_some()
    }

    /// Returns true if this is a USING condition.
    pub fn is_using(&self) -> bool {
        support::token(&self.0, SyntaxKind::USING_KW).is_some()
    }

    /// Returns the ON expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

// =============================================================================
// WHERE Clause
// =============================================================================

/// The WHERE clause specifying filter conditions.
///
/// Structure: `WHERE expression`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WhereClause(pub(crate) SyntaxNode);

impl AstNode for WhereClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::WHERE_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl WhereClause {
    /// Returns the filter expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

// =============================================================================
// GROUP BY Clause
// =============================================================================

/// The GROUP BY clause.
///
/// Structure: `GROUP BY expression [, expression ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GroupByClause(pub(crate) SyntaxNode);

impl AstNode for GroupByClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::GROUP_BY_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl GroupByClause {
    /// Returns an iterator over grouping expressions.
    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}

// =============================================================================
// HAVING Clause
// =============================================================================

/// The HAVING clause for filtering grouped results.
///
/// Structure: `HAVING expression`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HavingClause(pub(crate) SyntaxNode);

impl AstNode for HavingClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::HAVING_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl HavingClause {
    /// Returns the having expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

// =============================================================================
// ORDER BY Clause
// =============================================================================

/// The ORDER BY clause.
///
/// Structure: `ORDER BY order_by_item [, order_by_item ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OrderByClause(pub(crate) SyntaxNode);

impl AstNode for OrderByClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ORDER_BY_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl OrderByClause {
    /// Returns an iterator over order by items.
    pub fn items(&self) -> impl Iterator<Item = OrderByItem> {
        support::children(&self.0)
    }
}

/// A single item in an ORDER BY clause.
///
/// Structure: `expression [ASC|DESC] [NULLS FIRST|NULLS LAST]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OrderByItem(pub(crate) SyntaxNode);

impl AstNode for OrderByItem {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ORDER_BY_ITEM
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// Sort direction for ORDER BY.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub enum SortDirection {
    #[default]
    Asc,
    Desc,
}

/// NULL ordering for ORDER BY.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NullsOrder {
    First,
    Last,
}

impl OrderByItem {
    /// Returns the sort expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the sort direction.
    pub fn direction(&self) -> SortDirection {
        if support::token(&self.0, SyntaxKind::DESC_KW).is_some() {
            SortDirection::Desc
        } else {
            SortDirection::Asc
        }
    }

    /// Returns the NULL ordering if specified.
    pub fn nulls_order(&self) -> Option<NullsOrder> {
        if support::token(&self.0, SyntaxKind::NULLS_KW).is_some() {
            if support::token(&self.0, SyntaxKind::FIRST_KW).is_some() {
                Some(NullsOrder::First)
            } else if support::token(&self.0, SyntaxKind::LAST_KW).is_some() {
                Some(NullsOrder::Last)
            } else {
                None
            }
        } else {
            None
        }
    }
}

// =============================================================================
// LIMIT / OFFSET Clauses
// =============================================================================

/// The LIMIT clause.
///
/// Structure: `LIMIT count` or `LIMIT ALL`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LimitClause(pub(crate) SyntaxNode);

impl AstNode for LimitClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LIMIT_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl LimitClause {
    /// Returns the limit count expression.
    pub fn count(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns true if LIMIT ALL is specified.
    pub fn is_all(&self) -> bool {
        support::token(&self.0, SyntaxKind::ALL_KW).is_some()
    }
}

/// The OFFSET clause.
///
/// Structure: `OFFSET count [ROW|ROWS]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OffsetClause(pub(crate) SyntaxNode);

impl AstNode for OffsetClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::OFFSET_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl OffsetClause {
    /// Returns the offset count expression.
    pub fn count(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

// =============================================================================
// INSERT-specific Clauses
// =============================================================================

/// The column list in an INSERT statement.
///
/// Structure: `(column [, column ...])`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InsertColumns(pub(crate) SyntaxNode);

impl AstNode for InsertColumns {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::INSERT_COLUMNS
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl InsertColumns {
    /// Returns an iterator over column names.
    pub fn columns(&self) -> impl Iterator<Item = SyntaxToken> {
        support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
    }
}

/// The VALUES clause in an INSERT statement.
///
/// Structure: `VALUES (expr [, expr ...]) [, (expr [, expr ...]) ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValuesClause(pub(crate) SyntaxNode);

impl AstNode for ValuesClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::VALUES_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ValuesClause {
    /// Returns an iterator over value rows.
    pub fn rows(&self) -> impl Iterator<Item = ValuesRow> {
        support::children(&self.0)
    }
}

/// A single row in a VALUES clause.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValuesRow(pub(crate) SyntaxNode);

impl AstNode for ValuesRow {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::VALUES_ROW
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ValuesRow {
    /// Returns an iterator over expressions in this row.
    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}

/// The ON CONFLICT clause in an INSERT statement.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OnConflictClause(pub(crate) SyntaxNode);

impl AstNode for OnConflictClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ON_CONFLICT_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl OnConflictClause {
    /// Returns the conflict target if specified.
    pub fn target(&self) -> Option<ConflictTarget> {
        support::child(&self.0)
    }

    /// Returns the conflict action.
    pub fn action(&self) -> Option<ConflictAction> {
        support::child(&self.0)
    }
}

/// The conflict target in ON CONFLICT.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConflictTarget(pub(crate) SyntaxNode);

impl AstNode for ConflictTarget {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CONFLICT_TARGET
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The conflict action (DO NOTHING or DO UPDATE).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConflictAction(pub(crate) SyntaxNode);

impl AstNode for ConflictAction {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CONFLICT_ACTION
            || kind == SyntaxKind::DO_UPDATE
            || kind == SyntaxKind::DO_NOTHING
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ConflictAction {
    /// Returns true if this is DO NOTHING.
    pub fn is_nothing(&self) -> bool {
        self.0.kind() == SyntaxKind::DO_NOTHING
            || support::token(&self.0, SyntaxKind::NOTHING_KW).is_some()
    }

    /// Returns true if this is DO UPDATE.
    pub fn is_update(&self) -> bool {
        self.0.kind() == SyntaxKind::DO_UPDATE
            || support::token(&self.0, SyntaxKind::UPDATE_KW).is_some()
    }
}

// =============================================================================
// UPDATE-specific Clauses
// =============================================================================

/// The SET clause in an UPDATE statement.
///
/// Structure: `SET column = expr [, column = expr ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SetClause(pub(crate) SyntaxNode);

impl AstNode for SetClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SET_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SetClause {
    /// Returns an iterator over set items (assignments).
    pub fn items(&self) -> impl Iterator<Item = SetItem> {
        support::children(&self.0)
    }
}

/// A single assignment in a SET clause.
///
/// Structure: `column = expr`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SetItem(pub(crate) SyntaxNode);

impl AstNode for SetItem {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SET_ITEM
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SetItem {
    /// Returns the column name.
    pub fn column(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::IDENT)
            .or_else(|| support::token(&self.0, SyntaxKind::QUOTED_IDENT))
    }

    /// Returns the value expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

// =============================================================================
// DELETE-specific Clauses
// =============================================================================

/// The USING clause in a DELETE statement.
///
/// Structure: `USING table_ref [, table_ref ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UsingClause(pub(crate) SyntaxNode);

impl AstNode for UsingClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::USING_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl UsingClause {
    /// Returns an iterator over table references.
    pub fn table_refs(&self) -> impl Iterator<Item = TableRef> {
        support::children(&self.0)
    }
}

// =============================================================================
// RETURNING Clause (shared by INSERT, UPDATE, DELETE)
// =============================================================================

/// The RETURNING clause.
///
/// Structure: `RETURNING select_item [, select_item ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ReturningClause(pub(crate) SyntaxNode);

impl AstNode for ReturningClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::RETURNING_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ReturningClause {
    /// Returns an iterator over returned items.
    pub fn items(&self) -> impl Iterator<Item = SelectItem> {
        support::children(&self.0)
    }
}

// =============================================================================
// Alias
// =============================================================================

/// An alias for a table or expression.
///
/// Structure: `[AS] name [(column [, column ...])]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Alias(pub(crate) SyntaxNode);

impl AstNode for Alias {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ALIAS
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Alias {
    /// Returns the alias name.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::IDENT)
            .or_else(|| support::token(&self.0, SyntaxKind::QUOTED_IDENT))
    }

    /// Returns an iterator over column aliases if present.
    pub fn columns(&self) -> impl Iterator<Item = SyntaxToken> {
        // Skip the first IDENT (the alias name) and return the rest
        support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
            .skip(1)
    }
}
