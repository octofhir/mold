//! Expression AST nodes.
//!
//! This module provides typed wrappers for SQL expression nodes including
//! literals, column references, operators, function calls, and more.

use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

use super::super::support;
use super::super::traits::AstNode;

/// Enum representing all SQL expression types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Literal(Literal),
    ColumnRef(ColumnRef),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Paren(ParenExpr),
    FuncCall(FuncCall),
    Cast(CastExpr),
    Case(CaseExpr),
    Between(BetweenExpr),
    In(InExpr),
    Like(LikeExpr),
    Is(IsExpr),
    Exists(ExistsExpr),
    Subquery(SubqueryExpr),
    Array(ArrayExpr),
    Row(RowExpr),
    Coalesce(CoalesceExpr),
    NullIf(NullIfExpr),
    Greatest(GreatestExpr),
    Least(LeastExpr),
    Jsonb(super::jsonb::JsonbExpr),
}

impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::LITERAL
                | SyntaxKind::COLUMN_REF
                | SyntaxKind::BINARY_EXPR
                | SyntaxKind::UNARY_EXPR
                | SyntaxKind::PAREN_EXPR
                | SyntaxKind::FUNC_CALL
                | SyntaxKind::CAST_EXPR
                | SyntaxKind::CASE_EXPR
                | SyntaxKind::BETWEEN_EXPR
                | SyntaxKind::IN_EXPR
                | SyntaxKind::LIKE_EXPR
                | SyntaxKind::IS_EXPR
                | SyntaxKind::EXISTS_EXPR
                | SyntaxKind::SUBQUERY_EXPR
                | SyntaxKind::ARRAY_EXPR
                | SyntaxKind::ROW_EXPR
                | SyntaxKind::COALESCE_EXPR
                | SyntaxKind::NULLIF_EXPR
                | SyntaxKind::GREATEST_EXPR
                | SyntaxKind::LEAST_EXPR
                | SyntaxKind::JSONB_ACCESS_EXPR
                | SyntaxKind::JSONB_PATH_EXPR
                | SyntaxKind::JSONB_CONTAINS_EXPR
                | SyntaxKind::JSONB_EXISTS_EXPR
                | SyntaxKind::QUALIFIED_NAME
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::LITERAL => Some(Expr::Literal(Literal(node))),
            SyntaxKind::COLUMN_REF | SyntaxKind::QUALIFIED_NAME => {
                Some(Expr::ColumnRef(ColumnRef(node)))
            }
            SyntaxKind::BINARY_EXPR => Some(Expr::Binary(BinaryExpr(node))),
            SyntaxKind::UNARY_EXPR => Some(Expr::Unary(UnaryExpr(node))),
            SyntaxKind::PAREN_EXPR => Some(Expr::Paren(ParenExpr(node))),
            SyntaxKind::FUNC_CALL => Some(Expr::FuncCall(FuncCall(node))),
            SyntaxKind::CAST_EXPR => Some(Expr::Cast(CastExpr(node))),
            SyntaxKind::CASE_EXPR => Some(Expr::Case(CaseExpr(node))),
            SyntaxKind::BETWEEN_EXPR => Some(Expr::Between(BetweenExpr(node))),
            SyntaxKind::IN_EXPR => Some(Expr::In(InExpr(node))),
            SyntaxKind::LIKE_EXPR => Some(Expr::Like(LikeExpr(node))),
            SyntaxKind::IS_EXPR => Some(Expr::Is(IsExpr(node))),
            SyntaxKind::EXISTS_EXPR => Some(Expr::Exists(ExistsExpr(node))),
            SyntaxKind::SUBQUERY_EXPR => Some(Expr::Subquery(SubqueryExpr(node))),
            SyntaxKind::ARRAY_EXPR => Some(Expr::Array(ArrayExpr(node))),
            SyntaxKind::ROW_EXPR => Some(Expr::Row(RowExpr(node))),
            SyntaxKind::COALESCE_EXPR => Some(Expr::Coalesce(CoalesceExpr(node))),
            SyntaxKind::NULLIF_EXPR => Some(Expr::NullIf(NullIfExpr(node))),
            SyntaxKind::GREATEST_EXPR => Some(Expr::Greatest(GreatestExpr(node))),
            SyntaxKind::LEAST_EXPR => Some(Expr::Least(LeastExpr(node))),
            SyntaxKind::JSONB_ACCESS_EXPR
            | SyntaxKind::JSONB_PATH_EXPR
            | SyntaxKind::JSONB_CONTAINS_EXPR
            | SyntaxKind::JSONB_EXISTS_EXPR => super::jsonb::JsonbExpr::cast(node).map(Expr::Jsonb),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::Literal(it) => it.syntax(),
            Expr::ColumnRef(it) => it.syntax(),
            Expr::Binary(it) => it.syntax(),
            Expr::Unary(it) => it.syntax(),
            Expr::Paren(it) => it.syntax(),
            Expr::FuncCall(it) => it.syntax(),
            Expr::Cast(it) => it.syntax(),
            Expr::Case(it) => it.syntax(),
            Expr::Between(it) => it.syntax(),
            Expr::In(it) => it.syntax(),
            Expr::Like(it) => it.syntax(),
            Expr::Is(it) => it.syntax(),
            Expr::Exists(it) => it.syntax(),
            Expr::Subquery(it) => it.syntax(),
            Expr::Array(it) => it.syntax(),
            Expr::Row(it) => it.syntax(),
            Expr::Coalesce(it) => it.syntax(),
            Expr::NullIf(it) => it.syntax(),
            Expr::Greatest(it) => it.syntax(),
            Expr::Least(it) => it.syntax(),
            Expr::Jsonb(it) => it.syntax(),
        }
    }
}

// =============================================================================
// Literal
// =============================================================================

/// A literal value (number, string, boolean, null).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal(pub(crate) SyntaxNode);

impl AstNode for Literal {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LITERAL
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The kind of literal value.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Integer,
    Float,
    String,
    DollarString,
    BitString,
    HexString,
    Boolean(bool),
    Null,
}

impl Literal {
    /// Returns the literal token.
    pub fn token(&self) -> Option<SyntaxToken> {
        support::first_token(&self.0)
    }

    /// Returns the literal kind.
    pub fn kind(&self) -> Option<LiteralKind> {
        let token = self.token()?;
        match token.kind() {
            SyntaxKind::INTEGER => Some(LiteralKind::Integer),
            SyntaxKind::FLOAT => Some(LiteralKind::Float),
            SyntaxKind::STRING => Some(LiteralKind::String),
            SyntaxKind::DOLLAR_STRING => Some(LiteralKind::DollarString),
            SyntaxKind::BIT_STRING => Some(LiteralKind::BitString),
            SyntaxKind::HEX_STRING => Some(LiteralKind::HexString),
            SyntaxKind::TRUE_KW => Some(LiteralKind::Boolean(true)),
            SyntaxKind::FALSE_KW => Some(LiteralKind::Boolean(false)),
            SyntaxKind::NULL_KW => Some(LiteralKind::Null),
            _ => None,
        }
    }

    /// Returns the text of the literal.
    pub fn text(&self) -> Option<String> {
        self.token().map(|t| t.text().to_string())
    }
}

// =============================================================================
// Column Reference
// =============================================================================

/// A reference to a column, optionally qualified with table/schema.
///
/// Structure: `[[schema.]table.]column`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ColumnRef(pub(crate) SyntaxNode);

impl AstNode for ColumnRef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::COLUMN_REF || kind == SyntaxKind::QUALIFIED_NAME
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ColumnRef {
    /// Returns the column name (the last identifier).
    pub fn column(&self) -> Option<SyntaxToken> {
        // The column is the last IDENT in the chain
        support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
            .last()
    }

    /// Returns the table name if present (the second-to-last identifier).
    pub fn table(&self) -> Option<SyntaxToken> {
        let idents: Vec<_> = support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
            .collect();

        if idents.len() >= 2 {
            idents.get(idents.len() - 2).cloned()
        } else {
            None
        }
    }

    /// Returns the schema name if present (the third-to-last identifier).
    pub fn schema(&self) -> Option<SyntaxToken> {
        let idents: Vec<_> = support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
            .collect();

        if idents.len() >= 3 {
            idents.get(idents.len() - 3).cloned()
        } else {
            None
        }
    }

    /// Returns all name parts in order.
    pub fn parts(&self) -> impl Iterator<Item = SyntaxToken> {
        support::tokens(&self.0, SyntaxKind::IDENT)
            .chain(support::tokens(&self.0, SyntaxKind::QUOTED_IDENT))
    }
}

// =============================================================================
// Binary Expression
// =============================================================================

/// A binary expression (lhs op rhs).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BinaryExpr(pub(crate) SyntaxNode);

impl AstNode for BinaryExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::BINARY_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// Binary operator kinds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    // String
    Concat,
    // Pattern matching
    Like,
    ILike,
    SimilarTo,
    RegexMatch,
    RegexMatchInsensitive,
    RegexNotMatch,
    RegexNotMatchInsensitive,
    // Other
    Unknown,
}

impl BinaryExpr {
    /// Returns the left-hand side expression.
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the right-hand side expression.
    pub fn rhs(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the operator token.
    pub fn op_token(&self) -> Option<SyntaxToken> {
        // Find the first operator token in the expression
        support::tokens(&self.0, SyntaxKind::EQ)
            .chain(support::tokens(&self.0, SyntaxKind::NE))
            .chain(support::tokens(&self.0, SyntaxKind::LT))
            .chain(support::tokens(&self.0, SyntaxKind::LE))
            .chain(support::tokens(&self.0, SyntaxKind::GT))
            .chain(support::tokens(&self.0, SyntaxKind::GE))
            .chain(support::tokens(&self.0, SyntaxKind::PLUS))
            .chain(support::tokens(&self.0, SyntaxKind::MINUS))
            .chain(support::tokens(&self.0, SyntaxKind::STAR))
            .chain(support::tokens(&self.0, SyntaxKind::SLASH))
            .chain(support::tokens(&self.0, SyntaxKind::PERCENT))
            .chain(support::tokens(&self.0, SyntaxKind::CARET))
            .chain(support::tokens(&self.0, SyntaxKind::PIPE_PIPE))
            .chain(support::tokens(&self.0, SyntaxKind::AND_KW))
            .chain(support::tokens(&self.0, SyntaxKind::OR_KW))
            .next()
    }

    /// Returns the operator kind.
    pub fn op(&self) -> BinaryOp {
        self.op_token()
            .map(|t| match t.kind() {
                SyntaxKind::EQ => BinaryOp::Eq,
                SyntaxKind::NE => BinaryOp::Ne,
                SyntaxKind::LT => BinaryOp::Lt,
                SyntaxKind::LE => BinaryOp::Le,
                SyntaxKind::GT => BinaryOp::Gt,
                SyntaxKind::GE => BinaryOp::Ge,
                SyntaxKind::AND_KW => BinaryOp::And,
                SyntaxKind::OR_KW => BinaryOp::Or,
                SyntaxKind::PLUS => BinaryOp::Add,
                SyntaxKind::MINUS => BinaryOp::Sub,
                SyntaxKind::STAR => BinaryOp::Mul,
                SyntaxKind::SLASH => BinaryOp::Div,
                SyntaxKind::PERCENT => BinaryOp::Mod,
                SyntaxKind::CARET => BinaryOp::Exp,
                SyntaxKind::PIPE_PIPE => BinaryOp::Concat,
                SyntaxKind::LIKE_KW => BinaryOp::Like,
                SyntaxKind::ILIKE_KW => BinaryOp::ILike,
                SyntaxKind::TILDE => BinaryOp::RegexMatch,
                SyntaxKind::TILDE_STAR => BinaryOp::RegexMatchInsensitive,
                SyntaxKind::BANG_TILDE => BinaryOp::RegexNotMatch,
                SyntaxKind::BANG_TILDE_STAR => BinaryOp::RegexNotMatchInsensitive,
                _ => BinaryOp::Unknown,
            })
            .unwrap_or(BinaryOp::Unknown)
    }
}

// =============================================================================
// Unary Expression
// =============================================================================

/// A unary expression (op expr).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnaryExpr(pub(crate) SyntaxNode);

impl AstNode for UnaryExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::UNARY_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// Unary operator kinds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    Neg,
    Pos,
    BitwiseNot,
    Unknown,
}

impl UnaryExpr {
    /// Returns the operand expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the operator kind.
    pub fn op(&self) -> UnaryOp {
        if support::token(&self.0, SyntaxKind::NOT_KW).is_some() {
            UnaryOp::Not
        } else if support::token(&self.0, SyntaxKind::MINUS).is_some() {
            UnaryOp::Neg
        } else if support::token(&self.0, SyntaxKind::PLUS).is_some() {
            UnaryOp::Pos
        } else if support::token(&self.0, SyntaxKind::TILDE).is_some() {
            UnaryOp::BitwiseNot
        } else {
            UnaryOp::Unknown
        }
    }
}

// =============================================================================
// Parenthesized Expression
// =============================================================================

/// A parenthesized expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ParenExpr(pub(crate) SyntaxNode);

impl AstNode for ParenExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::PAREN_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ParenExpr {
    /// Returns the inner expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

// =============================================================================
// Function Call
// =============================================================================

/// A function call expression.
///
/// Structure: `name([args]) [FILTER ...] [OVER ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FuncCall(pub(crate) SyntaxNode);

impl AstNode for FuncCall {
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

impl FuncCall {
    /// Returns the function name.
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::IDENT)
            .or_else(|| support::token(&self.0, SyntaxKind::QUOTED_IDENT))
    }

    /// Returns the argument list.
    pub fn arg_list(&self) -> Option<ArgList> {
        support::child(&self.0)
    }

    /// Returns the FILTER clause if present.
    pub fn filter_clause(&self) -> Option<FilterClause> {
        support::child(&self.0)
    }

    /// Returns the OVER clause if present (for window functions).
    pub fn over_clause(&self) -> Option<OverClause> {
        support::child(&self.0)
    }

    /// Returns true if DISTINCT is specified.
    pub fn is_distinct(&self) -> bool {
        support::token(&self.0, SyntaxKind::DISTINCT_KW).is_some()
    }
}

/// An argument list in a function call.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArgList(pub(crate) SyntaxNode);

impl AstNode for ArgList {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ARG_LIST
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ArgList {
    /// Returns an iterator over arguments.
    pub fn args(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}

/// A FILTER clause on an aggregate function.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FilterClause(pub(crate) SyntaxNode);

impl AstNode for FilterClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::FILTER_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl FilterClause {
    /// Returns the filter expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

/// An OVER clause for window functions.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OverClause(pub(crate) SyntaxNode);

impl AstNode for OverClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::OVER_CLAUSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl OverClause {
    /// Returns the window specification.
    pub fn window_spec(&self) -> Option<WindowSpec> {
        support::child(&self.0)
    }

    /// Returns the window name if referencing a named window.
    pub fn window_name(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::IDENT)
    }
}

/// A window specification.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WindowSpec(pub(crate) SyntaxNode);

impl AstNode for WindowSpec {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::WINDOW_SPEC
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

// =============================================================================
// CAST Expression
// =============================================================================

/// A CAST expression.
///
/// Structure: `CAST(expr AS type)` or `expr::type`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CastExpr(pub(crate) SyntaxNode);

impl AstNode for CastExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CAST_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl CastExpr {
    /// Returns the expression being cast.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the target type.
    pub fn type_name(&self) -> Option<TypeName> {
        support::child(&self.0)
    }
}

/// A type name in a CAST or column definition.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeName(pub(crate) SyntaxNode);

impl AstNode for TypeName {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::TYPE_NAME
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

// =============================================================================
// CASE Expression
// =============================================================================

/// A CASE expression.
///
/// Structure: `CASE [expr] WHEN ... THEN ... [ELSE ...] END`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CaseExpr(pub(crate) SyntaxNode);

impl AstNode for CaseExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CASE_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl CaseExpr {
    /// Returns the operand expression (for simple CASE).
    pub fn operand(&self) -> Option<Expr> {
        // The operand is the first expression directly under CASE (before any WHEN)
        support::child(&self.0)
    }

    /// Returns an iterator over WHEN clauses.
    pub fn whens(&self) -> impl Iterator<Item = CaseWhen> {
        support::children(&self.0)
    }

    /// Returns the ELSE clause if present.
    pub fn else_clause(&self) -> Option<CaseElse> {
        support::child(&self.0)
    }
}

/// A WHEN clause in a CASE expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CaseWhen(pub(crate) SyntaxNode);

impl AstNode for CaseWhen {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CASE_WHEN
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl CaseWhen {
    /// Returns the condition expression.
    pub fn condition(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the result expression.
    pub fn result(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }
}

/// An ELSE clause in a CASE expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CaseElse(pub(crate) SyntaxNode);

impl AstNode for CaseElse {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CASE_ELSE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl CaseElse {
    /// Returns the else expression.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

// =============================================================================
// BETWEEN Expression
// =============================================================================

/// A BETWEEN expression.
///
/// Structure: `expr [NOT] BETWEEN low AND high`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BetweenExpr(pub(crate) SyntaxNode);

impl AstNode for BetweenExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::BETWEEN_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl BetweenExpr {
    /// Returns the expression being tested.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the low bound.
    pub fn low(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the high bound.
    pub fn high(&self) -> Option<Expr> {
        support::nth_child(&self.0, 2)
    }

    /// Returns true if NOT BETWEEN.
    pub fn is_negated(&self) -> bool {
        support::token(&self.0, SyntaxKind::NOT_KW).is_some()
    }
}

// =============================================================================
// IN Expression
// =============================================================================

/// An IN expression.
///
/// Structure: `expr [NOT] IN (values | subquery)`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InExpr(pub(crate) SyntaxNode);

impl AstNode for InExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::IN_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl InExpr {
    /// Returns the expression being tested.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns an iterator over the value list.
    pub fn values(&self) -> impl Iterator<Item = Expr> {
        support::children::<Expr>(&self.0).skip(1)
    }

    /// Returns true if NOT IN.
    pub fn is_negated(&self) -> bool {
        support::token(&self.0, SyntaxKind::NOT_KW).is_some()
    }
}

// =============================================================================
// LIKE Expression
// =============================================================================

/// A LIKE expression.
///
/// Structure: `expr [NOT] LIKE pattern [ESCAPE escape]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LikeExpr(pub(crate) SyntaxNode);

impl AstNode for LikeExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LIKE_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl LikeExpr {
    /// Returns the expression being tested.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the pattern.
    pub fn pattern(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns true if NOT LIKE.
    pub fn is_negated(&self) -> bool {
        support::token(&self.0, SyntaxKind::NOT_KW).is_some()
    }

    /// Returns true if ILIKE (case-insensitive).
    pub fn is_ilike(&self) -> bool {
        support::token(&self.0, SyntaxKind::ILIKE_KW).is_some()
    }
}

// =============================================================================
// IS Expression
// =============================================================================

/// An IS expression.
///
/// Structure: `expr IS [NOT] NULL | TRUE | FALSE | UNKNOWN | DISTINCT FROM expr`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IsExpr(pub(crate) SyntaxNode);

impl AstNode for IsExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::IS_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl IsExpr {
    /// Returns the expression being tested.
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns true if IS NOT.
    pub fn is_negated(&self) -> bool {
        support::token(&self.0, SyntaxKind::NOT_KW).is_some()
    }

    /// Returns true if IS NULL.
    pub fn is_null(&self) -> bool {
        support::token(&self.0, SyntaxKind::NULL_KW).is_some()
    }

    /// Returns true if IS TRUE.
    pub fn is_true(&self) -> bool {
        support::token(&self.0, SyntaxKind::TRUE_KW).is_some()
    }

    /// Returns true if IS FALSE.
    pub fn is_false(&self) -> bool {
        support::token(&self.0, SyntaxKind::FALSE_KW).is_some()
    }
}

// =============================================================================
// EXISTS Expression
// =============================================================================

/// An EXISTS expression.
///
/// Structure: `EXISTS (subquery)`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExistsExpr(pub(crate) SyntaxNode);

impl AstNode for ExistsExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::EXISTS_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ExistsExpr {
    /// Returns the subquery.
    pub fn subquery(&self) -> Option<SubqueryExpr> {
        support::child(&self.0)
    }
}

// =============================================================================
// Subquery Expression
// =============================================================================

/// A subquery expression.
///
/// Structure: `(SELECT ...)`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SubqueryExpr(pub(crate) SyntaxNode);

impl AstNode for SubqueryExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SUBQUERY_EXPR || kind == SyntaxKind::SUBQUERY
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SubqueryExpr {
    /// Returns the select statement.
    pub fn query(&self) -> Option<super::statements::SelectStmt> {
        support::child(&self.0)
    }
}

// =============================================================================
// Array Expression
// =============================================================================

/// An ARRAY expression.
///
/// Structure: `ARRAY[expr, ...]` or `ARRAY(subquery)`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArrayExpr(pub(crate) SyntaxNode);

impl AstNode for ArrayExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ARRAY_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ArrayExpr {
    /// Returns an iterator over array elements.
    pub fn elements(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}

// =============================================================================
// ROW Expression
// =============================================================================

/// A ROW expression.
///
/// Structure: `ROW(expr, ...)` or `(expr, expr, ...)`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RowExpr(pub(crate) SyntaxNode);

impl AstNode for RowExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ROW_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl RowExpr {
    /// Returns an iterator over row elements.
    pub fn elements(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}

// =============================================================================
// COALESCE, NULLIF, GREATEST, LEAST
// =============================================================================

/// A COALESCE expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CoalesceExpr(pub(crate) SyntaxNode);

impl AstNode for CoalesceExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::COALESCE_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl CoalesceExpr {
    /// Returns an iterator over arguments.
    pub fn args(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}

/// A NULLIF expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NullIfExpr(pub(crate) SyntaxNode);

impl AstNode for NullIfExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::NULLIF_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl NullIfExpr {
    /// Returns the first argument.
    pub fn first(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the second argument.
    pub fn second(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }
}

/// A GREATEST expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GreatestExpr(pub(crate) SyntaxNode);

impl AstNode for GreatestExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::GREATEST_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl GreatestExpr {
    /// Returns an iterator over arguments.
    pub fn args(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}

/// A LEAST expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LeastExpr(pub(crate) SyntaxNode);

impl AstNode for LeastExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LEAST_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl LeastExpr {
    /// Returns an iterator over arguments.
    pub fn args(&self) -> impl Iterator<Item = Expr> {
        support::children(&self.0)
    }
}
