//! JSONB expression AST nodes.
//!
//! This module provides typed wrappers for PostgreSQL JSONB expression nodes,
//! supporting the full range of JSONB operators like `->`, `->>`, `@>`, `?`, etc.

use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

use super::super::support;
use super::super::traits::AstNode;
use super::expressions::Expr;

/// Enum representing all JSONB expression types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum JsonbExpr {
    Access(JsonbAccessExpr),
    Path(JsonbPathExpr),
    Contains(JsonbContainsExpr),
    Exists(JsonbExistsExpr),
}

impl AstNode for JsonbExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::JSONB_ACCESS_EXPR
                | SyntaxKind::JSONB_PATH_EXPR
                | SyntaxKind::JSONB_CONTAINS_EXPR
                | SyntaxKind::JSONB_EXISTS_EXPR
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::JSONB_ACCESS_EXPR => Some(JsonbExpr::Access(JsonbAccessExpr(node))),
            SyntaxKind::JSONB_PATH_EXPR => Some(JsonbExpr::Path(JsonbPathExpr(node))),
            SyntaxKind::JSONB_CONTAINS_EXPR => Some(JsonbExpr::Contains(JsonbContainsExpr(node))),
            SyntaxKind::JSONB_EXISTS_EXPR => Some(JsonbExpr::Exists(JsonbExistsExpr(node))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            JsonbExpr::Access(it) => it.syntax(),
            JsonbExpr::Path(it) => it.syntax(),
            JsonbExpr::Contains(it) => it.syntax(),
            JsonbExpr::Exists(it) => it.syntax(),
        }
    }
}

// =============================================================================
// JSONB Access Expression (-> and ->>)
// =============================================================================

/// A JSONB access expression using `->` or `->>` operators.
///
/// - `->` extracts a JSON value (returns jsonb)
/// - `->>` extracts a JSON value as text (returns text)
///
/// Structure: `expr -> key` or `expr ->> key`
///
/// The key can be:
/// - A string literal (field access): `data -> 'field'`
/// - An integer literal (array access): `data -> 0`
/// - An expression: `data -> idx`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JsonbAccessExpr(pub(crate) SyntaxNode);

impl AstNode for JsonbAccessExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JSONB_ACCESS_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The type of JSONB access operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsonbAccessOp {
    /// `->` - Extract as JSON/JSONB
    Arrow,
    /// `->>` - Extract as text
    ArrowText,
}

impl JsonbAccessExpr {
    /// Returns the base expression (left side of the operator).
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the accessor expression (right side of the operator).
    pub fn accessor(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the operator used.
    pub fn operator(&self) -> Option<JsonbAccessOp> {
        if support::token(&self.0, SyntaxKind::ARROW_TEXT).is_some() {
            Some(JsonbAccessOp::ArrowText)
        } else if support::token(&self.0, SyntaxKind::ARROW).is_some() {
            Some(JsonbAccessOp::Arrow)
        } else {
            None
        }
    }

    /// Returns the operator token.
    pub fn operator_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::ARROW_TEXT)
            .or_else(|| support::token(&self.0, SyntaxKind::ARROW))
    }

    /// Returns true if this operator extracts text (`->>`) rather than JSON (`->`).
    ///
    /// This is crucial for type analysis:
    /// - `data -> 'key'` returns `jsonb`
    /// - `data ->> 'key'` returns `text`
    pub fn extracts_text(&self) -> bool {
        matches!(self.operator(), Some(JsonbAccessOp::ArrowText))
    }
}

// =============================================================================
// JSONB Path Expression (#> and #>>)
// =============================================================================

/// A JSONB path expression using `#>` or `#>>` operators.
///
/// - `#>` extracts a JSON value at a path (returns jsonb)
/// - `#>>` extracts a JSON value at a path as text (returns text)
///
/// Structure: `expr #> path` or `expr #>> path`
///
/// The path is typically an array literal: `data #> '{a,b,c}'`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JsonbPathExpr(pub(crate) SyntaxNode);

impl AstNode for JsonbPathExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JSONB_PATH_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The type of JSONB path operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsonbPathOp {
    /// `#>` - Path extract as JSON/JSONB
    HashArrow,
    /// `#>>` - Path extract as text
    HashArrowText,
}

impl JsonbPathExpr {
    /// Returns the base expression (left side of the operator).
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the path expression (right side of the operator).
    pub fn path(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the operator used.
    pub fn operator(&self) -> Option<JsonbPathOp> {
        if support::token(&self.0, SyntaxKind::HASH_ARROW_TEXT).is_some() {
            Some(JsonbPathOp::HashArrowText)
        } else if support::token(&self.0, SyntaxKind::HASH_ARROW).is_some() {
            Some(JsonbPathOp::HashArrow)
        } else {
            None
        }
    }

    /// Returns the operator token.
    pub fn operator_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::HASH_ARROW_TEXT)
            .or_else(|| support::token(&self.0, SyntaxKind::HASH_ARROW))
    }

    /// Returns true if this operator extracts text (`#>>`) rather than JSON (`#>`).
    pub fn extracts_text(&self) -> bool {
        matches!(self.operator(), Some(JsonbPathOp::HashArrowText))
    }
}

// =============================================================================
// JSONB Contains Expression (@> and <@)
// =============================================================================

/// A JSONB containment expression using `@>` or `<@` operators.
///
/// - `@>` - left contains right
/// - `<@` - left is contained by right
///
/// Structure: `expr @> expr` or `expr <@ expr`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JsonbContainsExpr(pub(crate) SyntaxNode);

impl AstNode for JsonbContainsExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JSONB_CONTAINS_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The type of JSONB containment operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsonbContainsOp {
    /// `@>` - Contains
    Contains,
    /// `<@` - Contained by
    ContainedBy,
}

impl JsonbContainsExpr {
    /// Returns the left-hand side expression.
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the right-hand side expression.
    pub fn rhs(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the operator used.
    pub fn operator(&self) -> Option<JsonbContainsOp> {
        if support::token(&self.0, SyntaxKind::AT_GT).is_some() {
            Some(JsonbContainsOp::Contains)
        } else if support::token(&self.0, SyntaxKind::LT_AT).is_some() {
            Some(JsonbContainsOp::ContainedBy)
        } else {
            None
        }
    }

    /// Returns the operator token.
    pub fn operator_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::AT_GT)
            .or_else(|| support::token(&self.0, SyntaxKind::LT_AT))
    }
}

// =============================================================================
// JSONB Exists Expression (?, ?|, ?&)
// =============================================================================

/// A JSONB key existence expression using `?`, `?|`, or `?&` operators.
///
/// - `?` - Does the key exist?
/// - `?|` - Do any of these keys exist?
/// - `?&` - Do all of these keys exist?
///
/// Structure: `expr ? key` or `expr ?| array` or `expr ?& array`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JsonbExistsExpr(pub(crate) SyntaxNode);

impl AstNode for JsonbExistsExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JSONB_EXISTS_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The type of JSONB exists operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsonbExistsOp {
    /// `?` - Key exists
    Exists,
    /// `?|` - Any key exists
    AnyExists,
    /// `?&` - All keys exist
    AllExist,
}

impl JsonbExistsExpr {
    /// Returns the base JSONB expression.
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the key or keys expression.
    pub fn keys(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the operator used.
    pub fn operator(&self) -> Option<JsonbExistsOp> {
        if support::token(&self.0, SyntaxKind::QUESTION_AMP).is_some() {
            Some(JsonbExistsOp::AllExist)
        } else if support::token(&self.0, SyntaxKind::QUESTION_PIPE).is_some() {
            Some(JsonbExistsOp::AnyExists)
        } else if support::token(&self.0, SyntaxKind::QUESTION).is_some() {
            Some(JsonbExistsOp::Exists)
        } else {
            None
        }
    }

    /// Returns the operator token.
    pub fn operator_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::QUESTION_AMP)
            .or_else(|| support::token(&self.0, SyntaxKind::QUESTION_PIPE))
            .or_else(|| support::token(&self.0, SyntaxKind::QUESTION))
    }
}

// =============================================================================
// JSONPath Expression (PostgreSQL 12+ SQL/JSON path)
// =============================================================================

/// A JSONPath expression using `@?` or `@@` operators.
///
/// - `@?` - Does the JSONPath return any items?
/// - `@@` - Returns the result of JSONPath predicate check
///
/// These operators work with SQL/JSON path expressions (PostgreSQL 12+).
///
/// Structure: `expr @? jsonpath` or `expr @@ jsonpath`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JsonPathExpr(pub(crate) SyntaxNode);

impl AstNode for JsonPathExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        // Note: This might be captured as JSONB_PATH_EXPR in some cases
        kind == SyntaxKind::JSONB_PATH_EXPR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The type of JSONPath operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsonPathOp {
    /// `@?` - JSONPath exists
    Exists,
    /// `@@` - JSONPath match
    Match,
}

impl JsonPathExpr {
    /// Returns the base expression.
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    /// Returns the JSONPath expression.
    pub fn path(&self) -> Option<Expr> {
        support::nth_child(&self.0, 1)
    }

    /// Returns the operator used.
    pub fn operator(&self) -> Option<JsonPathOp> {
        if support::token(&self.0, SyntaxKind::AT_QUESTION).is_some() {
            Some(JsonPathOp::Exists)
        } else if support::token(&self.0, SyntaxKind::AT_AT).is_some() {
            Some(JsonPathOp::Match)
        } else {
            None
        }
    }

    /// Returns the operator token.
    pub fn operator_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, SyntaxKind::AT_QUESTION)
            .or_else(|| support::token(&self.0, SyntaxKind::AT_AT))
    }
}
