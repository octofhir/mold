//! JSONPath AST nodes.
//!
//! This module provides typed wrappers for SQL/JSON path expression nodes,
//! supporting the PostgreSQL 12+ JSONPath syntax used with `@?` and `@@` operators.
//!
//! JSONPath syntax: `'$[.member][filter][method()]...'`

use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

use super::super::support;
use super::super::traits::AstNode;

/// A complete JSONPath expression.
///
/// Structure: `[mode] $ path_elements...`
///
/// Example: `'strict $.store.book[*].author'`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JsonPath(pub(crate) SyntaxNode);

impl AstNode for JsonPath {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JSONPATH_LITERAL || kind == SyntaxKind::JSONPATH_CONTENT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JsonPath {
    /// Returns the mode if specified (strict or lax).
    pub fn mode(&self) -> Option<JpMode> {
        support::child(&self.0)
    }

    /// Returns the root element ($).
    pub fn root(&self) -> Option<JpRoot> {
        support::child(&self.0)
    }

    /// Returns an iterator over path elements.
    pub fn elements(&self) -> impl Iterator<Item = JpElement> {
        support::children(&self.0)
    }
}

/// JSONPath mode (strict or lax).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpMode(pub(crate) SyntaxNode);

impl AstNode for JpMode {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_MODE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The mode type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JpModeKind {
    Strict,
    Lax,
}

impl JpMode {
    /// Returns the mode kind.
    pub fn kind(&self) -> Option<JpModeKind> {
        if support::token(&self.0, SyntaxKind::JP_STRICT_KW).is_some() {
            Some(JpModeKind::Strict)
        } else if support::token(&self.0, SyntaxKind::JP_LAX_KW).is_some() {
            Some(JpModeKind::Lax)
        } else {
            None
        }
    }
}

/// Enum representing all JSONPath element types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum JpElement {
    Root(JpRoot),
    Current(JpCurrent),
    MemberAccess(JpMemberAccess),
    ArrayAccess(JpArrayAccess),
    Wildcard(JpWildcard),
    RecursiveDescent(JpRecursiveDescent),
    Filter(JpFilter),
    Method(JpMethod),
}

impl AstNode for JpElement {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::JP_ROOT
                | SyntaxKind::JP_CURRENT
                | SyntaxKind::JP_MEMBER_ACCESS
                | SyntaxKind::JP_ARRAY_ACCESS
                | SyntaxKind::JP_MEMBER_WILDCARD
                | SyntaxKind::JP_ARRAY_WILDCARD
                | SyntaxKind::JP_RECURSIVE_DESCENT
                | SyntaxKind::JP_FILTER
                | SyntaxKind::JP_METHOD
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::JP_ROOT => Some(JpElement::Root(JpRoot(node))),
            SyntaxKind::JP_CURRENT => Some(JpElement::Current(JpCurrent(node))),
            SyntaxKind::JP_MEMBER_ACCESS => Some(JpElement::MemberAccess(JpMemberAccess(node))),
            SyntaxKind::JP_ARRAY_ACCESS => Some(JpElement::ArrayAccess(JpArrayAccess(node))),
            SyntaxKind::JP_MEMBER_WILDCARD | SyntaxKind::JP_ARRAY_WILDCARD => {
                Some(JpElement::Wildcard(JpWildcard(node)))
            }
            SyntaxKind::JP_RECURSIVE_DESCENT => {
                Some(JpElement::RecursiveDescent(JpRecursiveDescent(node)))
            }
            SyntaxKind::JP_FILTER => Some(JpElement::Filter(JpFilter(node))),
            SyntaxKind::JP_METHOD => Some(JpElement::Method(JpMethod(node))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            JpElement::Root(it) => it.syntax(),
            JpElement::Current(it) => it.syntax(),
            JpElement::MemberAccess(it) => it.syntax(),
            JpElement::ArrayAccess(it) => it.syntax(),
            JpElement::Wildcard(it) => it.syntax(),
            JpElement::RecursiveDescent(it) => it.syntax(),
            JpElement::Filter(it) => it.syntax(),
            JpElement::Method(it) => it.syntax(),
        }
    }
}

// =============================================================================
// Root ($)
// =============================================================================

/// The JSONPath root element (`$`).
///
/// Represents the root of the JSON document being queried.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpRoot(pub(crate) SyntaxNode);

impl AstNode for JpRoot {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_ROOT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

// =============================================================================
// Current (@)
// =============================================================================

/// The JSONPath current item element (`@`).
///
/// Represents the current item in a filter expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpCurrent(pub(crate) SyntaxNode);

impl AstNode for JpCurrent {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_CURRENT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

// =============================================================================
// Member Access (.key or ."key")
// =============================================================================

/// A JSONPath member access element.
///
/// Structure: `.key` or `."quoted key"`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpMemberAccess(pub(crate) SyntaxNode);

impl AstNode for JpMemberAccess {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_MEMBER_ACCESS
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JpMemberAccess {
    /// Returns the key being accessed.
    pub fn key(&self) -> Option<JpKey> {
        support::child(&self.0)
    }

    /// Returns the key name as a string.
    pub fn key_name(&self) -> Option<String> {
        self.key().and_then(|k| k.name())
    }
}

/// A JSONPath key (member name).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpKey(pub(crate) SyntaxNode);

impl AstNode for JpKey {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_KEY
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JpKey {
    /// Returns the key name.
    pub fn name(&self) -> Option<String> {
        support::first_token(&self.0).map(|t| t.text().to_string())
    }

    /// Returns true if this is a quoted key.
    pub fn is_quoted(&self) -> bool {
        support::token(&self.0, SyntaxKind::JP_STRING_LIT).is_some()
    }
}

// =============================================================================
// Array Access ([index] or [start:end])
// =============================================================================

/// A JSONPath array access element.
///
/// Structure: `[index]` or `[start:end]` or `[*]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpArrayAccess(pub(crate) SyntaxNode);

impl AstNode for JpArrayAccess {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_ARRAY_ACCESS || kind == SyntaxKind::JP_ARRAY_SLICE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JpArrayAccess {
    /// Returns the index expression.
    pub fn index(&self) -> Option<JpExpr> {
        support::child(&self.0)
    }

    /// Returns true if this is an array slice.
    pub fn is_slice(&self) -> bool {
        self.0.kind() == SyntaxKind::JP_ARRAY_SLICE
            || support::token(&self.0, SyntaxKind::JP_COLON).is_some()
    }

    /// Returns the start index for slices.
    pub fn start(&self) -> Option<JpExpr> {
        support::child(&self.0)
    }

    /// Returns the end index for slices.
    pub fn end(&self) -> Option<JpExpr> {
        support::nth_child(&self.0, 1)
    }
}

// =============================================================================
// Wildcard (* or [*])
// =============================================================================

/// A JSONPath wildcard element.
///
/// Structure: `.*` (member wildcard) or `[*]` (array wildcard)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpWildcard(pub(crate) SyntaxNode);

impl AstNode for JpWildcard {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_MEMBER_WILDCARD || kind == SyntaxKind::JP_ARRAY_WILDCARD
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JpWildcard {
    /// Returns true if this is a member wildcard (`.*`).
    pub fn is_member(&self) -> bool {
        self.0.kind() == SyntaxKind::JP_MEMBER_WILDCARD
    }

    /// Returns true if this is an array wildcard (`[*]`).
    pub fn is_array(&self) -> bool {
        self.0.kind() == SyntaxKind::JP_ARRAY_WILDCARD
    }
}

// =============================================================================
// Recursive Descent (..)
// =============================================================================

/// A JSONPath recursive descent element.
///
/// Structure: `..` or `..*`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpRecursiveDescent(pub(crate) SyntaxNode);

impl AstNode for JpRecursiveDescent {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_RECURSIVE_DESCENT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

// =============================================================================
// Filter (?(...))
// =============================================================================

/// A JSONPath filter element.
///
/// Structure: `?( condition )`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpFilter(pub(crate) SyntaxNode);

impl AstNode for JpFilter {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_FILTER
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JpFilter {
    /// Returns the filter condition expression.
    pub fn condition(&self) -> Option<JpExpr> {
        support::child(&self.0)
    }
}

// =============================================================================
// Method (.method())
// =============================================================================

/// A JSONPath method call.
///
/// Structure: `.method()` or `.method(args)`
///
/// Common methods: `type()`, `size()`, `double()`, `ceiling()`, `floor()`,
/// `abs()`, `keyvalue()`, `datetime()`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpMethod(pub(crate) SyntaxNode);

impl AstNode for JpMethod {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_METHOD
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// Known JSONPath method names.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JpMethodKind {
    Type,
    Size,
    Double,
    Ceiling,
    Floor,
    Abs,
    Keyvalue,
    Datetime,
    Unknown,
}

impl JpMethod {
    /// Returns the method name token.
    pub fn name_token(&self) -> Option<SyntaxToken> {
        support::first_token(&self.0)
    }

    /// Returns the method name as a string.
    pub fn name(&self) -> Option<String> {
        self.name_token().map(|t| t.text().to_string())
    }

    /// Returns the method kind.
    pub fn kind(&self) -> JpMethodKind {
        if support::token(&self.0, SyntaxKind::JP_TYPE_KW).is_some() {
            JpMethodKind::Type
        } else if support::token(&self.0, SyntaxKind::JP_SIZE_KW).is_some() {
            JpMethodKind::Size
        } else if support::token(&self.0, SyntaxKind::JP_DOUBLE_KW).is_some() {
            JpMethodKind::Double
        } else if support::token(&self.0, SyntaxKind::JP_CEILING_KW).is_some() {
            JpMethodKind::Ceiling
        } else if support::token(&self.0, SyntaxKind::JP_FLOOR_KW).is_some() {
            JpMethodKind::Floor
        } else if support::token(&self.0, SyntaxKind::JP_ABS_KW).is_some() {
            JpMethodKind::Abs
        } else if support::token(&self.0, SyntaxKind::JP_KEYVALUE_KW).is_some() {
            JpMethodKind::Keyvalue
        } else if support::token(&self.0, SyntaxKind::JP_DATETIME_KW).is_some() {
            JpMethodKind::Datetime
        } else {
            JpMethodKind::Unknown
        }
    }

    /// Returns an iterator over method arguments.
    pub fn args(&self) -> impl Iterator<Item = JpExpr> {
        support::children(&self.0)
    }
}

// =============================================================================
// JSONPath Expressions (for filter conditions)
// =============================================================================

/// A JSONPath expression (used in filters).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum JpExpr {
    Comparison(JpComparison),
    LogicalAnd(JpLogicalAnd),
    LogicalOr(JpLogicalOr),
    LogicalNot(JpLogicalNot),
    Exists(JpExists),
    Arithmetic(JpArithmetic),
    Paren(JpParen),
    Literal(JpLiteral),
    Path(JsonPath),
}

impl AstNode for JpExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::JP_COMPARISON
                | SyntaxKind::JP_LOGICAL_AND
                | SyntaxKind::JP_LOGICAL_OR
                | SyntaxKind::JP_LOGICAL_NOT
                | SyntaxKind::JP_EXISTS
                | SyntaxKind::JP_ARITHMETIC
                | SyntaxKind::JP_PAREN
                | SyntaxKind::JP_STRING
                | SyntaxKind::JP_NUMBER
                | SyntaxKind::JP_BOOL
                | SyntaxKind::JP_NULL
                | SyntaxKind::JSONPATH_CONTENT
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::JP_COMPARISON => Some(JpExpr::Comparison(JpComparison(node))),
            SyntaxKind::JP_LOGICAL_AND => Some(JpExpr::LogicalAnd(JpLogicalAnd(node))),
            SyntaxKind::JP_LOGICAL_OR => Some(JpExpr::LogicalOr(JpLogicalOr(node))),
            SyntaxKind::JP_LOGICAL_NOT => Some(JpExpr::LogicalNot(JpLogicalNot(node))),
            SyntaxKind::JP_EXISTS => Some(JpExpr::Exists(JpExists(node))),
            SyntaxKind::JP_ARITHMETIC => Some(JpExpr::Arithmetic(JpArithmetic(node))),
            SyntaxKind::JP_PAREN => Some(JpExpr::Paren(JpParen(node))),
            SyntaxKind::JP_STRING | SyntaxKind::JP_NUMBER | SyntaxKind::JP_BOOL | SyntaxKind::JP_NULL => {
                Some(JpExpr::Literal(JpLiteral(node)))
            }
            SyntaxKind::JSONPATH_CONTENT => Some(JpExpr::Path(JsonPath(node))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            JpExpr::Comparison(it) => it.syntax(),
            JpExpr::LogicalAnd(it) => it.syntax(),
            JpExpr::LogicalOr(it) => it.syntax(),
            JpExpr::LogicalNot(it) => it.syntax(),
            JpExpr::Exists(it) => it.syntax(),
            JpExpr::Arithmetic(it) => it.syntax(),
            JpExpr::Paren(it) => it.syntax(),
            JpExpr::Literal(it) => it.syntax(),
            JpExpr::Path(it) => it.syntax(),
        }
    }
}

/// A JSONPath comparison expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpComparison(pub(crate) SyntaxNode);

impl AstNode for JpComparison {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_COMPARISON
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// A JSONPath logical AND expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpLogicalAnd(pub(crate) SyntaxNode);

impl AstNode for JpLogicalAnd {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_LOGICAL_AND
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// A JSONPath logical OR expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpLogicalOr(pub(crate) SyntaxNode);

impl AstNode for JpLogicalOr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_LOGICAL_OR
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// A JSONPath logical NOT expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpLogicalNot(pub(crate) SyntaxNode);

impl AstNode for JpLogicalNot {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_LOGICAL_NOT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// A JSONPath exists expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpExists(pub(crate) SyntaxNode);

impl AstNode for JpExists {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_EXISTS
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// A JSONPath arithmetic expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpArithmetic(pub(crate) SyntaxNode);

impl AstNode for JpArithmetic {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_ARITHMETIC
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// A JSONPath parenthesized expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpParen(pub(crate) SyntaxNode);

impl AstNode for JpParen {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::JP_PAREN
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl JpParen {
    /// Returns the inner expression.
    pub fn expr(&self) -> Option<JpExpr> {
        support::child(&self.0)
    }
}

/// A JSONPath literal value.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JpLiteral(pub(crate) SyntaxNode);

impl AstNode for JpLiteral {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::JP_STRING | SyntaxKind::JP_NUMBER | SyntaxKind::JP_BOOL | SyntaxKind::JP_NULL
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

/// The kind of JSONPath literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum JpLiteralKind {
    String(String),
    Number,
    Boolean(bool),
    Null,
}

impl JpLiteral {
    /// Returns the literal kind.
    pub fn kind(&self) -> Option<JpLiteralKind> {
        match self.0.kind() {
            SyntaxKind::JP_STRING => support::first_token(&self.0)
                .map(|t| JpLiteralKind::String(t.text().to_string())),
            SyntaxKind::JP_NUMBER => Some(JpLiteralKind::Number),
            SyntaxKind::JP_BOOL => {
                if support::token(&self.0, SyntaxKind::JP_TRUE_KW).is_some() {
                    Some(JpLiteralKind::Boolean(true))
                } else {
                    Some(JpLiteralKind::Boolean(false))
                }
            }
            SyntaxKind::JP_NULL => Some(JpLiteralKind::Null),
            _ => None,
        }
    }

    /// Returns the literal text.
    pub fn text(&self) -> Option<String> {
        support::first_token(&self.0).map(|t| t.text().to_string())
    }
}
