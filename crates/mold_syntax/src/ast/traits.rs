//! Core traits for AST projection over the untyped CST.
//!
//! The AST layer provides typed wrappers around CST nodes, enabling convenient
//! tree traversal and analysis while maintaining zero-cost abstraction.

use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

/// Trait for typed AST nodes that wrap CST `SyntaxNode` instances.
///
/// Implementors are newtype wrappers around `SyntaxNode` that provide
/// type-safe access to specific node kinds and their children.
///
/// # Example
///
/// ```ignore
/// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// pub struct SelectStmt(SyntaxNode);
///
/// impl AstNode for SelectStmt {
///     fn can_cast(kind: SyntaxKind) -> bool {
///         kind == SyntaxKind::SELECT_STMT
///     }
///
///     fn cast(node: SyntaxNode) -> Option<Self> {
///         Self::can_cast(node.kind()).then(|| Self(node))
///     }
///
///     fn syntax(&self) -> &SyntaxNode {
///         &self.0
///     }
/// }
/// ```
pub trait AstNode: Clone {
    /// Returns `true` if a node of the given kind can be cast to this type.
    fn can_cast(kind: SyntaxKind) -> bool;

    /// Attempts to cast a generic `SyntaxNode` to this specific AST node type.
    /// Returns `None` if the node's kind doesn't match.
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Returns a reference to the underlying `SyntaxNode`.
    fn syntax(&self) -> &SyntaxNode;
}

/// Trait for typed AST tokens that wrap CST `SyntaxToken` instances.
///
/// Implementors are newtype wrappers around `SyntaxToken` that provide
/// type-safe access to specific token kinds and their text.
pub trait AstToken: Clone {
    /// Returns `true` if a token of the given kind can be cast to this type.
    fn can_cast(kind: SyntaxKind) -> bool;

    /// Attempts to cast a generic `SyntaxToken` to this specific AST token type.
    /// Returns `None` if the token's kind doesn't match.
    fn cast(token: SyntaxToken) -> Option<Self>;

    /// Returns a reference to the underlying `SyntaxToken`.
    fn syntax(&self) -> &SyntaxToken;

    /// Returns the text of this token.
    fn text(&self) -> &str {
        self.syntax().text()
    }
}
