//! Support functions for AST node traversal.
//!
//! These functions provide convenient access to child nodes and tokens
//! within the CST, filtering by type through the `AstNode` trait.

use crate::{SyntaxKind, SyntaxNode, SyntaxToken};
use cstree::util::NodeOrToken;

use super::traits::AstNode;

/// Returns the first child node of type `N` from the parent.
///
/// Iterates through all children of `parent` and returns the first
/// one that can be successfully cast to type `N`.
///
/// # Example
///
/// ```ignore
/// let from_clause: Option<FromClause> = support::child(&select_stmt.syntax());
/// ```
pub fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
    parent.children().filter_map(|n| N::cast(n.clone())).next()
}

/// Returns an iterator over all child nodes of type `N` from the parent.
///
/// Iterates through all children of `parent` and yields those that
/// can be successfully cast to type `N`.
///
/// # Example
///
/// ```ignore
/// for stmt in support::children::<Stmt>(&source_file.syntax()) {
///     // process each statement
/// }
/// ```
pub fn children<N: AstNode>(parent: &SyntaxNode) -> impl Iterator<Item = N> + '_ {
    parent.children().filter_map(|n| N::cast(n.clone()))
}

/// Returns the first token with the specified kind from the parent node.
///
/// Searches through all direct child tokens (not descendants) and returns
/// the first one matching the given `kind`.
///
/// # Example
///
/// ```ignore
/// let select_kw = support::token(&node, SyntaxKind::SELECT_KW);
/// ```
pub fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    parent.children_with_tokens().find_map(|it| match it {
        NodeOrToken::Token(token) if token.kind() == kind => Some(token.clone()),
        _ => None,
    })
}

/// Returns an iterator over all tokens with the specified kind from the parent node.
///
/// Searches through all direct child tokens (not descendants) and yields
/// those matching the given `kind`.
///
/// # Example
///
/// ```ignore
/// for comma in support::tokens(&node, SyntaxKind::COMMA) {
///     // process each comma
/// }
/// ```
pub fn tokens(parent: &SyntaxNode, kind: SyntaxKind) -> impl Iterator<Item = SyntaxToken> + '_ {
    parent
        .children_with_tokens()
        .filter_map(move |it| match it {
            NodeOrToken::Token(token) if token.kind() == kind => Some(token.clone()),
            _ => None,
        })
}

/// Returns the nth child node of type `N` from the parent (0-indexed).
///
/// Useful when a node type can appear multiple times and you need
/// a specific occurrence.
///
/// # Example
///
/// ```ignore
/// // Get the second expression in a binary expression
/// let rhs: Option<Expr> = support::nth_child(&binary_expr.syntax(), 1);
/// ```
pub fn nth_child<N: AstNode>(parent: &SyntaxNode, n: usize) -> Option<N> {
    parent.children().filter_map(|c| N::cast(c.clone())).nth(n)
}

/// Returns the first token from the parent node, regardless of kind.
///
/// Useful for getting the leading token of a node.
pub fn first_token(parent: &SyntaxNode) -> Option<SyntaxToken> {
    parent.children_with_tokens().find_map(|it| match it {
        NodeOrToken::Token(token) => Some(token.clone()),
        _ => None,
    })
}

/// Returns the last token from the parent node, regardless of kind.
///
/// Useful for getting the trailing token of a node.
pub fn last_token(parent: &SyntaxNode) -> Option<SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|it| match it {
            NodeOrToken::Token(token) => Some(token.clone()),
            _ => None,
        })
        .last()
}
