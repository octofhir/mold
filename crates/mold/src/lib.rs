//! Mold: Pure Rust PostgreSQL Parser for LSP
//!
//! A lossless CST parser with strong error recovery, JSONB/JSONPath support,
//! and sqlstyle.guide formatting.

pub use mold_syntax::{Parse, ParseError, SyntaxKind, SyntaxNode, SyntaxToken, Token};

pub mod lexer {
    pub use mold_lexer::tokenize;
}

pub mod parser {
    pub use mold_parser::parse;
}
