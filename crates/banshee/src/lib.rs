//! Banshee: Pure Rust PostgreSQL Parser for LSP
//!
//! A lossless CST parser with strong error recovery, JSONB/JSONPath support,
//! and sqlstyle.guide formatting.
//!
//! # Usage
//!
//! ```ignore
//! let parse = banshee::parser::parse("SELECT * FROM users");
//! assert!(parse.errors().is_empty());
//! let formatted = banshee_format::format_sqlstyle("select * from users");
//! println!("{formatted}");
//! ```

pub use banshee_syntax::{Parse, ParseError, SyntaxKind, SyntaxNode, SyntaxToken, Token};

pub mod lexer {
    pub use banshee_lexer::tokenize;
}

pub mod parser {
    pub use banshee_parser::parse;
}
