mod syntax_kind;
pub mod ast;

pub use syntax_kind::{SyntaxKind, keyword_from_str};

use cstree::{
    green::GreenNode,
    interning::TokenInterner,
    syntax::{ResolvedNode, ResolvedToken},
};
use std::sync::Arc;
use text_size::{TextRange, TextSize};

pub type SyntaxNode = ResolvedNode<SyntaxKind>;
pub type SyntaxToken = ResolvedToken<SyntaxKind>;

#[derive(Clone)]
pub struct Parse {
    green: GreenNode,
    errors: Vec<ParseError>,
    interner: Arc<TokenInterner>,
}

impl Parse {
    pub fn new(green: GreenNode, errors: Vec<ParseError>, interner: Arc<TokenInterner>) -> Self {
        Self {
            green,
            errors,
            interner,
        }
    }

    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root_with_resolver(self.green.clone(), self.interner.clone())
    }

    pub fn green(&self) -> &GreenNode {
        &self.green
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    pub fn interner(&self) -> &Arc<TokenInterner> {
        &self.interner
    }

    pub fn ok(self) -> Result<Parse, Vec<ParseError>> {
        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(self.errors)
        }
    }
}

impl std::fmt::Debug for Parse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parse")
            .field("errors", &self.errors)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub range: TextRange,
}

impl ParseError {
    pub fn new(message: impl Into<String>, range: TextRange) -> Self {
        Self {
            message: message.into(),
            range,
        }
    }

    pub fn at_offset(message: impl Into<String>, offset: TextSize) -> Self {
        Self::new(message, TextRange::empty(offset))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "error at {}..{}: {}",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            self.message
        )
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub len: TextSize,
}

impl Token {
    pub fn new(kind: SyntaxKind, len: TextSize) -> Self {
        Self { kind, len }
    }
}
