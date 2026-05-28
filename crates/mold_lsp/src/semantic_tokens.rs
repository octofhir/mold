//! Semantic highlighting (`textDocument/semanticTokens/full`).
//!
//! Classifies each non-trivia token by [`SyntaxKind`] and emits LSP semantic
//! tokens in the spec's delta encoding. Comments are emitted too (they carry
//! position info editors use for the comment scope).

use lsp_types::{SemanticToken, SemanticTokenType, SemanticTokens};
use mold_syntax::{SyntaxKind, SyntaxNode};

use crate::convert::LineIndex;

/// Token-type legend; the index into this array is what tokens reference.
pub const LEGEND: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,  // 0
    SemanticTokenType::OPERATOR, // 1
    SemanticTokenType::STRING,   // 2
    SemanticTokenType::NUMBER,   // 3
    SemanticTokenType::COMMENT,  // 4
    SemanticTokenType::VARIABLE, // 5
];

const KEYWORD: u32 = 0;
const OPERATOR: u32 = 1;
const STRING: u32 = 2;
const NUMBER: u32 = 3;
const COMMENT: u32 = 4;
const VARIABLE: u32 = 5;

fn token_type(kind: SyntaxKind) -> Option<u32> {
    if kind.is_keyword() {
        return Some(KEYWORD);
    }
    match kind {
        SyntaxKind::LINE_COMMENT | SyntaxKind::BLOCK_COMMENT => Some(COMMENT),
        SyntaxKind::STRING
        | SyntaxKind::DOLLAR_STRING
        | SyntaxKind::BIT_STRING
        | SyntaxKind::HEX_STRING => Some(STRING),
        SyntaxKind::INTEGER | SyntaxKind::FLOAT => Some(NUMBER),
        SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT => Some(VARIABLE),
        SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE => None,
        // Punctuation/operators: anything else that is a single token with
        // visible glyphs. Identifiers and literals are handled above; the rest
        // of the visible tokens read as operators/punctuation.
        _ => Some(OPERATOR),
    }
}

/// Builds delta-encoded semantic tokens for the whole document.
pub fn semantic_tokens(root: &SyntaxNode, index: &LineIndex) -> SemanticTokens {
    let mut data: Vec<SemanticToken> = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for element in root.descendants_with_tokens() {
        let Some(token) = element.into_token() else {
            continue;
        };
        let Some(ty) = token_type(token.kind()) else {
            continue;
        };
        let range = token.text_range();
        let start = index.position(u32::from(range.start()));
        let length = u32::from(range.end()) - u32::from(range.start());
        // Multi-line tokens (block comments) are rare; encode against the start
        // line only, which is acceptable for highlighting.
        let line = start.line;
        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start.character - prev_start
        } else {
            start.character
        };

        data.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: ty,
            token_modifiers_bitset: 0,
        });

        prev_line = line;
        prev_start = start.character;
    }

    SemanticTokens {
        result_id: None,
        data,
    }
}
