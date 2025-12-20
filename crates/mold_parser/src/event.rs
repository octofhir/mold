use mold_syntax::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Event {
    Start {
        kind: SyntaxKind,
        forward_parent: Option<u32>,
    },
    Finish,
    Token {
        kind: SyntaxKind,
        n_raw_tokens: u8,
    },
    Error {
        msg: String,
    },
    Placeholder,
}
