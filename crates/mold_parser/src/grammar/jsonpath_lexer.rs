//! JSONPath lexer for embedded JSONPath expressions.
//!
//! This lexer tokenizes JSONPath content from within SQL string literals.
//! It produces JP_* tokens that the JSONPath parser consumes.

use mold_syntax::SyntaxKind;
use text_size::TextSize;

/// A JSONPath token
#[derive(Debug, Clone, Copy)]
pub struct JpToken {
    pub kind: SyntaxKind,
    pub len: TextSize,
}

/// Tokenize a JSONPath string (content of a SQL string literal, without quotes)
pub fn tokenize_jsonpath(source: &str) -> Vec<JpToken> {
    let mut tokens = Vec::new();
    let mut cursor = Cursor::new(source);

    while !cursor.is_eof() {
        let token = cursor.advance_token();
        tokens.push(token);
    }

    tokens
}

struct Cursor<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn peek(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.source[self.pos..].chars();
        chars.next();
        chars.next()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn advance_token(&mut self) -> JpToken {
        let start = self.pos;

        let kind = match self.bump() {
            None => SyntaxKind::ERROR,

            Some('$') => SyntaxKind::JP_DOLLAR,
            Some('@') => SyntaxKind::JP_AT,
            Some('*') => SyntaxKind::JP_STAR,
            Some('[') => SyntaxKind::JP_L_BRACKET,
            Some(']') => SyntaxKind::JP_R_BRACKET,
            Some('(') => SyntaxKind::JP_L_PAREN,
            Some(')') => SyntaxKind::JP_R_PAREN,
            Some(',') => SyntaxKind::JP_COMMA,
            Some(':') => SyntaxKind::JP_COLON,
            Some('+') => SyntaxKind::JP_PLUS,

            Some('.') => {
                if self.peek() == Some('.') {
                    self.bump();
                    SyntaxKind::JP_DOTDOT
                } else {
                    SyntaxKind::JP_DOT
                }
            }

            Some('?') => SyntaxKind::JP_QUESTION,

            Some('=') => {
                if self.peek() == Some('=') {
                    self.bump();
                    SyntaxKind::JP_EQ_EQ
                } else {
                    SyntaxKind::ERROR // Single = is not valid in JSONPath
                }
            }

            Some('!') => {
                if self.peek() == Some('=') {
                    self.bump();
                    SyntaxKind::JP_BANG_EQ
                } else {
                    SyntaxKind::JP_BANG
                }
            }

            Some('<') => {
                if self.peek() == Some('=') {
                    self.bump();
                    SyntaxKind::JP_LE
                } else {
                    SyntaxKind::JP_LT
                }
            }

            Some('>') => {
                if self.peek() == Some('=') {
                    self.bump();
                    SyntaxKind::JP_GE
                } else {
                    SyntaxKind::JP_GT
                }
            }

            Some('&') => {
                if self.peek() == Some('&') {
                    self.bump();
                    SyntaxKind::JP_AMP_AMP
                } else {
                    SyntaxKind::ERROR
                }
            }

            Some('|') => {
                if self.peek() == Some('|') {
                    self.bump();
                    SyntaxKind::JP_PIPE_PIPE
                } else {
                    SyntaxKind::ERROR
                }
            }

            Some('-') => {
                // Could be minus or start of negative number
                // We'll treat it as just minus; numbers handle their own signs
                SyntaxKind::JP_MINUS
            }

            Some('"') => self.string_literal(),

            Some(c) if c.is_ascii_whitespace() => {
                self.eat_whitespace();
                SyntaxKind::JP_WHITESPACE
            }

            Some(c) if c.is_ascii_digit() => {
                self.number_literal();
                SyntaxKind::JP_NUMBER_LIT
            }

            Some(c) if is_ident_start(c) => {
                self.identifier();
                self.classify_keyword(&self.source[start..self.pos])
            }

            Some(_) => SyntaxKind::ERROR,
        };

        JpToken {
            kind,
            len: TextSize::new((self.pos - start) as u32),
        }
    }

    fn eat_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_ascii_whitespace() {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn string_literal(&mut self) -> SyntaxKind {
        // Already consumed opening "
        loop {
            match self.peek() {
                None => break, // Unterminated string
                Some('"') => {
                    self.bump();
                    break;
                }
                Some('\\') => {
                    self.bump(); // backslash
                    self.bump(); // escaped char
                }
                Some(_) => {
                    self.bump();
                }
            }
        }
        SyntaxKind::JP_STRING_LIT
    }

    fn number_literal(&mut self) {
        // Consume digits
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.bump();
            } else {
                break;
            }
        }

        // Check for decimal part
        if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            self.bump(); // .
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.bump();
                } else {
                    break;
                }
            }
        }

        // Check for exponent
        if let Some('e' | 'E') = self.peek() {
            self.bump();
            if let Some('+' | '-') = self.peek() {
                self.bump();
            }
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.bump();
                } else {
                    break;
                }
            }
        }
    }

    fn identifier(&mut self) {
        while let Some(c) = self.peek() {
            if is_ident_continue(c) {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn classify_keyword(&self, text: &str) -> SyntaxKind {
        match text {
            "strict" => SyntaxKind::JP_STRICT_KW,
            "lax" => SyntaxKind::JP_LAX_KW,
            "true" => SyntaxKind::JP_TRUE_KW,
            "false" => SyntaxKind::JP_FALSE_KW,
            "null" => SyntaxKind::JP_NULL_KW,
            "last" => SyntaxKind::JP_LAST_KW,
            "to" => SyntaxKind::JP_TO_KW,
            "exists" => SyntaxKind::JP_EXISTS_KW,
            "like_regex" => SyntaxKind::JP_LIKE_REGEX_KW,
            "starts" => SyntaxKind::JP_STARTS_KW,
            "with" => SyntaxKind::JP_WITH_KW,
            "flag" => SyntaxKind::JP_FLAG_KW,
            "is" => SyntaxKind::JP_IS_KW,
            "unknown" => SyntaxKind::JP_UNKNOWN_KW,
            "type" => SyntaxKind::JP_TYPE_KW,
            "size" => SyntaxKind::JP_SIZE_KW,
            "double" => SyntaxKind::JP_DOUBLE_KW,
            "ceiling" => SyntaxKind::JP_CEILING_KW,
            "floor" => SyntaxKind::JP_FLOOR_KW,
            "abs" => SyntaxKind::JP_ABS_KW,
            "keyvalue" => SyntaxKind::JP_KEYVALUE_KW,
            "datetime" => SyntaxKind::JP_DATETIME_KW,
            _ => SyntaxKind::JP_IDENT,
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(s: &str) -> Vec<SyntaxKind> {
        tokenize_jsonpath(s).into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn test_simple_path() {
        assert_eq!(
            lex("$.name"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_DOT,
                SyntaxKind::JP_IDENT,
            ]
        );
    }

    #[test]
    fn test_array_access() {
        assert_eq!(
            lex("$.items[0]"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_DOT,
                SyntaxKind::JP_IDENT,
                SyntaxKind::JP_L_BRACKET,
                SyntaxKind::JP_NUMBER_LIT,
                SyntaxKind::JP_R_BRACKET,
            ]
        );
    }

    #[test]
    fn test_wildcard() {
        assert_eq!(
            lex("$[*]"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_L_BRACKET,
                SyntaxKind::JP_STAR,
                SyntaxKind::JP_R_BRACKET,
            ]
        );
    }

    #[test]
    fn test_recursive_descent() {
        assert_eq!(
            lex("$..name"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_DOTDOT,
                SyntaxKind::JP_IDENT,
            ]
        );
    }

    #[test]
    fn test_filter() {
        assert_eq!(
            lex("$[?(@.price > 10)]"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_L_BRACKET,
                SyntaxKind::JP_QUESTION,
                SyntaxKind::JP_L_PAREN,
                SyntaxKind::JP_AT,
                SyntaxKind::JP_DOT,
                SyntaxKind::JP_IDENT,
                SyntaxKind::JP_WHITESPACE,
                SyntaxKind::JP_GT,
                SyntaxKind::JP_WHITESPACE,
                SyntaxKind::JP_NUMBER_LIT,
                SyntaxKind::JP_R_PAREN,
                SyntaxKind::JP_R_BRACKET,
            ]
        );
    }

    #[test]
    fn test_mode() {
        assert_eq!(
            lex("strict $.name"),
            vec![
                SyntaxKind::JP_STRICT_KW,
                SyntaxKind::JP_WHITESPACE,
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_DOT,
                SyntaxKind::JP_IDENT,
            ]
        );
    }

    #[test]
    fn test_quoted_key() {
        assert_eq!(
            lex(r#"$."special-key""#),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_DOT,
                SyntaxKind::JP_STRING_LIT,
            ]
        );
    }

    #[test]
    fn test_comparison_operators() {
        assert_eq!(lex("=="), vec![SyntaxKind::JP_EQ_EQ]);
        assert_eq!(lex("!="), vec![SyntaxKind::JP_BANG_EQ]);
        assert_eq!(lex("<="), vec![SyntaxKind::JP_LE]);
        assert_eq!(lex(">="), vec![SyntaxKind::JP_GE]);
        assert_eq!(lex("<"), vec![SyntaxKind::JP_LT]);
        assert_eq!(lex(">"), vec![SyntaxKind::JP_GT]);
    }

    #[test]
    fn test_logical_operators() {
        assert_eq!(lex("&&"), vec![SyntaxKind::JP_AMP_AMP]);
        assert_eq!(lex("||"), vec![SyntaxKind::JP_PIPE_PIPE]);
        assert_eq!(lex("!"), vec![SyntaxKind::JP_BANG]);
    }

    #[test]
    fn test_last_keyword() {
        assert_eq!(
            lex("$[last]"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_L_BRACKET,
                SyntaxKind::JP_LAST_KW,
                SyntaxKind::JP_R_BRACKET,
            ]
        );
    }

    #[test]
    fn test_slice() {
        assert_eq!(
            lex("$[0 to 5]"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_L_BRACKET,
                SyntaxKind::JP_NUMBER_LIT,
                SyntaxKind::JP_WHITESPACE,
                SyntaxKind::JP_TO_KW,
                SyntaxKind::JP_WHITESPACE,
                SyntaxKind::JP_NUMBER_LIT,
                SyntaxKind::JP_R_BRACKET,
            ]
        );
    }

    #[test]
    fn test_method() {
        assert_eq!(
            lex("$.items.size()"),
            vec![
                SyntaxKind::JP_DOLLAR,
                SyntaxKind::JP_DOT,
                SyntaxKind::JP_IDENT,
                SyntaxKind::JP_DOT,
                SyntaxKind::JP_SIZE_KW,
                SyntaxKind::JP_L_PAREN,
                SyntaxKind::JP_R_PAREN,
            ]
        );
    }
}
