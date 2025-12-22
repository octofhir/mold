//! JSONPath parser for embedded JSONPath expressions.
//!
//! Parses JSONPath content from SQL string literals and produces a tree.
//! Can be used standalone for JSONPath analysis or completion.
//!
//! # Example
//! ```ignore
//! use mold_parser::parse_jsonpath;
//!
//! let parse = parse_jsonpath("$.name.first");
//! assert!(parse.errors().is_empty());
//! ```

use super::jsonpath_lexer::{JpToken, tokenize_jsonpath};
use crate::event::Event;
use cstree::build::GreenNodeBuilder;
use cstree::green::GreenNode;
use cstree::interning::TokenInterner;
use cstree::syntax::ResolvedNode;
use mold_syntax::SyntaxKind;
use std::sync::Arc;
use text_size::{TextRange, TextSize};

/// Parse a JSONPath string and return a parse result with errors.
pub fn parse_jsonpath(source: &str) -> JpParse {
    let tokens = tokenize_jsonpath(source);
    let mut parser = JpParser::new(&tokens, source);
    jp_path_expr(&mut parser);
    let (events, errors) = parser.finish();
    let (green, interner) = JpSink::new(&tokens, source, events).finish();
    JpParse {
        green,
        interner,
        errors,
    }
}

/// Result of parsing a JSONPath expression.
#[must_use]
#[derive(Clone)]
pub struct JpParse {
    green: GreenNode,
    interner: Arc<TokenInterner>,
    errors: Vec<JpParseError>,
}

impl JpParse {
    /// Returns the syntax tree root.
    pub fn syntax(&self) -> ResolvedNode<SyntaxKind> {
        ResolvedNode::new_root_with_resolver(self.green.clone(), self.interner.clone())
    }

    /// Returns the green node.
    pub fn green(&self) -> &GreenNode {
        &self.green
    }

    /// Returns parse errors with their text ranges.
    pub fn errors(&self) -> &[JpParseError] {
        &self.errors
    }

    /// Returns the token interner.
    pub fn interner(&self) -> &Arc<TokenInterner> {
        &self.interner
    }

    /// Returns Ok if no errors, Err with errors otherwise.
    pub fn ok(self) -> Result<JpParse, Vec<JpParseError>> {
        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(self.errors)
        }
    }
}

impl std::fmt::Debug for JpParse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JpParse")
            .field("errors", &self.errors)
            .finish_non_exhaustive()
    }
}

/// A JSONPath parse error with location information.
#[must_use]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JpParseError {
    /// The error message.
    pub message: String,
    /// The text range where the error occurred.
    pub range: TextRange,
}

impl JpParseError {
    /// Creates a new parse error.
    pub fn new(message: impl Into<String>, range: TextRange) -> Self {
        Self {
            message: message.into(),
            range,
        }
    }

    /// Creates an error at a specific offset.
    pub fn at_offset(message: impl Into<String>, offset: TextSize) -> Self {
        Self::new(message, TextRange::empty(offset))
    }
}

impl std::fmt::Display for JpParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "JSONPath error at {}..{}: {}",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            self.message
        )
    }
}

impl std::error::Error for JpParseError {}

// =============================================================================
// JSONPath Parser
// =============================================================================

struct JpParser<'t> {
    tokens: &'t [JpToken],
    #[allow(dead_code)] // Kept for potential future use in error messages
    source: &'t str,
    pos: usize,
    events: Vec<Event>,
    errors: Vec<JpParseError>,
}

impl<'t> JpParser<'t> {
    fn new(tokens: &'t [JpToken], source: &'t str) -> Self {
        Self {
            tokens,
            source,
            pos: 0,
            events: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn finish(self) -> (Vec<Event>, Vec<JpParseError>) {
        (self.events, self.errors)
    }

    /// Returns the current text position.
    fn current_text_pos(&self) -> TextSize {
        let mut pos = TextSize::new(0);
        for i in 0..self.pos {
            if i < self.tokens.len() {
                pos += self.tokens[i].len;
            }
        }
        pos
    }

    fn start(&mut self) -> JpMarker {
        let pos = self.events.len() as u32;
        self.events.push(Event::Placeholder);
        JpMarker { pos }
    }

    fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    fn nth(&self, n: usize) -> SyntaxKind {
        let mut pos = self.pos;
        let mut count = 0;
        while pos < self.tokens.len() {
            let kind = self.tokens[pos].kind;
            if !kind.is_jp_trivia() {
                if count == n {
                    return kind;
                }
                count += 1;
            }
            pos += 1;
        }
        SyntaxKind::ERROR
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: SyntaxKind) {
        if !self.eat(kind) {
            self.error(format!("expected {:?}", kind));
        }
    }

    fn bump(&mut self) {
        self.skip_trivia();
        if self.pos < self.tokens.len() {
            let token = &self.tokens[self.pos];
            self.events.push(Event::Token {
                kind: token.kind,
                n_raw_tokens: 1,
            });
            self.pos += 1;
        }
        self.skip_trivia();
    }

    fn bump_any(&mut self) {
        if !self.at_end() {
            self.bump();
        }
    }

    fn skip_trivia(&mut self) {
        while self.pos < self.tokens.len() && self.tokens[self.pos].kind.is_jp_trivia() {
            let token = &self.tokens[self.pos];
            self.events.push(Event::Token {
                kind: token.kind,
                n_raw_tokens: 1,
            });
            self.pos += 1;
        }
    }

    fn error(&mut self, msg: impl Into<String>) {
        let msg = msg.into();
        let pos = self.current_text_pos();
        // Get the current token's range if available
        let range = if self.pos < self.tokens.len() {
            TextRange::at(pos, self.tokens[self.pos].len)
        } else {
            TextRange::empty(pos)
        };
        self.errors.push(JpParseError::new(msg.clone(), range));
        self.events.push(Event::Error { msg });
    }
}

struct JpMarker {
    pos: u32,
}

impl JpMarker {
    fn complete(self, p: &mut JpParser<'_>, kind: SyntaxKind) -> JpCompletedMarker {
        let event = &mut p.events[self.pos as usize];
        assert!(matches!(event, Event::Placeholder));
        *event = Event::Start {
            kind,
            forward_parent: None,
        };
        p.events.push(Event::Finish);
        JpCompletedMarker { pos: self.pos }
    }
}

#[allow(dead_code)]
struct JpCompletedMarker {
    pos: u32,
}

impl JpCompletedMarker {
    #[allow(dead_code)]
    fn precede(self, p: &mut JpParser<'_>) -> JpMarker {
        let new_pos = p.events.len() as u32;
        p.events.push(Event::Placeholder);
        if let Event::Start { forward_parent, .. } = &mut p.events[self.pos as usize] {
            *forward_parent = Some(new_pos);
        }
        JpMarker { pos: new_pos }
    }
}

// =============================================================================
// JSONPath Sink (event -> tree)
// =============================================================================

struct JpSink<'t> {
    builder: GreenNodeBuilder<'static, 'static, SyntaxKind>,
    tokens: &'t [JpToken],
    source: &'t str,
    cursor: usize,
    text_pos: TextSize,
    events: Vec<Event>,
}

impl<'t> JpSink<'t> {
    fn new(tokens: &'t [JpToken], source: &'t str, events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            source,
            cursor: 0,
            text_pos: TextSize::new(0),
            events,
        }
    }

    fn finish(mut self) -> (GreenNode, Arc<TokenInterner>) {
        for i in 0..self.events.len() {
            match std::mem::replace(&mut self.events[i], Event::Placeholder) {
                Event::Start {
                    kind,
                    forward_parent,
                } => {
                    let mut kinds = vec![kind];
                    let mut fp = forward_parent;

                    while let Some(parent_idx) = fp {
                        fp = match std::mem::replace(
                            &mut self.events[parent_idx as usize],
                            Event::Placeholder,
                        ) {
                            Event::Start {
                                kind: parent_kind,
                                forward_parent: parent_fp,
                            } => {
                                kinds.push(parent_kind);
                                parent_fp
                            }
                            _ => unreachable!(),
                        };
                    }

                    for kind in kinds.into_iter().rev() {
                        self.builder.start_node(kind);
                    }
                }
                Event::Finish => {
                    self.builder.finish_node();
                }
                Event::Token { kind, n_raw_tokens } => {
                    self.token(kind, n_raw_tokens);
                }
                Event::Error { .. } => {
                    // Errors are handled differently for JSONPath - we just skip for now
                }
                Event::Placeholder => {}
            }
        }

        let (node, cache) = self.builder.finish();
        let interner = Arc::new(
            cache
                .expect("builder should have cache")
                .into_interner()
                .expect("cache should have interner"),
        );
        (node, interner)
    }

    fn token(&mut self, kind: SyntaxKind, n_raw_tokens: u8) {
        let mut len = TextSize::new(0);
        for _ in 0..n_raw_tokens {
            if self.cursor < self.tokens.len() {
                len += self.tokens[self.cursor].len;
                self.cursor += 1;
            }
        }
        let text = &self.source[self.text_pos.into()..][..len.into()];
        self.text_pos += len;
        self.builder.token(kind, text);
    }
}

// =============================================================================
// JSONPath Grammar
// =============================================================================

/// Parse a JSONPath expression: [mode] path_expr
fn jp_path_expr(p: &mut JpParser<'_>) {
    let m = p.start();

    // Optional mode: strict or lax
    if p.at(SyntaxKind::JP_STRICT_KW) || p.at(SyntaxKind::JP_LAX_KW) {
        let mm = p.start();
        p.bump();
        mm.complete(p, SyntaxKind::JP_MODE);
    }

    // Root: $
    if p.at(SyntaxKind::JP_DOLLAR) {
        let rm = p.start();
        p.bump();
        rm.complete(p, SyntaxKind::JP_ROOT);

        // Parse accessors
        while !p.at_end() {
            if !jp_accessor(p) {
                break;
            }
        }
    } else if p.at(SyntaxKind::JP_AT) {
        // Filter context can start with @
        let cm = p.start();
        p.bump();
        cm.complete(p, SyntaxKind::JP_CURRENT);

        while !p.at_end() {
            if !jp_accessor(p) {
                break;
            }
        }
    } else if !p.at_end() {
        p.error("expected $ or @ at start of JSONPath");
        p.bump_any();
    }

    m.complete(p, SyntaxKind::JSONPATH_LITERAL);
}

/// Parse an accessor: member_access | array_access | filter | method
fn jp_accessor(p: &mut JpParser<'_>) -> bool {
    match p.current() {
        SyntaxKind::JP_DOT => jp_member_access(p),
        SyntaxKind::JP_DOTDOT => jp_recursive_descent(p),
        SyntaxKind::JP_L_BRACKET => jp_array_access(p),
        SyntaxKind::JP_QUESTION => jp_filter(p),
        _ => return false,
    }
    true
}

/// Parse member access: .key | .* | ."quoted"
fn jp_member_access(p: &mut JpParser<'_>) {
    let m = p.start();
    p.bump(); // .

    if p.at(SyntaxKind::JP_STAR) {
        p.bump();
        m.complete(p, SyntaxKind::JP_MEMBER_WILDCARD);
    } else if p.at(SyntaxKind::JP_IDENT) || is_jp_method_keyword(p.current()) {
        // Check if this is a method call
        if is_jp_method_keyword(p.current()) && p.nth(1) == SyntaxKind::JP_L_PAREN {
            jp_method(p, m);
        } else {
            let km = p.start();
            p.bump();
            km.complete(p, SyntaxKind::JP_KEY);
            m.complete(p, SyntaxKind::JP_MEMBER_ACCESS);
        }
    } else if p.at(SyntaxKind::JP_STRING_LIT) {
        let km = p.start();
        p.bump();
        km.complete(p, SyntaxKind::JP_KEY);
        m.complete(p, SyntaxKind::JP_MEMBER_ACCESS);
    } else {
        // Incomplete path like "$." - still produce valid tree for autocomplete
        m.complete(p, SyntaxKind::JP_MEMBER_ACCESS);
    }
}

/// Parse recursive descent: ..key | ..*
fn jp_recursive_descent(p: &mut JpParser<'_>) {
    let m = p.start();
    p.bump(); // ..

    if p.at(SyntaxKind::JP_STAR) {
        p.bump();
    } else if p.at(SyntaxKind::JP_IDENT) || p.at(SyntaxKind::JP_STRING_LIT) {
        let km = p.start();
        p.bump();
        km.complete(p, SyntaxKind::JP_KEY);
    }
    // Incomplete ".." is also valid for autocomplete

    m.complete(p, SyntaxKind::JP_RECURSIVE_DESCENT);
}

/// Parse array access: [index] | [*] | [start to end]
fn jp_array_access(p: &mut JpParser<'_>) {
    let m = p.start();
    p.bump(); // [

    if p.at(SyntaxKind::JP_STAR) {
        p.bump();
        p.expect(SyntaxKind::JP_R_BRACKET);
        m.complete(p, SyntaxKind::JP_ARRAY_WILDCARD);
    } else if p.at(SyntaxKind::JP_QUESTION) {
        // Inline filter: [?(...)]
        jp_filter_expr(p);
        p.expect(SyntaxKind::JP_R_BRACKET);
        m.complete(p, SyntaxKind::JP_FILTER);
    } else {
        // Index or slice
        jp_index_expr(p);

        if p.at(SyntaxKind::JP_TO_KW) {
            // Slice: [start to end]
            p.bump();
            jp_index_expr(p);
            p.expect(SyntaxKind::JP_R_BRACKET);
            m.complete(p, SyntaxKind::JP_ARRAY_SLICE);
        } else if p.at(SyntaxKind::JP_COMMA) {
            // Multiple indices: [1, 2, 3]
            while p.eat(SyntaxKind::JP_COMMA) {
                jp_index_expr(p);
            }
            p.expect(SyntaxKind::JP_R_BRACKET);
            m.complete(p, SyntaxKind::JP_ARRAY_ACCESS);
        } else {
            p.expect(SyntaxKind::JP_R_BRACKET);
            m.complete(p, SyntaxKind::JP_ARRAY_ACCESS);
        }
    }
}

/// Parse index expression: number | last | last - n | last + n
fn jp_index_expr(p: &mut JpParser<'_>) {
    if p.at(SyntaxKind::JP_NUMBER_LIT) {
        p.bump();
    } else if p.at(SyntaxKind::JP_LAST_KW) {
        p.bump();
        // Optional +/- offset
        if p.at(SyntaxKind::JP_MINUS) || p.at(SyntaxKind::JP_PLUS) {
            p.bump();
            p.expect(SyntaxKind::JP_NUMBER_LIT);
        }
    } else if p.at(SyntaxKind::JP_MINUS) {
        // Negative index (shorthand for last - n)
        p.bump();
        p.expect(SyntaxKind::JP_NUMBER_LIT);
    }
    // Empty index for incomplete paths
}

/// Parse filter: ?( expr )
fn jp_filter(p: &mut JpParser<'_>) {
    let m = p.start();
    p.bump(); // ?
    p.expect(SyntaxKind::JP_L_PAREN);
    jp_filter_expr(p);
    p.expect(SyntaxKind::JP_R_PAREN);
    m.complete(p, SyntaxKind::JP_FILTER);
}

/// Parse filter expression: @ accessor* comparison? (logic_op filter_expr)?
fn jp_filter_expr(p: &mut JpParser<'_>) {
    let m = p.start();

    // Handle NOT prefix
    if p.at(SyntaxKind::JP_BANG) {
        p.bump();
    }

    // Handle parenthesized expression
    if p.at(SyntaxKind::JP_L_PAREN) {
        p.bump();
        jp_filter_expr(p);
        p.expect(SyntaxKind::JP_R_PAREN);
    } else if p.at(SyntaxKind::JP_EXISTS_KW) {
        // exists(path)
        p.bump();
        p.expect(SyntaxKind::JP_L_PAREN);
        jp_filter_path(p);
        p.expect(SyntaxKind::JP_R_PAREN);
    } else {
        // @ accessor* comparison?
        jp_filter_path(p);

        // Optional comparison
        if is_comparison_op(p.current()) {
            let cm = p.start();
            p.bump(); // comparison operator
            jp_filter_value(p);
            cm.complete(p, SyntaxKind::JP_COMPARISON);
        }
    }

    // Check for logical operators
    if p.at(SyntaxKind::JP_AMP_AMP) {
        p.bump();
        jp_filter_expr(p);
        m.complete(p, SyntaxKind::JP_LOGICAL_AND);
    } else if p.at(SyntaxKind::JP_PIPE_PIPE) {
        p.bump();
        jp_filter_expr(p);
        m.complete(p, SyntaxKind::JP_LOGICAL_OR);
    } else {
        m.complete(p, SyntaxKind::JP_FILTER_EXPR);
    }
}

/// Parse a path in filter context: @ accessor*
fn jp_filter_path(p: &mut JpParser<'_>) {
    if p.at(SyntaxKind::JP_AT) {
        let m = p.start();
        p.bump();
        m.complete(p, SyntaxKind::JP_CURRENT);

        while jp_accessor(p) {}
    } else if p.at(SyntaxKind::JP_DOLLAR) {
        let m = p.start();
        p.bump();
        m.complete(p, SyntaxKind::JP_ROOT);

        while jp_accessor(p) {}
    }
}

/// Parse a value in filter: literal | path
fn jp_filter_value(p: &mut JpParser<'_>) {
    match p.current() {
        SyntaxKind::JP_NUMBER_LIT => {
            let m = p.start();
            p.bump();
            m.complete(p, SyntaxKind::JP_NUMBER);
        }
        SyntaxKind::JP_STRING_LIT => {
            let m = p.start();
            p.bump();
            m.complete(p, SyntaxKind::JP_STRING);
        }
        SyntaxKind::JP_TRUE_KW | SyntaxKind::JP_FALSE_KW => {
            let m = p.start();
            p.bump();
            m.complete(p, SyntaxKind::JP_BOOL);
        }
        SyntaxKind::JP_NULL_KW => {
            let m = p.start();
            p.bump();
            m.complete(p, SyntaxKind::JP_NULL);
        }
        SyntaxKind::JP_AT | SyntaxKind::JP_DOLLAR => {
            jp_filter_path(p);
        }
        _ => {}
    }
}

/// Parse method call: .method_name()
fn jp_method(p: &mut JpParser<'_>, m: JpMarker) {
    p.bump(); // method name keyword
    p.expect(SyntaxKind::JP_L_PAREN);
    // Some methods take arguments
    if !p.at(SyntaxKind::JP_R_PAREN) {
        jp_method_args(p);
    }
    p.expect(SyntaxKind::JP_R_PAREN);
    m.complete(p, SyntaxKind::JP_METHOD);
}

/// Parse method arguments
fn jp_method_args(p: &mut JpParser<'_>) {
    // For now, just consume tokens until )
    // datetime() and like_regex have special argument formats
    while !p.at_end() && !p.at(SyntaxKind::JP_R_PAREN) {
        p.bump_any();
    }
}

fn is_comparison_op(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::JP_EQ_EQ
            | SyntaxKind::JP_BANG_EQ
            | SyntaxKind::JP_LT
            | SyntaxKind::JP_LE
            | SyntaxKind::JP_GT
            | SyntaxKind::JP_GE
            | SyntaxKind::JP_LIKE_REGEX_KW
            | SyntaxKind::JP_STARTS_KW
    )
}

fn is_jp_method_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::JP_TYPE_KW
            | SyntaxKind::JP_SIZE_KW
            | SyntaxKind::JP_DOUBLE_KW
            | SyntaxKind::JP_CEILING_KW
            | SyntaxKind::JP_FLOOR_KW
            | SyntaxKind::JP_ABS_KW
            | SyntaxKind::JP_KEYVALUE_KW
            | SyntaxKind::JP_DATETIME_KW
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use cstree::util::NodeOrToken;

    fn format_tree(source: &str) -> String {
        let parse = parse_jsonpath(source);
        let resolver = parse.interner.as_ref();
        format_node(&parse.green, resolver, 0)
    }

    fn format_node(node: &GreenNode, resolver: &TokenInterner, indent: usize) -> String {
        use cstree::Syntax;
        let mut result = String::new();
        let prefix = "  ".repeat(indent);
        let kind: SyntaxKind = SyntaxKind::from_raw(node.kind());
        result.push_str(&format!("{}{:?}\n", prefix, kind));

        for child in node.children() {
            match child {
                NodeOrToken::Node(n) => {
                    result.push_str(&format_node(n, resolver, indent + 1));
                }
                NodeOrToken::Token(t) => {
                    let text = t.text(resolver);
                    let tkind: SyntaxKind = SyntaxKind::from_raw(t.kind());
                    result.push_str(&format!("{}  {:?} {:?}\n", prefix, tkind, text));
                }
            }
        }

        result
    }

    #[test]
    fn test_simple_path() {
        insta::assert_snapshot!(format_tree("$.name"));
    }

    #[test]
    fn test_array_access() {
        insta::assert_snapshot!(format_tree("$.items[0]"));
    }

    #[test]
    fn test_wildcard() {
        insta::assert_snapshot!(format_tree("$.items[*]"));
    }

    #[test]
    fn test_recursive() {
        insta::assert_snapshot!(format_tree("$..name"));
    }

    #[test]
    fn test_filter() {
        insta::assert_snapshot!(format_tree("$.items[*] ? (@.price > 10)"));
    }

    #[test]
    fn test_method() {
        insta::assert_snapshot!(format_tree("$.items.size()"));
    }

    #[test]
    fn test_mode_strict() {
        insta::assert_snapshot!(format_tree("strict $.name"));
    }

    #[test]
    fn test_slice() {
        insta::assert_snapshot!(format_tree("$[0 to 5]"));
    }

    #[test]
    fn test_last_index() {
        insta::assert_snapshot!(format_tree("$[last]"));
    }

    #[test]
    fn test_last_minus() {
        insta::assert_snapshot!(format_tree("$[last - 1]"));
    }

    #[test]
    fn test_quoted_key() {
        insta::assert_snapshot!(format_tree(r#"$."special-key""#));
    }

    #[test]
    fn test_incomplete_path() {
        // This should produce a valid tree for autocomplete
        insta::assert_snapshot!(format_tree("$."));
    }

    #[test]
    fn test_complex_filter() {
        insta::assert_snapshot!(format_tree("$[*] ? (@.price > 10 && @.active == true)"));
    }

    #[test]
    fn test_error_has_span() {
        // Invalid JSONPath - missing $ at start
        let parse = parse_jsonpath("name");
        assert!(!parse.errors().is_empty());

        let error = &parse.errors()[0];
        // Error should have a non-empty range
        assert!(error.range.start() <= error.range.end());
        assert!(error.message.contains("expected"));
    }

    #[test]
    fn test_valid_path_no_errors() {
        let parse = parse_jsonpath("$.name.first");
        assert!(parse.errors().is_empty());
    }

    #[test]
    fn test_parse_ok_result() {
        let parse = parse_jsonpath("$.name");
        assert!(parse.ok().is_ok());

        let parse = parse_jsonpath("invalid");
        assert!(parse.ok().is_err());
    }

    #[test]
    fn test_error_display() {
        let error = JpParseError::new(
            "test error",
            TextRange::new(TextSize::new(5), TextSize::new(10)),
        );
        let display = error.to_string();
        assert!(display.contains("5..10"));
        assert!(display.contains("test error"));
    }

    #[test]
    fn test_parse_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<JpParse>();
        assert_send_sync::<JpParseError>();
    }
}
