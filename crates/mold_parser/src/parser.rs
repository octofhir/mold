use crate::event::Event;
use crate::token_set::TokenSet;
use mold_syntax::{SyntaxKind, Token};

pub struct Parser<'t> {
    tokens: &'t [Token],
    #[allow(dead_code)]
    source: &'t str,
    pos: usize,
    events: Vec<Event>,
    fuel: u32,
}

impl<'t> Parser<'t> {
    pub fn new(tokens: &'t [Token], source: &'t str) -> Self {
        Self {
            tokens,
            source,
            pos: 0,
            events: Vec::new(),
            fuel: 256,
        }
    }

    pub fn finish(self) -> Vec<Event> {
        self.events
    }

    pub fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;
        self.events.push(Event::Placeholder);
        Marker {
            pos,
            completed: false,
        }
    }

    pub fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    pub fn nth(&self, n: usize) -> SyntaxKind {
        if self.fuel == 0 {
            return SyntaxKind::ERROR;
        }
        let mut pos = self.pos;
        let mut count = 0;
        while pos < self.tokens.len() {
            let kind = self.tokens[pos].kind;
            if !kind.is_trivia() {
                if count == n {
                    return kind;
                }
                count += 1;
            }
            pos += 1;
        }
        SyntaxKind::ERROR
    }

    pub fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    pub fn at_set(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    pub fn at_end(&self) -> bool {
        self.current() == SyntaxKind::ERROR && self.pos >= self.tokens.len()
    }

    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, kind: SyntaxKind) {
        if !self.eat(kind) {
            self.error(format!("expected {:?}", kind));
        }
    }

    /// Expects a token, recovering to the given set if not found.
    /// Returns true if the expected token was found.
    pub fn expect_recover(&mut self, kind: SyntaxKind, recovery: TokenSet) -> bool {
        if self.eat(kind) {
            return true;
        }

        // If we're already at a recovery token, just report the error
        if self.at_set(recovery) {
            self.error(format!("expected {:?}", kind));
            return false;
        }

        // Skip tokens until we find the expected one or a recovery point
        let m = self.start();
        self.error(format!("expected {:?}", kind));
        while !self.at_end() && !self.at(kind) && !self.at_set(recovery) {
            self.bump_any();
        }
        m.complete(self, SyntaxKind::ERROR);

        // Try to consume the expected token if we found it
        self.eat(kind)
    }

    /// Skips tokens until reaching one in the recovery set.
    #[allow(dead_code)]
    pub fn recover_to(&mut self, recovery: TokenSet) {
        if self.at_set(recovery) || self.at_end() {
            return;
        }
        let m = self.start();
        while !self.at_end() && !self.at_set(recovery) {
            self.bump_any();
        }
        m.complete(self, SyntaxKind::ERROR);
    }

    /// Returns the current token position in the source.
    #[allow(dead_code)]
    pub fn current_pos(&self) -> usize {
        self.pos
    }

    pub fn bump(&mut self) {
        assert!(!self.at_end(), "bump at end of file");
        self.fuel = 256;
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

    pub fn bump_any(&mut self) {
        if !self.at_end() {
            self.bump();
        }
    }

    fn skip_trivia(&mut self) {
        while self.pos < self.tokens.len() && self.tokens[self.pos].kind.is_trivia() {
            let token = &self.tokens[self.pos];
            self.events.push(Event::Token {
                kind: token.kind,
                n_raw_tokens: 1,
            });
            self.pos += 1;
        }
    }

    pub fn error(&mut self, msg: impl Into<String>) {
        self.events.push(Event::Error { msg: msg.into() });
    }

    pub fn err_recover(&mut self, msg: impl Into<String>, recovery: TokenSet) {
        if self.at_set(recovery) {
            self.error(msg);
            return;
        }
        let m = self.start();
        self.error(msg);
        while !self.at_end() && !self.at_set(recovery) {
            self.bump_any();
        }
        m.complete(self, SyntaxKind::ERROR);
    }

    #[allow(dead_code)]
    pub fn text_at(&self, pos: usize) -> &'t str {
        let mut offset = 0u32;
        for (i, tok) in self.tokens.iter().enumerate() {
            if i == pos {
                let start = offset as usize;
                let end = start + u32::from(tok.len) as usize;
                return &self.source[start..end];
            }
            offset += u32::from(tok.len);
        }
        ""
    }

    #[allow(dead_code)]
    pub fn current_text(&self) -> &'t str {
        let mut text_pos = self.pos;
        while text_pos < self.tokens.len() && self.tokens[text_pos].kind.is_trivia() {
            text_pos += 1;
        }
        if text_pos < self.tokens.len() {
            self.text_at(text_pos)
        } else {
            ""
        }
    }
}

#[derive(Debug)]
pub struct Marker {
    pos: u32,
    completed: bool,
}

impl Marker {
    pub fn complete(mut self, p: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
        self.completed = true;
        let event = &mut p.events[self.pos as usize];
        assert!(matches!(event, Event::Placeholder));
        *event = Event::Start {
            kind,
            forward_parent: None,
        };
        p.events.push(Event::Finish);
        CompletedMarker { pos: self.pos }
    }

    #[allow(dead_code)]
    pub fn abandon(mut self, p: &mut Parser<'_>) {
        self.completed = true;
        if self.pos as usize == p.events.len() - 1 {
            match p.events.pop() {
                Some(Event::Placeholder) => {}
                _ => unreachable!(),
            }
        }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.completed {
            panic!("Marker dropped without being completed or abandoned");
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CompletedMarker {
    pos: u32,
}

impl CompletedMarker {
    pub fn precede(self, p: &mut Parser<'_>) -> Marker {
        let new_pos = p.events.len() as u32;
        p.events.push(Event::Placeholder);
        if let Event::Start { forward_parent, .. } = &mut p.events[self.pos as usize] {
            *forward_parent = Some(new_pos);
        }
        Marker {
            pos: new_pos,
            completed: false,
        }
    }
}
