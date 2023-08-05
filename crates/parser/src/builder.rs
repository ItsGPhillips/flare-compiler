use std::sync::Arc;

use diagnostics::Diagnostic;
use itertools::{peek_nth, Itertools, PeekNth};
use lexer::{
    token::{Token, NULL_TOKEN},
    TokenStream,
};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language};
use syntax::SyntaxKind;

use crate::{Flare, Tkn};

pub(crate) struct SyntaxTreeBuilder {
    src: Arc<str>,
    tokens: PeekNth<TokenStream>,
    builder: GreenNodeBuilder<'static>,
    pub(crate) errors: Vec<Diagnostic>,
}

impl SyntaxTreeBuilder {
    pub fn new(src: Arc<str>) -> Self {
        let this = Self {
            src: src.clone(),
            tokens: peek_nth(TokenStream::new(src)),
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        };
        this
    }

    /// gets the current [Token]
    #[inline]
    pub(crate) fn current_token(&mut self) -> Token {
        if let Some(token) = self.tokens.peek() {
            token.clone()
        } else {
            NULL_TOKEN
        }
    }

    /// Advances the underlying [TokenStream].
    /// If **skip_whitespace** is true, all whitespace tokens will be consumed
    /// until a non-whitespace token is reached
    pub(crate) fn advance(&mut self, skip_whitespace: bool) {
        let token = self.tokens.next().expect("Unexpected EOF");
        self.builder.token(
            Flare::kind_to_raw(token.kind()),
            token.span().get_str(&self.src),
        );
        if !skip_whitespace {
            return;
        }
        self.skip_whitespace();
    }

    /// Skips whitespace tokens until a non-whitespace token is reached
    pub(crate) fn skip_whitespace(&mut self) {
        while let Some(token) = self.tokens.peek() {
            if token.is(SyntaxKind::WHITESPACE) {
                let token = self.tokens.next().unwrap();
                self.builder.token(
                    Flare::kind_to_raw(SyntaxKind::WHITESPACE),
                    token.span().get_str(&self.src),
                );
            } else {
                break;
            }
        }
    }

    /// Peeks **offset** [Tokens](Token) ahead without advancing the underlying [TokenStream].
    /// If **skip_whitespace** is true, whitespace [Tokens](Token) will not be included in
    /// offest calculations
    pub(crate) fn peek_kind(&mut self, mut offset: usize, skip_whitespace: bool) -> SyntaxKind {
        // Early return for peeking the current token
        if offset == 0 {
            return self.current_token().kind();
        }
        if skip_whitespace {
            // Start at 1 so we look and the token next to the current token
            let mut true_offset = 1;
            while let Some(token) = self.tokens.peek_nth(true_offset) {
                match token.kind() {
                    SyntaxKind::WHITESPACE => true_offset += 1,
                    _ => {
                        offset -= 1;
                        if offset == 0 {
                            return token.kind();
                        }
                    }
                }
            }
            return SyntaxKind::MISC_NULL;
        } else {
            match self.tokens.peek_nth(offset) {
                Some(token) => token.kind(),
                None => SyntaxKind::MISC_NULL,
            }
        }
    }

    pub(crate) fn expect_one_of<'a>(
        &mut self,
        mut kinds: impl Iterator<Item = SyntaxKind>,
        should_advance: bool,
    ) -> Option<SyntaxKind> {
        let current = self.current_token();
        if let Some(kind) = kinds.find(|kind| self.current_token().is(*kind)) {
            if should_advance {
                self.advance(false);
            }
            Some(kind)
        } else {
            self.errors.push(Diagnostic {
                level: diagnostics::DiagnosticLevel::FATAL,
                title: "Syntax Error".into(),
                message: format!(
                    "Unexpected Token: found {}, expected one of {}",
                    current.kind().as_str(),
                    kinds.map(|kind| kind.as_str()).join(", ")
                ),
                span: current.span(),
            });
            None
        }
    }

    pub(crate) fn expect<'a>(&mut self, kind: SyntaxKind, should_advance: bool) -> bool {
        let current = self.current_token();
        if self.current_token().is(kind) {
            if should_advance {
                self.advance(false);
            }
            true
        } else {
            self.errors.push(Diagnostic {
                level: diagnostics::DiagnosticLevel::FATAL,
                title: "Syntax Error".into(),
                message: format!(
                    "Unexpected Token: found {}, expected {}",
                    current.kind().as_str(),
                    kind.as_str(),
                ),
                span: current.span(),
            });
            false
        }
    }

    /// **until** should be a function that returns an iterator over the
    /// legal token kinds.
    pub(crate) fn error_until<F, I>(&mut self, until: F) -> Option<SyntaxKind>
    where
        F: Fn() -> I,
        I: Iterator<Item = SyntaxKind>,
    {
        let c = self.checkpoint();
        let mut has_errors = false;
        loop {
            match until().find(|legal| &self.peek_kind(0, false) == legal) {
                Some(found) => {
                    has_errors.then(|| self.finish_node_at(c, SyntaxKind::ERROR));
                    return Some(found);
                }
                None => {
                    if self.current_token().is(Tkn![NULL]) {
                        self.finish_node_at(c, SyntaxKind::ERROR);
                        return None;
                    }
                    has_errors = true;
                    self.advance(false);
                }
            }
        }
    }

    #[inline(always)]
    #[allow(unused)]
    pub(crate) fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }
    #[inline(always)]
    #[allow(unused)]
    pub(crate) fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(Flare::kind_to_raw(kind))
    }
    #[inline(always)]
    #[allow(unused)]
    pub(crate) fn finish_node(&mut self) {
        self.builder.finish_node()
    }
    #[inline(always)]
    #[allow(unused)]
    pub(crate) fn finish_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, Flare::kind_to_raw(kind));
        self.finish_node();
    }
    #[inline(always)]
    #[allow(unused)]
    pub(crate) fn finish(self) -> GreenNode {
        self.builder.finish()
    }
}
