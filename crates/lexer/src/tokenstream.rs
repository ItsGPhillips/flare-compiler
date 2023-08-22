use diagnostics::span::{Span, NULL_SPAN};
use std::{iter::Peekable, mem, sync::Arc};
use syntax::SyntaxKind;

use crate::{text_input_tokenstream::TextInputTokenStream, token::Token, TokenStreamSource};

#[derive(Debug, Clone)]
pub struct TokenStream<T: TokenStreamSource = TextInputTokenStream<'static>> {
    src: Arc<str>,
    span: Span,
    inner: Peekable<T>,
}

impl Iterator for TokenStream {
    type Item = Token;

    #[rustfmt::skip]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(base_token) = self.inner.peek() {
            use SyntaxKind::*;
            self.span = base_token.span.clone();
            let base_token_clone = base_token.clone();
            let token = match base_token.kind {
                WHITESPACE   => self.tokenize_whitespace(),
                IDENTIFIER   => self.tokenize_identifier(),
                LIT_INTEGER  => self.tokenize_number(),
                MISC_UNKNOWN => self.tokenize_unknown(),
                _ if base_token.is_punctuation() => self.tokenize_punctuation(base_token_clone),
                _ => unreachable!()
            };
            Some(token)
        } else {
            None
        }
    }
}

impl TokenStream<TextInputTokenStream<'static>> {
    pub fn new(src: Arc<str>) -> Self {
        // SAFETY: TokenStream owns a reference to the ArcStr and will keep the ArcStr alive
        // for as long as its alive. RawTokenStream requires a lifetime as it contains an iterator over the str.
        // As the ArcStr will never be dropped as long as TokenStream is alive we can safely
        // extend the life time to 'static. Any references to the 'static str will be dropped
        // when RawTokenStream is dropped.
        let static_src: &'static str = unsafe { mem::transmute(src.as_ref()) };
        Self {
            src,
            span: NULL_SPAN,
            inner: TextInputTokenStream::from(static_src).peekable(),
        }
    }

    pub fn src(&self) -> Arc<str> {
        self.src.clone()
    }
}

impl<T: TokenStreamSource> TokenStream<T> {
    fn tokenize_identifier(&mut self) -> Token {
        use SyntaxKind::*;
        self.consume();

        let keyword = self.try_tokenize_keyword();
        let mut is_keyword = keyword.is_some();

        while let Some(base_token) = self.inner.peek() {
            match base_token.kind {
                PUNC_UNDERSCORE | IDENTIFIER => {
                    is_keyword = false;
                    self.consume();
                }
                _ => break,
            }
        }
        if is_keyword {
            self.create_token(keyword.expect("Compiler Bug: This should always be a keyword"))
        } else {
            self.create_token(IDENTIFIER)
        }
    }

    #[rustfmt::skip]
    fn try_tokenize_keyword(&mut self) -> Option<SyntaxKind> {
      use SyntaxKind::*;
        let src_str = self.span.get_str(&self.src);
        match src_str {
            "fn"       => Some(KW_FN     ),
            "pub"      => Some(KW_PUB    ),
            "mut"      => Some(KW_MUT    ),
            "let"      => Some(KW_LET    ),
            "for"      => Some(KW_FOR    ),
            "in"       => Some(KW_IN     ),
            "loop"     => Some(KW_LOOP   ),
            "const"    => Some(KW_CONST  ),
            "static"   => Some(KW_STATIC ),
            "mod"      => Some(KW_MOD    ),
            "struct"   => Some(KW_STRUCT ),
            "return"   => Some(KW_RETURN ),
            "if"       => Some(KW_IF     ),
            "else"     => Some(KW_ELSE   ),
            "while"    => Some(KW_WHILE  ),
            "true"     => Some(KW_TRUE   ),
            "false"    => Some(KW_FALSE  ),
            "break"    => Some(KW_FALSE  ),
            "continue" => Some(KW_FALSE  ),
            "Self"     => Some(KW_SELF_TYPE  ),
            "self"     => Some(KW_SELF_VALUE ),
            "super"    => Some(KW_SUPER  ),
            "crate"    => Some(KW_CRATE  ),
            _ => None,
        }
    }

    fn tokenize_number(&mut self) -> Token {
        use SyntaxKind::*;
        self.consume();
        let mut local_iter = self.inner.clone();

        let has_period = local_iter
            .next()
            .and_then(|raw_token| match raw_token.kind {
                PUNC_PERIOD => Some(()),
                _ => None,
            });

        if has_period.is_none() {
            return self.create_token(LIT_INTEGER);
        }

        let is_float = local_iter
            .next()
            .and_then(|raw_token| match raw_token.kind {
                LIT_INTEGER => Some(()),
                _ => None,
            });

        if is_float.is_none() {
            return self.create_token(LIT_INTEGER);
        } else {
            // eat the period token
            self.consume();
            // eat the decimnal part
            self.consume();
            return self.create_token(LIT_FLOAT);
        }
    }

    fn tokenize_punctuation(&mut self, token: Token) -> Token {
        use syntax::SyntaxKind::*;
        self.consume();
        match token.kind {
            PUNC_UNDERSCORE => {
                let mut is_ident = false;
                while let Some(raw_token) = self.inner.peek() {
                    match raw_token.kind {
                        PUNC_UNDERSCORE | LIT_INTEGER | IDENTIFIER => {
                            is_ident = true;
                            self.consume();
                        }
                        _ => break,
                    }
                }
                if is_ident {
                    self.create_token(IDENTIFIER)
                } else {
                    token
                }
            }
            _ => token,
        }
    }

    fn tokenize_whitespace(&mut self) -> Token {
        self.consume();
        self.create_token(SyntaxKind::WHITESPACE)
    }

    fn tokenize_unknown(&mut self) -> Token {
        self.consume();
        self.create_token(SyntaxKind::MISC_UNKNOWN)
    }

    #[inline(always)]
    fn create_token(&mut self, kind: SyntaxKind) -> Token {
        Token {
            kind,
            span: self.span.clone(),
        }
    }

    fn consume(&mut self) {
        if let Some(raw_token) = self.inner.next() {
            if self.span.is_empty() {
                self.span = raw_token.span;
            } else {
                self.span.end = raw_token.span.end;
            }
        }
    }
}
