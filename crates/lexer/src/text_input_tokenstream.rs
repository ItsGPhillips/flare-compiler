use diagnostics::span::{Span, NULL_SPAN};
use std::{iter::Peekable, str::CharIndices};
use syntax::SyntaxKind;

use crate::{token::Token, TokenStreamSource};

#[derive(Debug, Clone)]
pub struct TextInputTokenStream<'src> {
    span: Span,
    chars: Peekable<CharIndices<'src>>,
}

impl<'src> TextInputTokenStream<'src> {
    fn tokenize_identifier(&mut self) {
        let pos = self.consume().0;
        *self.span = pos..pos;

        while let Some((.., ch)) = self.chars.peek() {
            if ch.is_ascii_alphanumeric() {
                self.span.end = self.consume().0;
            } else {
                break;
            }
        }
    }

    #[rustfmt::skip]
    fn tokenize_punctuation(&mut self) -> SyntaxKind {
        use SyntaxKind::*;
        let (pos, c) = self.consume();
        *self.span = pos..pos;
        match c {
            '+'  => PUNC_PLUS,
            '-'  => PUNC_HYPHEN,
            '_'  => PUNC_UNDERSCORE,
            '/'  => PUNC_SLASH,
            '\\' => PUNC_BACKSLASH,
            '*'  => PUNC_ASTERISK,
            ')'  => PUNC_RPAREN,
            '('  => PUNC_LPAREN,
            '>'  => PUNC_RANGLE_BRACKET,
            '<'  => PUNC_LANGLE_BRACKET,
            '}'  => PUNC_RBRACE,
            '{'  => PUNC_LBRACE,
            ']'  => PUNC_RBRACKET,
            '['  => PUNC_LBRACKET,
            '='  => PUNC_EQUALS,
            '|'  => PUNC_PIPE,
            '?'  => PUNC_QUESTIONMARK,
            '!'  => PUNC_EXCLAMATION,
            '.'  => PUNC_PERIOD,
            '&'  => PUNC_AMPERSAND,
            ':'  => PUNC_COLON,
            ';'  => PUNC_SEMICOLON,
            '"'  => PUNC_QUOTE,
            '\'' => PUNC_SINGLE_QUOTE,
            '%'  => PUNC_PERCENT,
            '#'  => PUNC_HASH,
            '@'  => PUNC_AT,
            '$'  => PUNC_DOLLAR,
            '~'  => PUNC_TILDE,
            '`'  => PUNC_BACKQUOTE,
            ','  => PUNC_COMMA,
            _ => unreachable!(),
        }
    }

    fn tokenize_integer(&mut self) {
        let (pos, ..) = self.consume();
        *self.span = pos..pos;

        while let Some((.., ch)) = self.chars.peek() {
            if ch.is_numeric() {
                self.span.end = self.consume().0;
            } else {
                break;
            }
        }
    }

    fn tokenize_whitespace(&mut self) {
        let (pos, _) = self.consume();
        *self.span = pos..pos;

        while let Some((.., ch)) = self.chars.peek() {
            if ch.is_whitespace() {
                self.span.end = self.consume().0;
            } else {
                break;
            }
        }
    }

    fn tokenize_unknown(&mut self) {
        let (pos, ..) = self.consume();
        *self.span = pos..pos;
    }

    /// eats the current char and returns its byte pos in the &str
    fn consume(&mut self) -> (u32, char) {
        let (pos, c) = self.chars.next().expect("This should always be Some");
        (pos as u32, c)
    }
}

impl<'src> From<&'src str> for TextInputTokenStream<'src> {
    fn from(src: &'src str) -> Self {
        Self {
            span: NULL_SPAN,
            chars: src.char_indices().peekable(),
        }
    }
}

impl<'src> Iterator for TextInputTokenStream<'src> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        use SyntaxKind::*;
        if let Some((_, ch)) = self.chars.peek() {
            let kind = if ch.is_whitespace() {
                self.tokenize_whitespace();
                WHITESPACE
            } else if ch.is_alphabetic() {
                self.tokenize_identifier();
                IDENTIFIER
            } else if ch.is_ascii_punctuation() {
                self.tokenize_punctuation()
            } else if ch.is_numeric() {
                self.tokenize_integer();
                LIT_INTEGER
            } else {
                self.tokenize_unknown();
                MISC_UNKNOWN
            };

            Some(Token {
                kind,
                span: {
                    let mut span = self.span.clone();
                    // push the end +1 so exclusive ranges work
                    span.end += 1;
                    span
                },
            })
        } else {
            None
        }
    }
}

impl TokenStreamSource for TextInputTokenStream<'_> {}
