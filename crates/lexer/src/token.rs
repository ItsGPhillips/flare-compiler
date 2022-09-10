use derive_more::*;
use diagnostics::span::{Span, NULL_SPAN};
use static_assertions::const_assert_eq;
use syntax::SyntaxKind;

const_assert_eq!(core::mem::size_of::<Token>(), 12);

pub const NULL_TOKEN: Token = Token {
    kind: SyntaxKind::MISC_NULL,
    span: NULL_SPAN,
};

/// Represents a single atom of source code.
#[derive(Debug, Display, Clone)]
#[display(fmt = "kind: {{{kind:?} span: {span:?}}}")]
pub struct Token {
    pub(crate) kind: SyntaxKind,
    pub(crate) span: Span,
}

impl Token {
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }
    #[inline]
    pub fn span(&self) -> Span {
        self.span.clone()
    }
    #[inline]
    pub fn get_str<'src>(&self, src: &'src str) -> &'src str {
        self.span.get_str(src)
    }
    #[inline]
    pub fn is(&self, kind: SyntaxKind) -> bool {
        self.kind == kind
    }
    #[inline(always)]
    pub fn is_null(&self) -> bool {
        matches!(self.kind, SyntaxKind::MISC_NULL)
    }
    #[inline(always)]
    pub fn is_keyword(&self) -> bool {
        self.kind.is_keyword()
    }
    #[inline(always)]
    pub fn is_literal(&self) -> bool {
        self.kind.is_keyword()
    }
    #[inline(always)]
    pub fn is_punctuation(&self) -> bool {
        self.kind.is_punctution()
    }
}
