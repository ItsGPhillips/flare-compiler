use syntax::SyntaxKind;

use crate::{ast_token, SyntaxElement, SyntaxToken};

use super::{AstElement, AstError};
pub trait AstToken: Sized {
    fn can_cast(kind: crate::SyntaxKind) -> bool;
    fn cast(syntax: crate::SyntaxToken) -> Option<Self>;
    fn syntax(&self) -> &crate::SyntaxToken;
}

ast_token!(Ident, IDENTIFIER);

#[derive(Debug)]
pub struct LitString(SyntaxToken);
impl AstElement for LitString {
    fn cast(element: SyntaxElement) -> Result<Self, AstError> {
        element
            .as_token()
            .filter(|node| node.kind() == SyntaxKind::LIT_STRING)
            .map(|token| Self(token.clone()))
            .ok_or_else(|| AstError::InvalidCast)
    }
    fn syntax(&self) -> crate::SyntaxElement {
        SyntaxElement::Token(self.0.clone())
    }
}
#[derive(Debug)]
pub struct LitChar(SyntaxToken);
impl AstElement for LitChar {
    fn cast(element: SyntaxElement) -> Result<Self, AstError> {
        element
            .as_token()
            .filter(|node| node.kind() == SyntaxKind::LIT_CHAR)
            .map(|token| Self(token.clone()))
            .ok_or_else(|| AstError::InvalidCast)
    }
    fn syntax(&self) -> SyntaxElement {
        SyntaxElement::Token(self.0.clone())
    }
}
#[derive(Debug)]
pub struct LitInt(SyntaxToken);
impl AstElement for LitInt {
    fn cast(element: SyntaxElement) -> Result<Self, AstError> {
        element
            .as_token()
            .filter(|node| node.kind() == SyntaxKind::LIT_INTEGER)
            .map(|token| Self(token.clone()))
            .ok_or_else(|| AstError::InvalidCast)
    }
    fn syntax(&self) -> SyntaxElement {
        SyntaxElement::Token(self.0.clone())
    }
}
#[derive(Debug)]
pub struct LitFloat(SyntaxToken);
impl AstElement for LitFloat {
    fn cast(element: SyntaxElement) -> Result<Self, AstError> {
        element
            .as_token()
            .filter(|node| node.kind() == SyntaxKind::LIT_FLOAT)
            .map(|token| Self(token.clone()))
            .ok_or_else(|| AstError::InvalidCast)
    }
    fn syntax(&self) -> SyntaxElement {
        SyntaxElement::Token(self.0.clone())
    }
}
