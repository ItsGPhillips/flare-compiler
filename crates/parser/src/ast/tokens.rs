use syntax::SyntaxKind;

use crate::{ast_token, SyntaxElement, SyntaxToken};

use super::{AstElement, AstError};
pub trait AstToken: Sized {
    fn can_cast(kind: crate::SyntaxKind) -> bool;
    fn cast(syntax: crate::SyntaxToken) -> Option<Self>;
    fn syntax(&self) -> &crate::SyntaxToken;
}

ast_token!(Ident, IDENTIFIER);

ast_token!(LitInteger, LIT_INTEGER);
ast_token!(LitFloat, LIT_FLOAT);
ast_token!(LitString, LIT_STRING);
ast_token!(LitChar, LIT_CHAR);
