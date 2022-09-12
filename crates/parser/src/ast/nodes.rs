use super::{tokens::LitString, AstElement, AstError};
use crate::{
    ast::tokens::{LitChar, LitFloat, LitInt},
    ast_node, SyntaxNode, impl_expr_from,
};
use syntax::SyntaxKind;

ast_node!(Module, MODULE);
ast_node!(Path, PATH);

ast_node!(RefExpr, UNOP_REF);
ast_node!(NotExpr, UNOP_NOT);
ast_node!(DerefExpr, UNOP_DEREF);
ast_node!(NegExpr, UNOP_NEG);

impl_expr_from! {
    LitString => String,
    LitChar   => Char,
    LitInt    => Integer,
    LitFloat  => Float,
    Path      => Path,
    RefExpr   => UnopRef,
    NotExpr   => UnopNot,
    DerefExpr => UnopDeref,
    NegExpr   => UnopNeg,
}


#[derive(Debug)]
pub enum Expr {
    String(LitString),
    Char(LitChar),
    Integer(LitInt),
    Float(LitFloat),
    Path(Path),
    UnopRef(RefExpr),
    UnopNot(NotExpr),
    UnopDeref(DerefExpr),
    UnopNeg(NegExpr),
}

impl AstElement for Expr {
    #[rustfmt::skip]
    fn cast(element: crate::SyntaxElement) -> Result<Self, AstError> {
        use SyntaxKind::*;
        match element.kind() {
            LIT_STRING  => Ok(LitString::cast(element)?.into()),
            LIT_CHAR    => Ok(LitChar::cast(element)?.into()),
            LIT_INTEGER => Ok(LitInt::cast(element)?.into()),
            LIT_FLOAT   => Ok(LitFloat::cast(element)?.into()),
            PATH        => Ok(Path::cast(element)?.into()),
            UNOP_NOT    => Ok(NotExpr::cast(element)?.into()),
            UNOP_REF    => Ok(RefExpr::cast(element)?.into()),
            UNOP_DEREF  => Ok(DerefExpr::cast(element)?.into()),
            UNOP_NEG    => Ok(NegExpr::cast(element)?.into()),
            _           => Err(AstError::InvalidCast),
        }
    }
    #[rustfmt::skip]
    fn syntax(&self) -> crate::SyntaxElement {
        match self {
            Expr::String(token)  => token.syntax(),
            Expr::Char(token)    => token.syntax(),
            Expr::Integer(token) => token.syntax(),
            Expr::Float(token)   => token.syntax(),
            Expr::Path(node)     => node.syntax(),
            Expr::UnopRef(node)  => node.syntax(),
            Expr::UnopNot(node)  => node.syntax(),
            Expr::UnopDeref(node) => node.syntax(),
            Expr::UnopNeg(node)  => node.syntax(),
        }
    }
}


