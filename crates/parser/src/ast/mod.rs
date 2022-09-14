use thiserror::Error;

use crate::SyntaxElement;

pub mod nodes;
pub mod tokens;
pub mod visitor;

#[derive(Error, Debug)]
pub enum AstError {
    #[error("Invalid cast")]
    InvalidCast,

    #[error("Unexpected EOF")]
    UnexpectedEOF,
}

pub trait AstElement: Sized {
    fn can_cast(element: ::syntax::SyntaxKind) -> bool;
    fn cast(element: SyntaxElement) -> Result<Self, AstError>;
    fn syntax(&self) -> SyntaxElement;
}

pub(crate) mod utils {
    use crate::SyntaxToken;

    use super::nodes;

    pub fn children<T: crate::ast::AstElement>(
        node: &crate::SyntaxNode,
    ) -> impl Iterator<Item = T> {
        node.children_with_tokens()
            .filter(|e| !e.kind().is_whitespace() && !e.kind().is_error())
            .filter_map(|e| T::cast(e).ok())
    }

    pub fn token(
        node: &crate::SyntaxNode,
        kind: syntax::SyntaxKind,
    ) -> impl Iterator<Item = SyntaxToken> {
        node.children_with_tokens()
            .filter_map(|child| child.as_token().cloned())
            .filter(|token| !token.kind().is_whitespace())
            .filter(move |token| token.kind() == kind)
    }

    pub fn errors(node: &crate::SyntaxNode) -> impl Iterator<Item = nodes::Error> {
        children::<nodes::Error>(node)
    }
}
