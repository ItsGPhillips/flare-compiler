use thiserror::Error;

use crate::{SyntaxElement, SyntaxNode};

pub mod nodes;
pub mod tokens;

#[derive(Error, Debug)]
pub enum AstError {
    #[error("Invalid cast")]
    InvalidCast,

    #[error("Unexpected EOF")]
    UnexpectedEOF,
}

pub trait AstElement: Sized {
    fn cast(element: SyntaxElement) -> Result<Self, AstError>;
    fn syntax(&self) -> SyntaxElement;
}

pub(crate) mod utils {
    use super::nodes;

    pub fn children<T: crate::ast::AstElement>(
        node: &crate::SyntaxNode,
    ) -> impl Iterator<Item = T> {
        node.children_with_tokens()
            .filter(|e| !e.kind().is_whitespace() && !e.kind().is_error())
            .filter_map(|e| T::cast(e).ok())
    }

    fn errors(node: &crate::SyntaxNode) -> impl Iterator<Item = nodes::Error> {
        children::<nodes::Error>(node)
    }
}
