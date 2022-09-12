use thiserror::Error;

use crate::SyntaxElement;

pub mod nodes;
pub mod tokens;

#[derive(Error, Debug)]
pub enum AstError {
    #[error("Invalid cast")]
    InvalidCast,
}

pub trait AstElement: Sized {
    fn cast(element: SyntaxElement) -> Result<Self, AstError>;
    fn syntax(&self) -> SyntaxElement;
}
