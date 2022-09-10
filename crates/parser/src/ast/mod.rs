pub mod tokens;

use crate::{ast_node, SyntaxNode};
use rowan::ast::{support, AstNode};
use syntax::SyntaxKind;

use self::tokens::{AstToken, Ident};

ast_node!(Path, PATH);
impl Path {
    pub fn segments(&self) -> impl Iterator<Item = PathSegmentNamed> {
        self.children().filter_map(PathSegmentNamed::cast)
    }
}
ast_node!(PathSegmentNamed, PATH_SEGMENT_NAMED);
impl PathSegmentNamed {
    pub fn ident(&self) -> Ident {
        support::token(self, SyntaxKind::IDENTIFIER)
            .and_then(Ident::cast)
            .expect("Invalid PathSegmentNamed")
    }
}
ast_node!(PathSeperator, PATH_SEPERATOR);
