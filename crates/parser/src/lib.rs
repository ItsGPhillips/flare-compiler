pub mod ast;
mod builder;
mod grammar;
mod macros;

use std::sync::Arc;

use builder::SyntaxTreeBuilder;
use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Flare {}
impl rowan::Language for Flare {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_raw(raw.0)
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_raw())
    }
}

type SyntaxNode = rowan::SyntaxNode<Flare>;
type SyntaxToken = rowan::SyntaxToken<Flare>;
type SyntaxElement = rowan::SyntaxElement<Flare>;

pub fn parse_text(src: Arc<str>) -> SyntaxNode {
    let mut builder = SyntaxTreeBuilder::new(src);
    builder.parse_path();
    SyntaxNode::new_root(builder.finish())
}

pub fn print_syntax_tree(src: Arc<str>) {
    let node = parse_text(src);
    println!("{:#?}", node)
}
