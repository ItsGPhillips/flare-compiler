pub mod ast;
mod builder;
mod grammar;
mod macros;

use crate::ast::{nodes::Module, AstElement};
use builder::SyntaxTreeBuilder;
use std::{num, sync::Arc};
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

pub fn parse_text(src: Arc<str>) -> Module {
    let mut builder = SyntaxTreeBuilder::new(src);
    builder.parse_module();

    for error in std::mem::take(&mut builder.errors) {
        println!("{:#?}", error);
    }

    let root = SyntaxNode::new_root(builder.finish());
    print_tree_recurssive(root.clone(), "".into(), false, true);
    Module::cast(root.into()).unwrap()
}

pub fn print_tree_recurssive(
    node: SyntaxNode,
    mut indent: String,
    is_last: bool,
    is_root: bool,
) {
    use owo_colors::*;

    if !is_root {
        print!("{indent}");
        if is_last {
            print!(" └─");
            indent += "  ";
        } else {
            print!(" ├─");
            indent += " │";
        }
    }

    let num_children = node.children_with_tokens().count();
    if node.kind().is_error() {
        println!("{}[{}]", node.kind().fg_rgb::<224, 96, 92>(), num_children);
    } else {
        println!("{}[{}]", node.kind().fg_rgb::<235, 232, 61>(), num_children);
    }

    for (idx, element) in node.children_with_tokens().enumerate() {
        match element {
            SyntaxElement::Node(node) => {
                print_tree_recurssive(node, indent.clone(), num_children == idx + 1, false);
            }
            SyntaxElement::Token(token) => {
                let corner = if num_children == idx + 1 {
                    " └─"
                } else {
                    " ├─"
                };
                println!(
                    "{indent}{corner}{} {}",
                    token.kind().fg_rgb::<66, 179, 167>(),
                    token.text().fg_rgb::<230, 196, 85>(),
                )
            }
        }
    }
}
