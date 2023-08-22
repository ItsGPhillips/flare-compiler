mod scope_builder;
pub mod scope_id;

use std::hash::Hash;

use diagnostics::span::Span;
use parser::{
    ast::{
        self,
        node::{BlockExpr, FunctionItem, Module, Path},
        visitor::{base, Visitor},
        AstElement,
    },
    GreenNode, SyntaxNode, SyntaxNodePtr,
};
use scopeguard::guard;

#[derive(Default)]
struct ScopeBuilder {
    current: usize,
    stack: Vec<usize>,
}

impl ScopeBuilder {
    pub fn current_scope_id(&self) -> usize {
        self.current
    }
    pub fn push_scope(&mut self) -> usize {
        let current = self.current;
        self.stack.push(current);
        self.current = current + 1;
        current
    }
    pub fn pop_scope(&mut self) -> Option<usize> {
        self.stack.pop()
    }
}

struct HirPath(SyntaxNodePtr);
interner::impl_internable!(HirPath);

pub struct ModuleNormaliser {
    sb: ScopeBuilder,
    root: GreenNode,
}

impl ModuleNormaliser {
    pub fn new(root: GreenNode) -> Self {
        Self {
            sb: ScopeBuilder::default(),
            root,
        }
    }
}

impl Visitor for ModuleNormaliser {
    fn visit_module(&mut self, module: Module) -> Result<(), ast::AstError> {
        self.sb.push_scope();
        let mut v = guard(self, |mn| {
            mn.sb.pop_scope();
        });
        base::visit_module(module, &mut *v)
    }

    fn visit_block_expr(&mut self, block: BlockExpr) -> Result<(), ast::AstError> {
        self.sb.push_scope();
        let mut v = guard(self, |mn| {
            mn.sb.pop_scope();
        });
        base::visit_block_expr(block, &mut *v)
    }

    fn visit_fn_item(&mut self, item: FunctionItem) -> Result<(), ast::AstError> {
        let ptr = SyntaxNodePtr::new(item.syntax().as_node().unwrap());
        visit_function(ptr, self.root.clone());
        Ok(())
    }

    fn visit_path(&mut self, path: Path) -> Result<(), ast::AstError> {
        use std::hash::Hasher;

        Ok(())
    }
}

fn visit_function(syntax_ptr: SyntaxNodePtr, root: GreenNode) {
    executor::spawn(async move {
        let root = SyntaxNode::new_root(root).into();
        let fn_item = FunctionItem::cast(syntax_ptr.to_node(&root).into()).unwrap();
        println!("{:#?}", fn_item);
    })
    .detach();
}

#[cfg(test)]
mod tests {
    use parser::{ast::node::Item, parse_source_file};
    use std::sync::Arc;

    use super::*;

    #[test]
    fn it_works() {
        let src: Arc<str> = Arc::from("fn test() {}");
        let (module, _root, _diagnostics) = parse_source_file(src.clone());

        for item in module.items() {
            match item {
                Item::Function(i) => {}
                _ => unimplemented!(),
            }
        }
    }
}
