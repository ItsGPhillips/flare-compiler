pub mod scope;

use interner::{impl_internable, Interned}
use std::{
    borrow::Cow,
    hash::{Hash, Hasher},
};

use ahash::AHasher;
use dashmap::DashMap;
use diagnostics::{Diagnostic, DiagnosticLevel, TextRange};
use parser::{
    ast::{
        ast::{AstNode, AstPtr},
        node::{BlockExpr, FunctionItem, FunctionSig, Module, Path},
        visitor::{base, Visitor},
    },
    GreenNode, SyntaxNode, SyntaxNodePtr,
};
use scopeguard::guard;

pub struct InternableAstPtr<N: AstNode>(AstPtr<N>);
impl<N: AstNode> std::ops::Deref for InternableAstPtr<N> {
     
}

impl_internable!(InternableAstPtr<N>);

#[derive(Debug, PartialEq, Eq, Hash)]
struct HirPath(SyntaxNodePtr);
impl_internable!(HirPath);

struct ScopeState {
    id: usize,
    variables: im::HashMap<Interned<str>, ()>,
    path_ids: Vec<u64>,
}

pub struct HirBuilder {
    sb: scope::Builder,
    root: GreenNode,
    //TODO: struct/types
    scopes: DashMap<usize, ScopeState>,
    blocks: DashMap<usize, ()>,
}

pub struct HirTy {
    span: TextRange,
    path: Interned<AstPtr<Path>>,
}

impl HirBuilder {
    pub fn new(root: GreenNode) -> Self {
        Self {
            sb: scope::Builder::default(),
            scopes: Default::default(),
            blocks: Default::default(),
            root,
        }
    }
}

impl Visitor for HirBuilder {
    fn visit_module(&mut self, module: Module) {
        self.sb.push_scope();
        let mut v = guard(self, |mn| {
            mn.sb.pop_scope();
        });
        base::visit_module(module, &mut *v)
    }

    fn visit_fn_item(&mut self, item: FunctionItem) {
        let ptr = SyntaxNodePtr::new(item.syntax());
        visit_function(ptr, self.root.clone(), self.sb.current_scope_id());
    }

    fn visit_path(&mut self, path: Path) {
        path.create_id();
    }

    fn visit_block_expr(&mut self, block: BlockExpr) {
        self.sb.push_scope();
        let mut v = guard(self, |mn| {
            mn.sb.pop_scope();
        });
        base::visit_block_expr(block, &mut *v)
    }
}

pub trait HirId {
    fn create_id(&self) -> u64;
}

impl HirId for Path {
    fn create_id(&self) -> u64 {
        let mut hasher = AHasher::default();
        self.segments().for_each(|s| s.hash(&mut hasher));
        hasher.finish()
    }
}

fn register_fn_signature(sig: &FunctionSig, scope_id: usize) {
    let mut hasher = AHasher::default();
    sig.name().hash(&mut hasher);

    if let Some(pl) = sig.param_list() {
        pl.parameters().map(|parameter| {
            let id = parameter
                .param_type()
                .and_then(|ty| ty.path())
                .map(|path| path.create_id());
        });
    }
}

async fn visit_function(syntax_ptr: SyntaxNodePtr, root: GreenNode, scope_id: usize) {
    use futures::join;

    let sig_fut = (syntax_ptr.clone(), root.clone());
    let block_fut = (syntax_ptr.clone(), root.clone());

    let signature = executor::spawn(async move {
        let (sptr, root) = sig_fut;
        let root = SyntaxNode::new_root(root).into();
        let fn_item = FunctionItem::cast(sptr.to_node(&root).into()).unwrap();

        let sig = fn_item.signature();
        if let Some(sig) = sig {
            register_fn_signature(&sig, scope_id);
        } else {
            diagnostics::report(Diagnostic {
                level: DiagnosticLevel::FATAL,
                title: Cow::Borrowed("Compiler Bug"),
                message: Cow::Borrowed("Function Item did not have a signature"),
                span: fn_item.syntax().text_range(),
            });
        };
    });

    let block = executor::spawn(async move {
        let (sptr, root) = block_fut;
        let root = SyntaxNode::new_root(root).into();
        let fn_item = FunctionItem::cast(sptr.to_node(&root).into()).unwrap();

        let block = if let Some(block) = fn_item.block_expr() {
            // TODO
        } else {
            diagnostics::report(Diagnostic {
                level: DiagnosticLevel::FATAL,
                title: Cow::Borrowed("Compiler Bug"),
                message: Cow::Borrowed("Function Item did not have a body"),
                span: fn_item.syntax().text_range(),
            });
        };
    });

    let (_, _) = join!(signature, block);
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
