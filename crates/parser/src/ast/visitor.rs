use std::cell::RefCell;

use super::{
    nodes::{
        BlockExpr, EnumItem, Expr, FnItem, FnParamList, FnParameter, FnSig, ImportItem, Item,
        StructItem,
    },
    AstElement, AstError,
};

#[derive(Default)]
pub struct TestVisitor {
    fn_name: RefCell<String>,
}

impl Visitor for TestVisitor {
    fn visit_fn_signature(&self, sig: FnSig) -> Result<(), AstError> {
        self.fn_name.swap(&RefCell::new(sig.name()?.to_string()));
        self.visit_fn_param_list(sig.param_list()?)
    }
    fn visit_fn_parameter(&self, param: FnParameter) -> Result<(), AstError> {
        // println!("{} - {}", self.fn_name.borrow(), param.name()?);
        Ok(())
    }
    fn visit_expression(&self, expr: Expr) -> Result<(), AstError> {
        Ok(())
    }
}

pub trait Visitor
where
    Self: Sized,
{
    fn visit_fn_item(&self, item: FnItem) -> Result<(), AstError> {
        self.visit_fn_signature(item.signature()?)?;
        self.visit_block_expr(item.block_expr()?)?;
        Ok(())
    }
    fn visit_fn_signature(&self, sig: FnSig) -> Result<(), AstError> {
        self.visit_fn_param_list(sig.param_list()?)
    }
    fn visit_fn_param_list(&self, sig: FnParamList) -> Result<(), AstError> {
        sig.parameters().for_each(|param| {
            let _ = self.visit_fn_parameter(param);
        });
        Ok(())
    }
    fn visit_fn_parameter(&self, _param: FnParameter) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_block_expr(&self, block: BlockExpr) -> Result<(), AstError> {
        for stmt in block.statements() {
            stmt.visit(self)?;
        }
        Ok(())
    }
    fn visit_struct_item(&self, item: StructItem) {}
    fn visit_enum_item(&self, item: EnumItem) {}
    fn visit_import_item(&self, item: ImportItem) {}

    fn visit_expression(&self, expr: Expr) -> Result<(), AstError> {
        Ok(())
    }

    fn visit_item(&self, item: Item) {}
}

impl<T: Visitor> Visitor for &T {
    fn visit_block_expr(&self, block: BlockExpr) -> Result<(), AstError> {
        T::visit_block_expr(&self, block)
    }
    fn visit_expression(&self, expr: Expr) -> Result<(), AstError> {
        T::visit_expression(&self, expr)
    }
    fn visit_fn_item(&self, item: FnItem) -> Result<(), AstError> {
        T::visit_fn_item(&self, item)
    }
    fn visit_fn_signature(&self, sig: FnSig) -> Result<(), AstError> {
        T::visit_fn_signature(&self, sig)
    }
    fn visit_fn_param_list(&self, sig: FnParamList) -> Result<(), AstError> {
        T::visit_fn_param_list(&self, sig)
    }
    fn visit_fn_parameter(&self, param: FnParameter) -> Result<(), AstError> {
        T::visit_fn_parameter(&self, param)
    }
    fn visit_struct_item(&self, item: StructItem) {
        T::visit_struct_item(&self, item)
    }
    fn visit_enum_item(&self, item: EnumItem) {
        T::visit_enum_item(&self, item)
    }
    fn visit_import_item(&self, item: ImportItem) {
        T::visit_import_item(&self, item)
    }
    fn visit_item(&self, item: Item) {
        T::visit_item(&self, item)
    }
}
