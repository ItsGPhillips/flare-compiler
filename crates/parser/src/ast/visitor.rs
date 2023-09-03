use super::{
    node::{
        BinopAdd, BinopAddAssign, BinopAnd, BinopBitAnd, BinopBitOr, BinopDiv, BinopDivAssign,
        BinopEq, BinopGT, BinopGTE, BinopLT, BinopLTE, BinopMod, BinopMul, BinopMulAssign,
        BinopNotEq, BinopOr, BinopShiftL, BinopShiftLAssign, BinopShiftR, BinopShiftRAssign,
        BinopSub, BinopSubAssign, BlockExpr, DerefExpr, EnumItem, Expr, FunctionItem, FunctionSig,
        ImportItem, Item, Module, NegExpr, NotExpr, Parameter, ParameterList, Path, RefExpr,
        Statement, StructItem, Type, TypeArray, TypeBinding, TypeNamed, TypeTuple, TypeUnnamed,
    },
    tokens::{LitChar, LitFloat, LitInteger, LitString},
};

pub trait Visitor
where
    Self: Sized,
{
    fn visit_statement(&mut self, stmt: Statement) {
        base::visit_statement(stmt, self)
    }
    fn visit_module(&mut self, module: Module) {
        base::visit_module(module, self)
    }
    fn visit_fn_item(&mut self, item: FunctionItem) {
        base::visit_fn_item(item, self)
    }
    fn visit_fn_signature(&mut self, sig: FunctionSig) {
        base::visit_fn_signature(sig, self)
    }
    fn visit_fn_param_list(&mut self, pl: ParameterList) {
        base::visit_fn_param_list(pl, self)
    }
    fn visit_fn_parameter(&mut self, _param: Parameter) {}
    fn visit_struct_item(&mut self, item: StructItem) {}
    fn visit_enum_item(&mut self, item: EnumItem) {}
    fn visit_import_item(&mut self, item: ImportItem) {}
    fn visit_item(&mut self, item: Item) {
        match item {
            Item::Function(item) => self.visit_fn_item(item),
            Item::Struct(_) => todo!(),
            Item::Enum(_) => todo!(),
            Item::Import(_) => todo!(),
        }
    }

    fn visit_type_named(&mut self, ty: TypeNamed) {}
    fn visit_type_unnamed(&mut self, ty: TypeUnnamed) {}
    fn visit_type_tuple(&mut self, ty: TypeTuple) {}
    fn visit_type_array(&mut self, ty: TypeArray) {}
    fn visit_type(&mut self, ty: Type) {
        base::visit_type(ty, self)
    }
    fn visit_type_binding(&mut self, tb: TypeBinding) {
        println!("visit_type_binding");
        base::visit_type_binding(tb, self)
    }
    fn visit_path(&mut self, path: Path) {
        base::visit_path(path, self)
    }
    // -------------------------------------------------------------------------
    fn visit_expr(&mut self, expr: Expr) {
        base::visit_expr(expr, self)
    }
    fn visit_block_expr(&mut self, block: BlockExpr) {
        base::visit_block_expr(block, self)
    }
    fn visit_string_expr(&mut self, _ls: LitString) {}
    fn visit_char_expr(&mut self, _lc: LitChar) {}
    fn visit_int_expr(&mut self, _li: LitInteger) {}
    fn visit_float_expr(&mut self, _lf: LitFloat) {}
    fn visit_ref_expr(&mut self, _e: RefExpr) {}
    fn visit_not_expr(&mut self, _e: NotExpr) {}
    fn visit_deref_expr(&mut self, _e: DerefExpr) {}
    fn visit_neg_expr(&mut self, _e: NegExpr) {}
    fn visit_add_expr(&mut self, _e: BinopAdd) {}
    fn visit_sub_expr(&mut self, _e: BinopSub) {}
    fn visit_mul_expr(&mut self, _e: BinopMul) {}
    fn visit_div_expr(&mut self, _e: BinopDiv) {}
    fn visit_mod_expr(&mut self, _e: BinopMod) {}
    fn visit_eq_expr(&mut self, _e: BinopEq) {}
    fn visit_neq_expr(&mut self, _e: BinopNotEq) {}
    fn visit_and_expr(&mut self, _e: BinopAnd) {}
    fn visit_or_expr(&mut self, _e: BinopOr) {}
    fn visit_gt_expr(&mut self, _e: BinopGT) {}
    fn visit_gte_expr(&mut self, _e: BinopGTE) {}
    fn visit_lt_expr(&mut self, _e: BinopLT) {}
    fn visit_lte_expr(&mut self, _e: BinopLTE) {}
    fn visit_bitor_expr(&mut self, _e: BinopBitOr) {}
    fn visit_bitand_expr(&mut self, _e: BinopBitAnd) {}
    fn visit_add_assign_expr(&mut self, _e: BinopAddAssign) {}
    fn visit_sub_assign_expr(&mut self, _e: BinopSubAssign) {}
    fn visit_div_assign_expr(&mut self, _e: BinopDivAssign) {}
    fn visit_mul_assign_expr(&mut self, _e: BinopMulAssign) {}
    fn visit_shift_l_expr(&mut self, _e: BinopShiftL) {}
    fn visit_shift_r_expr(&mut self, _e: BinopShiftR) {}
    fn visit_shift_l_assign_expr(&mut self, _e: BinopShiftLAssign) {}
    fn visit_shift_r_assign_expr(&mut self, _e: BinopShiftRAssign) {}
}

pub mod base {
    use crate::ast::node::Statement;

    use super::*;

    pub fn visit_module(module: Module, mut visitor: impl Visitor) {
        for item in module.items() {
            visitor.visit_item(item);
        }
    }

    pub fn visit_expr(expr: Expr, mut v: impl Visitor) {
        match expr {
            Expr::String(e) => v.visit_string_expr(e),
            Expr::Char(e) => v.visit_char_expr(e),
            Expr::Integer(e) => v.visit_int_expr(e),
            Expr::Float(e) => v.visit_float_expr(e),
            Expr::UnopRef(e) => v.visit_ref_expr(e),
            Expr::UnopNot(e) => v.visit_not_expr(e),
            Expr::UnopDeref(e) => v.visit_deref_expr(e),
            Expr::UnopNeg(e) => v.visit_neg_expr(e),
            Expr::BinopAdd(e) => v.visit_add_expr(e),
            Expr::BinopSub(e) => v.visit_sub_expr(e),
            Expr::BinopMul(e) => v.visit_mul_expr(e),
            Expr::BinopDiv(e) => v.visit_div_expr(e),
            Expr::BinopMod(e) => v.visit_mod_expr(e),
            Expr::BinopEq(e) => v.visit_eq_expr(e),
            Expr::BinopNotEq(e) => v.visit_neq_expr(e),
            Expr::BinopAnd(e) => v.visit_and_expr(e),
            Expr::BinopOr(e) => v.visit_or_expr(e),
            Expr::BinopGT(e) => v.visit_gt_expr(e),
            Expr::BinopGTE(e) => v.visit_gte_expr(e),
            Expr::BinopLT(e) => v.visit_lt_expr(e),
            Expr::BinopLTE(e) => v.visit_lte_expr(e),
            Expr::BinopBitOr(e) => v.visit_bitor_expr(e),
            Expr::BinopBitAnd(e) => v.visit_bitand_expr(e),
            Expr::BinopAddAssign(e) => v.visit_add_assign_expr(e),
            Expr::BinopSubAssign(e) => v.visit_sub_assign_expr(e),
            Expr::BinopDivAssign(e) => v.visit_div_assign_expr(e),
            Expr::BinopMulAssign(e) => v.visit_mul_assign_expr(e),
            Expr::BinopShiftL(e) => v.visit_shift_l_expr(e),
            Expr::BinopShiftR(e) => v.visit_shift_r_expr(e),
            Expr::BinopShiftLAssign(e) => v.visit_shift_l_assign_expr(e),
            Expr::BinopShiftRAssign(e) => v.visit_shift_r_assign_expr(e),
            _ => todo!(),
        }
    }
    pub fn visit_block_expr(block: BlockExpr, mut visitor: impl Visitor) {
        for stmt in block.statements() {
            visitor.visit_statement(stmt);
        }
    }
    pub fn visit_fn_param_list(pl: ParameterList, mut visitor: impl Visitor) {
        pl.parameters().for_each(|param| {
            visitor.visit_fn_parameter(param);
        });
    }
    pub fn visit_fn_signature(sig: FunctionSig, mut visitor: impl Visitor) {
        sig.param_list().map(|pl| visitor.visit_fn_param_list(pl));
        sig.return_type()
            .map(|rtty| visitor.visit_type_binding(rtty));
    }
    pub fn visit_type(ty: Type, mut visitor: impl Visitor) {
        match ty {
            Type::Named(n) => visitor.visit_type_named(n),
            Type::Unnamed(n) => visitor.visit_type_unnamed(n),
            Type::Tuple(n) => visitor.visit_type_tuple(n),
            Type::Array(n) => visitor.visit_type_array(n),
        }
    }
    pub fn visit_fn_item(item: FunctionItem, mut visitor: impl Visitor) {
        item.signature().map(|sig| visitor.visit_fn_signature(sig));
        item.block_expr().map(|expr| visitor.visit_block_expr(expr));
    }
    pub fn visit_type_binding(tb: TypeBinding, mut visitor: impl Visitor) {
        if let Some(ty) = tb.btype() {
            visitor.visit_type(ty)
        }
    }
    pub fn visit_path(path: Path, visitor: impl Visitor) {}

    pub fn visit_statement(stmt: Statement, visitor: impl Visitor) {}
}

impl<T: Visitor> Visitor for &mut T {
    fn visit_module(&mut self, module: Module) {
        T::visit_module(self, module)
    }
    fn visit_statement(&mut self, stmt: Statement) {
        T::visit_statement(self, stmt)
    }
    fn visit_block_expr(&mut self, block: BlockExpr) {
        T::visit_block_expr(self, block)
    }
    fn visit_fn_item(&mut self, item: FunctionItem) {
        T::visit_fn_item(self, item)
    }
    fn visit_fn_signature(&mut self, sig: FunctionSig) {
        T::visit_fn_signature(self, sig)
    }
    fn visit_fn_param_list(&mut self, sig: ParameterList) {
        T::visit_fn_param_list(self, sig)
    }
    fn visit_fn_parameter(&mut self, param: Parameter) {
        T::visit_fn_parameter(self, param)
    }
    fn visit_struct_item(&mut self, item: StructItem) {
        T::visit_struct_item(self, item)
    }
    fn visit_enum_item(&mut self, item: EnumItem) {
        T::visit_enum_item(self, item)
    }
    fn visit_import_item(&mut self, item: ImportItem) {
        T::visit_import_item(self, item)
    }
    fn visit_item(&mut self, item: Item) {
        T::visit_item(self, item)
    }
    fn visit_type_named(&mut self, ty: TypeNamed) {
        T::visit_type_named(self, ty)
    }
    fn visit_type_unnamed(&mut self, ty: TypeUnnamed) {
        T::visit_type_unnamed(self, ty)
    }
    fn visit_type_tuple(&mut self, ty: TypeTuple) {
        T::visit_type_tuple(self, ty)
    }
    fn visit_type_array(&mut self, ty: TypeArray) {
        T::visit_type_array(self, ty)
    }
    fn visit_type(&mut self, ty: Type) {
        T::visit_type(self, ty)
    }
    fn visit_type_binding(&mut self, tb: TypeBinding) {
        T::visit_type_binding(self, tb)
    }
    fn visit_path(&mut self, path: Path) {
        T::visit_path(self, path)
    }
    fn visit_expr(&mut self, expr: Expr) {
        T::visit_expr(self, expr)
    }
    fn visit_string_expr(&mut self, ls: LitString) {
        T::visit_string_expr(self, ls)
    }
    fn visit_char_expr(&mut self, lc: LitChar) {
        T::visit_char_expr(self, lc)
    }
    fn visit_int_expr(&mut self, li: LitInteger) {
        T::visit_int_expr(self, li)
    }
    fn visit_float_expr(&mut self, lf: LitFloat) {
        T::visit_float_expr(self, lf)
    }
    fn visit_ref_expr(&mut self, e: RefExpr) {
        T::visit_ref_expr(self, e)
    }
    fn visit_not_expr(&mut self, e: NotExpr) {
        T::visit_not_expr(self, e)
    }
    fn visit_deref_expr(&mut self, e: DerefExpr) {
        T::visit_deref_expr(self, e)
    }
    fn visit_neg_expr(&mut self, e: NegExpr) {
        T::visit_neg_expr(self, e)
    }
    fn visit_add_expr(&mut self, e: BinopAdd) {
        T::visit_add_expr(self, e)
    }
    fn visit_sub_expr(&mut self, e: BinopSub) {
        T::visit_sub_expr(self, e)
    }
    fn visit_mul_expr(&mut self, e: BinopMul) {
        T::visit_mul_expr(self, e)
    }
    fn visit_div_expr(&mut self, e: BinopDiv) {
        T::visit_div_expr(self, e)
    }
    fn visit_mod_expr(&mut self, e: BinopMod) {
        T::visit_mod_expr(self, e)
    }
    fn visit_eq_expr(&mut self, e: BinopEq) {
        T::visit_eq_expr(self, e)
    }
    fn visit_neq_expr(&mut self, e: BinopNotEq) {
        T::visit_neq_expr(self, e)
    }
    fn visit_and_expr(&mut self, e: BinopAnd) {
        T::visit_and_expr(self, e)
    }
    fn visit_or_expr(&mut self, e: BinopOr) {
        T::visit_or_expr(self, e)
    }
    fn visit_gt_expr(&mut self, e: BinopGT) {
        T::visit_gt_expr(self, e)
    }
    fn visit_gte_expr(&mut self, e: BinopGTE) {
        T::visit_gte_expr(self, e)
    }
    fn visit_lt_expr(&mut self, e: BinopLT) {
        T::visit_lt_expr(self, e)
    }
    fn visit_lte_expr(&mut self, e: BinopLTE) {
        T::visit_lte_expr(self, e)
    }
    fn visit_bitor_expr(&mut self, e: BinopBitOr) {
        T::visit_bitor_expr(self, e)
    }
    fn visit_bitand_expr(&mut self, e: BinopBitAnd) {
        T::visit_bitand_expr(self, e)
    }
    fn visit_add_assign_expr(&mut self, e: BinopAddAssign) {
        T::visit_add_assign_expr(self, e)
    }
    fn visit_sub_assign_expr(&mut self, e: BinopSubAssign) {
        T::visit_sub_assign_expr(self, e)
    }
    fn visit_div_assign_expr(&mut self, e: BinopDivAssign) {
        T::visit_div_assign_expr(self, e)
    }
    fn visit_mul_assign_expr(&mut self, e: BinopMulAssign) {
        T::visit_mul_assign_expr(self, e)
    }
    fn visit_shift_l_expr(&mut self, e: BinopShiftL) {
        T::visit_shift_l_expr(self, e)
    }
    fn visit_shift_r_expr(&mut self, e: BinopShiftR) {
        T::visit_shift_r_expr(self, e)
    }
    fn visit_shift_l_assign_expr(&mut self, e: BinopShiftLAssign) {
        T::visit_shift_l_assign_expr(self, e)
    }
    fn visit_shift_r_assign_expr(&mut self, e: BinopShiftRAssign) {
        T::visit_shift_r_assign_expr(self, e)
    }
}
