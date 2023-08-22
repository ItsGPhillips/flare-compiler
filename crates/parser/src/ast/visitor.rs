use super::{
    node::{
        BinopAdd, BinopAddAssign, BinopAnd, BinopBitAnd, BinopBitOr, BinopDiv, BinopDivAssign,
        BinopEq, BinopGT, BinopGTE, BinopLT, BinopLTE, BinopMod, BinopMul, BinopMulAssign,
        BinopNotEq, BinopOr, BinopShiftL, BinopShiftLAssign, BinopShiftR, BinopShiftRAssign,
        BinopSub, BinopSubAssign, BlockExpr, DerefExpr, EnumItem, Expr, FunctionItem, FunctionSig,
        ImportItem, Item, Module, NegExpr, NotExpr, Parameter, ParameterList, Path, RefExpr,
        StructItem, Type, TypeArray, TypeBinding, TypeNamed, TypeTuple, TypeUnnamed,
    },
    tokens::{LitChar, LitFloat, LitInteger, LitString},
    AstError,
};

pub trait Visitor
where
    Self: Sized,
{
    fn visit_module(&mut self, module: Module) -> Result<(), AstError> {
        base::visit_module(module, self)
    }
    fn visit_fn_item(&mut self, item: FunctionItem) -> Result<(), AstError> {
        base::visit_fn_item(item, self)
    }
    fn visit_fn_signature(&mut self, sig: FunctionSig) -> Result<(), AstError> {
        base::visit_fn_signature(sig, self)
    }
    fn visit_fn_param_list(&mut self, pl: ParameterList) -> Result<(), AstError> {
        base::visit_fn_param_list(pl, self)
    }
    fn visit_fn_parameter(&mut self, _param: Parameter) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_struct_item(&mut self, item: StructItem) {}
    fn visit_enum_item(&mut self, item: EnumItem) {}
    fn visit_import_item(&mut self, item: ImportItem) {}
    fn visit_item(&mut self, item: Item) -> Result<(), AstError> {
        match item {
            Item::Function(item) => self.visit_fn_item(item),
            Item::Struct(_) => todo!(),
            Item::Enum(_) => todo!(),
            Item::Import(_) => todo!(),
        }
    }

    fn visit_type_named(&mut self, ty: TypeNamed) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_type_unnamed(&mut self, ty: TypeUnnamed) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_type_tuple(&mut self, ty: TypeTuple) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_type_array(&mut self, ty: TypeArray) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_type(&mut self, ty: Type) -> Result<(), AstError> {
        base::visit_type(ty, self)
    }
    fn visit_type_binding(&mut self, tb: TypeBinding) -> Result<(), AstError> {
        println!("visit_type_binding");
        base::visit_type_binding(tb, self)
    }
    fn visit_path(&mut self, path: Path) -> Result<(), AstError> {
        base::visit_path(path, self)
    }
    // -------------------------------------------------------------------------
    fn visit_expr(&mut self, expr: Expr) -> Result<(), AstError> {
        base::visit_expr(expr, self)
    }
    fn visit_block_expr(&mut self, block: BlockExpr) -> Result<(), AstError> {
        base::visit_block_expr(block, self)
    }
    fn visit_string_expr(&mut self, _ls: LitString) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_char_expr(&mut self, _lc: LitChar) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_int_expr(&mut self, _li: LitInteger) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_float_expr(&mut self, _lf: LitFloat) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_ref_expr(&mut self, _e: RefExpr) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_not_expr(&mut self, _e: NotExpr) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_deref_expr(&mut self, _e: DerefExpr) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_neg_expr(&mut self, _e: NegExpr) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_add_expr(&mut self, _e: BinopAdd) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_sub_expr(&mut self, _e: BinopSub) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_mul_expr(&mut self, _e: BinopMul) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_div_expr(&mut self, _e: BinopDiv) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_mod_expr(&mut self, _e: BinopMod) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_eq_expr(&mut self, _e: BinopEq) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_neq_expr(&mut self, _e: BinopNotEq) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_and_expr(&mut self, _e: BinopAnd) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_or_expr(&mut self, _e: BinopOr) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_gt_expr(&mut self, _e: BinopGT) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_gte_expr(&mut self, _e: BinopGTE) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_lt_expr(&mut self, _e: BinopLT) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_lte_expr(&mut self, _e: BinopLTE) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_bitor_expr(&mut self, _e: BinopBitOr) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_bitand_expr(&mut self, _e: BinopBitAnd) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_add_assign_expr(&mut self, _e: BinopAddAssign) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_sub_assign_expr(&mut self, _e: BinopSubAssign) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_div_assign_expr(&mut self, _e: BinopDivAssign) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_mul_assign_expr(&mut self, _e: BinopMulAssign) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_shift_l_expr(&mut self, _e: BinopShiftL) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_shift_r_expr(&mut self, _e: BinopShiftR) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_shift_l_assign_expr(&mut self, _e: BinopShiftLAssign) -> Result<(), AstError> {
        Ok(())
    }
    fn visit_shift_r_assign_expr(&mut self, _e: BinopShiftRAssign) -> Result<(), AstError> {
        Ok(())
    }
}

pub mod base {
    use super::*;

    pub fn visit_module(module: Module, mut visitor: impl Visitor) -> Result<(), AstError> {
        for item in module.items() {
            visitor.visit_item(item)?;
        }
        Ok(())
    }

    pub fn visit_expr(expr: Expr, mut v: impl Visitor) -> Result<(), AstError> {
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
    pub fn visit_block_expr(block: BlockExpr, mut visitor: impl Visitor) -> Result<(), AstError> {
        for stmt in block.statements() {
            stmt.visit(&mut visitor)?;
        }
        Ok(())
    }
    pub fn visit_fn_param_list(
        pl: ParameterList,
        mut visitor: impl Visitor,
    ) -> Result<(), AstError> {
        pl.parameters().for_each(|param| {
            let _ = visitor.visit_fn_parameter(param);
        });
        Ok(())
    }
    pub fn visit_fn_signature(sig: FunctionSig, mut visitor: impl Visitor) -> Result<(), AstError> {
        visitor.visit_fn_param_list(sig.param_list()?)?;
        if let Some(tb) = sig.return_type() {
            visitor.visit_type_binding(tb)?
        }
        Ok(())
    }
    pub fn visit_type(ty: Type, mut visitor: impl Visitor) -> Result<(), AstError> {
        match ty {
            Type::Named(n) => visitor.visit_type_named(n),
            Type::Unnamed(n) => visitor.visit_type_unnamed(n),
            Type::Tuple(n) => visitor.visit_type_tuple(n),
            Type::Array(n) => visitor.visit_type_array(n),
        }
    }
    pub fn visit_fn_item(item: FunctionItem, mut visitor: impl Visitor) -> Result<(), AstError> {
        visitor.visit_fn_signature(item.signature()?)?;
        visitor.visit_block_expr(item.block_expr()?)?;
        Ok(())
    }
    pub fn visit_type_binding(tb: TypeBinding, mut visitor: impl Visitor) -> Result<(), AstError> {
        visitor.visit_type(tb.btype()?)
    }
    pub fn visit_path(path: Path, visitor: impl Visitor) -> Result<(), AstError> {
        Ok(())
    }
}

impl<T: Visitor> Visitor for &mut T {
    fn visit_block_expr(&mut self, block: BlockExpr) -> Result<(), AstError> {
        T::visit_block_expr(self, block)
    }
    fn visit_fn_item(&mut self, item: FunctionItem) -> Result<(), AstError> {
        T::visit_fn_item(self, item)
    }
    fn visit_fn_signature(&mut self, sig: FunctionSig) -> Result<(), AstError> {
        T::visit_fn_signature(self, sig)
    }
    fn visit_fn_param_list(&mut self, sig: ParameterList) -> Result<(), AstError> {
        T::visit_fn_param_list(self, sig)
    }
    fn visit_fn_parameter(&mut self, param: Parameter) -> Result<(), AstError> {
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
    fn visit_item(&mut self, item: Item) -> Result<(), AstError> {
        T::visit_item(self, item)
    }
    fn visit_type_named(&mut self, ty: TypeNamed) -> Result<(), AstError> {
        T::visit_type_named(self, ty)
    }
    fn visit_type_unnamed(&mut self, ty: TypeUnnamed) -> Result<(), AstError> {
        T::visit_type_unnamed(self, ty)
    }
    fn visit_type_tuple(&mut self, ty: TypeTuple) -> Result<(), AstError> {
        T::visit_type_tuple(self, ty)
    }
    fn visit_type_array(&mut self, ty: TypeArray) -> Result<(), AstError> {
        T::visit_type_array(self, ty)
    }
    fn visit_type(&mut self, ty: Type) -> Result<(), AstError> {
        T::visit_type(self, ty)
    }
    fn visit_type_binding(&mut self, tb: TypeBinding) -> Result<(), AstError> {
        T::visit_type_binding(self, tb)
    }
    fn visit_path(&mut self, path: Path) -> Result<(), AstError> {
        T::visit_path(self, path)
    }
    fn visit_expr(&mut self, expr: Expr) -> Result<(), AstError> {
        T::visit_expr(self, expr)
    }
    fn visit_string_expr(&mut self, ls: LitString) -> Result<(), AstError> {
        T::visit_string_expr(self, ls)
    }
    fn visit_char_expr(&mut self, lc: LitChar) -> Result<(), AstError> {
        T::visit_char_expr(self, lc)
    }
    fn visit_int_expr(&mut self, li: LitInteger) -> Result<(), AstError> {
        T::visit_int_expr(self, li)
    }
    fn visit_float_expr(&mut self, lf: LitFloat) -> Result<(), AstError> {
        T::visit_float_expr(self, lf)
    }
    fn visit_ref_expr(&mut self, e: RefExpr) -> Result<(), AstError> {
        T::visit_ref_expr(self, e)
    }
    fn visit_not_expr(&mut self, e: NotExpr) -> Result<(), AstError> {
        T::visit_not_expr(self, e)
    }
    fn visit_deref_expr(&mut self, e: DerefExpr) -> Result<(), AstError> {
        T::visit_deref_expr(self, e)
    }
    fn visit_neg_expr(&mut self, e: NegExpr) -> Result<(), AstError> {
        T::visit_neg_expr(self, e)
    }
    fn visit_add_expr(&mut self, e: BinopAdd) -> Result<(), AstError> {
        T::visit_add_expr(self, e)
    }
    fn visit_sub_expr(&mut self, e: BinopSub) -> Result<(), AstError> {
        T::visit_sub_expr(self, e)
    }
    fn visit_mul_expr(&mut self, e: BinopMul) -> Result<(), AstError> {
        T::visit_mul_expr(self, e)
    }
    fn visit_div_expr(&mut self, e: BinopDiv) -> Result<(), AstError> {
        T::visit_div_expr(self, e)
    }
    fn visit_mod_expr(&mut self, e: BinopMod) -> Result<(), AstError> {
        T::visit_mod_expr(self, e)
    }
    fn visit_eq_expr(&mut self, e: BinopEq) -> Result<(), AstError> {
        T::visit_eq_expr(self, e)
    }
    fn visit_neq_expr(&mut self, e: BinopNotEq) -> Result<(), AstError> {
        T::visit_neq_expr(self, e)
    }
    fn visit_and_expr(&mut self, e: BinopAnd) -> Result<(), AstError> {
        T::visit_and_expr(self, e)
    }
    fn visit_or_expr(&mut self, e: BinopOr) -> Result<(), AstError> {
        T::visit_or_expr(self, e)
    }
    fn visit_gt_expr(&mut self, e: BinopGT) -> Result<(), AstError> {
        T::visit_gt_expr(self, e)
    }
    fn visit_gte_expr(&mut self, e: BinopGTE) -> Result<(), AstError> {
        T::visit_gte_expr(self, e)
    }
    fn visit_lt_expr(&mut self, e: BinopLT) -> Result<(), AstError> {
        T::visit_lt_expr(self, e)
    }
    fn visit_lte_expr(&mut self, e: BinopLTE) -> Result<(), AstError> {
        T::visit_lte_expr(self, e)
    }
    fn visit_bitor_expr(&mut self, e: BinopBitOr) -> Result<(), AstError> {
        T::visit_bitor_expr(self, e)
    }
    fn visit_bitand_expr(&mut self, e: BinopBitAnd) -> Result<(), AstError> {
        T::visit_bitand_expr(self, e)
    }
    fn visit_add_assign_expr(&mut self, e: BinopAddAssign) -> Result<(), AstError> {
        T::visit_add_assign_expr(self, e)
    }
    fn visit_sub_assign_expr(&mut self, e: BinopSubAssign) -> Result<(), AstError> {
        T::visit_sub_assign_expr(self, e)
    }
    fn visit_div_assign_expr(&mut self, e: BinopDivAssign) -> Result<(), AstError> {
        T::visit_div_assign_expr(self, e)
    }
    fn visit_mul_assign_expr(&mut self, e: BinopMulAssign) -> Result<(), AstError> {
        T::visit_mul_assign_expr(self, e)
    }
    fn visit_shift_l_expr(&mut self, e: BinopShiftL) -> Result<(), AstError> {
        T::visit_shift_l_expr(self, e)
    }
    fn visit_shift_r_expr(&mut self, e: BinopShiftR) -> Result<(), AstError> {
        T::visit_shift_r_expr(self, e)
    }
    fn visit_shift_l_assign_expr(&mut self, e: BinopShiftLAssign) -> Result<(), AstError> {
        T::visit_shift_l_assign_expr(self, e)
    }
    fn visit_shift_r_assign_expr(&mut self, e: BinopShiftRAssign) -> Result<(), AstError> {
        T::visit_shift_r_assign_expr(self, e)
    }
}
