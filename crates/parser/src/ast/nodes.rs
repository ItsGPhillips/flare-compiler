use super::{tokens::LitString, AstElement, AstError};
use crate::{
    ast::tokens::{LitChar, LitFloat, LitInt},
    ast_node, gen_binop_node, impl_from, SyntaxNode,
};
use syntax::SyntaxKind;

ast_node!(Module, MODULE);
ast_node!(Path, PATH);

ast_node!(RefExpr, UNOP_REF);
ast_node!(NotExpr, UNOP_NOT);
ast_node!(DerefExpr, UNOP_DEREF);
ast_node!(NegExpr, UNOP_NEG);

// mutation binops
gen_binop_node!(BinopAdd, BINOP_ADD);
gen_binop_node!(BinopSub, BINOP_SUB);
gen_binop_node!(BinopMul, BINOP_MUL);
gen_binop_node!(BinopDiv, BINOP_DIV);
gen_binop_node!(BinopMod, BINOP_MOD);

// boolean binops
gen_binop_node!(BinopEq, BINOP_EQ);
gen_binop_node!(BinopNotEq, BINOP_NOT_EQ);
gen_binop_node!(BinopAnd, BINOP_AND);
gen_binop_node!(BinopOr, BINOP_OR);
gen_binop_node!(BinopLT, BINOP_LT);
gen_binop_node!(BinopLTE, BINOP_LTE);
gen_binop_node!(BinopGT, BINOP_GT);
gen_binop_node!(BinopGTE, BINOP_GTE);

// assign binops
gen_binop_node!(BinopAddAssign, BINOP_ADD_ASSIGN);
gen_binop_node!(BinopSubAssign, BINOP_SUB_ASSIGN);
gen_binop_node!(BinopDivAssign, BINOP_DIV_ASSIGN);
gen_binop_node!(BinopMulAssign, BINOP_MUL_ASSIGN);

// bitwise binops
gen_binop_node!(BinopBitOr, BINOP_BITOR);
gen_binop_node!(BinopBitAnd, BINOP_BITAND);
gen_binop_node!(BinopShiftL, BINOP_SHIFT_L);
gen_binop_node!(BinopShiftR, BINOP_SHIFT_R);
gen_binop_node!(BinopShiftLAssign, BINOP_SHIFT_L_ASSIGN);
gen_binop_node!(BinopShiftRAssign, BINOP_SHIFT_R_ASSIGN);

#[derive(Debug)]
pub enum Expr {
    // literals
    String(LitString),
    Char(LitChar),
    Integer(LitInt),
    Float(LitFloat),
    Path(Path),

    // unops
    UnopRef(RefExpr),
    UnopNot(NotExpr),
    UnopDeref(DerefExpr),
    UnopNeg(NegExpr),

    // binops
    BinopAdd(BinopAdd),
    BinopSub(BinopSub),
    BinopMul(BinopMul),
    BinopDiv(BinopDiv),
    BinopMod(BinopMod),
    BinopEq(BinopEq),
    BinopNotEq(BinopNotEq),
    BinopAnd(BinopAnd),
    BinopOr(BinopOr),
    BinopGT(BinopGT),
    BinopGTE(BinopGTE),
    BinopLT(BinopLT),
    BinopLTE(BinopLTE),
    BinopBitOr(BinopBitOr),
    BinopBitAnd(BinopBitAnd),
    BinopAddAssign(BinopAddAssign),
    BinopSubAssign(BinopSubAssign),
    BinopDivAssign(BinopDivAssign),
    BinopMulAssign(BinopMulAssign),
    BinopShiftL(BinopShiftL),
    BinopShiftR(BinopShiftR),
    BinopShiftLAssign(BinopShiftLAssign),
    BinopShiftRAssign(BinopShiftRAssign),
}

impl AstElement for Expr {
    #[rustfmt::skip]
    fn cast(e: crate::SyntaxElement) -> Result<Self, AstError> {
        use SyntaxKind::*;
        match e.kind() {
            LIT_STRING             => Ok(LitString::cast(e)?.into()),
            LIT_CHAR               => Ok(LitChar::cast(e)?.into()),
            LIT_INTEGER            => Ok(LitInt::cast(e)?.into()),
            LIT_FLOAT              => Ok(LitFloat::cast(e)?.into()),
            PATH                   => Ok(Path::cast(e)?.into()),

            // unops
            UNOP_NOT               => Ok(NotExpr::cast(e)?.into()),
            UNOP_REF               => Ok(RefExpr::cast(e)?.into()),
            UNOP_DEREF             => Ok(DerefExpr::cast(e)?.into()),
            UNOP_NEG               => Ok(NegExpr::cast(e)?.into()),

            // Binops
            BINOP_ADD            => Ok(BinopAdd::cast(e)?.into()),
            BINOP_SUB            => Ok(BinopSub::cast(e)?.into()),
            BINOP_MUL            => Ok(BinopMul::cast(e)?.into()),
            BINOP_DIV            => Ok(BinopDiv::cast(e)?.into()),
            BINOP_MOD            => Ok(BinopMod::cast(e)?.into()),
            BINOP_EQ             => Ok(BinopEq::cast(e)?.into()),
            BINOP_NOT_EQ         => Ok(BinopNotEq::cast(e)?.into()),
            BINOP_AND            => Ok(BinopAnd::cast(e)?.into()),
            BINOP_OR             => Ok(BinopAnd::cast(e)?.into()),
            BINOP_GT             => Ok(BinopGT::cast(e)?.into()),
            BINOP_GTE            => Ok(BinopGTE::cast(e)?.into()),
            BINOP_LT             => Ok(BinopLT::cast(e)?.into()),
            BINOP_LTE            => Ok(BinopLTE::cast(e)?.into()),
            BINOP_BITOR          => Ok(BinopBitOr::cast(e)?.into()),
            BINOP_BITAND         => Ok(BinopBitAnd::cast(e)?.into()),
            BINOP_ADD_ASSIGN     => Ok(BinopAddAssign::cast(e)?.into()),
            BINOP_SUB_ASSIGN     => Ok(BinopSubAssign::cast(e)?.into()),
            BINOP_DIV_ASSIGN     => Ok(BinopDivAssign::cast(e)?.into()),
            BINOP_MUL_ASSIGN     => Ok(BinopMulAssign::cast(e)?.into()),
            BINOP_SHIFT_L        => Ok(BinopShiftL::cast(e)?.into()),
            BINOP_SHIFT_R        => Ok(BinopShiftL::cast(e)?.into()),
            BINOP_SHIFT_R_ASSIGN => Ok(BinopShiftLAssign::cast(e)?.into()),
            BINOP_SHIFT_L_ASSIGN => Ok(BinopShiftLAssign::cast(e)?.into()),

            _ => Err(AstError::InvalidCast),
        }
    }
    #[rustfmt::skip]
    fn syntax(&self) -> crate::SyntaxElement {
        match self {
            // literals
            Self::String(i)    => i.syntax(),
            Self::Char(i)      => i.syntax(),
            Self::Integer(i)   => i.syntax(),
            Self::Float(i)     => i.syntax(),
            
            // unops
            Self::UnopRef(i)   => i.syntax(),
            Self::UnopNot(i)   => i.syntax(),
            Self::UnopDeref(i) => i.syntax(),
            Self::UnopNeg(i)   => i.syntax(),

            // binops
            Self::BinopAdd(i)          => i.syntax(),
            Self::BinopSub(i)          => i.syntax(),
            Self::BinopMul(i)          => i.syntax(),
            Self::BinopDiv(i)          => i.syntax(),
            Self::BinopMod(i)          => i.syntax(),
            Self::BinopEq(i)           => i.syntax(),
            Self::BinopNotEq(i)        => i.syntax(),
            Self::BinopAnd(i)          => i.syntax(),
            Self::BinopOr(i)           => i.syntax(),
            Self::BinopGT(i)           => i.syntax(),
            Self::BinopGTE(i)          => i.syntax(),
            Self::BinopLT(i)           => i.syntax(),
            Self::BinopLTE(i)          => i.syntax(),
            Self::BinopBitOr(i)        => i.syntax(),
            Self::BinopBitAnd(i)       => i.syntax(),
            Self::BinopAddAssign(i)    => i.syntax(),
            Self::BinopSubAssign(i)    => i.syntax(),
            Self::BinopDivAssign(i)    => i.syntax(),
            Self::BinopMulAssign(i)    => i.syntax(),
            Self::BinopShiftL(i)       => i.syntax(),
            Self::BinopShiftR(i)       => i.syntax(),
            Self::BinopShiftLAssign(i) => i.syntax(),
            Self::BinopShiftRAssign(i) => i.syntax(),

            Self::Path(i)      => i.syntax(),
        }
    }
}

impl_from! {
    Expr,
    // literals
    LitString => String,
    LitChar   => Char,
    LitInt    => Integer,
    LitFloat  => Float,
    
    // unops
    Path      => Path,
    RefExpr   => UnopRef,
    NotExpr   => UnopNot,
    DerefExpr => UnopDeref,
    NegExpr   => UnopNeg,

    // binops
    BinopAdd => BinopAdd,
    BinopSub => BinopSub,
    BinopMul => BinopMul,
    BinopDiv => BinopDiv,
    BinopMod => BinopMod,
    BinopEq  => BinopEq,
    BinopNotEq  => BinopNotEq,
    BinopAnd => BinopAnd,
    BinopOr  => BinopOr,
    BinopGT  => BinopGT,
    BinopGTE  => BinopGTE,
    BinopLT  => BinopLT,
    BinopLTE  => BinopLTE,
    BinopBitOr => BinopBitOr,
    BinopBitAnd => BinopBitAnd,
    BinopAddAssign => BinopAddAssign,
    BinopSubAssign => BinopSubAssign,
    BinopDivAssign => BinopDivAssign,
    BinopMulAssign => BinopMulAssign,
    BinopShiftL => BinopShiftL,
    BinopShiftR => BinopShiftR,
    BinopShiftRAssign => BinopShiftRAssign,
    BinopShiftLAssign => BinopShiftLAssign,
}

ast_node!(Error, ERROR);
