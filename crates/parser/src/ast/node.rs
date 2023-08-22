use std::{
    hash::{Hash, Hasher},
};

use super::{tokens::LitString, utils, visitor::Visitor, AstElement, AstError};
use crate::{
    ast::tokens::{LitChar, LitFloat, LitInteger},
    ast_node, gen_binop_node, impl_from, SyntaxElement, SyntaxNode, SyntaxNodePtr, SyntaxToken,
    Tkn,
};
use interner::{impl_internable, Interned};
use rowan::ast::{support::*, AstNode};
use syntax::SyntaxKind;

ast_node!(Module, MODULE);
impl Module {
    pub fn items(&self) -> impl Iterator<Item = Item> {
        children::<Item>(&self.0)
    }
}

ast_node!(StructItem, ITEM_STRUCT);
ast_node!(EnumItem, ITEM_ENUM);
ast_node!(ImportItem, ITEM_IMPORT);
ast_node!(FunctionItem, ITEM_FN);

impl FunctionItem {
    pub fn fn_keyword(&self) -> Option<SyntaxToken> {
       token(&self.0, Tkn!["fn"]) 
    }
    pub fn signature(&self) -> Option<FunctionSig> {
        child::<FunctionSig>(&self.0)
    }
    pub fn block_expr(&self) -> Option<BlockExpr> {
        child::<BlockExpr>(&self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(FunctionItem),
    Struct(StructItem),
    Enum(EnumItem),
    Import(ImportItem),
}

impl AstNode for Item {
    type Language = crate::Flare;
    fn cast(node: crate::SyntaxNode) -> Option<Self> {
        if Self::can_cast(node.kind()) {
            Some(match node.kind() {
                ITEM_FN => Item::Function(FunctionItem(node.clone())),
                ITEM_STRUCT => Item::Struct(StructItem(node.clone())),
                ITEM_ENUM => Item::Enum(EnumItem(node.clone())),
                ITEM_IMPORT => Item::Import(ImportItem(node.clone())),
                _ => unreachable!(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &crate::SyntaxNode {
        match self {
            Item::Function(e) => e.syntax(),
            Item::Struct(e) => e.syntax(),
            Item::Enum(e) => e.syntax(),
            Item::Import(e) => e.syntax(),
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        use SyntaxKind::*;
        matches!(kind, ITEM_FN | ITEM_STRUCT | ITEM_ENUM | ITEM_IMPORT)
    }
}

ast_node!(FunctionSig, FN_SIGNATURE);
impl FunctionSig {
    pub fn name(&self) -> Result<Interned<str>, AstError> {
        utils::nth_token::<0>(&self.0, SyntaxKind::IDENTIFIER)
            .map(|token| Interned::new_str(token.text()))
    }
    pub fn param_list(&self) -> Option<ParameterList> {
        child::<ParameterList>(&self.0)
    }
    pub fn return_type(&self) -> Option<TypeBinding> {
        child::<TypeBinding>(&self.0)
    }
    pub fn to_node_ptr(&self) -> SyntaxNodePtr {
        SyntaxNodePtr::new(&self.0)
    }
}

ast_node!(ParameterList, PARAMETER_LIST);
impl ParameterList {
    pub fn parameters(&self) -> impl Iterator<Item = Parameter> {
        children::<Parameter>(&self.0)
    }
}

ast_node!(Parameter, PARAMETER);
impl Parameter {
    pub fn name(&self) -> Option<Interned<str>> {
        token(&self.0, Tkn![IDENT]).map(|t| Interned::new_str(t.text()))
    }
    pub fn param_type<'a>(&'a self) -> Option<TypeNamed> {
        child::<TypeNamed>(&self.0)
    }
}

ast_node!(TypeBinding, TYPE_BINDING);
impl TypeBinding {
    pub fn colon(&self) -> Option<SyntaxToken> {
        token(&self.0, Tkn![":"])
    }
    /// Binding Type
    pub fn btype(&self) -> Option<Type> {
        child::<Type>(&self.0)
    }
}

// Type -----------------------------------------------------------------------------

ast_node!(TypeNamed, RETURN_TYPE);
impl TypeNamed {
    pub fn path(&self) -> Option<Path> {
        child::<Path, 0>(&self.0)
    }
}

ast_node!(TypeUnnamed, TYPE_UNNAMED);
ast_node!(TypeTuple, TYPE_TUPLE);
ast_node!(TypeArray, TYPE_ARRAY);

#[derive(Debug, Clone)]
pub enum Type {
    Named(TypeNamed),
    Unnamed(TypeUnnamed),
    Tuple(TypeTuple),
    Array(TypeArray),
}

impl AstNode for Type {
    type Language = crate::Flare;
    fn can_cast(kind: syntax::SyntaxKind) -> bool {
        use SyntaxKind::*;
        matches!(kind, TYPE_NAMED | TYPE_UNNAMED | TYPE_TUPLE | TYPE_ARRAY)
    }
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Self::can_cast(node.kind()) {
            Some(match node.kind() {
                TYPE_NAMED => Type::Named(TypeNamed(node.clone())),
                TYPE_UNNAMED => Type::Unnamed(TypeUnnamed(node.clone())),
                TYPE_TUPLE => Type::Tuple(TypeTuple(node.clone())),
                TYPE_ARRAY => Type::Array(TypeArray(node.clone())),
                _ => unreachable!(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Type::Named(e) => e.syntax(),
            Type::Unnamed(e) => e.syntax(),
            Type::Tuple(e) => e.syntax(),
            Type::Array(e) => e.syntax(),
        }
    }
}

pub struct Statement(SyntaxNode);
impl AstNode for Statement {
    type Language = crate::Flare;
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Self::can_cast(node.kind()) {
            Some(Self(node.clone()))
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        use SyntaxKind::*;
        matches!(kind, EXPR_STMT)
    }
}


ast_node!(BlockExpr, EXPR_BLOCK);
impl BlockExpr {
    pub fn statements(&self) -> impl Iterator<Item = Statement> {
        children::<Statement>(&self.0)           
    }
}

ast_node!(NamedPathSegement, PATH_SEGMENT_NAMED);
impl NamedPathSegement {
    pub fn text(&self) -> Option<Interned<str>> {
        token(&self.0, Tkn![IDENT]).map(|t| Interned::new_str(t.text()))
    }
}

ast_node!(GenericPathSegement, PATH_SEGMENT_GENERIC);

pub enum PathSegment {
    Named(NamedPathSegement),
    Generic,
}

impl AstNode for PathSegment {
    type Language = crate::Flare;
    fn can_cast(kind: syntax::SyntaxKind) -> bool {
        use SyntaxKind::*;
        matches!(kind, PATH_SEGMENT_NAMED | PATH_SEGMENT_GENERIC)
    }
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Self::can_cast(node.kind()) {
            use SyntaxKind::*;
            Some(match node.kind() {
                PATH_SEGMENT_NAMED => PathSegment::Named(NamedPathSegement(node.clone())),
                PATH_SEGMENT_GENERIC => PathSegment::Generic,
                _ => unreachable!(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            PathSegment::Named(n) => self.syntax(),
            PathSegment::Generic => todo!(),
        }
    }
}

impl Hash for PathSegment {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            PathSegment::Named(segment) => segment.text().map(|text| text.hash(state)),
            PathSegment::Generic => unimplemented!(),
        };
    }
}

ast_node!(Path, PATH);
impl Path {
    pub fn segments(&self) -> impl Iterator<Item = PathSegment> {
        children::<PathSegment>(&self.0)
    }
}

impl Hash for Path {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.segments().for_each(|segment| segment.hash(state));
    }
}

ast_node!(RefExpr, UNOP_REF);
ast_node!(NotExpr, UNOP_NOT);
ast_node!(DerefExpr, UNOP_DEREF);
ast_node!(NegExpr, UNOP_NEG);

ast_node!(Operator, OPERATOR);

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
    Integer(LitInteger),
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

impl AstNode for Expr {
    type Language = crate::Flare;
    
    #[rustfmt::skip]
    fn cast(e: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;
        match e.kind() {
            LIT_STRING             => Ok(LitString::cast(e)?.into()),
            LIT_CHAR               => Ok(LitChar::cast(e)?.into()),
            LIT_INTEGER            => Ok(LitInteger::cast(e)?.into()),
            LIT_FLOAT              => Ok(LitFloat::cast(e)?.into()),
            PATH                   => Ok(Path::cast(e)?.into()),

            // Unops
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
            BINOP_OR             => Ok(BinopOr::cast(e)?.into()),
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
            BINOP_SHIFT_R        => Ok(BinopShiftR::cast(e)?.into()),
            BINOP_SHIFT_L_ASSIGN => Ok(BinopShiftLAssign::cast(e)?.into()),
            BINOP_SHIFT_R_ASSIGN => Ok(BinopShiftRAssign::cast(e)?.into()),

            _ => Err(AstError::InvalidCast),
        }
    }
    #[rustfmt::skip]
    fn syntax(&self) -> &crate::SyntaxNode {
        match self {
            // literals
            // Self::String(i)    => i.syntax(),
            // Self::Char(i)      => i.syntax(),
            // Self::Integer(i)   => i.syntax(),
            // Self::Float(i)     => i.syntax(),
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

            _ => todo!()
        }
    }

    fn can_cast(element: syntax::SyntaxKind) -> bool {
        todo!()
    }
}

impl_from! {
    Expr,
    // literals
    LitString => String,
    LitChar   => Char,
    LitInteger=> Integer,
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

impl Expr {}

ast_node!(Error, ERROR);
