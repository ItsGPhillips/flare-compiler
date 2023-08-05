use super::{tokens::LitString, utils, visitor::Visitor, AstElement, AstError};
use crate::{
    ast::tokens::{LitChar, LitFloat, LitInteger},
    ast_node, gen_binop_node, impl_from, SyntaxNode, SyntaxToken, Tkn,
};
use syntax::SyntaxKind;

ast_node!(Module, MODULE);
impl Module {
    pub fn items(&self) -> impl Iterator<Item = Item> {
        utils::children::<Item>(&self.0)
    }
}
// Items ============================================================================

ast_node!(FnItem, ITEM_FN);
impl FnItem {
    pub fn fn_keyword(&self) -> Option<SyntaxToken> {
        utils::token(&self.0, Tkn!["fn"]).next()
    }
    pub fn signature(&self) -> Result<FnSig, AstError> {
        utils::children::<FnSig>(&self.0)
            .next()
            .ok_or(AstError::MissingNode)
    }
    pub fn block_expr(&self) -> Result<BlockExpr, AstError> {
        utils::children(&self.0).next().ok_or(AstError::MissingNode)
    }
}

ast_node!(FnSig, FN_SIGNATURE);
impl FnSig {
    pub fn name<'a>(&'a self) -> Result<&'a str, AstError> {
        utils::token(&self.0, SyntaxKind::IDENTIFIER)
            .next()
            .map(|token| unsafe {
                // SAFETY: the lifetime of t.text() is tied to the current iterator.
                // the text bytes are stored in the underlying tree and is kept alive
                // by the node itself.
                // As long as the t.text() is only alive as long as the node it the transmute
                // will be ok
                std::mem::transmute(token.text())
            })
            .ok_or(AstError::MissingNode)
    }
    pub fn param_list(&self) -> Result<FnParamList, AstError> {
        utils::children::<FnParamList>(&self.0)
            .next()
            .ok_or(AstError::MissingNode)
    }
    pub fn return_type(&self) -> Option<FnReturnType> {
        utils::children::<FnReturnType>(&self.0).next()
    }
}

ast_node!(FnReturnType, RETURN_TYPE);
impl FnReturnType {
    pub fn named(&self) -> Option<TypeNamed> {
        utils::children::<TypeNamed>(&self.0).next()
    }
}

ast_node!(TypeNamed, RETURN_TYPE);
impl TypeNamed {
    pub fn path(&self) -> Option<Path> {
        utils::children::<Path>(&self.0).next()
    }
}

ast_node!(FnParamList, PARAMETER_LIST);
impl FnParamList {
    pub fn parameters(&self) -> impl Iterator<Item = FnParameter> {
        utils::children::<FnParameter>(&self.0)
    }
}

ast_node!(FnParameter, PARAMETER);
impl FnParameter {
    pub fn name<'a>(&'a self) -> Result<&'a str, AstError> {
        utils::token(&self.0, SyntaxKind::IDENTIFIER)
            .nth(0)
            .map(|t| unsafe { std::mem::transmute(t.text()) })
            .ok_or(AstError::MissingNode)
    }
    pub fn param_type<'a>(&'a self) -> Result<TypeNamed, AstError> {
        utils::children::<TypeNamed>(&self.0)
            .next()
            .ok_or(AstError::MissingNode)
    }
}

ast_node!(StructItem, ITEM_STRUCT);
ast_node!(EnumItem, ITEM_ENUM);
ast_node!(ImportItem, ITEM_IMPORT);

pub struct Item(SyntaxNode);
impl AstElement for Item {
    fn cast(element: crate::SyntaxElement) -> Result<Self, AstError> {
        element
            .as_node()
            .filter(|node| Self::can_cast(node.kind()))
            .map(|node| Self(node.clone()))
            .ok_or_else(|| crate::ast::AstError::InvalidCast)
    }
    fn syntax(&self) -> crate::SyntaxElement {
        crate::SyntaxElement::Node(self.0.clone())
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        use SyntaxKind::*;
        matches!(kind, ITEM_FN | ITEM_STRUCT | ITEM_ENUM | ITEM_IMPORT)
    }
}

impl Item {
    pub fn visit(&self, visitor: impl Visitor) -> Result<(), AstError> {
        FnItem::cast(self.syntax()).map(|item| visitor.visit_fn_item(item))?
    }
}

// Expressions ======================================================================

pub struct Statement(SyntaxNode);
impl AstElement for Statement {
    fn cast(element: crate::SyntaxElement) -> Result<Self, AstError> {
        element
            .as_node()
            .filter(|node| Self::can_cast(node.kind()))
            .map(|node| Self(node.clone()))
            .ok_or_else(|| crate::ast::AstError::InvalidCast)
    }
    fn syntax(&self) -> crate::SyntaxElement {
        crate::SyntaxElement::Node(self.0.clone())
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        use SyntaxKind::*;
        matches!(kind, EXPR_STMT)
    }
}

impl Statement {
    pub fn visit(&self, visitor: impl Visitor) -> Result<(), AstError> {
        use SyntaxKind::*;
        match self.0.kind() {
            EXPR_STMT => visitor.visit_expression(
                utils::children::<Expr>(&self.0)
                    .next()
                    .ok_or(AstError::MissingNode)?,
            ),
            _ => unreachable!(),
        }
    }
}

ast_node!(BlockExpr, EXPR_BLOCK);
impl BlockExpr {
    pub fn statements(&self) -> impl Iterator<Item = Statement> {
        utils::children(&self.0)
    }
}

ast_node!(Path, PATH);
impl Path {
    pub fn segments(&self) {}
}

ast_node!(RefExpr, UNOP_REF);
ast_node!(NotExpr, UNOP_NOT);
ast_node!(DerefExpr, UNOP_DEREF);
ast_node!(NegExpr, UNOP_NEG);

ast_node!(Operator, OPERATOR);

// mutation binops
gen_binop_node!(Add, BINOP_ADD);
gen_binop_node!(Sub, BINOP_SUB);
gen_binop_node!(Mul, BINOP_MUL);
gen_binop_node!(Div, BINOP_DIV);
gen_binop_node!(Mod, BINOP_MOD);

// boolean binops
gen_binop_node!(Eq, BINOP_EQ);
gen_binop_node!(NotEq, BINOP_NOT_EQ);
gen_binop_node!(And, BINOP_AND);
gen_binop_node!(Or, BINOP_OR);
gen_binop_node!(LT, BINOP_LT);
gen_binop_node!(LTE, BINOP_LTE);
gen_binop_node!(GT, BINOP_GT);
gen_binop_node!(GTE, BINOP_GTE);

// assign binops
gen_binop_node!(AddAssign, BINOP_ADD_ASSIGN);
gen_binop_node!(SubAssign, BINOP_SUB_ASSIGN);
gen_binop_node!(DivAssign, BINOP_DIV_ASSIGN);
gen_binop_node!(MulAssign, BINOP_MUL_ASSIGN);

// bitwise binops
gen_binop_node!(BitOr, BINOP_BITOR);
gen_binop_node!(BitAnd, BINOP_BITAND);
gen_binop_node!(ShiftL, BINOP_SHIFT_L);
gen_binop_node!(ShiftR, BINOP_SHIFT_R);
gen_binop_node!(ShiftLAssign, BINOP_SHIFT_L_ASSIGN);
gen_binop_node!(ShiftRAssign, BINOP_SHIFT_R_ASSIGN);

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
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    BinopDiv(Div),
    BinopMod(Mod),
    BinopEq(Eq),
    BinopNotEq(NotEq),
    BinopAnd(And),
    BinopOr(Or),
    BinopGT(GT),
    BinopGTE(GTE),
    BinopLT(LT),
    BinopLTE(LTE),
    BinopBitOr(BitOr),
    BinopBitAnd(BitAnd),
    BinopAddAssign(AddAssign),
    BinopSubAssign(SubAssign),
    BinopDivAssign(DivAssign),
    BinopMulAssign(MulAssign),
    BinopShiftL(ShiftL),
    BinopShiftR(ShiftR),
    BinopShiftLAssign(ShiftLAssign),
    BinopShiftRAssign(ShiftRAssign),
}

impl AstElement for Expr {
    #[rustfmt::skip]
    fn cast(e: crate::SyntaxElement) -> Result<Self, AstError> {
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
            BINOP_ADD            => Ok(Add::cast(e)?.into()),
            BINOP_SUB            => Ok(Sub::cast(e)?.into()),
            BINOP_MUL            => Ok(Mul::cast(e)?.into()),
            BINOP_DIV            => Ok(Div::cast(e)?.into()),
            BINOP_MOD            => Ok(Mod::cast(e)?.into()),
            BINOP_EQ             => Ok(Eq::cast(e)?.into()),
            BINOP_NOT_EQ         => Ok(NotEq::cast(e)?.into()),
            BINOP_AND            => Ok(And::cast(e)?.into()),
            BINOP_OR             => Ok(Or::cast(e)?.into()),
            BINOP_GT             => Ok(GT::cast(e)?.into()),
            BINOP_GTE            => Ok(GTE::cast(e)?.into()),
            BINOP_LT             => Ok(LT::cast(e)?.into()),
            BINOP_LTE            => Ok(LTE::cast(e)?.into()),
            BINOP_BITOR          => Ok(BitOr::cast(e)?.into()),
            BINOP_BITAND         => Ok(BitAnd::cast(e)?.into()),
            BINOP_ADD_ASSIGN     => Ok(AddAssign::cast(e)?.into()),
            BINOP_SUB_ASSIGN     => Ok(SubAssign::cast(e)?.into()),
            BINOP_DIV_ASSIGN     => Ok(DivAssign::cast(e)?.into()),
            BINOP_MUL_ASSIGN     => Ok(MulAssign::cast(e)?.into()),
            BINOP_SHIFT_L        => Ok(ShiftL::cast(e)?.into()),
            BINOP_SHIFT_R        => Ok(ShiftR::cast(e)?.into()),
            BINOP_SHIFT_L_ASSIGN => Ok(ShiftLAssign::cast(e)?.into()),
            BINOP_SHIFT_R_ASSIGN => Ok(ShiftRAssign::cast(e)?.into()),

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
            Self::Add(i)          => i.syntax(),
            Self::Sub(i)          => i.syntax(),
            Self::Mul(i)          => i.syntax(),
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
    Add => Add,
    Sub => Sub,
    Mul => Mul,
    Div => BinopDiv,
    Mod => BinopMod,
    Eq  => BinopEq,
    NotEq  => BinopNotEq,
    And => BinopAnd,
    Or  => BinopOr,
    GT  => BinopGT,
    GTE  => BinopGTE,
    LT  => BinopLT,
    LTE  => BinopLTE,
    BitOr => BinopBitOr,
    BitAnd => BinopBitAnd,
    AddAssign => BinopAddAssign,
    SubAssign => BinopSubAssign,
    DivAssign => BinopDivAssign,
    MulAssign => BinopMulAssign,
    ShiftL => BinopShiftL,
    ShiftR => BinopShiftR,
    ShiftRAssign => BinopShiftRAssign,
    ShiftLAssign => BinopShiftLAssign,
}

impl Expr {}

ast_node!(Error, ERROR);
