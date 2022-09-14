#[macro_export]
macro_rules! Tkn {
   [WHITESPACE]   => { ::syntax::SyntaxKind::WHITESPACE    };
   [IDENT]        => { ::syntax::SyntaxKind::IDENTIFIER    };
   [INTEGER]      => { ::syntax::SyntaxKind::LIT_INTEGER   };
   [FLOAT]        => { ::syntax::SyntaxKind::LIT_FLOAT     };
   ["Self"]       => { ::syntax::SyntaxKind::KW_SELF_TYPE  };
   ["self"]       => { ::syntax::SyntaxKind::KW_SELF_VALUE };
   ["mut"]        => { ::syntax::SyntaxKind::KW_MUT        };
   ["crate"]      => { ::syntax::SyntaxKind::KW_CRATE      };
   ["super"]      => { ::syntax::SyntaxKind::KW_SUPER      };
   ["pub"]        => { ::syntax::SyntaxKind::KW_PUB        };
   ["fn"]         => { ::syntax::SyntaxKind::KW_FN         };
   ["let"]        => { ::syntax::SyntaxKind::KW_LET        };
   ["struct"]     => { ::syntax::SyntaxKind::KW_STRUCT     };
   ["enum"]       => { ::syntax::SyntaxKind::KW_ENUM       };
   ["import"]     => { ::syntax::SyntaxKind::KW_IMPORT     };
   ["return"]     => { ::syntax::SyntaxKind::KW_RETURN     };


   ["\""]   =>  { ::syntax::SyntaxKind::PUNC_QUOTE          };
   ["\'"]   =>  { ::syntax::SyntaxKind::PUNC_SINGLE_QUOTE   };
   [":"]    =>  { ::syntax::SyntaxKind::PUNC_COLON          };
   [";"]    =>  { ::syntax::SyntaxKind::PUNC_SEMICOLON      };
   ["<"]    =>  { ::syntax::SyntaxKind::PUNC_LANGLE_BRACKET };
   [">"]    =>  { ::syntax::SyntaxKind::PUNC_RANGLE_BRACKET };
   [","]    =>  { ::syntax::SyntaxKind::PUNC_COMMA          };
   ["!"]    =>  { ::syntax::SyntaxKind::PUNC_EXCLAMATION    };
   ["*"]    =>  { ::syntax::SyntaxKind::PUNC_ASTERISK       };
   ["-"]    =>  { ::syntax::SyntaxKind::PUNC_HYPHEN         };
   ["&"]    =>  { ::syntax::SyntaxKind::PUNC_AMPERSAND      };
   ["?"]    =>  { ::syntax::SyntaxKind::PUNC_QUESTIONMARK   };
   ["+"]    =>  { ::syntax::SyntaxKind::PUNC_PLUS           };
   ["-"]    =>  { ::syntax::SyntaxKind::PUNC_HYPHEN         };
   ["_"]    =>  { ::syntax::SyntaxKind::PUNC_UNDERSCORE     };
   ["%"]    =>  { ::syntax::SyntaxKind::PUNC_PERCENT        };
   ["/"]    =>  { ::syntax::SyntaxKind::PUNC_SLASH          };
   ["="]    =>  { ::syntax::SyntaxKind::PUNC_EQUALS         };
   ["|"]    =>  { ::syntax::SyntaxKind::PUNC_PIPE           };
   ["("]    =>  { ::syntax::SyntaxKind::PUNC_LPAREN         };
   [")"]    =>  { ::syntax::SyntaxKind::PUNC_RPAREN         };
   ["["]    =>  { ::syntax::SyntaxKind::PUNC_LBRACKET       };
   ["]"]    =>  { ::syntax::SyntaxKind::PUNC_RBRACKET       };
   ["{"]    =>  { ::syntax::SyntaxKind::PUNC_LBRACE         };
   ["}"]    =>  { ::syntax::SyntaxKind::PUNC_RBRACE         };

   ["=="]    =>  { ::syntax::SyntaxKind::BINOP_EQ           };
   ["!="]    =>  { ::syntax::SyntaxKind::BINOP_NOT_EQ       };
   ["+="]    =>  { ::syntax::SyntaxKind::BINOP_ADD_ASSIGN   };
   ["-="]    =>  { ::syntax::SyntaxKind::BINOP_SUB_ASSIGN   };
   ["*="]    =>  { ::syntax::SyntaxKind::BINOP_MUL_ASSIGN   };
   ["/="]    =>  { ::syntax::SyntaxKind::BINOP_DIV_ASSIGN   };
   [">="]    =>  { ::syntax::SyntaxKind::BINOP_GTE          };
   ["<="]    =>  { ::syntax::SyntaxKind::BINOP_LTE          };
   ["<="]    =>  { ::syntax::SyntaxKind::BINOP_LTE          };
   [">>="]   =>  { ::syntax::SyntaxKind::BINOP_SHIFT_R_ASSIGN };
   ["<<="]   =>  { ::syntax::SyntaxKind::BINOP_SHIFT_L_ASSIGN };

   [NULL]    =>  { ::syntax::SyntaxKind::MISC_NULL };
}

#[macro_export]
macro_rules! ast_token {
    ($NAME:ident, $KIND:ident) => {
        #[derive(Debug)]
        pub struct $NAME(SyntaxToken);
        impl AstElement for $NAME {
            fn can_cast(kind: ::syntax::SyntaxKind) -> bool {
                kind == ::syntax::SyntaxKind::$KIND
            }
            fn cast(element: crate::SyntaxElement) -> Result<Self, crate::ast::AstError> {
                element
                    .as_token()
                    .filter(|token| Self::can_cast(token.kind()))
                    .map(|token| Self(token.clone()))
                    .ok_or_else(|| crate::ast::AstError::InvalidCast)
            }
            fn syntax(&self) -> crate::SyntaxElement {
                crate::SyntaxElement::Token(self.0.clone())
            }
        }
    };
}

#[macro_export]
macro_rules! ast_node {
    ($NAME:ident, $KIND:ident) => {
        #[derive(Debug)]
        pub struct $NAME(SyntaxNode);
        impl AstElement for $NAME {
            fn can_cast(kind: ::syntax::SyntaxKind) -> bool {
                kind == ::syntax::SyntaxKind::$KIND
            }
            fn cast(element: crate::SyntaxElement) -> Result<Self, crate::ast::AstError> {
                element
                    .as_node()
                    .filter(|node| Self::can_cast(node.kind()))
                    .map(|node| Self(node.clone()))
                    .ok_or_else(|| crate::ast::AstError::InvalidCast)
            }
            fn syntax(&self) -> crate::SyntaxElement {
                crate::SyntaxElement::Node(self.0.clone())
            }
        }
    };
}

#[macro_export]
macro_rules! impl_from {
    ($TYPE:ty, $(
        $NAME:ident => $VARIANT:ident,
    )*) => {
        $(
            impl From<$NAME> for $TYPE {
                fn from(lit: $NAME) -> Self {
                    Self::$VARIANT(lit)
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! gen_binop_node {
    ($NAME:ident, $KIND:ident) => {
        crate::ast_node!($NAME, $KIND);
        impl $NAME {
            #[inline]
            pub fn lhs_expr(&self) -> Option<crate::ast::nodes::Expr> {
                crate::ast::utils::children::<crate::ast::nodes::Expr>(&self.0).nth(0)
            }
            #[inline]
            pub fn rhs_expr(&self) -> Option<crate::ast::nodes::Expr> {
                crate::ast::utils::children::<crate::ast::nodes::Expr>(&self.0).nth(1)
            }
            #[inline]
            pub fn operator(&self) -> Option<crate::ast::nodes::Operator> {
                crate::ast::utils::children::<crate::ast::nodes::Operator>(&self.0).next()
            }
        }
    };
}
