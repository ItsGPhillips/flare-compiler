#[macro_export]
macro_rules! Tkn {
   [WHITESPACE]   =>  { ::syntax::SyntaxKind::WHITESPACE    };
   [IDENT]        =>  { ::syntax::SyntaxKind::IDENTIFIER    };
   [INTEGER]      =>  { ::syntax::SyntaxKind::LIT_INTEGER   };
   [FLOAT]        =>  { ::syntax::SyntaxKind::LIT_FLOAT     };
   ["Self"]       =>  { ::syntax::SyntaxKind::KW_SELF_TYPE  };
   ["self"]       =>  { ::syntax::SyntaxKind::KW_SELF_VALUE };
   ["mut"]        =>  { ::syntax::SyntaxKind::KW_MUT        };
   ["crate"]      =>  { ::syntax::SyntaxKind::KW_CRATE      };
   ["super"]      =>  { ::syntax::SyntaxKind::KW_SUPER      };

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
   ["("]    =>  { ::syntax::SyntaxKind::PUNC_LPAREN         };
   [")"]    =>  { ::syntax::SyntaxKind::PUNC_RPAREN         };
   ["+"]    =>  { ::syntax::SyntaxKind::PUNC_PLUS           };
   ["-"]    =>  { ::syntax::SyntaxKind::PUNC_HYPHEN         };
   ["%"]    =>  { ::syntax::SyntaxKind::PUNC_PERCENT        };
   ["/"]    =>  { ::syntax::SyntaxKind::PUNC_SLASH          };
   ["="]    =>  { ::syntax::SyntaxKind::PUNC_EQUALS         };
   ["|"]    =>  { ::syntax::SyntaxKind::PUNC_PIPE           };

   ["=="]    =>  { ::syntax::SyntaxKind::BINOP_EQ           };
   ["!="]    =>  { ::syntax::SyntaxKind::BINOP_NOT_EQ       };
   ["+="]    =>  { ::syntax::SyntaxKind::BINOP_ADD_ASSIGN   };
   ["-="]    =>  { ::syntax::SyntaxKind::BINOP_SUB_ASSIGN   };
   ["*="]    =>  { ::syntax::SyntaxKind::BINOP_MUL_ASSIGN   };
   ["/="]    =>  { ::syntax::SyntaxKind::BINOP_DIV_ASSIGN   };
   [">="]    =>  { ::syntax::SyntaxKind::BINOP_GTE          };
   ["<="]    =>  { ::syntax::SyntaxKind::BINOP_LTE          };

   [NULL]    =>  { ::syntax::SyntaxKind::MISC_NULL };
}

#[macro_export]
macro_rules! ast_token {
    ($NAME:ident, $KIND:ident) => {
        #[derive(::derive_more::Deref, ::derive_more::DerefMut, Clone, Debug)]
        pub struct $NAME(crate::SyntaxToken);
        impl crate::ast::tokens::AstToken for $NAME {
            fn can_cast(kind: ::syntax::SyntaxKind) -> bool
            where
                Self: Sized,
            {
                kind == ::syntax::SyntaxKind::$KIND
            }
            fn cast(node: crate::SyntaxToken) -> Option<Self>
            where
                Self: Sized,
            {
                Self::can_cast(node.kind()).then(|| Self(node))
            }
            fn syntax(&self) -> &crate::SyntaxToken {
                &self
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
            fn cast(element: crate::SyntaxElement) -> Result<Self, crate::ast::AstError> {
                element
                    .as_node()
                    .filter(|node| node.kind() == ::syntax::SyntaxKind::$KIND)
                    .map(|token| Self(token.clone()))
                    .ok_or_else(|| crate::ast::AstError::InvalidCast)
            }
            fn syntax(&self) -> crate::SyntaxElement {
                crate::SyntaxElement::Node(self.0.clone())
            }
        }
    };
}

#[macro_export]
macro_rules! impl_expr_from {
    ($(
        $NAME:ident => $VARIANT:ident,
    )*) => {
        $(
            impl From<$NAME> for Expr {
                fn from(lit: $NAME) -> Self {
                    Self::$VARIANT(lit)
                }
            }
        )*
    };
}