#[macro_export]
macro_rules! Tkn {
   [WHITESPACE]   =>  { ::syntax::SyntaxKind::WHITESPACE    };
   [IDENT]        =>  { ::syntax::SyntaxKind::IDENTIFIER    };
   [INTEGER]      =>  { ::syntax::SyntaxKind::LIT_INTEGER   };
   [FLOAT]        =>  { ::syntax::SyntaxKind::LIT_FLOAT     };
   ["Self"]       =>  { ::syntax::SyntaxKind::KW_SELF_TYPE  };
   ["self"]       =>  { ::syntax::SyntaxKind::KW_SELF_VALUE };
   ["crate"]      =>  { ::syntax::SyntaxKind::KW_CRATE      };
   ["super"]      =>  { ::syntax::SyntaxKind::KW_SUPER      };

   ["\""]   =>  { ::syntax::SyntaxKind::PUNC_QUOTE          };
   ["\'"]   =>  { ::syntax::SyntaxKind::PUNC_SINGLE_QUOTE   };
   [":"]    =>  { ::syntax::SyntaxKind::PUNC_COLON          };
   [";"]    =>  { ::syntax::SyntaxKind::PUNC_SEMICOLON      };
   ["<"]    =>  { ::syntax::SyntaxKind::PUNC_LANGLE_BRACKET };
   [">"]    =>  { ::syntax::SyntaxKind::PUNC_RANGLE_BRACKET };
   [","]    =>  { ::syntax::SyntaxKind::PUNC_COMMA          };
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
       #[derive(::derive_more::Deref, ::derive_more::DerefMut, Clone, Debug)]
       pub struct $NAME(SyntaxNode);
       impl AstNode for $NAME {
           type Language = crate::Flare;
           fn can_cast(kind: ::syntax::SyntaxKind) -> bool
           where
               Self: Sized,
           {
               kind == ::syntax::SyntaxKind::$KIND
           }
           fn cast(node: crate::SyntaxNode) -> Option<Self>
           where
               Self: Sized,
           {
               Self::can_cast(node.kind()).then(|| Self(node))
           }
           fn syntax(&self) -> &crate::SyntaxNode {
               &self
           }
       }
   };
}