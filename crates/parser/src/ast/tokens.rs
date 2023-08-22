use crate::{ast_token, SyntaxToken};

ast_token!(Ident, IDENTIFIER);
ast_token!(LitInteger, LIT_INTEGER);
ast_token!(LitFloat, LIT_FLOAT);
ast_token!(LitString, LIT_STRING);
ast_token!(LitChar, LIT_CHAR);
