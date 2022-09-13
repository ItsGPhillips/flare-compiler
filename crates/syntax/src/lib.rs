use derive_more::Display;

#[repr(u16)]
#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Display, Hash)]
pub enum SyntaxKind {
    WHITESPACE,
    IDENTIFIER,

    #[doc(hidden)]
    _KW_START_,

    /// fn
    KW_FN,
    /// let
    KW_LET,
    /// pub
    KW_PUB,
    /// mut
    KW_MUT,
    /// for
    KW_FOR,
    /// in
    KW_IN,
    /// loop
    KW_LOOP,
    /// const
    KW_CONST,
    /// static
    KW_STATIC,
    /// mod
    KW_MOD,
    /// struct
    KW_STRUCT,
    /// return
    KW_RETURN,
    /// if
    KW_IF,
    /// else
    KW_ELSE,
    /// while
    KW_WHILE,
    /// true
    KW_TRUE,
    /// false
    KW_FALSE,
    /// break
    KW_BREAK,
    /// continue
    KW_CONTINUE,
    /// Self
    KW_SELF_TYPE,
    /// self
    KW_SELF_VALUE,
    /// crate
    KW_CRATE,
    /// super
    KW_SUPER,

    #[doc(hidden)]
    _KW_END_,
    #[doc(hidden)]
    _PUNC_START_,
    /// \+
    PUNC_PLUS,
    /// \-
    PUNC_HYPHEN,
    /// \_
    PUNC_UNDERSCORE,
    /// \*
    PUNC_ASTERISK,
    /// \/
    PUNC_SLASH,
    /// \\
    PUNC_BACKSLASH,
    /// \)
    PUNC_RPAREN,
    /// \(
    PUNC_LPAREN,
    /// \>
    PUNC_RANGLE_BRACKET,
    /// \<
    PUNC_LANGLE_BRACKET,
    /// \}
    PUNC_RBRACE,
    /// \{
    PUNC_LBRACE,
    /// \]
    PUNC_RBRACKET,
    /// \[
    PUNC_LBRACKET,
    /// \=
    PUNC_EQUALS,
    /// \|
    PUNC_PIPE,
    /// \?
    PUNC_QUESTIONMARK,
    /// \!
    PUNC_EXCLAMATION,
    /// \&
    PUNC_AMPERSAND,
    /// \.
    PUNC_PERIOD,
    /// \:
    PUNC_COLON,
    /// \;
    PUNC_SEMICOLON,
    /// \"
    PUNC_QUOTE,
    /// \'
    PUNC_SINGLE_QUOTE,
    /// \%
    PUNC_PERCENT,
    /// \#
    PUNC_HASH,
    /// \@
    PUNC_AT,
    /// \$
    PUNC_DOLLAR,
    /// \~
    PUNC_TILDE,
    /// \`
    PUNC_BACKQUOTE,
    /// \,
    PUNC_COMMA,

    #[doc(hidden)]
    _PUNC_END_,
    #[doc(hidden)]
    _MISC_START_,
    /// NULL
    MISC_NULL,
    /// Unknown
    MISC_UNKNOWN,


    #[doc(hidden)]
    _MISC_END_,

    #[doc(hidden)]
    _EXPR_START_,

    #[doc(hidden)]
    _LIT_START_,

    LIT_INTEGER,
    LIT_FLOAT,
    LIT_STRING,
    LIT_CHAR,

    #[doc(hidden)]
    _LIT_END_,

    EXPR_TUPLE,

    /// ==
    BINOP_EQ,
    /// !=
    BINOP_NOT_EQ,
    /// +
    BINOP_ADD,
    /// -
    BINOP_SUB,
    /// *
    BINOP_MUL,
    /// /
    BINOP_DIV,
    /// %
    BINOP_MOD,
    /// >
    BINOP_GT,
    /// <
    BINOP_LT,
    /// >=
    BINOP_GTE,
    /// <=
    BINOP_LTE,
    /// ||
    BINOP_OR,
    /// |
    BINOP_BITOR,
    /// \^
    BINOP_XOR,
    /// &&
    BINOP_AND,
    /// &
    BINOP_BITAND,
    /// =
    BINOP_ASSIGN,
    /// +=
    BINOP_ADD_ASSIGN,
    /// -=
    BINOP_SUB_ASSIGN,
    /// *=
    BINOP_MUL_ASSIGN,
    /// /=
    BINOP_DIV_ASSIGN,

    /// <<
    BINOP_SHIFT_L,
    /// <<=
    BINOP_SHIFT_L_ASSIGN,
    /// \>>
    BINOP_SHIFT_R,
    /// \>>=
    BINOP_SHIFT_R_ASSIGN,
    
    /// !
    UNOP_NOT,
    /// &
    UNOP_REF,
    /// *
    UNOP_DEREF,
    /// -
    UNOP_NEG,
    /// ?
    UNOP_TRY,
    /// (...)
    UNOP_CALL,

    #[doc(hidden)]
    _EXPR_END_,

    OPERATOR,

    PATH,
    PATH_SEPERATOR,
    PATH_SEGMENT_NAMED,
    PATH_SEGMENT_GENERIC,
    
    MODULE,
    ERROR,
    // IMPORTANT: Make sure _LAST_MARKER_ is always the last variant
    #[doc(hidden)]
    _LAST_MARKER_,
}

impl SyntaxKind {
    /// converts a u16 into its [SyntaxKind] representation
    pub const fn from_raw(value: u16) -> Self {
        debug_assert!(value < Self::_LAST_MARKER_ as u16, "value was invalid");
        unsafe { std::mem::transmute(value) }
    }
    /// converts a [SyntaxKind] into its raw u16 representation
    pub const fn to_raw(&self) -> u16 {
        *self as u16
    }
    #[inline(always)]
    pub fn is_keyword(&self) -> bool {
        *self > Self::_KW_START_ && *self < Self::_KW_END_
    }
    #[inline(always)]
    pub fn is_literal(&self) -> bool {
        *self > Self::_LIT_START_ && *self < Self::_LIT_END_
    }
    #[inline(always)]
    pub fn is_punctution(&self) -> bool {
        *self > Self::_PUNC_START_ && *self < Self::_PUNC_END_
    }
    #[inline(always)]
    pub fn is_expression(&self) -> bool {
        *self > Self::_EXPR_START_ && *self < Self::_EXPR_END_ || matches!(*self, Self::PATH)
    }
    #[inline(always)]
    pub fn is_error(&self) -> bool {
        *self == Self::ERROR
    }
    #[inline(always)]
    pub fn is_whitespace(&self) -> bool {
        *self == Self::WHITESPACE
    }
    #[inline]
    #[rustfmt::skip]
    pub const fn as_str(&self) -> &'static str {
        use SyntaxKind::*;
        match self {
            WHITESPACE          => "WHITESACE",
            IDENTIFIER          => "IDENT",
            LIT_INTEGER         => "INTEGER",
            LIT_FLOAT           => "FLOAT",
            LIT_STRING          => "STRING",
            LIT_CHAR            => "CHAR",
            KW_FN               => "fn",
            KW_LET              => "let",
            KW_PUB              => "pub",
            KW_MUT              => "mut",
            KW_FOR              => "for",
            KW_IN               => "in",
            KW_LOOP             => "loop",
            KW_CONST            => "const",
            KW_STATIC           => "static",
            KW_MOD              => "mod",
            KW_STRUCT           => "struct",
            KW_RETURN           => "return",
            KW_IF               => "if",
            KW_ELSE             => "else",
            KW_WHILE            => "while",
            KW_TRUE             => "true",
            KW_FALSE            => "false",
            KW_BREAK            => "break",
            KW_CONTINUE         => "continue",
            KW_SELF_TYPE        => "Self",
            KW_SELF_VALUE       => "self",
            KW_SUPER            => "super",
            KW_CRATE            => "crate",
            PUNC_PLUS           => "+",
            PUNC_HYPHEN         => "-",
            PUNC_UNDERSCORE     => "_",
            PUNC_ASTERISK       => "*",
            PUNC_SLASH          => "/",
            PUNC_BACKSLASH      => "\\",
            PUNC_RPAREN         => ")",
            PUNC_LPAREN         => "(",
            PUNC_RANGLE_BRACKET => ">",
            PUNC_LANGLE_BRACKET => "<",
            PUNC_RBRACE         => "}",
            PUNC_LBRACE         => "{",
            PUNC_RBRACKET       => "]",
            PUNC_LBRACKET       => "[",
            PUNC_EQUALS         => "=",
            PUNC_PIPE           => "|",
            PUNC_QUESTIONMARK   => "?",
            PUNC_EXCLAMATION    => "!",
            PUNC_AMPERSAND      => "&",
            PUNC_PERIOD         => ".",
            PUNC_COLON          => ":",
            PUNC_SEMICOLON      => ";",
            PUNC_QUOTE          => "\"",
            PUNC_SINGLE_QUOTE    => "'",
            PUNC_PERCENT        => "%",
            PUNC_HASH           => "#",
            PUNC_AT             => "@",
            PUNC_DOLLAR         => "$",
            PUNC_TILDE          => "`",
            PUNC_BACKQUOTE      => "`",
            PUNC_COMMA          => ",",
            MISC_NULL           => "NULL",
            MISC_UNKNOWN        => "UNKNOWN",
            _ => panic!("Not Convertable to str")
        }
    }
}
