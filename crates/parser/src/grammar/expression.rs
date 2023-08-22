use rowan::Checkpoint;
use scopeguard::guard;
use syntax::SyntaxKind;

use crate::{builder::SyntaxTreeBuilder, Tkn};

impl SyntaxTreeBuilder {
    pub(crate) fn legal_expr_start() -> impl Iterator<Item = SyntaxKind> {
        [
            Tkn![IDENT],
            Tkn![INTEGER],
            Tkn![FLOAT],
            Tkn!["\""],
            Tkn!["\'"],
            Tkn!["{"],
            Tkn!["let"],
            Tkn!["&"],
            Tkn!["return"],
            Tkn!["Self"],
            Tkn!["self"],
            Tkn![":"],
            Tkn!["crate"],
            Tkn!["super"],
        ]
        .into_iter()
    }

    pub(crate) fn parse_expr(&mut self) -> SyntaxKind {
        #[inline]
        fn precedence_parser_impl(
            mut stb: &mut SyntaxTreeBuilder,
            last_binop: SyntaxKind,
        ) -> SyntaxKind {
            stb.skip_whitespace();
            let c = stb.checkpoint();
            let mut kind = stb.parse_operand();
            stb.skip_whitespace();
            loop {
                if let Some((new_binop, _)) = stb.peek_binop() {
                    let new_prec = get_binop_precidence(new_binop);
                    let last_prec = get_binop_precidence(last_binop);
                    if new_prec > last_prec {
                        stb.try_parse_binop();
                        kind = precedence_parser_impl(&mut stb, new_binop);
                        stb.finish_node_at(c, new_binop)
                    } else {
                        return kind;
                    }
                } else {
                    return kind;
                }
            }
        }
        precedence_parser_impl(self, Tkn![NULL])
    }

    pub(crate) fn try_parse_binop(&mut self) -> Option<SyntaxKind> {
        self.skip_whitespace();
        if let Some((kind, num)) = self.peek_binop() {
            self.start_node(SyntaxKind::OPERATOR);
            (0..num).for_each(|_| self.advance(false));
            self.finish_node();
            Some(kind)
        } else {
            None
        }
    }

    fn peek_binop(&mut self) -> Option<(SyntaxKind, u8)> {
        use SyntaxKind::*;
        let current = self.current_token();
        let peek1 = self.peek_kind(1, false);
        let peek2 = self.peek_kind(2, false);
        let out = match (current.kind(), peek1, peek2) {
            (Tkn!["<"], Tkn!["<"], Tkn!["="]) => (BINOP_SHIFT_L_ASSIGN, 3),
            (Tkn![">"], Tkn![">"], Tkn!["="]) => (BINOP_SHIFT_R_ASSIGN, 3),
            (Tkn!["+"], Tkn!["="], _) => (BINOP_ADD_ASSIGN, 2),
            (Tkn!["-"], Tkn!["="], _) => (BINOP_SUB_ASSIGN, 2),
            (Tkn!["*"], Tkn!["="], _) => (BINOP_MUL_ASSIGN, 2),
            (Tkn!["/"], Tkn!["="], _) => (BINOP_DIV_ASSIGN, 2),
            (Tkn!["="], Tkn!["="], _) => (BINOP_EQ, 2),
            (Tkn!["!"], Tkn!["="], _) => (BINOP_NOT_EQ, 2),
            (Tkn!["<"], Tkn!["<"], _) => (BINOP_SHIFT_L, 2),
            (Tkn![">"], Tkn![">"], _) => (BINOP_SHIFT_R, 2),
            (Tkn![">"], Tkn!["="], _) => (BINOP_GTE, 2),
            (Tkn!["<"], Tkn!["="], _) => (BINOP_LTE, 2),
            (Tkn!["&"], Tkn!["&"], _) => (BINOP_AND, 2),
            (Tkn!["|"], Tkn!["|"], _) => (BINOP_OR, 2),
            (Tkn!["&"], _, _) => (BINOP_BITAND, 1),
            (Tkn!["|"], _, _) => (BINOP_BITOR, 1),
            (Tkn!["="], _, _) => (BINOP_ASSIGN, 1),
            (Tkn!["+"], _, _) => (BINOP_ADD, 1),
            (Tkn!["-"], _, _) => (BINOP_SUB, 1),
            (Tkn!["*"], _, _) => (BINOP_MUL, 1),
            (Tkn!["/"], _, _) => (BINOP_DIV, 1),
            (Tkn!["%"], _, _) => (BINOP_MOD, 1),
            (Tkn![">"], _, _) => (BINOP_GT, 1),
            (Tkn!["<"], _, _) => (BINOP_LT, 1),
            _ => return None,
        };
        Some(out)
    }

    pub(crate) fn parse_operand(&mut self) -> SyntaxKind {
        self.try_parse_prefix_ops();

        let c = self.checkpoint();
        let current = self.current_token();

        let kind = match current.kind() {
            Tkn![IDENT]
            | Tkn!["Self"]
            | Tkn!["self"]
            | Tkn![":"]
            | Tkn!["crate"]
            | Tkn!["super"] => self.parse_path(true),
            Tkn!["{"] => {
                self.parse_block_expr();
                SyntaxKind::EXPR_BLOCK
            }
            Tkn!["return"] => self.parse_return_expr(),
            Tkn!["let"] => self.parse_let_expr(),
            Tkn![INTEGER] | Tkn![FLOAT] => {
                self.advance(false);
                current.kind()
            }
            Tkn!["("] => self.parse_tuple_expr(),
            Tkn!["\""] => self.parse_string_literal(),
            Tkn!["\'"] => self.parse_char_literal(),
            _ => Tkn![NULL],
        };
        self.try_parse_postfix_ops(c);

        kind
    }

    pub(crate) fn parse_tuple_expr(&mut self) -> SyntaxKind {
        let c = self.checkpoint();
        // eat the ( token
        self.advance(true);

        // create the node when the function returns
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::EXPR_TUPLE);
        });

        while !stb.current_token().is(Tkn![")"]) {
            match stb.error_until(|| Self::legal_expr_start().chain([Tkn![","], Tkn![")"]])) {
                Some(Tkn![","]) => {
                    stb.advance(true);
                }
                Some(Tkn![")"]) => {
                    stb.advance(false);
                    break;
                }
                Some(_) => {
                    stb.parse_expr();
                }
                None => break,
            }
        }
        SyntaxKind::EXPR_TUPLE
    }

    pub(crate) fn try_parse_postfix_ops(&mut self, c: Checkpoint) -> Option<SyntaxKind> {
        let mut kind = None;
        loop {
            match self.current_token().kind() {
                Tkn!["?"] => {
                    self.advance(true);
                    self.finish_node_at(c, SyntaxKind::UNOP_TRY);
                    kind = Some(SyntaxKind::UNOP_TRY);
                }
                Tkn!["."] => {
                    // x.
                    self.advance(true);

                    match self.current_token().kind() {
                        _ => {
                            self.parse_operand();
                        }
                    }
                    self.finish_node_at(c, SyntaxKind::MEMBER_ACCESS);
                    kind = Some(SyntaxKind::MEMBER_ACCESS);
                }
                Tkn!["("] => kind = Some(self.parse_call_op(c)),
                _ => return kind,
            }
        }
    }
    pub(crate) fn parse_call_op(&mut self, c: Checkpoint) -> SyntaxKind {
        // eat the (
        self.advance(true);
        self.skip_whitespace();
        // create the node when the function returns
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::UNOP_CALL);
        });

        while !stb.current_token().is(Tkn![")"]) {
            match stb.error_until(|| Self::legal_expr_start().chain([Tkn![","], Tkn![")"]])) {
                Some(Tkn![","]) => {
                    stb.advance(true);
                }
                Some(Tkn![")"]) => {
                    stb.advance(false);
                    return SyntaxKind::UNOP_CALL;
                }
                Some(_) => {
                    stb.parse_expr();
                }
                None => return SyntaxKind::UNOP_CALL,
            }
        }
        stb.advance(false);

        SyntaxKind::UNOP_CALL
    }
    pub(crate) fn try_parse_prefix_ops(&mut self) {
        // TODO(George): make this non-recurssive???
        match self.current_token().kind() {
            Tkn!["!"] => {
                self.start_node(SyntaxKind::UNOP_NOT);
                self.advance(true);
            }
            Tkn!["&"] => {
                self.start_node(SyntaxKind::UNOP_REF);
                self.advance(true);
                self.current_token()
                    .is(Tkn!["mut"])
                    .then(|| self.advance(true));
            }
            Tkn!["*"] => {
                self.start_node(SyntaxKind::UNOP_DEREF);
                self.advance(true);
            }
            Tkn!["-"] => {
                self.start_node(SyntaxKind::UNOP_NEG);
                self.advance(true);
            }
            _ => return,
        }
        self.parse_expr();
        self.finish_node();
    }

    pub(crate) fn parse_string_literal(&mut self) -> SyntaxKind {
        self.start_node(SyntaxKind::LIT_STRING);
        self.advance(false);
        while !self.current_token().is(Tkn!["\""]) {
            self.advance(true);
        }
        self.advance(false);
        self.finish_node();
        SyntaxKind::LIT_STRING
    }

    pub(crate) fn parse_char_literal(&mut self) -> SyntaxKind {
        self.start_node(SyntaxKind::LIT_CHAR);
        self.advance(false);
        while !self.current_token().is(Tkn!["\'"]) {
            self.advance(false);
        }
        self.finish_node();
        SyntaxKind::LIT_CHAR
    }

    pub(crate) fn parse_return_expr(&mut self) -> SyntaxKind {
        self.skip_whitespace();
        let c = self.checkpoint();

        self.expect(Tkn!["return"], true);
        self.skip_whitespace();

        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::EXPR_RETURN);
        });

        match stb.error_until(|| Self::legal_expr_start().chain([Tkn![";"]])) {
            Some(Tkn![";"]) | None => {}
            Some(_) => {
                stb.parse_expr();
            }
        };
        SyntaxKind::EXPR_RETURN
    }

    pub(crate) fn parse_block_expr(&mut self) {
        assert!(self.current_token().kind() == Tkn!["{"]);

        self.start_node(SyntaxKind::EXPR_BLOCK);

        // eat the {
        self.advance(true);

        let mut stb = guard(self, |stb| {
            stb.finish_node();
        });
        while !stb.current_token().is(Tkn!["}"]) {
            stb.parse_block_item();
            stb.skip_whitespace();
        }
        stb.advance(false);
    }

    pub(crate) fn parse_let_expr(&mut self) -> SyntaxKind {
        assert!(matches!(
            self.current_token().kind(),
            Tkn!["let"] | Tkn!["const"] | Tkn!["static"]
        ));

        let binding_expr = self.current_token().kind().to_binding_expr();
        self.start_node(binding_expr);

        // eat the let | const | static
        self.advance(true);

        let mut stb = guard(self, |stb| {
            stb.finish_node();
        });

        if stb.current_token().is(Tkn!["mut"]) {
            stb.advance(true);
        }

        stb.parse_pattern(false);
        stb.skip_whitespace();

        match stb.error_until(|| [Tkn![":"], Tkn!["="], Tkn![";"]].into_iter()) {
            Some(Tkn![":"]) => {
                stb.parse_type_binding();
            }
            Some(Tkn!["="]) => {
                stb.parse_let_assignment();
                return binding_expr;
            }
            None | Some(Tkn![";"]) | Some(_) => {
                return binding_expr;
            }
        }

        stb.skip_whitespace();

        match stb.error_until(|| [Tkn!["="], Tkn![";"]].into_iter()) {
            Some(Tkn!["="]) => {
                stb.parse_let_assignment();
                return binding_expr;
            }
            Some(Tkn![";"]) | Some(_) | None => {
                return binding_expr;
            }
        }
    }

    fn parse_let_assignment(&mut self) {
        assert!(matches!(self.current_token().kind(), Tkn!["="]));
        self.start_node(SyntaxKind::ASSIGMENT);
        self.advance(true);
        self.parse_expr();
        self.finish_node();
    }
}

fn get_binop_precidence(kind: SyntaxKind) -> u8 {
    use SyntaxKind::*;
    match kind {
        Tkn![NULL] => 0,
        BINOP_ADD | BINOP_SUB => 3,
        BINOP_MUL | BINOP_DIV | BINOP_MOD => 5,
        BINOP_GT | BINOP_LT | BINOP_GTE | BINOP_LTE => 9,
        Tkn!["=="] | Tkn!["!="] => 10,
        BINOP_BITAND => 11,
        BINOP_XOR => 12,
        BINOP_BITOR => 13,
        BINOP_AND => 14,
        BINOP_OR => 15,
        Tkn!["+="] | Tkn!["-="] | Tkn!["*="] | Tkn!["/="] | Tkn!["<<="] | Tkn![">>="] => 16,
        kind => panic!("{kind} is not a Binop"),
    }
}
