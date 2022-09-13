use rowan::Checkpoint;
use scopeguard::guard;
use syntax::SyntaxKind;

use crate::{builder::SyntaxTreeBuilder, Tkn};

impl SyntaxTreeBuilder {
    pub(crate) fn legal_expr_start() -> impl Iterator<Item = SyntaxKind> {
        [
            Tkn![IDENT],
            Tkn!["Self"],
            Tkn!["self"],
            Tkn![":"],
            Tkn!["crate"],
            Tkn!["super"],
            Tkn![INTEGER],
            Tkn![FLOAT],
            Tkn!["\""],
            Tkn!["\'"],
        ]
        .into_iter()
    }

    pub(crate) fn parse_module(&mut self) {
        self.start_node(SyntaxKind::MODULE);
        self.parse_expr(0);
        self.finish_node();
    }

    pub(crate) fn parse_expr(&mut self, current_prec: u8) {
        // self.skip_whitespace();
        // let expr = self.checkpoint();
        // self.parse_operand();

        // loop {
        //     if let Some((kind, _)) = self.peek_binop() {
        //         self.skip_whitespace();

        //         let new_prec = get_binop_precidence(kind);
        //         if current_prec < new_prec {
        //             self.try_parse_binop().expect("This must always be some");
        //             self.parse_expr(new_prec);
        //         } else {
        //             self.parse_operand();
        //             self.finish_node_at(expr, kind);
        //         }

        //     } else {
        //         break;
        //     }
        // }

        self.binop_impl(Tkn![NULL]);
    }

    fn binop_impl(&mut self, last_binop: SyntaxKind) {
        self.skip_whitespace();
        let c = self.checkpoint();
        self.parse_operand();
        self.skip_whitespace();
        loop {
            if let Some((new_binop, _)) = self.peek_binop() {
                let new_prec = get_binop_precidence(new_binop);
                let last_prec = get_binop_precidence(last_binop);
                if new_prec > last_prec {
                    self.try_parse_binop();
                    self.binop_impl(new_binop);
                    self.finish_node_at(c, new_binop)
                } else {
                    return;
                }
            } else {
                return;
            }
        }
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

    pub(crate) fn parse_operand(&mut self) {
        self.try_parse_prefix_ops();

        let c = self.checkpoint();
        let current = self.current_token();

        match current.kind() {
            Tkn![IDENT]
            | Tkn!["Self"]
            | Tkn!["self"]
            | Tkn![":"]
            | Tkn!["crate"]
            | Tkn!["super"] => self.parse_path(),

            Tkn![INTEGER] | Tkn![FLOAT] => self.advance(false),
            Tkn!["\""] => self.parse_string_literal(),
            Tkn!["\'"] => self.parse_char_literal(),
            _ => {}
        }
        self.try_parse_postfix_ops(c);
    }

    pub(crate) fn try_parse_postfix_ops(&mut self, c: Checkpoint) {
        loop {
            match self.current_token().kind() {
                Tkn!["?"] => {
                    self.advance(true);
                    self.finish_node_at(c, SyntaxKind::UNOP_TRY)
                }
                Tkn!["("] => self.parse_call_op(c),
                _ => return,
            }
        }
    }

    pub(crate) fn parse_call_op(&mut self, c: Checkpoint) {
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
                    return;
                }
                Some(_) => stb.parse_expr(0),
                None => return,
            }
        }
        stb.advance(false);
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
        self.parse_expr(0);
        self.finish_node();
    }

    pub(crate) fn parse_string_literal(&mut self) {
        self.start_node(SyntaxKind::LIT_STRING);
        self.advance(false);
        while !self.current_token().is(Tkn!["\""]) {
            self.advance(true);
        }
        self.advance(false);
        self.finish_node();
    }

    pub(crate) fn parse_char_literal(&mut self) {
        self.start_node(SyntaxKind::LIT_CHAR);
        self.advance(false);
        while !self.current_token().is(Tkn!["\'"]) {
            self.advance(false);
        }
        self.finish_node();
    }
}

fn get_binop_precidence(kind: SyntaxKind) -> u8 {
    use SyntaxKind::*;
    match kind {
        Tkn![NULL] => 0,
        BINOP_ADD | BINOP_SUB => 3,
        BINOP_MUL | BINOP_DIV | BINOP_MOD => 5,
        Tkn!["=="] | Tkn!["!="] => 5,
        Tkn!["="] | Tkn!["+="] | Tkn!["-="] | Tkn!["*="] | Tkn!["/="] => 16,
        kind => panic!("{kind} is not a Binop"),
    }
}
