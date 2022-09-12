use rowan::Checkpoint;
use syntax::SyntaxKind;

use crate::{builder::SyntaxTreeBuilder, Tkn};

impl SyntaxTreeBuilder {
    pub(crate) fn parse_module(&mut self) {
        self.start_node(SyntaxKind::MODULE);
        self.parse_expr();
        self.finish_node();
    }

    pub(crate) fn parse_expr(&mut self) {
        let expr = self.checkpoint();
        self.parse_operand();
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
                Tkn!["?"] => self.finish_node_at(c, SyntaxKind::UNOP_TRY),
                _ => return
            }
            self.advance(true);
        }
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
