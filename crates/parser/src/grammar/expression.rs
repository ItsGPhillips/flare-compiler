use syntax::SyntaxKind;

use crate::{builder::SyntaxTreeBuilder, Tkn};

impl SyntaxTreeBuilder {
    pub(crate) fn parse_expr(&mut self) {}

    pub(crate) fn parse_operand(&mut self) {
        let current = self.current_token();
        match current.kind() {
            Tkn![IDENT]
            | Tkn!["Self"]
            | Tkn!["self"]
            | Tkn![":"]
            | Tkn!["crate"]
            | Tkn!["super"] => self.parse_path(),

            Tkn![INTEGER] => self.parse_integer_literal(),
            Tkn![FLOAT] => self.parse_string_literal(),
            Tkn!["\""] => self.parse_string_literal(),
            Tkn!["\'"] => self.parse_char_literal(),

            _ => {}
        }
    }

    pub(crate) fn parse_float_literal(&mut self) {
        self.start_node(SyntaxKind::LIT_FLOAT);
        self.advance(false);
        self.finish_node();
        self.skip_whitespace();
    }

    pub(crate) fn parse_integer_literal(&mut self) {
        self.start_node(SyntaxKind::LIT_INTEGER);
        self.advance(false);
        self.finish_node();
        self.skip_whitespace();
    }

    pub(crate) fn parse_string_literal(&mut self) {
        self.start_node(SyntaxKind::LIT_STRING);
        self.advance(false);
        while !self.current_token().is(Tkn!["\""]) {
            self.advance(false);
        }
        self.finish_node();
        self.skip_whitespace();
    }

    pub(crate) fn parse_char_literal(&mut self) {
        self.start_node(SyntaxKind::LIT_CHAR);
        self.advance(false);
        while !self.current_token().is(Tkn!["\'"]) {
            self.advance(false);
        }
        self.finish_node();
        self.skip_whitespace();
    }
}
