use itertools::Itertools;
use syntax::SyntaxKind;

use crate::{builder::SyntaxTreeBuilder, Tkn};

impl SyntaxTreeBuilder {
    pub(crate) fn parse_block_item(&mut self) {
        self.skip_whitespace();
        if self.current_token().is(Tkn![";"]) {
            self.start_node(SyntaxKind::EXPR_STMT);
            self.advance(false);
            self.finish_node();
            return;
        }
        match self.error_until(|| Self::legal_expr_start().chain(Self::legal_item_start())) {
            Some(t) if Self::legal_expr_start().contains(&t) => self.parse_expr_stmt(),
            Some(t) if Self::legal_item_start().contains(&t) => self.parse_item(),
            _ => return,
        }
    }
    fn parse_expr_stmt(&mut self) {
        let c = self.checkpoint();
        if requires_semicolon_as_stmt(self.parse_expr()) {
            if self.expect(Tkn![";"], true) {
            }
        } else if self.current_token().is(Tkn![";"]) {
            self.advance(false);
        }
        self.finish_node_at(c, SyntaxKind::EXPR_STMT);
        self.skip_whitespace();
        return;
    }
}

fn requires_semicolon_as_stmt(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(kind, EXPR_LET | EXPR_RETURN)
}
