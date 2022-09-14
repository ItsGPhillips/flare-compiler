use crate::{builder::SyntaxTreeBuilder, Tkn};
use rowan::Checkpoint;
use scopeguard::guard;
use syntax::SyntaxKind;

impl SyntaxTreeBuilder {
    pub(crate) fn legal_item_start() -> impl Iterator<Item = SyntaxKind> {
        [Tkn!["fn"], Tkn!["struct"], Tkn!["enum"], Tkn!["import"]].into_iter()
    }
    pub(crate) fn parse_module(&mut self) {
        let c = self.checkpoint();
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::MODULE);
        });
        while !stb.current_token().is_null() {
            stb.skip_whitespace();
            stb.parse_item();
            stb.skip_whitespace();
        }
    }
    pub(crate) fn parse_item(&mut self) {
        let mut c = None;
        if self.current_token().is(Tkn!["pub"]) {
            c = Some(self.checkpoint());
            self.advance(true);
        }
        match self.error_until(Self::legal_item_start) {
            Some(Tkn!["fn"]) => {
                self.parse_fn_item(c);
                return;
            }
            Some(_kind) => {
                return;
            }
            None => return,
        }
    }
    fn parse_fn_item(&mut self, c: Option<Checkpoint>) {
        let fallback_c = self.checkpoint();
        // eat the "fn" keyword
        self.advance(true);
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c.unwrap_or(fallback_c), SyntaxKind::ITEM_FN);
        });
        stb.parse_fn_sig();
        stb.skip_whitespace();
        match stb.error_until(|| [Tkn!["{"]].into_iter()) {
            Some(Tkn!["{"]) => stb.parse_block_expr(),
            _ => return,
        }
    }
    fn parse_fn_sig(&mut self) {
        self.skip_whitespace();
        let c = self.checkpoint();
        // create the node when the function returns
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::FN_SIGNATURE);
        });
        match stb.error_until(|| [Tkn![IDENT]].into_iter()) {
            Some(_) => stb.advance(true),
            None => return,
        }
        match stb.error_until(|| [Tkn!["("]].into_iter()) {
            Some(_) => stb.parse_fn_parameters(),
            None => return,
        }

        if stb.current_token().is(Tkn![":"]) {
            let ret_ty = stb.checkpoint();
            stb.advance(true);
            stb.parse_type();
            stb.finish_node_at(ret_ty, SyntaxKind::RETURN_TYPE);
        }
    }
    fn parse_fn_parameters(&mut self) {
        let c = self.checkpoint();
        // eat the (
        self.advance(true);
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::FN_PARAM_LIST);
        });
        while !stb.current_token().is(Tkn![")"]) {
            match stb.error_until(|| [Tkn![IDENT], Tkn!["mut"], Tkn![","], Tkn![")"]].into_iter()) {
                Some(Tkn![","]) => {
                    stb.advance(true);
                }
                Some(Tkn![")"]) => {
                    stb.advance(false);
                    return;
                }
                Some(_) => stb.parse_typed_pattern(),
                None => return,
            }
        }
        stb.advance(false);
    }
    fn parse_typed_pattern(&mut self) {
        self.skip_whitespace();
        let c = self.checkpoint();
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::FN_PARAMETER);
        });
        if stb.current_token().is(Tkn!["mut"]) {
            stb.advance(true);
            stb.skip_whitespace();
        }
        match stb.error_until(|| [Tkn![IDENT], Tkn![","], Tkn![")"]].into_iter()) {
            Some(Tkn![IDENT]) => stb.advance(true),
            _ => return,
        }
        match stb.error_until(|| [Tkn![":"], Tkn![","], Tkn![")"]].into_iter()) {
            Some(Tkn![":"]) => stb.advance(true),
            _ => return,
        }
        stb.parse_type();
        stb.skip_whitespace();
    }
}
