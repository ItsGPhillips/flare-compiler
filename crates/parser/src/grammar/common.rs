use scopeguard::guard;
use syntax::SyntaxKind;

use crate::{SyntaxTreeBuilder, Tkn};

impl SyntaxTreeBuilder {
    pub(crate) fn legal_path_start() -> impl Iterator<Item = SyntaxKind> {
        [
            Tkn![IDENT],
            Tkn!["Self"],
            Tkn!["self"],
            Tkn![":"],
            Tkn!["crate"],
            Tkn!["super"],
        ]
        .into_iter()
    }

    pub(crate) fn legal_type_start() -> impl Iterator<Item = SyntaxKind> {
        [
            Tkn![IDENT],
            Tkn!["Self"],
            Tkn!["_"],
            Tkn!["("],
            Tkn!["["],
            Tkn!["*"],
        ]
        .into_iter()
    }

    pub(crate) fn parse_path(&mut self) -> SyntaxKind {
        fn try_parse_path_seperator(stb: &mut SyntaxTreeBuilder) {
            let c = stb.checkpoint();
            if stb.current_token().is(Tkn![":"]) {
                stb.advance(false);
                stb.expect(Tkn![":"], true);
                stb.finish_node_at(c, SyntaxKind::PATH_SEPERATOR);
                stb.skip_whitespace();
            }
        }
        // ==========================================================================

        let path = self.checkpoint();
        loop {
            try_parse_path_seperator(self);
            let segment = self.checkpoint();
            match self.expect_one_of(Self::legal_path_start(), false) {
                Some(_) => {
                    self.advance(false);
                    self.finish_node_at(segment, SyntaxKind::PATH_SEGMENT_NAMED);
                    self.skip_whitespace();
                }
                None => {
                    let error = self.checkpoint();
                    self.advance(false);
                    self.finish_node_at(error, SyntaxKind::ERROR);
                    self.skip_whitespace();
                }
            }
            if !self.current_token().is(Tkn![":"]) {
                break;
            }
        }
        self.finish_node_at(path, SyntaxKind::PATH);
        SyntaxKind::PATH
    }

    pub(crate) fn parse_type(&mut self)
    {
        self.skip_whitespace();
        let c = self.checkpoint();

        if self.current_token().is(Tkn!["&"]) {
            let ref_mod = self.checkpoint();
            self.advance(true);
            if self.current_token().is(Tkn!["mut"]) {
                self.advance(false);
            }
            self.finish_node_at(ref_mod, SyntaxKind::REF_MODIFIER);
            self.skip_whitespace();
        }

        match self.error_until(|| Self::legal_type_start()) {
            Some(Tkn![IDENT]) => {
                self.parse_path();
                self.finish_node_at(c, SyntaxKind::TYPE_NAMED);
            }
            Some(Tkn!["("]) => {
                self.parse_tuple_type();
            }
            Some(Tkn!["_"]) => {
                self.advance(false);
                self.finish_node_at(c, SyntaxKind::TYPE_UNNAMED);
                self.skip_whitespace();
            }
            Some(_) => unimplemented!(),
            None => return,
        }
    }

    // (&mut T, String, ())
    pub(crate) fn parse_tuple_type(&mut self) {
        let c = self.checkpoint();
        // eat the [
        self.advance(true);
        self.skip_whitespace();
        // create the node when the function returns
        let mut stb = guard(self, |stb| {
            stb.finish_node_at(c, SyntaxKind::TYPE_TUPLE);
        });
        stb.advance(false);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SyntaxNode;
    use indoc::indoc;
    use pretty_assertions::{assert_eq, assert_ne};

    fn syntax_output(src: &str) -> String {
        let mut builder = SyntaxTreeBuilder::new(src.into());
        builder.parse_path();
        let root = SyntaxNode::new_root(builder.finish());
        format!("{:#?}", root)
    }

    #[test]
    fn path_01() {
        let output = syntax_output("test");
        assert_eq!(
            output,
            concat!(
                "PATH@0..4\n",
                "  PATH_SEGMENT_NAMED@0..4\n",
                "    IDENTIFIER@0..4 \"test\"\n",
            )
        )
    }

    #[test]
    fn path_02() {
        let output = syntax_output("::A");
        assert_eq!(
            output,
            concat!(
                "PATH@0..3\n",
                "  PATH_SEPERATOR@0..2\n",
                "    PUNC_COLON@0..1 \":\"\n",
                "    PUNC_COLON@1..2 \":\"\n",
                "  PATH_SEGMENT_NAMED@2..3\n",
                "    IDENTIFIER@2..3 \"A\"\n",
            )
        )
    }
    #[test]
    fn path_03() {
        let output = syntax_output(":A");
        assert_eq!(
            output,
            concat!(
                "PATH@0..2\n",
                "  PATH_SEPERATOR@0..1\n",
                "    PUNC_COLON@0..1 \":\"\n",
                "  PATH_SEGMENT_NAMED@1..2\n",
                "    IDENTIFIER@1..2 \"A\"\n",
            )
        )
    }
    #[test]
    fn path_04() {
        let output = syntax_output("::A::B");
        assert_eq!(
            output,
            concat!(
                "PATH@0..6\n",
                "  PATH_SEPERATOR@0..2\n",
                "    PUNC_COLON@0..1 \":\"\n",
                "    PUNC_COLON@1..2 \":\"\n",
                "  PATH_SEGMENT_NAMED@2..3\n",
                "    IDENTIFIER@2..3 \"A\"\n",
                "  PATH_SEPERATOR@3..5\n",
                "    PUNC_COLON@3..4 \":\"\n",
                "    PUNC_COLON@4..5 \":\"\n",
                "  PATH_SEGMENT_NAMED@5..6\n",
                "    IDENTIFIER@5..6 \"B\"\n",
            )
        )
    }
    #[test]
    fn path_05() {
        let output = syntax_output(":: A");
        assert_eq!(
            output,
            concat!(
                "PATH@0..4\n",
                "  PATH_SEPERATOR@0..2\n",
                "    PUNC_COLON@0..1 \":\"\n",
                "    PUNC_COLON@1..2 \":\"\n",
                "  WHITESPACE@2..3 \" \"\n",
                "  PATH_SEGMENT_NAMED@3..4\n",
                "    IDENTIFIER@3..4 \"A\"\n",
            )
        )
    }
    #[test]
    fn path_06() {
        let output = syntax_output(":: A :: B");
        assert_eq!(
            output,
            indoc! {"
                PATH@0..9
                  PATH_SEPERATOR@0..2
                    PUNC_COLON@0..1 \":\"
                    PUNC_COLON@1..2 \":\"
                  WHITESPACE@2..3 \" \"
                  PATH_SEGMENT_NAMED@3..4
                    IDENTIFIER@3..4 \"A\"
                  WHITESPACE@4..5 \" \"
                  PATH_SEPERATOR@5..7
                    PUNC_COLON@5..6 \":\"
                    PUNC_COLON@6..7 \":\"
                  WHITESPACE@7..8 \" \"
                  PATH_SEGMENT_NAMED@8..9
                    IDENTIFIER@8..9 \"B\"
            "}
        )
    }
}
