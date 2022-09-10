use syntax::SyntaxKind;

use crate::{SyntaxTreeBuilder, Tkn};

impl SyntaxTreeBuilder {
    pub(crate) fn parse_path(&mut self) {
        // let core::Thing { value } : core::Thing = 10;
        // ::core
        // ::a::b
        // a::b
        // ::thing::<f32>::call()

        fn try_parse_path_seperator(stb: &mut SyntaxTreeBuilder) {
            let c = stb.checkpoint();
            if stb.current_token().is(Tkn![":"]) {
                stb.advance(false);
                stb.expect(Tkn![":"], true);
                stb.finish_node_at(c, SyntaxKind::PATH_SEPERATOR);
            }
        }

        // ==========================================================================

        let path = self.checkpoint();

        loop {
            try_parse_path_seperator(self);
            let c = self.checkpoint();

            match self.expect_one_of(
                &[
                    Tkn![IDENT],
                    Tkn!["Self"],
                    Tkn!["self"],
                    Tkn!["crate"],
                    Tkn!["super"],
                ],
                false,
            ) {
                Some(_) => {
                    self.advance(false);
                    self.finish_node_at(c, SyntaxKind::PATH_SEGMENT_NAMED);
                    self.skip_whitespace();
                }
                None => {}
            }

            if !self.current_token().is(Tkn![":"]) {
                break;
            }
        }
        self.finish_node_at(path, SyntaxKind::PATH)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SyntaxNode;
    use pretty_assertions::{assert_eq, assert_ne};

    fn syntax_output(src: &str) -> String {
        let mut builder = SyntaxTreeBuilder::new(src.into());
        builder.parse_path();
        let root = SyntaxNode::new_root(builder.finish());
        format!("{:#?}", root).replace(" ", "")
    }

    #[test]
    fn path_01() {
        let output = syntax_output("test");
        assert_eq!(
            output,
            "PATH@0..4\n\
                PATH_SEGMENT_NAMED@0..4\n\
                    IDENTIFIER@0..4\"test\"\n"
        )
    }
    #[test]
    fn path_02() {
        let output = syntax_output("::A");
        assert_eq!(
            output,
            "PATH@0..3\n\
                PATH_SEPERATOR@0..2\n\
                    PUNC_COLON@0..1\":\"\n\
                    PUNC_COLON@1..2\":\"\n\
                PATH_SEGMENT_NAMED@2..3\n\
                    IDENTIFIER@2..3\"A\"\n"
        )
    }
    #[test]
    fn path_03() {
        let output = syntax_output(":A");
        assert_eq!(
            output,
            "PATH@0..2\n\
                PATH_SEPERATOR@0..1\n\
                    PUNC_COLON@0..1\":\"\n\
                PATH_SEGMENT_NAMED@1..2\n\
                    IDENTIFIER@1..2\"A\"\n"
        )
    }
    #[test]
    fn path_04() {
        let output = syntax_output("::A::B");
        assert_eq!(
            output,
            "PATH@0..6\n\
                PATH_SEPERATOR@0..2\n\
                    PUNC_COLON@0..1\":\"\n\
                    PUNC_COLON@1..2\":\"\n\
                PATH_SEGMENT_NAMED@2..3\n\
                    IDENTIFIER@2..3\"A\"\n\
                PATH_SEPERATOR@3..5\n\
                    PUNC_COLON@3..4\":\"\n\
                    PUNC_COLON@4..5\":\"\n\
                PATH_SEGMENT_NAMED@5..6\n\
                    IDENTIFIER@5..6\"B\"\n"
        )
    }
    #[test]
    fn path_05() {
        let output = syntax_output(":: A");
        assert_eq!(
            output,
            "PATH@0..6\n\
                PATH_SEPERATOR@0..2\n\
                    PUNC_COLON@0..1\":\"\n\
                    PUNC_COLON@1..2\":\"\n\
                PATH_SEGMENT_NAMED@2..3\n\
                    IDENTIFIER@2..3\"A\"\n\
                PATH_SEPERATOR@3..5\n\
                    PUNC_COLON@3..4\":\"\n\
                    PUNC_COLON@4..5\":\"\n\
                PATH_SEGMENT_NAMED@5..6\n\
                    IDENTIFIER@5..6\"B\"\n"
        )
    }
}
