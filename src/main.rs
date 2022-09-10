use std::sync::Arc;

use parser::print_syntax_tree;

const SRC: &str = ":A";

fn main() {
    print_syntax_tree(Arc::from(SRC));
}
