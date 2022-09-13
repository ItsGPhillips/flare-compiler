
use parser::parse_text;

const SRC: &str = "1 * 2 + 3 * 4";

fn main() {
    parse_text(SRC.into());
}
