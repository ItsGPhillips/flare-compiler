
use parser::parse_text;

const SRC: &str = "&&test::core::thing???";

fn main() {
    parse_text(SRC.into());
}
