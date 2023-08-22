use bevy_tasks::{Scope, TaskPool};
use hir::ModuleNormaliser;
use parser::{ast::visitor::Visitor, parse_source_file, print_syntax_tree};

const SRC: &str = r#"
    fn main (args: String): std::string::thing {
        let _ = "hello world";
        a::b::c * e::f::g;
    }
"#;

fn main() {
    print_syntax_tree(SRC.into());

    executor::scope(|s| {
        s.spawn(async {
            let (module, root, errors) = parse_source_file(SRC.into());
            for error in errors {
                println!("ERROR[{:?}]: {}", error.level, error.title);
                println!("  {}", error.message);

                let (line_number, line) = error.span.get_str_line(SRC).expect("oops");
                println!("\n{line_number}| {}\n", line.trim());
                println!("----------------");
            }

            let mut mn = ModuleNormaliser::new(root);

            mn.visit_module(module);
        });
    });
}
