use parser::{
    ast::AstElement,
    ast::{
        nodes::Expr,
        visitor::{TestVisitor, Visitor},
    },
    parse_text, print_syntax_tree,
};

const SRC: &str = r#"
    fn main (test::foo: Foo, _ : What): string {
        let thing: Thing = 10;
    }

"#;

fn main() {
    print_syntax_tree(SRC.into());
    let (module_ast, errors) = parse_text(SRC.into());

    for error in errors {
        println!("ERROR[{:?}]: {}", error.level, error.title);
        println!("  {}", error.message);

        let (line_number, line) = error.span.get_str_line(SRC).expect("oops");
        println!("\n{line_number}| {}\n", line.trim());
        println!("----------------");
    }

    module_ast.items().for_each(|item| {
        let _ = item.visit(TestVisitor::default());
    });

    println!("{}", module_ast.items().count());
}
