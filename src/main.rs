use parser::{
    ast::AstElement,
    ast::{nodes::Expr, visitor::ItemPrinter},
    parse_text, print_syntax_tree,
};

const SRC: &str = "
fn main (): string {
    1 + 2;
    &test(1, name);
    return 10;
}
";

fn main() {
    print_syntax_tree(SRC.into());
    let (module_ast, errors) = parse_text(SRC.into());
    
    for error in errors {
        println!("ERROR[{:?}]: {}", error.level, error.title);
        println!("  {}", error.message);
        println!("----------------");
    }
    module_ast.items().for_each(|item| item.visit::<ItemPrinter>());
}
