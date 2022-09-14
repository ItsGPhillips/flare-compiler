use super::{
    nodes::{FnItem, Item, StructItem, EnumItem, ImportItem},
    AstElement,
};

pub struct ItemPrinter;
impl Visitor for ItemPrinter {
    type Target = Item;

    fn visit_fn_item(item: FnItem) {
        println!("{}", item.syntax().to_string());
    }
    fn visit_struct_item(item: StructItem) {
        println!("{}", item.syntax().to_string());
    }
    fn visit_enum_item(item: EnumItem) {
        println!("{}", item.syntax().to_string());
    }
    fn visit_import_item(item: ImportItem) {
        println!("{}", item.syntax().to_string());
    }
}

pub trait Visitor {
    type Target: AstElement;
    fn visit_fn_item(item: FnItem);
    fn visit_struct_item(item: StructItem);
    fn visit_enum_item(item: EnumItem);
    fn visit_import_item(item: ImportItem);
}
