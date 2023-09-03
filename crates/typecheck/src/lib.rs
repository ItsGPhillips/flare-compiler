use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use diagnostics::TextRange;
use interner::{impl_internable, Interned};

static NEXT_TYPE_ID: AtomicUsize = AtomicUsize::new(0);
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(usize);
impl TypeId {
    pub fn new() -> Self {
        Self(NEXT_TYPE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug)]
pub enum TypeVariant {
    Primitive,
    Struct {
        feilds: HashMap<Interned<str>, TypeDef>,
    },
    TupleStruct {
        fields: Vec<TypeDef>,
    },
}

#[derive(Debug)]
pub struct TypeDef {
    id: TypeId,
    span: TextRange,
    variant: TypeVariant,
}

impl PartialEq for TypeDef {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl std::hash::Hash for TypeDef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl Eq for TypeDef {}
impl_internable!(TypeDef);

impl TypeDef {
    pub fn new(variant: TypeVariant, span: TextRange) -> Interned<Self> {
        Interned::new(Self {
            id: TypeId::new(),
            span,
            variant,
        })
    }
}
