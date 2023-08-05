use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use indextree::Arena;

static NEXT_SCOPE_ID: AtomicUsize = AtomicUsize::new(0);
struct ScopeId(usize);
impl ScopeId {
    pub fn new() -> Self {
        Self(NEXT_SCOPE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

struct ScopeState {}

struct TC2 {
    db: Arena<ScopeState>,
}
impl TC2 {
    pub fn t(&mut self) {
        let node = self.db.new_node(ScopeState {});
    }
}
