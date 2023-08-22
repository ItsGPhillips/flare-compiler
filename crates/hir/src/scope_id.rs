// use im::hashmap::HashMap as ImHashMap;
// use std::sync::atomic::{AtomicUsize, Ordering};
//
// static mut CURRENT_SCOPE_ID: AtomicUsize = AtomicUsize::new(0);
//
// #[derive(Debug, Hash, PartialEq, Eq)]
// pub struct ScopeId(usize);
//
// impl ScopeId {
//     pub fn new() -> Self {
//         Self(unsafe { CURRENT_SCOPE_ID.fetch_add(1, Ordering::Relaxed) })
//     }
// }
//
// impl std::ops::Deref for ScopeId {
//     type Target = usize;
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }
//
// pub struct ScopeState {
//     attrs: Vec<()>,
//     functions: Vec<()>,
//     tys: Vec<()>,
// }
//
// struct Scope {
//     storage: ImHashMap<ScopeId, ScopeState>,
// }
//
// impl Scope {
//     pub fn new(storage: ImHashMap<ScopeId, ()>) -> Self {
//         Self { storage }
//     }
// }
//
// fn test() {
//     let base = im::hashmap::HashMap::<ScopeId, ()>::new();
// }
