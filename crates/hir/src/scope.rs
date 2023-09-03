#[derive(Default)]
pub struct Builder {
    current: usize,
    stack: Vec<usize>,
}

impl Builder {
    pub fn current_scope_id(&self) -> usize {
        self.current
    }
    pub fn push_scope(&mut self) -> usize {
        let current = self.current;
        self.stack.push(current);
        self.current = current + 1;
        current
    }
    pub fn pop_scope(&mut self) -> Option<usize> {
        self.stack.pop()
    }
}
