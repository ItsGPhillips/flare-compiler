use std::borrow::Cow;

pub use rowan::TextRange;
pub mod span;

#[derive(Debug)]
pub enum DiagnosticLevel {
    FATAL,
    WARNING,
    HINT,
    INFO,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub title: Cow<'static, str>,
    pub message: Cow<'static, str>,
    pub span: TextRange,
}

pub fn report(_diagnostic: Diagnostic) {
    // TODO
}
