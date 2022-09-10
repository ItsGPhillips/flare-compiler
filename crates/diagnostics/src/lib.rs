use span::Span;
pub mod span;

pub enum DiagnosticLevel {
   FATAL,
   WARNING,
   HINT,
   INFO,
}

pub struct Diagnostic {
   pub level: DiagnosticLevel,
   pub title: String,
   pub message: String,
   pub span: Span,
}