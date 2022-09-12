use span::Span;
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
   pub title: String,
   pub message: String,
   pub span: Span,
}