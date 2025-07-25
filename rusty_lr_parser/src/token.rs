use proc_macro2::{Ident, Span};

/// for syntax <Ident> '=' <Token>
#[derive(Debug, Clone)]
pub struct TokenMapped {
    /// terminal or non-terminal name
    pub token: rusty_lr_core::Token<rusty_lr_core::TerminalSymbol<usize>, usize>,

    /// variable name that the token's data will be mapped to
    pub mapto: Option<Ident>,

    /// span of the token
    pub begin_span: Span,
    pub end_span: Span,
}
