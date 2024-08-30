use proc_macro2::{Ident, Span};

use rusty_lr_core::Token;

/// for syntax <Ident> '=' <Token>
#[derive(Debug, Clone)]
pub struct TokenMapped {
    /// terminal or non-terminal name
    pub token: Token<usize, usize>,

    /// variable name that the token's data will be mapped to
    pub mapto: Option<Ident>,

    /// span of the token
    pub begin_span: Span,
    pub end_span: Span,
}
