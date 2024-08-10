use proc_macro2::Ident;

/// for syntax <Ident> '=' <Token>
#[derive(Debug, Clone)]
pub struct TokenMapped {
    /// terminal or non-terminal name
    pub token: Ident,

    /// variable name that the token's data will be mapped to
    pub mapto: Ident,
}
