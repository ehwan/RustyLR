use proc_macro2::Ident;

#[derive(Debug, Clone)]
pub enum Token {
    NonTerm(Ident),
    Term(Ident),
}

/// for syntax <Ident> '=' <Token>
#[derive(Debug, Clone)]
pub struct TokenMapped {
    pub token: Token,
    pub mapped: Option<Ident>,
}
