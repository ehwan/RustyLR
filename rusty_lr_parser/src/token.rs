use proc_macro2::Ident;

#[derive(Debug, Clone)]
pub enum Token {
    NonTerm(Ident),
    Term(Ident),
    Plus(Ident),
    Star(Ident),
    Question(Ident),
}

impl Token {
    pub fn ident(&self) -> Ident {
        match self {
            Token::NonTerm(ident) => ident.clone(),
            Token::Term(ident) => ident.clone(),
            Token::Plus(ident) => ident.clone(),
            Token::Star(ident) => ident.clone(),
            Token::Question(ident) => ident.clone(),
        }
    }
}

/// for syntax <Ident> '=' <Token>
#[derive(Debug, Clone)]
pub struct TokenMapped {
    /// token variant
    pub token: Token,

    /// variable name that the token's data will be mapped to
    pub mapto: Ident,
}
