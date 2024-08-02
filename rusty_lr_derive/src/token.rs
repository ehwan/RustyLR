use proc_macro2::Ident;

#[derive(Debug, Clone)]
pub enum Token {
    NonTerm(Ident),
    Term(Ident),
}
