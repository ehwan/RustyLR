use proc_macro2::Ident;
use proc_macro2::Literal;
#[derive(Debug)]
pub enum Token {
    NonTerm(Ident),
    Term(Ident),
    Literal(Literal),
}
