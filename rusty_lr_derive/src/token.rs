use proc_macro2::Ident;
#[derive(Debug)]
pub enum Token {
    NonTerm(Ident),
    Term(Ident),
}
