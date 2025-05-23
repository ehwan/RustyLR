use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

/// Token represents a terminal or non-terminal symbol in the grammar.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token<Term, NonTerm> {
    Term(Term),
    NonTerm(NonTerm),
}
impl<Term: Display, NonTerm: Display> Display for Token<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Term(term) => write!(f, "{}", term),
            Token::NonTerm(nonterm) => write!(f, "{}", nonterm),
        }
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for Token<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Term(term) => write!(f, "{:?}", term),
            Token::NonTerm(nonterm) => write!(f, "{:?}", nonterm),
        }
    }
}

impl<Term, NonTerm> Token<Term, NonTerm> {
    pub fn map<NewTerm, NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> NewTerm,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> Token<NewTerm, NewNonTerm> {
        match self {
            Token::Term(term) => Token::Term(term_map(term)),
            Token::NonTerm(nonterm) => Token::NonTerm(nonterm_map(nonterm)),
        }
    }
}
