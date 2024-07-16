use std::fmt::Debug;
use std::fmt::Display;

use crate::term::NonTermTraitBound;
use crate::term::TermTraitBound;

/// Token represents a terminal or non-terminal symbol in the grammar.
#[derive(Clone)]
pub enum Token<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    Term(Term),
    NonTerm(NonTerm),
}
impl<Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
    for Token<Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Term(term) => write!(f, "{}", term),
            Token::NonTerm(nonterm) => write!(f, "{}", nonterm),
        }
    }
}
impl<Term: TermTraitBound + Debug, NonTerm: NonTermTraitBound + Debug> Debug
    for Token<Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Term(term) => write!(f, "{:?}", term),
            Token::NonTerm(nonterm) => write!(f, "{:?}", nonterm),
        }
    }
}
