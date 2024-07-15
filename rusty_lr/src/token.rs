use std::fmt::Debug;
use std::fmt::Display;

use crate::term::TermTraitBound;

/// Token represents a terminal or non-terminal symbol in the grammar.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token<Term: TermTraitBound, NonTerm: TermTraitBound> {
    Term(Term),
    NonTerm(NonTerm),
}
impl<Term: TermTraitBound + Display, NonTerm: TermTraitBound + Display> Display
    for Token<Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Term(term) => write!(f, "{}", term),
            Token::NonTerm(nonterm) => write!(f, "{}", nonterm),
        }
    }
}
