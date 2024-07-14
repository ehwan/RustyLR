use std::fmt::Debug;
use std::fmt::Display;

use crate::term::TermTraitBound;

/// Token represents a terminal or non-terminal symbol in the grammar.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token<Term: TermTraitBound> {
    Term(Term),
    NonTerm(String),

    // special token for end of input stream
    End,
}
impl<Term: TermTraitBound + Display> Display for Token<Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Term(term) => write!(f, "{}", term),
            Token::NonTerm(rule) => write!(f, "{}", rule),
            Token::End => write!(f, "$"),
        }
    }
}
