use std::fmt::Debug;
use std::fmt::Display;

use crate::nonterminal::TokenData;

/// Error type for feed()
#[derive(Clone)]
pub struct ParseError<Data: TokenData> {
    /// The terminal symbol that caused the error.
    pub term: crate::TerminalSymbol<Data::Term>,
    /// Location of the terminal symbol
    pub location: Data::Location,
    /// Error from reduce action (from every diverged paths)
    pub reduce_action_errors: Vec<Data::ReduceActionError>,
    /// Rule indices when shift/reduce conflict occur with no shift/reduce precedence defined.
    /// This is same as when setting %nonassoc in Bison.
    pub no_precedences: Vec<usize>,
}

impl<Data: TokenData> ParseError<Data> {
    pub fn location(&self) -> &Data::Location {
        &self.location
    }
    pub fn term(&self) -> &crate::TerminalSymbol<Data::Term> {
        &self.term
    }
}

impl<Data: TokenData> Display for ParseError<Data>
where
    Data::Term: Display,
    Data::ReduceActionError: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError error: {}", self.term)
    }
}

impl<Data: TokenData> Debug for ParseError<Data>
where
    Data::Term: Debug,
    Data::ReduceActionError: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError error: {:?}", self.term)
    }
}
