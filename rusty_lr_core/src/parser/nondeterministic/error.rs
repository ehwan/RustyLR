use std::fmt::Debug;
use std::fmt::Display;

use crate::nonterminal::DataStack;

/// Error type for feed()
#[derive(Clone)]
pub struct ParseError<Data: DataStack> {
    /// The terminal symbol that caused the error.
    pub term: crate::TerminalSymbol<Data::Term>,
    /// Location of the terminal symbol.
    /// location will be `None` if the terminal was eof.
    pub location: Option<Data::Location>,
    /// Error from reduce action (from every diverged paths)
    pub reduce_action_errors: Vec<Data::ReduceActionError>,
    /// Rule indices when shift/reduce conflict occur with no shift/reduce precedence defined.
    /// This is same as when setting %nonassoc in Bison.
    pub no_precedences: Vec<usize>,
}

impl<Data: DataStack> ParseError<Data> {
    /// location will be `None` if the terminal was eof.
    pub fn location(&self) -> &Option<Data::Location> {
        &self.location
    }
    pub fn term(&self) -> &crate::TerminalSymbol<Data::Term> {
        &self.term
    }
}

impl<Data: DataStack> Display for ParseError<Data>
where
    Data::Term: Display,
    Data::ReduceActionError: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError error: {}", self.term)
    }
}

impl<Data: DataStack> Debug for ParseError<Data>
where
    Data::Term: Debug,
    Data::ReduceActionError: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError error: {:?}", self.term)
    }
}
