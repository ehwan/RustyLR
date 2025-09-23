use std::fmt::Debug;
use std::fmt::Display;

/// Error type for feed()
#[derive(Clone, Debug)]
pub struct ParseError<Term, Location, ReduceActionError> {
    /// The terminal symbol that caused the error.
    pub term: crate::TerminalSymbol<Term>,
    /// Location of the terminal symbol.
    /// location will be `None` if the terminal was eof.
    pub location: Option<Location>,
    /// Error from reduce action (from every diverged paths)
    pub reduce_action_errors: Vec<ReduceActionError>,
    /// Rule indices when shift/reduce conflict occur with no shift/reduce precedence defined.
    /// This is same as when setting %nonassoc in Bison.
    pub no_precedences: Vec<usize>,

    /// States when the error occurred (from all diverged paths)
    pub(crate) states: Vec<usize>,
}

impl<Term, Location, ReduceActionError> ParseError<Term, Location, ReduceActionError> {
    /// location will be `None` if the terminal was eof.
    pub fn location(&self) -> &Option<Location> {
        &self.location
    }
    pub fn term(&self) -> &crate::TerminalSymbol<Term> {
        &self.term
    }
    /// States when the error occurred (from all diverged paths)
    pub fn states(&self) -> impl Iterator<Item = usize> + '_ {
        self.states.iter().copied()
    }
}

impl<Term, Location, ReduceActionError> Display for ParseError<Term, Location, ReduceActionError>
where
    Term: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError: {}, States: {:?}", self.term, self.states)
    }
}
