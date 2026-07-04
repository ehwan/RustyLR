use std::fmt::Debug;
use std::fmt::Display;

use crate::TerminalSymbol;

#[derive(Clone, Debug)]
pub struct NoActionError<Term, Location> {
    pub term: TerminalSymbol<Term>,
    pub location: Location,
    pub state: usize,
}
#[derive(Clone, Debug)]
pub struct ReduceActionError<Term, Location, Source> {
    pub term: TerminalSymbol<Term>,
    pub location: Location,
    pub state: usize,
    pub source: Source,
}

/// Error type for feed()
#[derive(Clone, Debug)]
pub enum ParseError<Term, Location, ReduceAction> {
    /// No action defined for the given terminal in the parser table.
    NoAction(NoActionError<Term, Location>),

    /// Error from reduce action.
    ReduceAction(ReduceActionError<Term, Location, ReduceAction>),
}

impl<Term, Location, ReduceAction> ParseError<Term, Location, ReduceAction> {
    pub fn location(&self) -> &Location {
        match self {
            ParseError::NoAction(err) => &err.location,
            ParseError::ReduceAction(err) => &err.location,
        }
    }

    pub fn term(&self) -> &TerminalSymbol<Term> {
        match self {
            ParseError::NoAction(err) => &err.term,
            ParseError::ReduceAction(err) => &err.term,
        }
    }

    pub fn state(&self) -> usize {
        match self {
            ParseError::NoAction(err) => err.state,
            ParseError::ReduceAction(err) => err.state,
        }
    }
}

impl<Term, Location, ReduceAction> Display for ParseError<Term, Location, ReduceAction>
where
    Term: Display,
    ReduceAction: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoAction(err) => {
                write!(f, "NoAction: {}, State: {}", err.term, err.state)
            }
            ParseError::ReduceAction(err) => {
                write!(
                    f,
                    "ReduceAction: {}, State: {}\nSource: {}",
                    err.term, err.state, err.source
                )
            }
        }
    }
}
