use std::fmt::Debug;
use std::fmt::Display;

use crate::TerminalSymbol;

#[derive(Clone, Debug)]
pub struct NoActionError<Term, Location> {
    pub term: TerminalSymbol<Term>,
    pub location: Option<Location>,
    pub state: usize,
}
#[derive(Clone, Debug)]
pub struct ReduceActionError<Term, Location, Source> {
    pub term: TerminalSymbol<Term>,
    pub location: Option<Location>,
    pub state: usize,
    pub source: Source,
}

#[derive(Clone, Debug)]
pub struct NoPrecedenceError<Term, Location> {
    pub term: TerminalSymbol<Term>,
    pub location: Option<Location>,
    pub state: usize,
    pub rule: usize,
}

/// Error type for feed()
#[derive(Clone, Debug)]
pub enum ParseError<Term, Location, ReduceAction> {
    /// No action defined for the given terminal in the parser table.
    /// location will be `None` if the terminal was eof.
    NoAction(NoActionError<Term, Location>),

    /// Error from reduce action.
    /// location will be `None` if the terminal was eof.
    ReduceAction(ReduceActionError<Term, Location, ReduceAction>),

    /// Rule index when shift/reduce conflict occur with no shift/reduce precedence defined.
    /// This is same as when setting %nonassoc in Bison.
    /// location will be `None` if the terminal was eof.
    NoPrecedence(NoPrecedenceError<Term, Location>),
}

impl<Term, Location, ReduceAction> ParseError<Term, Location, ReduceAction> {
    /// location will be `None` if the terminal was eof.
    pub fn location(&self) -> &Option<Location> {
        match self {
            ParseError::NoAction(err) => &err.location,
            ParseError::ReduceAction(err) => &err.location,
            ParseError::NoPrecedence(err) => &err.location,
        }
    }

    pub fn term(&self) -> &TerminalSymbol<Term> {
        match self {
            ParseError::NoAction(err) => &err.term,
            ParseError::ReduceAction(err) => &err.term,
            ParseError::NoPrecedence(err) => &err.term,
        }
    }

    pub fn state(&self) -> usize {
        match self {
            ParseError::NoAction(err) => err.state,
            ParseError::ReduceAction(err) => err.state,
            ParseError::NoPrecedence(err) => err.state,
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
            ParseError::NoPrecedence(err) => {
                write!(f, "NoPrecedence: {}, State: {}", err.rule, err.state)
            }
        }
    }
}
