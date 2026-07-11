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
pub struct ReduceActionError<Term, Location, Source, UserData> {
    pub term: TerminalSymbol<Term>,
    pub location: Location,
    pub state: usize,
    pub source: Source,
    pub userdata: UserData,
}
#[derive(Clone, Debug)]
pub struct ConsumedContextError<Term, Location> {
    pub term: TerminalSymbol<Term>,
    pub location: Location,
    pub state: usize,
}

/// Error type for feed()
#[derive(Clone, Debug)]
pub enum ParseError<Term, Location, ReduceAction, UserData> {
    /// No action defined for the given terminal in the parser table.
    ///
    /// A deterministic parser has one active branch. This error is produced before any reduce
    /// action runs on that branch, so the parser context remains usable after a failed `feed` or
    /// `accept` and the user data stays in the context.
    NoAction(NoActionError<Term, Location>),

    /// Error from reduce action.
    ///
    /// A reduce action may have mutated semantic stacks or user data before returning `Err`. Since
    /// the deterministic context has no sibling branch to fall back to, the whole context is
    /// consumed and the user data is moved into this error.
    ReduceAction(ReduceActionError<Term, Location, ReduceAction, UserData>),

    /// A feed or accept was attempted after the context had already been consumed.
    ///
    /// This happens after successful `accept`, or after a previous `ReduceAction` error moved the
    /// user data out of the context. This error only reports the attempted terminal, location, and
    /// current parser state.
    ConsumedContext(ConsumedContextError<Term, Location>),
}

impl<Term, Location, ReduceAction, UserData> ParseError<Term, Location, ReduceAction, UserData> {
    pub fn location(&self) -> &Location {
        match self {
            ParseError::NoAction(err) => &err.location,
            ParseError::ReduceAction(err) => &err.location,
            ParseError::ConsumedContext(err) => &err.location,
        }
    }

    pub fn term(&self) -> &TerminalSymbol<Term> {
        match self {
            ParseError::NoAction(err) => &err.term,
            ParseError::ReduceAction(err) => &err.term,
            ParseError::ConsumedContext(err) => &err.term,
        }
    }

    pub fn state(&self) -> usize {
        match self {
            ParseError::NoAction(err) => err.state,
            ParseError::ReduceAction(err) => err.state,
            ParseError::ConsumedContext(err) => err.state,
        }
    }

    pub fn userdata(&self) -> Option<&UserData> {
        match self {
            ParseError::NoAction(_) => None,
            ParseError::ReduceAction(err) => Some(&err.userdata),
            ParseError::ConsumedContext(_) => None,
        }
    }

    pub fn into_userdata(self) -> Option<UserData> {
        match self {
            ParseError::NoAction(_) => None,
            ParseError::ReduceAction(err) => Some(err.userdata),
            ParseError::ConsumedContext(_) => None,
        }
    }

    pub fn reduce_action_error(&self) -> Option<&ReduceAction> {
        match self {
            ParseError::NoAction(_) => None,
            ParseError::ReduceAction(err) => Some(&err.source),
            ParseError::ConsumedContext(_) => None,
        }
    }

    pub fn is_consumed_context(&self) -> bool {
        matches!(self, ParseError::ConsumedContext(_))
    }
}

impl<Term, Location, ReduceAction, UserData> Display
    for ParseError<Term, Location, ReduceAction, UserData>
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
            ParseError::ConsumedContext(err) => {
                write!(f, "ConsumedContext: {}, State: {}", err.term, err.state)
            }
        }
    }
}
