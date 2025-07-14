use std::fmt::Debug;
use std::fmt::Display;

use crate::nonterminal::TokenData;
use crate::TerminalSymbol;

/// Error type for feed()
#[derive(Clone)]
pub enum ParseError<Data: TokenData> {
    /// No action defined for the given terminal in the parser table
    NoAction(TerminalSymbol<Data::Term>, Data::Location),

    /// Error from reduce action
    ReduceAction(
        TerminalSymbol<Data::Term>,
        Data::Location,
        Data::ReduceActionError,
    ),

    /// Rule index when shift/reduce conflict occur with no shift/reduce precedence defined.
    /// This is same as when setting %nonassoc in Bison.
    NoPrecedence(TerminalSymbol<Data::Term>, Data::Location, usize),
}

impl<Data: TokenData> ParseError<Data> {
    pub fn location(&self) -> &Data::Location {
        match self {
            ParseError::NoAction(_, location) => location,
            ParseError::ReduceAction(_, location, _) => location,
            ParseError::NoPrecedence(_, location, _) => location,
        }
    }

    pub fn term(&self) -> &TerminalSymbol<Data::Term> {
        match self {
            ParseError::NoAction(term, _) => term,
            ParseError::ReduceAction(term, _, _) => term,
            ParseError::NoPrecedence(term, _, _) => term,
        }
    }
}

impl<Data: TokenData> Display for ParseError<Data>
where
    Data::Term: Display,
    Data::ReduceActionError: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoAction(term, _location) => {
                write!(f, "NoAction: {}", term)
            }
            ParseError::ReduceAction(_term, _location, err) => {
                write!(f, "ReduceAction: {}", err)
            }
            ParseError::NoPrecedence(_term, _location, rule) => {
                write!(f, "NoPrecedence: {}", rule)
            }
        }
    }
}

impl<Data: TokenData> Debug for ParseError<Data>
where
    Data::Term: Debug,
    Data::ReduceActionError: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoAction(term, _location) => {
                write!(f, "{:?}", term)
            }
            ParseError::ReduceAction(_term, _location, err) => {
                write!(f, "{:?}", err)
            }
            ParseError::NoPrecedence(_term, _location, rule) => {
                write!(f, "{}", rule)
            }
        }
    }
}
