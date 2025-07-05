use std::fmt::Debug;
use std::fmt::Display;

use crate::nonterminal::TokenData;

/// Error type for feed()
#[derive(Clone)]
pub enum ParseError<Data: TokenData> {
    /// No action defined for the given terminal in the parser table
    NoAction(Data::Term, Data::Location),

    /// Error from reduce action (from every diverged paths)
    ReduceAction(Vec<Data::ReduceActionError>),
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
            ParseError::ReduceAction(err) => {
                write!(
                    f,
                    "ReduceAction: {}",
                    err.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
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
            ParseError::ReduceAction(err) => {
                write!(f, "{:?}", err)
            }
        }
    }
}
