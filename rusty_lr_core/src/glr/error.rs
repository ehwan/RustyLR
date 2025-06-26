use std::fmt::Debug;
use std::fmt::Display;

/// Error type for feed()
#[derive(Clone)]
pub enum ParseError<Term, ReduceActionError> {
    /// No action defined for the given terminal in the parser table
    NoAction(Term),

    /// Error from reduce action (from every diverged paths)
    ReduceAction(Vec<ReduceActionError>),
}

impl<Term: Display, ReduceActionError: Display> Display for ParseError<Term, ReduceActionError> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoAction(err) => {
                write!(f, "NoAction: {}", err)
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

impl<Term: Debug, ReduceActionError: Debug> Debug for ParseError<Term, ReduceActionError> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoAction(err) => {
                write!(f, "{:?}", err)
            }
            ParseError::ReduceAction(err) => {
                write!(f, "{:?}", err)
            }
        }
    }
}
