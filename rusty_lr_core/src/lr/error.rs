use std::fmt::Debug;
use std::fmt::Display;

/// Error type for feed(), when invalid terminal is feeded
#[derive(Clone)]
pub struct InvalidTerminalError<Term, NonTerm> {
    /// invalid terminal feeded
    pub term: Term,

    #[cfg(feature = "error")]
    pub(crate) expected: Vec<Term>,
    #[cfg(feature = "error")]
    pub(crate) expected_nonterm: Vec<NonTerm>,
    #[cfg(feature = "error")]
    pub(crate) backtrace: crate::Backtrace<usize, NonTerm>,

    #[cfg(not(feature = "error"))]
    pub(crate) _phantom: std::marker::PhantomData<(Term, NonTerm)>,
}

impl<Term: Display, NonTerm: Display> Display for InvalidTerminalError<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid Terminal: {}", self.term)?;
        #[cfg(feature = "error")]
        {
            use std::collections::BTreeSet;
            let expected = self
                .expected
                .iter()
                .map(|term| term.to_string())
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected: {}", expected)?;
            let expected_nonterm = self
                .expected_nonterm
                .iter()
                .map(|nonterm| nonterm.to_string())
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected(NonTerminals): {}", expected_nonterm)?;

            write!(f, "\nBacktrace: {}", self.backtrace)?;
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for InvalidTerminalError<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid Terminal: {:?}", self.term)?;
        #[cfg(feature = "error")]
        {
            use std::collections::BTreeSet;
            let expected = self
                .expected
                .iter()
                .map(|term| format!("{:?}", term))
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected: {}", expected)?;
            let expected_nonterm = self
                .expected_nonterm
                .iter()
                .map(|nonterm| format!("{:?}", nonterm))
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected(NonTerminals): {}", expected_nonterm)?;

            write!(f, "\nBacktrace: {:?}", self.backtrace)?;
        }
        Ok(())
    }
}

impl<Term: Display + Debug, NonTerm: Display + Debug> std::error::Error
    for InvalidTerminalError<Term, NonTerm>
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// Error type for feed()
#[derive(Clone)]
pub enum ParseError<Term, NonTerm, ReduceActionError> {
    /// Invalid terminal feeded
    InvalidTerminal(InvalidTerminalError<Term, NonTerm>),

    /// Error from reduce action
    ReduceAction(ReduceActionError),
}

impl<Term: Display, NonTerm: Display, ReduceActionError: Display> Display
    for ParseError<Term, NonTerm, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidTerminal(err) => {
                write!(f, "{}", err)
            }
            ParseError::ReduceAction(err) => {
                write!(f, "{}", err)
            }
        }
    }
}

impl<Term: Debug, NonTerm: Debug, ReduceActionError: Debug> Debug
    for ParseError<Term, NonTerm, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidTerminal(err) => {
                write!(f, "{:?}", err)
            }
            ParseError::ReduceAction(err) => {
                write!(f, "{:?}", err)
            }
        }
    }
}

impl<Term: Display + Debug, NonTerm: Display + Debug, ReduceActionError: std::error::Error>
    std::error::Error for ParseError<Term, NonTerm, ReduceActionError>
where
    InvalidTerminalError<Term, NonTerm>: std::error::Error,
    Term: 'static,
    NonTerm: 'static,
    ReduceActionError: 'static,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseError::InvalidTerminal(err) => Some(err),
            ParseError::ReduceAction(err) => Some(err),
        }
    }
}
