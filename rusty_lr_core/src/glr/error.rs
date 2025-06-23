use std::fmt::Debug;
use std::fmt::Display;

/// Error when there is an invalid terminal feeded to the parser.
#[derive(Clone)]
pub struct InvalidTerminalError<Term, NonTerm, ReduceActionError> {
    /// The terminal that feeded to the parser.
    pub term: Term,
    /// The reduce action errors.
    pub reduce_errors: Vec<ReduceActionError>,

    #[cfg(feature = "error")]
    pub(crate) backtraces: Vec<crate::Backtrace<&'static str, NonTerm>>,

    #[cfg(not(feature = "error"))]
    pub(crate) _phantom: std::marker::PhantomData<NonTerm>,
}

impl<Term: Display, NonTerm: Display, ReduceActionError: Display> Display
    for InvalidTerminalError<Term, NonTerm, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid Terminal: {}", self.term)?;

        for error in &self.reduce_errors {
            write!(f, "\nReduce action error: {}", error)?;
        }

        #[cfg(feature = "error")]
        {
            for (idx, backtrace) in self.backtraces.iter().enumerate() {
                write!(f, "\nBacktrace for path {}: ", idx)?;
                write!(f, "\n{}", backtrace)?;
            }
        }
        Ok(())
    }
}

impl<Term: Debug, NonTerm: Debug, ReduceActionError: Debug> Debug
    for InvalidTerminalError<Term, NonTerm, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid Terminal: {:?}", self.term)?;

        for error in &self.reduce_errors {
            write!(f, "\nReduce action error: {:?}", error)?;
        }

        #[cfg(feature = "error")]
        {
            for (idx, backtrace) in self.backtraces.iter().enumerate() {
                write!(f, "\nBacktrace for path {}: ", idx)?;
                write!(f, "\n{:?}", backtrace)?;
            }
        }
        Ok(())
    }
}

impl<Term: Display + Debug, NonTerm: Display + Debug, ReduceActionError: Display + Debug>
    std::error::Error for InvalidTerminalError<Term, NonTerm, ReduceActionError>
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
