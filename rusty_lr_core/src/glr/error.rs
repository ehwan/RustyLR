use std::collections::BTreeSet;
use std::fmt::Debug;
use std::fmt::Display;

/// Error when there is an invalid terminal feeded to the parser.
#[derive(Debug)]
pub struct InvalidTerminalError<Term, ReduceActionError> {
    /// The terminal that feeded to the parser.
    pub term: Term,
    /// The expected terminals, Along all the paths.
    pub expected: Vec<Term>,
    /// The reduce action errors.
    pub reduce_errors: Vec<ReduceActionError>,
}

impl<Term: Display, ReduceActionError: Display> Display
    for InvalidTerminalError<Term, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid Terminal: {}. ", self.term)?;

        if self.expected.is_empty() {
            write!(f, "No expected token")?;
        } else {
            let expected: BTreeSet<String> =
                self.expected.iter().map(|t| format!("{}", t)).collect();
            write!(f, "Expected one of: ")?;
            let len = expected.len();
            for (id, term) in expected.into_iter().enumerate() {
                write!(f, "{}", term)?;
                if id < len - 1 {
                    write!(f, ", ")?;
                }
            }
        }
        for error in &self.reduce_errors {
            write!(f, "\nReduce action error: {}", error)?;
        }
        Ok(())
    }
}

impl<Term: Display + Debug, ReduceActionError: Display + Debug> std::error::Error
    for InvalidTerminalError<Term, ReduceActionError>
{
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// Error when there are multiple paths to represent the same string you feeded to the parser.
/// If you want to see the multiple paths, you can use `to_tree_lists` method.
#[derive(Debug)]
pub struct MultiplePathError;

impl Display for MultiplePathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MultiplePathError")
    }
}

impl std::error::Error for MultiplePathError {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
