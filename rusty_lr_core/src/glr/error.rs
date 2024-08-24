use crate::ProductionRule;

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
        write!(f, "{}", self.message())
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
    fn description(&self) -> &str {
        "Invalid terminal feeded"
    }
}

impl<Term, ReduceActionError> InvalidTerminalError<Term, ReduceActionError> {
    /// Generate brief error message.
    pub fn message(&self) -> String
    where
        Term: Display,
        ReduceActionError: Display,
    {
        let mut message = format!("Invalid Terminal: {}. ", self.term);

        if self.expected.is_empty() {
            message.push_str("No expected token");
        } else {
            let expected: BTreeSet<String> =
                self.expected.iter().map(|t| format!("{}", t)).collect();
            message.push_str("Expected one of: ");
            let len = expected.len();
            for (id, term) in expected.into_iter().enumerate() {
                message.push_str(&term);
                if id < len - 1 {
                    message.push_str(", ");
                }
            }
        }
        for error in &self.reduce_errors {
            message.push_str(&format!("\nReduce action error: {}", error));
        }
        message
    }
}

/// Error when there are multiple paths to represent the same string you feeded to the parser.
#[derive(Debug)]
pub struct MultiplePathError<Term, NonTerm> {
    pub rule1: ProductionRule<Term, NonTerm>,
    pub rule2: ProductionRule<Term, NonTerm>,
}

impl<Term: Display, NonTerm: Display> Display for MultiplePathError<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl<Term, NonTerm> MultiplePathError<Term, NonTerm> {
    pub fn message(&self) -> String
    where
        Term: Display,
        NonTerm: Display,
    {
        format!(
            "Multiple path detected:\n{}\nand\n{}",
            self.rule1, self.rule2
        )
    }
}

impl<Term: Display + Debug, NonTerm: Display + Debug> std::error::Error
    for MultiplePathError<Term, NonTerm>
{
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
    fn description(&self) -> &str {
        "Multiple path detected"
    }
}

#[derive(Debug)]
pub enum ParseError<Term, NonTerm, ReduceActionError> {
    /// Error when there is no valid path to represent the string you feeded to the parser.
    InvalidTerminal(InvalidTerminalError<Term, ReduceActionError>),
    /// Error when there are multiple paths to represent the same string you feeded to the parser.
    MultiplePath(MultiplePathError<Term, NonTerm>),
}

impl<Term: Display, NonTerm: Display, ReduceActionError: Display> Display
    for ParseError<Term, NonTerm, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidTerminal(err) => write!(f, "{}", err),
            ParseError::MultiplePath(err) => write!(f, "{}", err),
        }
    }
}

impl<Term: Display + Debug, NonTerm: Display + Debug, ReduceActionError: Display + Debug>
    std::error::Error for ParseError<Term, NonTerm, ReduceActionError>
{
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
    fn description(&self) -> &str {
        "Parse error"
    }
}

impl<Term, NonTerm, ReduceActionError> ParseError<Term, NonTerm, ReduceActionError> {
    pub fn message(&self) -> String
    where
        Term: Display,
        NonTerm: Display,
        ReduceActionError: Display,
    {
        match self {
            ParseError::InvalidTerminal(err) => err.message(),
            ParseError::MultiplePath(err) => err.message(),
        }
    }
}
