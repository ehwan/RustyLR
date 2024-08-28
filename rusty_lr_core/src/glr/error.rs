use crate::ProductionRule;
use crate::Token;

use std::collections::BTreeSet;
use std::fmt::Debug;
use std::fmt::Display;

use super::Parser;
use super::Tree0;
use super::Tree1;

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

#[derive(Debug)]
pub enum TerminalOrProductionRule<Term, NonTerm> {
    Terminal(Term),
    ProductionRule(ProductionRule<Term, NonTerm>),
}

#[derive(Debug)]
pub struct SinglePath<Term, NonTerm> {
    pub rule: NonTerm,
    pub tokens: Vec<TerminalOrProductionRule<Term, NonTerm>>,
}
impl<Term, NonTerm> SinglePath<Term, NonTerm> {
    pub fn from_tree1(
        tree: &Tree1,
        parser: &impl Parser<Term = Term, NonTerm = NonTerm>,
    ) -> SinglePath<Term, NonTerm>
    where
        Term: Clone,
        NonTerm: Clone,
    {
        match tree {
            Tree1::Terminal => unreachable!(),
            Tree1::NonTerminal(non_terminal) => {
                let rule = &parser.get_rules()[non_terminal.rule];

                let mut tokens = Vec::new();
                for (child, token) in non_terminal.tokens.iter().zip(rule.rule.iter()) {
                    match token {
                        Token::Term(term) => {
                            tokens.push(TerminalOrProductionRule::Terminal(term.clone()));
                        }
                        Token::NonTerm(_) => {
                            let rule = if let Tree0::NonTerminal(child) = child {
                                parser.get_rules()[child.rule].clone()
                            } else {
                                unreachable!();
                            };
                            tokens.push(TerminalOrProductionRule::ProductionRule(rule));
                        }
                    }
                }
                SinglePath {
                    rule: rule.name.clone(),
                    tokens,
                }
            }
        }
    }
}
impl<Term: Display, NonTerm: Display> Display for SinglePath<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} -> ", self.rule)?;
        for (idx, token) in self.tokens.iter().enumerate() {
            match token {
                TerminalOrProductionRule::Terminal(term) => write!(f, "\t{}", term)?,
                TerminalOrProductionRule::ProductionRule(rule) => write!(f, "\t{}", rule)?,
            }
            if idx < self.tokens.len() - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

/// Error when there are multiple paths to represent the same string you feeded to the parser.
#[derive(Debug)]
pub struct MultiplePathError<Term, NonTerm> {
    pub paths: Vec<SinglePath<Term, NonTerm>>,
}

impl<Term: Display, NonTerm: Display> Display for MultiplePathError<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl<Term, NonTerm> MultiplePathError<Term, NonTerm> {
    pub fn from_tree1<'a, It: Iterator<Item = &'a Tree1>>(
        trees: It,
        parser: &impl Parser<Term = Term, NonTerm = NonTerm>,
    ) -> MultiplePathError<Term, NonTerm>
    where
        Term: Clone,
        NonTerm: Clone,
    {
        MultiplePathError {
            paths: trees
                .map(|tree| SinglePath::from_tree1(tree, parser))
                .collect(),
        }
    }
    pub fn message(&self) -> String
    where
        Term: Display,
        NonTerm: Display,
    {
        let mut message = format!("Multiple path detected:");
        for path in self.paths.iter() {
            message.push_str(format!("\n{}", path).as_str());
        }
        message
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

// #[derive(Debug)]
// pub enum ParseError<Term, NonTerm, ReduceActionError> {
// Error when there is no valid path to represent the string you feeded to the parser.
// InvalidTerminal(InvalidTerminalError<Term, ReduceActionError>),
// Error when there are multiple paths to represent the same string you feeded to the parser.
// MultiplePath(MultiplePathError<Term, NonTerm>),
// }

// impl<Term: Display, NonTerm: Display, ReduceActionError: Display> Display
//     for ParseError<Term, NonTerm, ReduceActionError>
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             ParseError::InvalidTerminal(err) => write!(f, "{}", err),
//             // ParseError::MultiplePath(err) => write!(f, "{}", err),
//         }
//     }
// }

// impl<Term: Display + Debug, NonTerm: Display + Debug, ReduceActionError: Display + Debug>
//     std::error::Error for ParseError<Term, NonTerm, ReduceActionError>
// {
//     fn cause(&self) -> Option<&dyn std::error::Error> {
//         None
//     }
//     fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
//         None
//     }
//     fn description(&self) -> &str {
//         "Parse error"
//     }
// }

// impl<Term, NonTerm, ReduceActionError> ParseError<Term, NonTerm, ReduceActionError> {
//     pub fn message(&self) -> String
//     where
//         Term: Display,
//         NonTerm: Display,
//         ReduceActionError: Display,
//     {
//         match self {
//             ParseError::InvalidTerminal(err) => err.message(),
//             // ParseError::MultiplePath(err) => err.message(),
//         }
//     }
// }
