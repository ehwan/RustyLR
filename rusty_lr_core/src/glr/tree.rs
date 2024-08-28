use std::fmt::Display;

use super::{error::SinglePath, Parser};

/// Tree represention of single non-terminal token.
/// This tree contains only 2-height nodes.
/// Used for error reporting.
#[derive(Debug, Clone)]
pub struct TreeNonTerminal1 {
    /// index of production rule that this token reduced from
    pub rule: usize,
    /// children of this token consumed by reduction
    pub tokens: Vec<Tree0>,
}

/// Tree representation of single token.
/// This tree contains only 2-height nodes.
/// Used for error reporting.
#[derive(Debug, Clone)]
pub enum Tree1 {
    /// A terminal token
    Terminal,
    /// A non-terminal token
    NonTerminal(TreeNonTerminal1),
}
impl Tree1 {
    pub fn to_tree0(&self) -> Tree0 {
        match self {
            Tree1::Terminal => Tree0::Terminal,
            Tree1::NonTerminal(non_terminal) => Tree0::NonTerminal(TreeNonTerminal0 {
                rule: non_terminal.rule,
            }),
        }
    }

    /// For debug.
    /// Convert this tree to visual string.
    pub fn to_string<P: Parser>(&self, parser: &P) -> String
    where
        P::Term: Clone + Display,
        P::NonTerm: Clone + Display,
    {
        match self {
            Tree1::Terminal => "Terminal".to_string(),
            Tree1::NonTerminal(_) => SinglePath::from_tree1(self, parser).to_string(),
        }
    }
}

/// Tree represention of single non-terminal token.
/// This tree contains only 2-height nodes.
/// Used for error reporting and conflict resolving in reduce action.
#[derive(Debug, Clone)]
pub struct TreeNonTerminal0 {
    /// index of production rule that this token reduced from
    pub rule: usize,
}

/// Tree representation of single token.
/// This tree contains only 2-height nodes.
/// Used for error reporting and conflict resolving in reduce action.
#[derive(Debug, Clone)]
pub enum Tree0 {
    /// A terminal token
    Terminal,
    /// A non-terminal token
    NonTerminal(TreeNonTerminal0),
}
