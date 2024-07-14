use std::collections::BTreeMap;
use std::fmt::Display;

use crate::action::Action;
use crate::term::TermTraitBound;
use crate::token::Token;

/// A struct for state in LR(1) parser
#[derive(Debug, Clone)]
pub struct State<Term: TermTraitBound> {
    pub action_map: BTreeMap<Token<Term>, Action<Term>>,
}
impl<Term: TermTraitBound> State<Term> {
    pub fn new() -> Self {
        State {
            action_map: BTreeMap::new(),
        }
    }

    /// feed one token and get action
    pub fn feed<'a>(&'a self, token: &Token<Term>) -> Option<&'a Action<Term>> {
        self.action_map.get(token)
    }
}
impl<Term: TermTraitBound + Display> Display for State<Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (token, action) in self.action_map.iter() {
            writeln!(f, "{}: {}", token, action)?;
        }
        Ok(())
    }
}
