use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt::Display;

use crate::action::Action;
use crate::term::TermTraitBound;
use crate::token::Token;

/// A struct for state in LR(1) parser
#[derive(Debug, Clone)]
pub struct State<Term: TermTraitBound, NonTerm: TermTraitBound> {
    pub action_map: HashMap<Token<Term, NonTerm>, Action<Term, NonTerm>>,
}
impl<Term: TermTraitBound, NonTerm: TermTraitBound> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            action_map: HashMap::new(),
        }
    }

    /// feed one token and get action
    pub fn feed<'a>(&'a self, token: &Token<Term, NonTerm>) -> Option<&'a Action<Term, NonTerm>> {
        self.action_map.get(token)
    }
}
impl<Term: TermTraitBound + Display, NonTerm: TermTraitBound + Display> Display
    for State<Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (token, action) in self.action_map.iter() {
            writeln!(f, "{}: {}", token, action)?;
        }
        Ok(())
    }
}
