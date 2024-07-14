use std::fmt::Display;

use crate::rule::*;
use crate::term::TermTraitBound;

#[derive(Debug, Clone)]
pub enum Action<Term: TermTraitBound> {
    Reduce(NamedShiftedRule<Term>),
    Goto(usize),
}
impl<Term: TermTraitBound> Action<Term> {
    /// if it is reduce action, feeded token should not be shifted; otherwise it should be shifted
    pub fn should_shift(&self) -> bool {
        match self {
            Action::Reduce(_) => false,
            Action::Goto(_) => true,
        }
    }

    /// get production rule if it is reduce action
    pub fn rule(self) -> Option<NamedShiftedRule<Term>> {
        match self {
            Action::Reduce(rule) => Some(rule),
            _ => None,
        }
    }
}
impl<Term: TermTraitBound + Display> Display for Action<Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Reduce(rule) => write!(f, "Reduce {}", rule)?,
            Action::Goto(state) => write!(f, "Goto {}", state)?,
        }
        Ok(())
    }
}
