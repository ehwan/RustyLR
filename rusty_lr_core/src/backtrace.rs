use std::fmt::Debug;
use std::fmt::Display;

use crate::ShiftedRule;

/// Backtracing information for parsing context.
/// What current parser was trying to parse, and what rules were applied.
#[derive(Clone)]
pub struct Backtrace<Term, NonTerm> {
    /// 0'th element is the current parsing state, and through the backtrace, it goes to the initial state.
    pub traces: Vec<Vec<ShiftedRule<Term, NonTerm>>>,
}

impl<Term: Display, NonTerm: Display> Display for Backtrace<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, rules) in self.traces.iter().enumerate() {
            if idx == 0 {
                writeln!(f, "Trying to parse:")?;
            } else {
                writeln!(f, "Backtrace:")?;
            }
            for rule in rules {
                writeln!(f, "\t>>> {}", rule)?;
            }
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for Backtrace<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, rules) in self.traces.iter().enumerate() {
            if idx == 0 {
                writeln!(f, "Trying to parse:")?;
            } else {
                writeln!(f, "Backtrace:")?;
            }
            for rule in rules {
                writeln!(f, "\t>>> {:?}", rule)?;
            }
        }
        Ok(())
    }
}
