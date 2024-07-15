use std::collections::HashMap;
use std::fmt::Display;

use crate::term::NonTermTraitBound;
use crate::term::TermTraitBound;

/// A struct for state in LR(1) parser
/// normally, the keys in shift_goto_map and reduce_map don't overlap
/// but in some case, they may overlap and need to be resolved by config(TODO)
#[derive(Debug, Clone)]
pub struct State<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    pub shift_goto_map_term: HashMap<Term, usize>,
    pub shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    pub reduce_map: HashMap<Term, usize>,
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            shift_goto_map_term: HashMap::new(),
            shift_goto_map_nonterm: HashMap::new(),
            reduce_map: HashMap::new(),
        }
    }

    /// feed one teminal and get action
    pub fn shift_goto_term(&self, term: &Term) -> Option<usize> {
        self.shift_goto_map_term.get(term).copied()
    }
    /// feed one non-teminal and get action
    pub fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize> {
        self.shift_goto_map_nonterm.get(nonterm).copied()
    }
    /// feed one token and get action
    pub fn reduce<'a>(&'a self, term: &Term) -> Option<usize> {
        self.reduce_map.get(term).copied()
    }
}
impl<Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
    for State<Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "GotoMap(Terminal):")?;
        for (term, goto) in self.shift_goto_map_term.iter() {
            writeln!(f, "{}: {}", term, goto)?;
        }
        writeln!(f, "GotoMap(Non-Terminal):")?;
        for (nonterm, goto) in self.shift_goto_map_nonterm.iter() {
            writeln!(f, "{}: {}", nonterm, goto)?;
        }
        writeln!(f, "ReduceMap:")?;
        for (token, action) in self.reduce_map.iter() {
            writeln!(f, "{}: {}", token, action)?;
        }
        Ok(())
    }
}
