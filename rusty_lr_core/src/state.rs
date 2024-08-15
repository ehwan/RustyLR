use std::collections::BTreeSet;
use std::fmt::Display;
use std::hash::Hash;

use super::hashmap::{HashMap, HashSet};
use super::rule::ShiftedRuleRef;

/// state in DFA
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: HashMap<Term, usize>,
    pub shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    pub reduce_map: HashMap<Term, usize>,
    pub ruleset: BTreeSet<ShiftedRuleRef>,
}
impl<Term, NonTerm> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            shift_goto_map_term: Default::default(),
            shift_goto_map_nonterm: Default::default(),
            reduce_map: Default::default(),
            ruleset: Default::default(),
        }
    }

    /// feed one teminal and get action
    pub fn shift_goto_term(&self, term: &Term) -> Option<usize>
    where
        Term: Hash + Eq,
    {
        self.shift_goto_map_term.get(term).copied()
    }
    /// feed one non-teminal and get action
    pub fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.shift_goto_map_nonterm.get(nonterm).copied()
    }
    /// feed one token and get action
    pub fn reduce(&self, term: &Term) -> Option<usize>
    where
        Term: Hash + Eq,
    {
        self.reduce_map.get(term).copied()
    }

    /// check if this state is accept state.
    /// Augmented -> Start EOF . is accept state.
    pub fn is_accept(&self) -> bool {
        self.reduce_map.is_empty()
            && self.shift_goto_map_term.is_empty()
            && self.shift_goto_map_nonterm.is_empty()
    }

    /// get expected terms set
    pub fn expected(&self) -> HashSet<&Term>
    where
        Term: Hash + Eq,
    {
        HashSet::from_iter(
            self.shift_goto_map_term
                .keys()
                .chain(self.reduce_map.keys()),
        )
    }
}
impl<Term: Display, NonTerm: Display> Display for State<Term, NonTerm> {
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

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
