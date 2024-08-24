use std::collections::BTreeSet;
use std::hash::Hash;

use crate::ShiftedRuleRef;
use crate::{HashMap, HashSet};

/// State in DFA.
/// This accepts multiple reduce for single terminal.
/// For GLR parser.
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: HashMap<Term, usize>,
    pub shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    pub reduce_map: HashMap<Term, Vec<usize>>,
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
    pub fn reduce(&self, term: &Term) -> Option<&[usize]>
    where
        Term: Hash + Eq,
    {
        self.reduce_map.get(term).map(|v| v.as_slice())
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

    /// Map terminal and non-terminal symbols to another type.
    /// This is useful when exporting & importing rules.
    pub fn map<NewTerm: Hash + Eq, NewNonTerm: Hash + Eq>(
        self,
        term_map: impl Fn(Term) -> NewTerm,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> State<NewTerm, NewNonTerm> {
        State {
            shift_goto_map_term: self
                .shift_goto_map_term
                .into_iter()
                .map(|(term, state)| (term_map(term), state))
                .collect(),
            shift_goto_map_nonterm: self
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state)| (nonterm_map(nonterm), state))
                .collect(),
            reduce_map: self
                .reduce_map
                .into_iter()
                .map(|(term, rule)| (term_map(term), rule))
                .collect(),
            ruleset: self.ruleset,
        }
    }
}

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}