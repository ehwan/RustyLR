use std::hash::Hash;

use crate::HashMap;
use crate::ShiftedRuleRef;

/// A type representing a single parser state and its associated table.
/// This is used in the GLR parser.
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    /// terminal symbol -> next state
    pub shift_goto_map_term: HashMap<Term, usize>,
    /// non-terminal symbol -> next state
    pub shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub reduce_map: HashMap<Term, Vec<usize>>,
    /// set of rules that this state is trying to parse
    pub ruleset: Vec<ShiftedRuleRef>,
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
    pub fn expected(&self) -> impl Iterator<Item = &Term> {
        self.shift_goto_map_term
            .keys()
            .chain(self.reduce_map.keys())
    }
    /// get expected non-terms set
    pub fn expected_nonterm(&self) -> impl Iterator<Item = &NonTerm> {
        self.shift_goto_map_nonterm.keys()
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
