use crate::hashmap::HashMap;
use crate::rule::LookaheadRuleRefSet;

use std::collections::BTreeSet;
use std::hash::Hash;

/// state in DFA building
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: HashMap<Term, usize>,
    pub shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    pub reduce_map: HashMap<Term, BTreeSet<usize>>,
    pub ruleset: LookaheadRuleRefSet<Term>,
}
impl<Term, NonTerm> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            shift_goto_map_term: Default::default(),
            shift_goto_map_nonterm: Default::default(),
            reduce_map: Default::default(),
            ruleset: LookaheadRuleRefSet::new(),
        }
    }
    /// We have two different `State` types. One in the `crate::builder` module and one in the `crate`.
    /// This state in `crate::builder` is used to build the DFA, which contains the lookaheads of the rules.
    /// This method converts the `State` in `crate::builder` to the `State` in `crate`.
    pub fn to_export(self) -> crate::lr::State<Term, NonTerm>
    where
        Term: Hash + Eq,
    {
        crate::lr::State {
            shift_goto_map_term: self.shift_goto_map_term,
            shift_goto_map_nonterm: self.shift_goto_map_nonterm,
            reduce_map: self
                .reduce_map
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().next().unwrap()))
                .collect(),
            ruleset: self.ruleset.rules.into_keys().collect(),
        }
    }
    /// We have two different `State` types. One in the `crate::builder` module and one in the `crate`.
    /// This state in `crate::builder` is used to build the DFA, which contains the lookaheads of the rules.
    /// This method converts the `State` in `crate::builder` to the `State` in `crate`.
    pub fn to_export_glr(self) -> crate::glr::State<Term, NonTerm>
    where
        Term: Hash + Eq,
    {
        crate::glr::State {
            shift_goto_map_term: self.shift_goto_map_term,
            shift_goto_map_nonterm: self.shift_goto_map_nonterm,
            reduce_map: self
                .reduce_map
                .into_iter()
                .map(|(k, v)| -> (Term, Vec<usize>) { (k, v.into_iter().collect()) })
                .collect(),
            ruleset: self.ruleset.rules.into_keys().collect(),
        }
    }
}

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
