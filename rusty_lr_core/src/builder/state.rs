use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// state in DFA building
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: BTreeMap<Term, usize>,
    pub shift_goto_map_nonterm: BTreeMap<NonTerm, usize>,
    pub reduce_map: BTreeMap<Term, BTreeSet<usize>>,
    pub ruleset: BTreeSet<crate::rule::ShiftedRuleRef>,
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

    /// shift -= 1 for all rules in the ruleset
    pub fn unshifted_ruleset(&self) -> impl Iterator<Item = crate::rule::ShiftedRuleRef> + '_ {
        self.ruleset
            .iter()
            .filter(|rule| rule.shifted > 0)
            .map(|rule| {
                let mut rule = *rule;
                rule.shifted -= 1;
                rule
            })
    }
}

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
