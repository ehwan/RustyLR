use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// state for internal usage during grammar building stage
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm, StateIndex, RuleIndex> {
    pub shift_goto_map_term: BTreeMap<Term, StateIndex>,
    pub shift_goto_map_nonterm: BTreeMap<NonTerm, StateIndex>,
    pub reduce_map: BTreeMap<Term, BTreeSet<RuleIndex>>,
    pub ruleset: BTreeSet<crate::rule::ShiftedRuleRef>,
}
impl<Term, NonTerm, StateIndex, RuleIndex> State<Term, NonTerm, StateIndex, RuleIndex> {
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

impl<Term, NonTerm, StateIndex, RuleIndex> Default for State<Term, NonTerm, StateIndex, RuleIndex> {
    fn default() -> Self {
        Self::new()
    }
}
