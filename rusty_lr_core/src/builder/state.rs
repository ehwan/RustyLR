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

impl<Term, NonTerm, StateIndex, RuleIndex> From<State<Term, NonTerm, StateIndex, RuleIndex>>
    for crate::parser::state::IntermediateState<Term, NonTerm, StateIndex, RuleIndex>
{
    fn from(state: crate::builder::State<Term, NonTerm, StateIndex, RuleIndex>) -> Self {
        crate::parser::state::IntermediateState {
            shift_goto_map_term: state.shift_goto_map_term.into_iter().collect(),
            shift_goto_map_nonterm: state.shift_goto_map_nonterm.into_iter().collect(),
            reduce_map: state
                .reduce_map
                .into_iter()
                .map(|(term, rules)| (term, rules.into_iter().collect()))
                .collect(),
            ruleset: state.ruleset.into_iter().collect(),
        }
    }
}
