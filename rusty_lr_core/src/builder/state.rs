use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// state for internal usage during grammar building stage
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: BTreeMap<Term, usize>,
    pub shift_goto_map_nonterm: BTreeMap<NonTerm, usize>,
    pub reduce_map: BTreeMap<Term, BTreeSet<usize>>,
    pub ruleset: BTreeSet<crate::rule::ShiftedRuleRef>,
    pub can_accept_error: bool,
}
impl<Term, NonTerm> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            shift_goto_map_term: Default::default(),
            shift_goto_map_nonterm: Default::default(),
            reduce_map: Default::default(),
            ruleset: Default::default(),
            can_accept_error: false,
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

impl<Term, NonTerm> From<State<Term, NonTerm>>
    for crate::parser::state::IntermediateState<Term, NonTerm, usize, usize>
where
    Term: Ord,
{
    fn from(state: crate::builder::State<Term, NonTerm>) -> Self {
        use crate::parser::state::ShiftTarget;

        crate::parser::state::IntermediateState {
            shift_goto_map_term: state
                .shift_goto_map_term
                .into_iter()
                .map(|(term, state_index)| {
                    (
                        term,
                        ShiftTarget {
                            state: state_index.into(),
                            push: true,
                        },
                    )
                })
                .collect(),
            shift_goto_map_nonterm: state
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state_index)| {
                    (
                        nonterm,
                        ShiftTarget {
                            state: state_index.into(),
                            push: true,
                        },
                    )
                })
                .collect(),
            reduce_map: state
                .reduce_map
                .into_iter()
                .map(|(term, rules)| (term, rules.into_iter().collect()))
                .collect(),
            ruleset: state.ruleset.into_iter().collect(),
            can_accept_error: state.can_accept_error,
        }
    }
}
