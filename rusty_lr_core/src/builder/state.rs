use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// state for internal usage during grammar building stage
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

impl<Term, NonTerm> From<State<crate::TerminalSymbol<Term>, NonTerm>>
    for crate::parser::state::IntermediateState<Term, NonTerm, usize, usize>
where
    Term: Ord,
{
    fn from(mut state: crate::builder::State<crate::TerminalSymbol<Term>, NonTerm>) -> Self {
        use crate::parser::state::ShiftTarget;

        let error_shift = state
            .shift_goto_map_term
            .remove(&crate::TerminalSymbol::Error);
        let eof_shift = state
            .shift_goto_map_term
            .remove(&crate::TerminalSymbol::Eof);
        let error_reduce = state
            .reduce_map
            .remove(&crate::TerminalSymbol::Error)
            .map(|rules| rules.into_iter().collect());
        let eof_reduce = state
            .reduce_map
            .remove(&crate::TerminalSymbol::Eof)
            .map(|rules| rules.into_iter().collect());
        crate::parser::state::IntermediateState {
            shift_goto_map_term: state
                .shift_goto_map_term
                .into_iter()
                .map(|(term, state_index)| {
                    (
                        term.into_term().unwrap(),
                        ShiftTarget {
                            state: state_index.into(),
                            push: true,
                        },
                    )
                })
                .collect(),
            error_shift,
            eof_shift,
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
                .map(|(term, rules)| (term.into_term().unwrap(), rules.into_iter().collect()))
                .collect(),
            error_reduce,
            eof_reduce,
            ruleset: state.ruleset.into_iter().collect(),
        }
    }
}
