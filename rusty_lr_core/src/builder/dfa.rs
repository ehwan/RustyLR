use std::vec::Vec;

use crate::{ShiftedRuleRef, Token};

use super::state::State;

/// struct for Deterministic Finite Automaton (DFA).
///
/// It contains Vec of production rules and states.
pub struct DFA<Term, NonTerm> {
    pub states: Vec<State<Term, NonTerm>>,
}

#[derive(Debug, Clone)]
pub struct ShiftSimulated {
    pub rules: Vec<ShiftedRuleRef>,
    pub state: usize,
}
#[derive(Debug, Clone)]
pub struct ReduceSimulated<Term, NonTerm> {
    pub rule: usize,
    pub state: usize,
    pub chain: ConflictSimulated<Term, NonTerm>,
}

#[derive(Debug, Clone)]
pub struct ConflictSimulated<Term, NonTerm> {
    pub shift: Option<ShiftSimulated>,
    pub reduces: Vec<ReduceSimulated<Term, NonTerm>>,
    pub path: Vec<Token<Term, NonTerm>>,
}

impl<Term, NonTerm> DFA<Term, NonTerm> {
    fn conflict_simulate_recursive<'a>(
        term: &Term,
        state_id: usize,
        token_stack: &[Token<Term, NonTerm>],
        state_stack: &[usize],
        states: &'a [State<Term, NonTerm>],
        rule_len: impl Fn(usize) -> usize + Copy,
        rule_nonterm: impl Fn(usize) -> NonTerm + Copy,
    ) -> Option<ConflictSimulated<Term, NonTerm>>
    where
        Term: Clone + Ord,
        NonTerm: Clone + Ord,
    {
        let state = &states[state_id];
        // shift/reduce conflict
        let shift = state.shift_goto_map_term.get(term).map(|&shift_state| {
            let shift_rules = states[shift_state].unshifted_ruleset().collect();
            ShiftSimulated {
                rules: shift_rules,
                state: shift_state,
            }
        });

        let reduces = if let Some(reduce_rules) = state.reduce_map.get(term) {
            reduce_rules
                .iter()
                .filter_map(|&reduce_rule| {
                    let mut state_stack = state_stack.to_vec();
                    state_stack.push(state_id);
                    let len = rule_len(reduce_rule);
                    state_stack.truncate(state_stack.len() - len);
                    let last_state = *state_stack.last().unwrap();
                    let new_state = *states[last_state]
                        .shift_goto_map_nonterm
                        .get(&rule_nonterm(reduce_rule))
                        .unwrap();

                    let mut token_stack = token_stack.to_vec();
                    token_stack.truncate(token_stack.len() - len);
                    token_stack.push(Token::NonTerm(rule_nonterm(reduce_rule)));

                    if let Some(chain) = Self::conflict_simulate_recursive(
                        term,
                        new_state,
                        &token_stack,
                        &state_stack,
                        states,
                        rule_len,
                        rule_nonterm,
                    ) {
                        Some(ReduceSimulated {
                            rule: reduce_rule,
                            state: new_state,
                            chain,
                        })
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        };

        if shift.is_none() && reduces.is_empty() {
            return None;
        }
        Some(ConflictSimulated {
            shift,
            reduces,
            path: token_stack.to_vec(),
        })
    }
    pub fn conflict_simulate<'a>(
        &'a self,
        rule_len: impl Fn(usize) -> usize + 'a,
        rule_nonterm: impl Fn(usize) -> NonTerm + 'a,
    ) -> impl Iterator<Item = Vec<(&'a Term, ConflictSimulated<Term, NonTerm>)>> + 'a
    where
        Term: Clone + Ord,
        NonTerm: Clone + Ord,
    {
        self.states.iter().enumerate().map(move |(i, state)| {
            let token_stack = state
                .shortest_path
                .iter()
                .map(|(token, _)| token.clone())
                .collect::<Vec<_>>();
            let state_stack = state
                .shortest_path
                .iter()
                .map(|(_, state)| *state)
                .collect::<Vec<_>>();
            let simulated = state
                .reduce_map
                .keys()
                .filter_map(|term| {
                    Self::conflict_simulate_recursive(
                        term,
                        i,
                        &token_stack,
                        &state_stack,
                        &self.states,
                        &rule_len,
                        &rule_nonterm,
                    )
                    .map(|chain| (term, chain))
                })
                .collect::<Vec<_>>();
            simulated
        })
    }
}
