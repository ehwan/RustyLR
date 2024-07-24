use std::collections::BTreeMap;
use std::hash::Hash;

use crate::BuildError;

use super::parser::Parser;

impl<Term: Clone + Ord + Hash + Eq, NonTerm: Clone + Hash + Eq> Parser<Term, NonTerm> {
    /// merge states with same core ruleset.
    /// this builds LALR(1) parser from LR(1) parser
    pub fn optimize_lalr(&mut self) -> Result<(), BuildError<'_, Term, NonTerm>> {
        // group of states with same core ruleset
        let state_groups: Vec<_> = {
            let mut ruleset_state_map = BTreeMap::new();
            for (idx, state) in self.states.iter().enumerate() {
                // lalr consider two states same if core ruleset is same
                // lr(1) consider both core ruleset + lookaheads
                let ruleset: Vec<_> = state.ruleset.rules.keys().collect();
                ruleset_state_map
                    .entry(ruleset)
                    .or_insert_with(Vec::new)
                    .push(idx);
            }
            ruleset_state_map.into_values().collect()
        };

        // map from old state index to new state index
        let mut state_old_to_new = vec![0; self.states.len()];
        for (group_idx, state_group) in state_groups.iter().enumerate() {
            for s in state_group.iter() {
                state_old_to_new[*s] = group_idx;
            }
        }

        // redirect state index
        for state in self.states.iter_mut() {
            for (_, goto) in state.shift_goto_map_term.iter_mut() {
                *goto = state_old_to_new[*goto];
            }
            for (_, goto) in state.shift_goto_map_nonterm.iter_mut() {
                *goto = state_old_to_new[*goto];
            }
        }

        let mut new_states = Vec::with_capacity(state_groups.len());
        for state_group in state_groups.into_iter() {
            let mut new_state = self.states[state_group[0]].clone();
            for s in state_group.into_iter().skip(1) {
                let state = &mut self.states[s];

                // merge lookaheads
                for (rule, lookahead) in state.ruleset.rules.iter_mut() {
                    new_state
                        .ruleset
                        .rules
                        .get_mut(rule)
                        .unwrap()
                        .append(lookahead);
                }

                // merge goto map
                for (term, goto) in state.shift_goto_map_term.iter() {
                    if let Some(old) = new_state.shift_goto_map_term.insert(term.clone(), *goto) {
                        if old != *goto {
                            // this never happens
                            unreachable!("merging shift/goto map failed");
                        }
                    }
                }
                for (nonterm, goto) in state.shift_goto_map_nonterm.iter() {
                    if let Some(old) = new_state
                        .shift_goto_map_nonterm
                        .insert(nonterm.clone(), *goto)
                    {
                        if old != *goto {
                            // this never happens
                            unreachable!("merging shift/goto map failed");
                        }
                    }
                }

                // merge reduce map
                for (term, ruleid) in state.reduce_map.iter() {
                    if let Some(old) = new_state.reduce_map.insert(term.clone(), *ruleid) {
                        if old != *ruleid {
                            return Err(BuildError::ReduceReduceConflict {
                                lookahead: term.clone(),
                                rule1: old,
                                rule2: *ruleid,
                                rules: &self.rules,
                            });
                        }
                    }
                }
            }
            new_states.push(new_state);
        }

        self.main_state = state_old_to_new[self.main_state];
        self.states = new_states;

        Ok(())
    }
}
