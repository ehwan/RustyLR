use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::vec::Vec;

use super::error::BuildError;
use crate::parser::parser::Parser;
use crate::rule::*;
use crate::state::State;
use crate::token::Token;

/// A set of production rules and main entry point
#[derive(Debug, Clone)]
pub struct Grammar<Term, NonTerm> {
    /// set of production rules
    pub rules: Vec<ProductionRule<Term, NonTerm>>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    firsts: HashMap<NonTerm, (HashSet<Term>, bool)>,
}

impl<Term: Clone + Hash + Eq + Ord, NonTerm: Clone + Hash + Eq> Grammar<Term, NonTerm> {
    pub fn new() -> Self {
        Grammar {
            rules: Vec::new(),
            firsts: HashMap::new(),
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule(
        &mut self,
        name: NonTerm,
        rule: Vec<Token<Term, NonTerm>>,
        reduce_type: ReduceType,
    ) -> usize {
        let id = self.rules.len();
        let rule = ProductionRule {
            name: name.clone(),
            rule,
            reduce_type,
        };
        self.rules.push(rule);
        id
    }

    /// build LR(1) parser table from given grammar
    pub fn build(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<Parser<Term, NonTerm>, BuildError<Term, NonTerm>> {
        self.calculate_first();

        // add main augmented rule
        let augmented_rule_set = {
            let augmented_rules = self.search_rules(&augmented_name);

            if augmented_rules.is_empty() {
                return Err(BuildError::NoAugmented);
            }

            let mut augmented_rules_set = LookaheadRuleRefSet::new();
            for rule in augmented_rules.into_iter() {
                augmented_rules_set
                    .rules
                    .insert(ShiftedRuleRef { rule, shifted: 0 }, BTreeSet::new());
            }
            augmented_rules_set
        };

        let mut states = Vec::new();
        let mut state_map = BTreeMap::new();
        let main_state = self.build_recursive(augmented_rule_set, &mut states, &mut state_map)?;

        Ok(Parser {
            rules: self.rules.clone(),
            states,
            main_state,
        })
    }

    /// search for every production rules with name 'name'
    fn search_rules(&self, name: &NonTerm) -> Vec<usize> {
        let mut ret = Vec::new();
        for (id, rule) in self.rules.iter().enumerate() {
            if &rule.name == name {
                ret.push(id);
            }
        }
        ret
    }
    /// calculate first terminals for each nonterminals
    fn calculate_first(&mut self) {
        loop {
            let mut changed = false;
            for rule in self.rules.iter() {
                let (mut firsts, mut canbe_empty) = self
                    .firsts
                    .entry(rule.name.clone())
                    .or_insert_with(|| {
                        changed = true;
                        (HashSet::new(), false)
                    })
                    .clone();

                let mut this_nonterm_changed = false;
                let mut this_rule_canbe_empty = true;
                for token in rule.rule.iter() {
                    match token {
                        Token::Term(term) => {
                            let insert_result = firsts.insert(term.clone());
                            if insert_result {
                                this_nonterm_changed = true;
                            }
                            this_rule_canbe_empty = false;
                            break;
                        }
                        Token::NonTerm(nonterm) => {
                            if let Some((child_firsts, child_canbe_empty)) =
                                self.firsts.get(nonterm)
                            {
                                let old_len = firsts.len();
                                firsts.extend(child_firsts.iter().cloned());
                                if old_len != firsts.len() {
                                    this_nonterm_changed = true;
                                }
                                if !child_canbe_empty {
                                    this_rule_canbe_empty = false;
                                    break;
                                }
                            } else {
                                this_rule_canbe_empty = false;
                                break;
                            }
                        }
                    }
                }
                if this_rule_canbe_empty && !canbe_empty {
                    canbe_empty = true;
                    this_nonterm_changed = true;
                }
                if this_nonterm_changed {
                    changed = true;
                    self.firsts.insert(rule.name.clone(), (firsts, canbe_empty));
                }
            }
            if !changed {
                break;
            }
        }
    }

    /// calculate lookahead tokens for given follow tokens and lookahead tokens
    /// this is equivalent to FIRST( follow_tokens, lookahead )
    fn lookahead(
        &self,
        follow_tokens: &[Token<Term, NonTerm>],
        lookahead: &BTreeSet<Term>,
    ) -> Result<BTreeSet<Term>, BuildError<Term, NonTerm>> {
        let mut ret = BTreeSet::new();
        for token in follow_tokens.iter() {
            match token {
                Token::Term(term) => {
                    ret.insert(term.clone());
                    return Ok(ret);
                }
                Token::NonTerm(nonterm) => {
                    let (firsts, canbe_empty) = if let Some(nonterm) = self.firsts.get(nonterm) {
                        nonterm
                    } else {
                        return Err(BuildError::RuleNotFound(nonterm.clone()));
                    };
                    ret.extend(firsts.iter().cloned());
                    // ret.append(&mut firsts.clone());
                    if !canbe_empty {
                        return Ok(ret);
                    }
                }
            }
        }
        ret.append(&mut lookahead.clone());
        Ok(ret)
    }

    /// for given set of production rules,
    /// if the first token of each rule is nonterminal, attach its production rules
    fn expand(
        &self,
        rules: &mut LookaheadRuleRefSet<Term>,
    ) -> Result<(), BuildError<Term, NonTerm>> {
        loop {
            let mut new_rules = Vec::new();
            for (rule_ref, lookaheads) in rules.rules.iter() {
                let rule = &self.rules[rule_ref.rule];
                if let Some(Token::NonTerm(ref nonterm_name)) = rule.rule.get(rule_ref.shifted) {
                    let searched_rules = self.search_rules(nonterm_name);
                    if !searched_rules.is_empty() {
                        // calculate lookaheads
                        let lookaheads =
                            self.lookahead(&rule.rule[rule_ref.shifted + 1..], lookaheads)?;

                        // init new LookaheadRule with searched rules
                        for searched_rule in searched_rules.iter() {
                            let rule = ShiftedRuleRef {
                                rule: *searched_rule,
                                shifted: 0,
                            };
                            new_rules.push((rule, lookaheads.clone()));
                        }
                    } else {
                        // rule not found
                        return Err(BuildError::RuleNotFound(nonterm_name.clone()));
                    }
                }
            }
            let mut changed = false;
            for (rule, lookaheads) in new_rules.into_iter() {
                changed |= rules.add(rule, lookaheads);
            }

            if !changed {
                return Ok(());
            }
        }
    }

    /// build new state with given production rules
    fn build_recursive(
        &self,
        mut rules: LookaheadRuleRefSet<Term>,
        states: &mut Vec<State<Term, NonTerm>>,
        state_map: &mut BTreeMap<LookaheadRuleRefSet<Term>, usize>,
    ) -> Result<usize, BuildError<Term, NonTerm>> {
        // expand
        self.expand(&mut rules)?;

        // check if this set of production rules already exists
        if let Some(state_id) = state_map.get(&rules) {
            return Ok(*state_id);
        }

        // new state id
        let state_id = states.len();
        state_map.insert(rules.clone(), state_id);
        states.push(State::new());

        let mut next_rules_term = HashMap::new();
        let mut next_rules_nonterm = HashMap::new();
        let mut empty_rules = Vec::new();
        for (mut rule_ref, lookaheads) in rules.rules.into_iter() {
            let rule = &self.rules[rule_ref.rule];
            match rule.rule.get(rule_ref.shifted).cloned() {
                Some(Token::Term(term)) => {
                    rule_ref.shifted += 1;
                    next_rules_term
                        .entry(term)
                        .or_insert_with(LookaheadRuleRefSet::new)
                        .add(rule_ref, lookaheads);
                    // Duplicated rule will be handled in reduce/reduce conflict
                }
                Some(Token::NonTerm(nonterm)) => {
                    rule_ref.shifted += 1;
                    next_rules_nonterm
                        .entry(nonterm)
                        .or_insert(LookaheadRuleRefSet::new())
                        .add(rule_ref, lookaheads);
                    // Duplicated rule will be handled in reduce/reduce conflict
                }
                None => {
                    empty_rules.push((rule_ref.rule, lookaheads));
                }
            }
        }

        // process rules that no more tokens left to shift
        // if next token is one of lookahead, add reduce action
        // if there are multiple recude rules for same lookahead, it is a reduce/reduce conflict
        for (empty_rule, lookaheads) in empty_rules.into_iter() {
            let state = &mut states[state_id];
            for lookahead in lookaheads.into_iter() {
                if let Some(old) = state.reduce_map.get_mut(&lookahead) {
                    if old == &empty_rule {
                        continue;
                    }
                    // conflict
                    return Err(BuildError::ReduceReduceConflict {
                        lookahead,
                        rule1: *old,
                        rule2: empty_rule,
                        grammar: self,
                    });
                } else {
                    state.reduce_map.insert(lookahead, empty_rule);
                }
            }
        }

        // process next rules with token
        // add shift and goto action
        // if next_token is in reduce_map, then it is a reduce/shift conflict
        for (next_term, next_rule_set) in next_rules_term.into_iter() {
            // check shift/reduce conflict
            if let Some(reduce_rule) = states[state_id].reduce_map.get(&next_term) {
                // check if all rules in next_rule_set has same reduce_type
                // Left => reduce first
                // Right => shift first
                // else => error

                // check if there is any Error in reduce_type
                for (rule, lookaheads) in next_rule_set.rules.iter() {
                    if self.rules[rule.rule].reduce_type == ReduceType::Error {
                        return Err(BuildError::ShiftReduceConflictError {
                            reduce: *reduce_rule,
                            shift: LookaheadRuleRef {
                                rule: rule.clone(),
                                lookaheads: lookaheads.clone(),
                            },
                            grammar: self,
                        });
                    }
                }

                // check if all rules in next_rule_set has same reduce_type
                // ReduceType::Error is filtered above, so only for Left/Right
                let mut reduce_left = None;
                let mut reduce_right = None;
                for (rule, lookaheads) in next_rule_set.rules.iter() {
                    match self.rules[rule.rule].reduce_type {
                        ReduceType::Left => {
                            reduce_left = Some((rule, lookaheads));
                            if reduce_right.is_some() {
                                break;
                            }
                        }
                        ReduceType::Right => {
                            reduce_right = Some((rule, lookaheads));
                            if reduce_left.is_some() {
                                break;
                            }
                        }
                        ReduceType::Error => {
                            // should not reach here
                            unreachable!("Unreachable code for resolving Shift/Reduce Conflict");
                        }
                    }
                }

                // if both reduce_left and reduce_right is Some, then it is a conflict
                if reduce_left.is_some() && reduce_right.is_some() {
                    let (left_rule, left_lookaheads) = reduce_left.unwrap();
                    let (right_rule, right_lookaheads) = reduce_right.unwrap();
                    return Err(BuildError::ShiftReduceConflict {
                        reduce: *reduce_rule,
                        left: LookaheadRuleRef {
                            rule: left_rule.clone(),
                            lookaheads: left_lookaheads.clone(),
                        },
                        right: LookaheadRuleRef {
                            rule: right_rule.clone(),
                            lookaheads: right_lookaheads.clone(),
                        },
                        grammar: self,
                    });
                }

                // if it is reduce_left, reduce first, so do nothing

                // if it is reduce_right, shift first,
                // remove next_term from reduce_map
                // and add shift action
                if reduce_right.is_some() {
                    // remove next_term from reduce_map
                    states[state_id].reduce_map.remove(&next_term);

                    let next_state_id = self.build_recursive(next_rule_set, states, state_map)?;

                    states[state_id]
                        .shift_goto_map_term
                        .insert(next_term, next_state_id);
                }
            } else {
                // add shift action
                let next_state_id = self.build_recursive(next_rule_set, states, state_map)?;

                states[state_id]
                    .shift_goto_map_term
                    .insert(next_term, next_state_id);
            }
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            let next_state_id = self.build_recursive(next_rule_set, states, state_map)?;

            states[state_id]
                .shift_goto_map_nonterm
                .insert(next_nonterm, next_state_id);
        }

        Ok(state_id)
    }
}

impl<Term: Display, NonTerm: Display> Display for Grammar<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, rule) in self.rules.iter().enumerate() {
            writeln!(f, "{}: {}", id, rule)?;
        }

        Ok(())
    }
}
