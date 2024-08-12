use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::vec::Vec;

use super::error::BuildError;
use crate::hashmap::HashMap;
use crate::parser::parser::Parser;
use crate::rule::*;
use crate::state::State;
use crate::token::Token;

/// A struct for Context Free Grammar and DFA construction
#[derive(Debug, Clone)]
pub struct Grammar<Term, NonTerm> {
    /// set of production rules
    pub rules: Vec<ProductionRule<Term, NonTerm>>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    firsts: HashMap<NonTerm, (BTreeSet<Term>, bool)>,

    /// reduce type for each terminal symbols for resolving shift/reduce conflict
    reduce_types: HashMap<Term, ReduceType>,

    /// rules for each nonterminals
    rules_map: HashMap<NonTerm, Vec<usize>>,
}

impl<Term, NonTerm> Grammar<Term, NonTerm> {
    pub fn new() -> Self {
        Grammar {
            rules: Vec::new(),
            firsts: Default::default(),
            reduce_types: Default::default(),
            rules_map: Default::default(),
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule(&mut self, name: NonTerm, rule: Vec<Token<Term, NonTerm>>) -> usize
    where
        NonTerm: Clone + Hash + Eq,
    {
        let id = self.rules.len();
        self.rules_map.entry(name.clone()).or_default().push(id);
        let rule = ProductionRule { name, rule };
        self.rules.push(rule);
        id
    }

    pub fn set_reduce_type(
        &mut self,
        term: Term,
        reduce_type: ReduceType,
    ) -> Result<(), BuildError<'static, Term, NonTerm>>
    where
        Term: Hash + Eq + Clone,
    {
        if let Some(old) = self.reduce_types.insert(term.clone(), reduce_type) {
            if old == reduce_type {
                Ok(())
            } else {
                Err(BuildError::MultipleReduceType(term))
            }
        } else {
            Ok(())
        }
    }

    /// build LR(1) parser table from given grammar
    pub fn build(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<Parser<Term, NonTerm>, BuildError<Term, NonTerm>>
    where
        Term: Clone + Ord + Hash,
        NonTerm: Clone + Hash + Ord,
    {
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
        if main_state != 0 {
            panic!("main state is not 0");
        }

        Ok(Parser {
            rules: self.rules.clone(),
            states,
        })
    }

    /// build LALR(1) parser table from given grammar
    pub fn build_lalr(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<Parser<Term, NonTerm>, BuildError<Term, NonTerm>>
    where
        Term: Clone + Ord + Hash,
        NonTerm: Clone + Hash + Ord,
    {
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
        let main_state =
            self.build_recursive_lalr(augmented_rule_set, &mut states, &mut state_map)?;
        if main_state != 0 {
            panic!("main state is not 0");
        }

        Ok(Parser {
            rules: self.rules.clone(),
            states,
        })
    }

    /// search for every production rules with name 'name'
    fn search_rules(&self, name: &NonTerm) -> Vec<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.rules_map.get(name).cloned().unwrap_or_default()
    }
    /// calculate first terminals for each nonterminals
    fn calculate_first(&mut self)
    where
        Term: Clone + Ord,
        NonTerm: Hash + Eq + Clone,
    {
        loop {
            let mut changed = false;
            for rule in self.rules.iter() {
                let (mut firsts, mut canbe_empty) = self
                    .firsts
                    .entry(rule.name.clone())
                    .or_insert_with(|| {
                        changed = true;
                        (BTreeSet::new(), false)
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
    ) -> Result<BTreeSet<Term>, BuildError<Term, NonTerm>>
    where
        Term: Ord + Clone,
        NonTerm: Clone + Hash + Eq,
    {
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
                    ret.append(&mut firsts.clone());
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
    fn expand(&self, rules: &mut LookaheadRuleRefSet<Term>) -> Result<(), BuildError<Term, NonTerm>>
    where
        Term: Clone + Ord,
        NonTerm: Clone + Hash + Eq,
    {
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
    ) -> Result<usize, BuildError<Term, NonTerm>>
    where
        Term: Hash + Ord + Clone,
        NonTerm: Hash + Ord + Clone,
    {
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
        states[state_id].ruleset = rules.clone();

        let mut next_rules_term = BTreeMap::new();
        let mut next_rules_nonterm = BTreeMap::new();
        let mut empty_rules = Vec::new();
        for (mut rule_ref, lookaheads) in rules.rules.into_iter() {
            let rule = &self.rules[rule_ref.rule];
            match rule.rule.get(rule_ref.shifted) {
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
                // check for shift/reduce conflict
                // if this 'lookahead' term is also in next_rules_term
                if next_rules_term.contains_key(&lookahead) {
                    // shift/reduce conflict may occur here, check reduce type

                    match self.reduce_types.get(&lookahead) {
                        Some(ReduceType::Left) => {
                            // reduce first

                            // remove lookahead from next_rules_term
                            next_rules_term.remove(&lookahead);

                            // check for reduce/reduce conflict
                            if let Some(old) = state.reduce_map.get_mut(&lookahead) {
                                // same rule, continue
                                if old == &empty_rule {
                                    continue;
                                }

                                // conflict
                                return Err(BuildError::ReduceReduceConflict {
                                    lookahead,
                                    rule1: *old,
                                    rule2: empty_rule,
                                    rules: &self.rules,
                                });
                            } else {
                                state.reduce_map.insert(lookahead, empty_rule);
                            }
                        }
                        Some(ReduceType::Right) => {
                            // shift first
                            // do not add this reduce action
                            // shift action will be added later
                        }
                        None => {
                            // shift/reduce error

                            // next_rules_term has set of rules that is already shifted.
                            // so shift -= 1 again to get the rule that is not shifted yet
                            let shifted_rules: BTreeMap<_, _> = next_rules_term[&lookahead]
                                .rules
                                .iter()
                                .map(|(rule_ref, lookaheads)| {
                                    let mut rule_ref = rule_ref.clone();
                                    rule_ref.shifted -= 1;
                                    (rule_ref, lookaheads.clone())
                                })
                                .collect();

                            return Err(BuildError::ShiftReduceConflict {
                                reduce: empty_rule,
                                shift: LookaheadRuleRefSet {
                                    rules: shifted_rules,
                                },
                                term: lookahead.clone(),
                                rules: &self.rules,
                            });
                        }
                    }
                } else {
                    // no shift/reduce conflict
                    // check for reduce/reduce conflict
                    if let Some(old) = state.reduce_map.get_mut(&lookahead) {
                        // same rule, continue
                        if old == &empty_rule {
                            continue;
                        }

                        // conflict
                        return Err(BuildError::ReduceReduceConflict {
                            lookahead,
                            rule1: *old,
                            rule2: empty_rule,
                            rules: &self.rules,
                        });
                    } else {
                        state.reduce_map.insert(lookahead, empty_rule);
                    }
                }
            }
        }

        // process next rules with token
        // add shift and goto action
        for (next_term, next_rule_set) in next_rules_term.into_iter() {
            // check for shift/reduce conflict again
            if states[state_id].reduce_map.contains_key(next_term) {
                match self.reduce_types.get(next_term) {
                    Some(ReduceType::Left) => {
                        // reduce first
                        continue;
                    }
                    Some(ReduceType::Right) => {
                        states[state_id].reduce_map.remove(next_term);
                    }
                    None => {
                        // shift/reduce error
                        return Err(BuildError::ShiftReduceConflict {
                            reduce: states[state_id].reduce_map[next_term],
                            shift: next_rule_set.clone(),
                            term: next_term.clone(),
                            rules: &self.rules,
                        });
                    }
                }
            }

            let next_state_id = self.build_recursive(next_rule_set, states, state_map)?;

            states[state_id]
                .shift_goto_map_term
                .insert(next_term.clone(), next_state_id);
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            let next_state_id = self.build_recursive(next_rule_set, states, state_map)?;

            states[state_id]
                .shift_goto_map_nonterm
                .insert(next_nonterm.clone(), next_state_id);
        }

        Ok(state_id)
    }

    /// build new state with given production rules
    fn build_recursive_lalr(
        &self,
        mut rules: LookaheadRuleRefSet<Term>,
        states: &mut Vec<State<Term, NonTerm>>,
        state_map: &mut BTreeMap<BTreeSet<ShiftedRuleRef>, usize>,
    ) -> Result<usize, BuildError<Term, NonTerm>>
    where
        Term: Hash + Eq + Ord + Clone,
        NonTerm: Hash + Eq + Ord + Clone,
    {
        // expand
        self.expand(&mut rules)?;

        let shifted_rules: BTreeSet<_> = rules.rules.keys().cloned().collect();
        let mut newly_added = false;

        let state_id = *state_map.entry(shifted_rules).or_insert_with(|| {
            newly_added = true;
            let new_state_id = states.len();
            states.push(State::new());
            for (shifted_rule, _) in rules.rules.iter() {
                states[new_state_id]
                    .ruleset
                    .rules
                    .insert(*shifted_rule, BTreeSet::new());
            }
            new_state_id
        });

        let mut lookaheads_empty = true;
        let mut next_rules_term = BTreeMap::new();
        let mut next_rules_nonterm = BTreeMap::new();
        let mut empty_rules = Vec::new();
        for ((mut rule_ref, mut lookaheads_src), (_, lookaheads_dst)) in rules
            .rules
            .into_iter()
            .zip(states[state_id].ruleset.rules.iter_mut())
        {
            let rule = &self.rules[rule_ref.rule];
            let lookaheads_diff: BTreeSet<_> =
                lookaheads_src.difference(lookaheads_dst).cloned().collect();
            lookaheads_empty &= lookaheads_diff.is_empty();
            lookaheads_dst.append(&mut lookaheads_src);
            match rule.rule.get(rule_ref.shifted) {
                Some(Token::Term(term)) => {
                    rule_ref.shifted += 1;
                    next_rules_term
                        .entry(term)
                        .or_insert_with(LookaheadRuleRefSet::new)
                        .add(rule_ref, lookaheads_diff);
                    // Duplicated rule will be handled in reduce/reduce conflict
                }
                Some(Token::NonTerm(nonterm)) => {
                    rule_ref.shifted += 1;
                    next_rules_nonterm
                        .entry(nonterm)
                        .or_insert(LookaheadRuleRefSet::new())
                        .add(rule_ref, lookaheads_diff);
                    // Duplicated rule will be handled in reduce/reduce conflict
                }
                None => {
                    empty_rules.push((rule_ref.rule, lookaheads_diff));
                }
            }
        }

        if !newly_added && lookaheads_empty {
            return Ok(state_id);
        }

        // process rules that no more tokens left to shift
        // if next token is one of lookahead, add reduce action
        // if there are multiple recude rules for same lookahead, it is a reduce/reduce conflict
        for (empty_rule, lookaheads) in empty_rules.into_iter() {
            let state = &mut states[state_id];
            for lookahead in lookaheads.into_iter() {
                // check for shift/reduce conflict
                // if this 'lookahead' term is also in next_rules_term
                if next_rules_term.contains_key(&lookahead) {
                    // shift/reduce conflict may occur here, check reduce type

                    match self.reduce_types.get(&lookahead) {
                        Some(ReduceType::Left) => {
                            // reduce first

                            // remove lookahead from next_rules_term
                            next_rules_term.remove(&lookahead);

                            // check for reduce/reduce conflict
                            if let Some(old) = state.reduce_map.get_mut(&lookahead) {
                                // same rule, continue
                                if old == &empty_rule {
                                    continue;
                                }

                                // conflict
                                return Err(BuildError::ReduceReduceConflict {
                                    lookahead,
                                    rule1: *old,
                                    rule2: empty_rule,
                                    rules: &self.rules,
                                });
                            } else {
                                state.reduce_map.insert(lookahead, empty_rule);
                            }
                        }
                        Some(ReduceType::Right) => {
                            // shift first
                            // do not add this reduce action
                            // shift action will be added later
                        }
                        None => {
                            // shift/reduce error
                            return Err(BuildError::ShiftReduceConflict {
                                reduce: empty_rule,
                                shift: next_rules_term[&lookahead].clone(),
                                term: lookahead.clone(),
                                rules: &self.rules,
                            });
                        }
                    }
                } else {
                    // no shift/reduce conflict
                    // check for reduce/reduce conflict
                    if let Some(old) = state.reduce_map.get_mut(&lookahead) {
                        // same rule, continue
                        if old == &empty_rule {
                            continue;
                        }

                        // conflict
                        return Err(BuildError::ReduceReduceConflict {
                            lookahead,
                            rule1: *old,
                            rule2: empty_rule,
                            rules: &self.rules,
                        });
                    } else {
                        state.reduce_map.insert(lookahead, empty_rule);
                    }
                }
            }
        }

        // process next rules with token
        // add shift and goto action
        // if next_token is in reduce_map, then it is a reduce/shift conflict
        for (next_term, next_rule_set) in next_rules_term.into_iter() {
            // check for shift/reduce conflict again
            if states[state_id].reduce_map.contains_key(next_term) {
                match self.reduce_types.get(next_term) {
                    Some(ReduceType::Left) => {
                        // reduce first
                        continue;
                    }
                    Some(ReduceType::Right) => {
                        states[state_id].reduce_map.remove(next_term);
                    }
                    None => {
                        // shift/reduce error
                        return Err(BuildError::ShiftReduceConflict {
                            reduce: states[state_id].reduce_map[next_term],
                            shift: next_rule_set.clone(),
                            term: next_term.clone(),
                            rules: &self.rules,
                        });
                    }
                }
            }

            let next_state_id = self.build_recursive_lalr(next_rule_set, states, state_map)?;

            states[state_id]
                .shift_goto_map_term
                .insert(next_term.clone(), next_state_id);
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            let next_state_id = self.build_recursive_lalr(next_rule_set, states, state_map)?;

            states[state_id]
                .shift_goto_map_nonterm
                .insert(next_nonterm.clone(), next_state_id);
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

impl<Term, NonTerm> Default for Grammar<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
