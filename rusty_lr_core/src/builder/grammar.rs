use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;

use super::BuildError;
use super::DiagnosticCollector;
use super::ReduceType;
use super::State;
use super::States;
use crate::hash::HashMap;
use crate::rule::*;
use crate::token::Token;

/// struct that holding pre-calculated information for `expand()` function.
#[derive(Debug, Clone)]
struct ExpandCache<Term> {
    rule: usize,
    lookaheads: BTreeSet<Term>,
    include_origin_lookaheads: bool,
}

#[derive(Debug, Clone)]
pub struct Rule<Term, NonTerm> {
    pub rule: ProductionRule<Term, NonTerm>,
    pub lookaheads: Option<BTreeSet<Term>>,
    /// for reduce/reduce conflict resolving
    pub priority: usize,
}

/// A struct for Context Free Grammar and DFA construction
#[derive(Debug, Clone)]
pub struct Grammar<Term, NonTerm> {
    /// set of production rules
    pub rules: Vec<Rule<Term, NonTerm>>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    pub firsts: HashMap<NonTerm, (BTreeSet<Term>, bool)>,

    /// rules for each nonterminals
    rules_map: HashMap<NonTerm, Vec<usize>>,

    expand_cache: HashMap<NonTerm, Vec<ExpandCache<Term>>>,

    /// precedence level for each terminal symbol
    pub precedence_levels: HashMap<Term, usize>,

    /// precedence type for each precedence level
    pub precedence_types: Vec<Option<ReduceType>>,
}

impl<Term, NonTerm> Grammar<Term, NonTerm> {
    pub fn new() -> Self {
        Grammar {
            rules: Vec::new(),
            firsts: Default::default(),
            rules_map: Default::default(),
            expand_cache: Default::default(),

            precedence_levels: Default::default(),
            precedence_types: Vec::new(),
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule(
        &mut self,
        name: NonTerm,
        rule: Vec<Token<Term, NonTerm>>,
        lookaheads: Option<BTreeSet<Term>>,
        precedence: Option<Precedence>,
        priority: usize,
    ) -> usize
    where
        NonTerm: Copy + Hash + Eq,
    {
        let index = self.rules.len();
        self.rules_map.entry(name).or_default().push(index);
        let rule = Rule {
            rule: ProductionRule {
                name,
                rule,
                precedence,
            },
            lookaheads,
            priority,
        };
        self.rules.push(rule);
        index
    }

    /// false if precedence already exists and different
    pub fn add_precedence(&mut self, term: Term, level: usize) -> bool
    where
        Term: Hash + Eq,
    {
        if let Some(old) = self.precedence_levels.insert(term, level) {
            if old != level {
                return false;
            }
        }
        true
    }

    /// error if different reduce type is assigned to same terminal symbol
    pub fn set_precedence_types(&mut self, types: Vec<Option<ReduceType>>) {
        self.precedence_types = types;
    }

    /// search for every production rules with name 'name'
    fn search_rules(&self, name: NonTerm) -> Result<&[usize], BuildError<Term, NonTerm>>
    where
        NonTerm: Hash + Eq,
    {
        match self.rules_map.get(&name) {
            Some(rules) => Ok(rules),
            None => Err(BuildError::RuleNotFound(name)),
        }
    }

    /// calculate first terminals for each nonterminals
    fn calculate_first(&mut self)
    where
        Term: Copy + Ord,
        NonTerm: Hash + Eq + Copy,
    {
        loop {
            let mut changed = false;
            for rule in self.rules.iter() {
                let rule = &rule.rule;
                let (mut firsts, mut canbe_empty) = self
                    .firsts
                    .entry(rule.name)
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
                            let insert_result = firsts.insert(*term);
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
                                firsts.extend(child_firsts.iter().copied());
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
                    self.firsts.insert(rule.name, (firsts, canbe_empty));
                }
            }
            if !changed {
                break;
            }
        }
    }

    /// pre calculate the information for `expand()` function.
    fn calculate_expand_cache(&mut self) -> Result<(), BuildError<Term, NonTerm>>
    where
        Term: Ord + Copy,
        NonTerm: Hash + Eq + Copy,
    {
        let mut pong: Vec<ExpandCache<Term>> = Vec::new();
        for (nonterm, nonterm_rules) in self.rules_map.iter() {
            let mut rules: BTreeMap<usize, ExpandCache<Term>> = nonterm_rules
                .iter()
                .map(|rule| {
                    (
                        *rule,
                        ExpandCache {
                            rule: *rule,
                            lookaheads: BTreeSet::new(),
                            include_origin_lookaheads: true,
                        },
                    )
                })
                .collect();
            pong.clear();

            loop {
                pong.clear();
                for (_, cur) in rules.iter() {
                    let rule = &self.rules[cur.rule].rule;
                    if let Some(Token::NonTerm(nonterm_name)) = rule.rule.first() {
                        // calculate lookaheads
                        let (lookaheads, canbe_empty) =
                            self.lookahead(&rule.rule[1..], &cur.lookaheads)?;

                        for searched_rule in self.search_rules(*nonterm_name)?.iter() {
                            pong.push(ExpandCache {
                                rule: *searched_rule,
                                lookaheads: lookaheads.clone(),
                                include_origin_lookaheads: canbe_empty
                                    && cur.include_origin_lookaheads,
                            });
                        }
                    }
                }

                let mut changed = false;
                for mut p in pong.drain(..) {
                    let cur = rules.entry(p.rule).or_insert_with(|| {
                        changed = true;
                        ExpandCache {
                            rule: p.rule,
                            lookaheads: Default::default(),
                            include_origin_lookaheads: false,
                        }
                    });
                    if p.include_origin_lookaheads && !cur.include_origin_lookaheads {
                        cur.include_origin_lookaheads = true;
                        changed = true;
                    }
                    let len0 = cur.lookaheads.len();
                    cur.lookaheads.append(&mut p.lookaheads);
                    if len0 != cur.lookaheads.len() {
                        changed = true;
                    }
                }
                if !changed {
                    break;
                }
            }

            self.expand_cache
                .insert(*nonterm, rules.into_values().collect());
        }
        Ok(())
    }

    /// calculate lookahead tokens for given follow tokens.
    /// this is equivalent to FIRST( follow_tokens, lookahead ).
    /// 1st `bool` of returned tuple is true if follow_tokens can be empty.
    fn lookahead(
        &self,
        follow_tokens: &[Token<Term, NonTerm>],
        lookaheads: &BTreeSet<Term>,
    ) -> Result<(BTreeSet<Term>, bool), BuildError<Term, NonTerm>>
    where
        Term: Ord + Copy,
        NonTerm: Copy + Hash + Eq,
    {
        let mut ret = BTreeSet::new();
        for token in follow_tokens.iter() {
            match token {
                Token::Term(term) => {
                    ret.insert(*term);
                    return Ok((ret, false));
                }
                Token::NonTerm(nonterm) => {
                    let (firsts, canbe_empty) = if let Some(nonterm) = self.firsts.get(nonterm) {
                        nonterm
                    } else {
                        return Err(BuildError::RuleNotFound(*nonterm));
                    };
                    ret.append(&mut firsts.clone());
                    if !canbe_empty {
                        return Ok((ret, false));
                    }
                }
            }
        }
        ret.append(&mut lookaheads.clone());
        Ok((ret, true))
    }

    /// for given set of production rules,
    /// if the first token of each rule is nonterminal, attach its production rules
    fn expand(&self, rules: &mut LookaheadRuleRefSet<Term>) -> Result<(), BuildError<Term, NonTerm>>
    where
        Term: Copy + Ord,
        NonTerm: Copy + Hash + Eq,
    {
        let mut new_rules = Vec::new();
        for (rule_ref, lookaheads) in rules.rules.iter() {
            let rule = &self.rules[rule_ref.rule].rule;
            if let Some(Token::NonTerm(nonterm_name)) = rule.rule.get(rule_ref.shifted) {
                let lookaheads = self
                    .lookahead(&rule.rule[rule_ref.shifted + 1..], lookaheads)?
                    .0;
                for c in self.expand_cache.get(nonterm_name).unwrap().iter() {
                    let lookaheads = if c.include_origin_lookaheads {
                        lookaheads.union(&c.lookaheads).copied().collect()
                    } else {
                        c.lookaheads.clone()
                    };
                    // check for force lookahead
                    let lookaheads =
                        if let Some(force_lookaheads) = self.rules[c.rule].lookaheads.as_ref() {
                            lookaheads.intersection(force_lookaheads).copied().collect()
                        } else {
                            lookaheads
                        };

                    new_rules.push((
                        ShiftedRuleRef {
                            rule: c.rule,
                            shifted: 0,
                        },
                        lookaheads,
                    ));
                }
            }
        }
        for (new_rule, lookaheads) in new_rules.into_iter() {
            rules.add(new_rule, lookaheads);
        }
        Ok(())
    }

    /// in certain state (with ruleset),
    /// given a subset of ruleset (which is `rules`),
    /// add other rules in `ruleset` that is related to `rules`
    fn expand_backward(&self, rules: &mut Vec<ShiftedRuleRef>, ruleset: &BTreeSet<ShiftedRuleRef>)
    where
        Term: PartialEq + Copy,
        NonTerm: Copy + PartialEq + Ord,
    {
        let mut zero_shifted: BTreeSet<_> = rules
            .iter()
            .filter_map(|rule| {
                if rule.shifted == 0 {
                    Some(self.rules[rule.rule].rule.name)
                } else {
                    None
                }
            })
            .collect();
        let mut next_zeros = BTreeSet::new();
        let mut inserted: BTreeSet<_> = rules.iter().copied().collect();
        loop {
            let mut changed = false;
            next_zeros.clear();
            for &nonterm in zero_shifted.iter() {
                // if rule is shifted = 0, which is newly added rule,
                // search for the rule that brings this rule to this state
                // and add that rule to the `rules`
                for &rule in ruleset.iter().filter(|&rule_ref| {
                    self.rules[rule_ref.rule].rule.rule.get(rule_ref.shifted)
                        == Some(&Token::NonTerm(nonterm))
                }) {
                    if inserted.insert(rule) {
                        changed = true;
                        rules.push(rule);
                        if rule.shifted == 0 {
                            next_zeros.insert(self.rules[rule.rule].rule.name);
                        }
                    }
                }
            }
            std::mem::swap(&mut zero_shifted, &mut next_zeros);
            if !changed {
                break;
            }
        }
    }

    /// check for any shift/reduce or reduce/reduce conflicts and report them to `diags`.
    fn check_conflicts(
        &self,
        states: &[State<Term, NonTerm>],
        diags: &mut DiagnosticCollector<Term>,
    ) where
        Term: Ord + Copy + Hash,
        NonTerm: Copy + PartialEq + Ord,
    {
        if !diags.enabled {
            return;
        }
        let mut from = vec![usize::MAX; states.len()];
        for (state_id, state) in states.iter().enumerate() {
            for &next_state in state
                .shift_goto_map_term
                .values()
                .chain(state.shift_goto_map_nonterm.values())
            {
                from[next_state] = from[next_state].min(state_id);
            }
        }
        for (state_id, state) in states.iter().enumerate() {
            for (&term, reduce_rules) in state.reduce_map.iter() {
                if let Some(&next_shift_state) = state.shift_goto_map_term.get(&term) {
                    // check if this can be resolved by operator precedence
                    if self.precedence_levels.contains_key(&term)
                        && reduce_rules
                            .iter()
                            .all(|&rule| self.rules[rule].rule.precedence.is_some())
                    {
                        continue;
                    }

                    // shift/reduce conflict
                    let shift_rules: Vec<_> =
                        states[next_shift_state].unshifted_ruleset().collect();
                    let mut shift_rules_backtrace = shift_rules.clone();
                    self.expand_backward(&mut shift_rules_backtrace, &states[state_id].ruleset);
                    let reduce_rules = reduce_rules
                        .iter()
                        .map(|&rule| {
                            let len = self.rules[rule].rule.rule.len();
                            let mut state = state_id;
                            for _ in 0..len {
                                if state == usize::MAX {
                                    break;
                                }
                                state = from[state];
                            }
                            let shift_rules = if state != usize::MAX {
                                let mut rules = vec![ShiftedRuleRef { rule, shifted: 0 }];
                                self.expand_backward(&mut rules, &states[state].ruleset);
                                rules
                            } else {
                                Default::default()
                            };
                            (rule, shift_rules)
                        })
                        .collect();
                    diags.add_shift_reduce_conflict(
                        term,
                        shift_rules,
                        shift_rules_backtrace,
                        reduce_rules,
                    );
                } else if reduce_rules.len() > 1 {
                    let reduce_rules = reduce_rules
                        .iter()
                        .map(|&rule| {
                            let len = self.rules[rule].rule.rule.len();
                            let mut state = state_id;
                            for _ in 0..len {
                                if state == usize::MAX {
                                    break;
                                }
                                state = from[state];
                            }
                            let shift_rules = if state != usize::MAX {
                                let mut rules = vec![ShiftedRuleRef { rule, shifted: 0 }];
                                self.expand_backward(&mut rules, &states[state].ruleset);
                                rules
                            } else {
                                Default::default()
                            };
                            (rule, shift_rules)
                        })
                        .collect();
                    // no shift/reduce conflict
                    // check for reduce/reduce conflict
                    diags.update_reduce_reduce_conflict(reduce_rules, term);
                }
            }
        }
    }

    /// build full LR(1) parser table from given grammar
    fn build_full(
        &mut self,
        augmented_name: NonTerm,
        diags: &mut DiagnosticCollector<Term>,
    ) -> Result<States<Term, NonTerm>, BuildError<Term, NonTerm>>
    where
        Term: Copy + Ord + Hash,
        NonTerm: Copy + Hash + Ord,
    {
        self.calculate_first();
        self.calculate_expand_cache()?;

        // add main augmented rule
        let augmented_rule_set = {
            let augmented_rules = self.search_rules(augmented_name)?;

            if augmented_rules.is_empty() {
                return Err(BuildError::NoAugmented);
            }

            let mut augmented_rules_set = LookaheadRuleRefSet::new();
            for rule in augmented_rules.iter() {
                augmented_rules_set.rules.insert(
                    ShiftedRuleRef {
                        rule: *rule,
                        shifted: 0,
                    },
                    BTreeSet::new(),
                );
            }
            augmented_rules_set
        };

        let mut states = Vec::new();
        let mut state_map = BTreeMap::new();
        let main_state =
            self.build_recursive(augmented_rule_set, &mut states, &mut state_map, diags)?;
        if main_state != 0 {
            panic!("main state is not 0");
        }

        Ok(States { states })
    }

    /// build minimal-LR(1) parser table from given grammar
    pub fn build(
        &mut self,
        augmented_name: NonTerm,
        diags: &mut DiagnosticCollector<Term>,
    ) -> Result<States<Term, NonTerm>, BuildError<Term, NonTerm>>
    where
        Term: Copy + Ord + Hash,
        NonTerm: Copy + Hash + Ord,
    {
        // build full LR(1) parser table first
        let mut states = self.build_full(augmented_name, diags)?.states;

        // building minimal LR(1)
        // now merge some states into LALR form, but without conflicts
        let mut core_map: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for (state_id, state) in states.iter().enumerate() {
            core_map
                .entry(state.ruleset.clone())
                .or_default()
                .push(state_id);
        }

        let mut merged_count = 0;
        loop {
            let mut merged = false;
            let mut merge_into = BTreeMap::new();
            for state_ids in core_map.values_mut() {
                /*
                mergeing states A and B
                state A and B can be merged if
                new conflicts are not created by merging them

                shift map
                A  AB  B

                reduce map
                a  ab  b

                AB must be equal
                ab must be equal

                B \cup a must be null )
                A \cup b must be null ) => merging must not make new conflict
                */
                for i in 0..state_ids.len() {
                    if state_ids[i] == usize::MAX {
                        continue;
                    }
                    for j in i + 1..state_ids.len() {
                        if state_ids[j] == usize::MAX {
                            continue;
                        }
                        let state_a = &states[state_ids[i]];
                        let state_b = &states[state_ids[j]];
                        let mut valid = true;

                        // check shift nonterm map
                        for (&shift_nonterm_a, &state_id_a) in state_a.shift_goto_map_nonterm.iter()
                        {
                            if let Some(&state_id_b) =
                                state_b.shift_goto_map_nonterm.get(&shift_nonterm_a)
                            {
                                // AB
                                if state_id_a != state_id_b {
                                    valid = false;
                                    break;
                                }
                            }
                        }

                        // check shift map
                        for (&shift_term_a, &state_id_a) in state_a.shift_goto_map_term.iter() {
                            if let Some(&state_id_b) =
                                state_b.shift_goto_map_term.get(&shift_term_a)
                            {
                                // AB
                                if state_id_a != state_id_b {
                                    valid = false;
                                    break;
                                }
                            } else {
                                // A
                                // check if term is in b but not in a
                                if state_b.reduce_map.contains_key(&shift_term_a) {
                                    if state_a.reduce_map.contains_key(&shift_term_a) {
                                        // continue
                                    } else {
                                        valid = false;
                                        break;
                                    }
                                } else {
                                    // continue
                                }
                            }
                        }
                        if !valid {
                            continue;
                        }
                        for (&reduce_term_a, state_ids_a) in state_a.reduce_map.iter() {
                            if let Some(state_ids_b) = state_b.reduce_map.get(&reduce_term_a) {
                                // ab
                                if state_ids_a != state_ids_b {
                                    valid = false;
                                    break;
                                }
                            } else {
                                // A
                                // check if term is in b but not in a
                                if state_b.shift_goto_map_term.contains_key(&reduce_term_a) {
                                    if state_a.shift_goto_map_term.contains_key(&reduce_term_a) {
                                        // continue
                                    } else {
                                        valid = false;
                                        break;
                                    }
                                } else {
                                    // continue
                                }
                            }
                        }
                        if valid {
                            merged_count += 1;
                            merge_into.insert(state_ids[j], state_ids[i]);
                            let mut state_b = std::mem::take(&mut states[state_ids[j]]);
                            state_ids[j] = usize::MAX;
                            let state_a = &mut states[state_ids[i]];
                            state_a
                                .shift_goto_map_term
                                .append(&mut state_b.shift_goto_map_term);
                            state_a
                                .shift_goto_map_nonterm
                                .append(&mut state_b.shift_goto_map_nonterm);
                            state_a.reduce_map.append(&mut state_b.reduce_map);
                            merged = true;
                        }
                    }
                }
                state_ids.retain(|&state_id| state_id != usize::MAX);
            }
            // update state ids
            if merged {
                for state in &mut states {
                    for next_state in state.shift_goto_map_term.values_mut() {
                        if let Some(&new_state) = merge_into.get(next_state) {
                            *next_state = new_state;
                        }
                    }
                    for next_state in state.shift_goto_map_nonterm.values_mut() {
                        if let Some(&new_state) = merge_into.get(next_state) {
                            *next_state = new_state;
                        }
                    }
                }
            } else {
                break;
            }
        }
        let mut new_states = Vec::with_capacity(states.len() - merged_count);
        let mut old_to_new = vec![0; states.len()];
        for (state_id, state) in states.into_iter().enumerate() {
            if state.ruleset.is_empty() {
                continue;
            }
            let new_state_id = new_states.len();
            new_states.push(state);
            old_to_new[state_id] = new_state_id;
        }

        for state in &mut new_states {
            for next_state in state.shift_goto_map_term.values_mut() {
                *next_state = old_to_new[*next_state];
            }
            for next_state in state.shift_goto_map_nonterm.values_mut() {
                *next_state = old_to_new[*next_state];
            }
        }

        states = new_states;
        self.check_conflicts(&states, diags);

        Ok(States { states })
    }

    /// build LALR(1) parser table from given grammar
    pub fn build_lalr(
        &mut self,
        augmented_name: NonTerm,
        diags: &mut DiagnosticCollector<Term>,
    ) -> Result<States<Term, NonTerm>, BuildError<Term, NonTerm>>
    where
        Term: Copy + Ord + Hash,
        NonTerm: Copy + Hash + Ord,
    {
        // build full LR(1) parser table first
        let mut states = self.build_full(augmented_name, diags)?.states;

        // building LALR(1)
        // merge every states with same core;
        // but we do have to check for resolving conflicts
        let mut core_map: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for (state_id, state) in states.iter().enumerate() {
            core_map
                .entry(state.ruleset.clone())
                .or_default()
                .push(state_id);
        }

        let mut merge_into = BTreeMap::new();
        for merge_states in core_map.into_values() {
            let mut merge_states = merge_states.into_iter();
            let state_a = merge_states.next().unwrap();
            for state_b in merge_states {
                merge_into.insert(state_b, state_a);

                let mut state_b = std::mem::take(&mut states[state_b]);
                states[state_a]
                    .shift_goto_map_term
                    .append(&mut state_b.shift_goto_map_term);
                states[state_a]
                    .shift_goto_map_nonterm
                    .append(&mut state_b.shift_goto_map_nonterm);
                for (term, mut reduce_rules) in state_b.reduce_map {
                    states[state_a]
                        .reduce_map
                        .entry(term)
                        .or_default()
                        .append(&mut reduce_rules);
                }
            }
        }
        let mut new_states = Vec::with_capacity(states.len() - merge_into.len());
        let mut old_to_new = vec![0; states.len()];
        for (state_id, state) in states.into_iter().enumerate() {
            if merge_into.contains_key(&state_id) {
                continue;
            }
            let new_state_id = new_states.len();
            new_states.push(state);
            old_to_new[state_id] = new_state_id;
        }

        for state in &mut new_states {
            for next_state in state.shift_goto_map_term.values_mut() {
                if let Some(&new_state) = merge_into.get(next_state) {
                    *next_state = new_state;
                }
                *next_state = old_to_new[*next_state];
            }
            for next_state in state.shift_goto_map_nonterm.values_mut() {
                if let Some(&new_state) = merge_into.get(next_state) {
                    *next_state = new_state;
                }
                *next_state = old_to_new[*next_state];
            }
        }
        states = new_states;

        for state in &mut states {
            // check reduce/reduce conflicts resolving by its priority
            for reduce_rules in state.reduce_map.values_mut() {
                if reduce_rules.len() <= 1 {
                    continue;
                }

                let max_priority = reduce_rules
                    .iter()
                    .map(|&rule| self.rules[rule].priority)
                    .max()
                    .unwrap();

                let deleted_rules: BTreeSet<usize> = reduce_rules
                    .iter()
                    .filter(|&&rule| self.rules[rule].priority != max_priority)
                    .copied()
                    .collect();
                if deleted_rules.is_empty() {
                    continue;
                }
                reduce_rules.retain(|rule| self.rules[*rule].priority == max_priority);

                diags.add_reduce_reduce_resolved(max_priority, reduce_rules.clone(), deleted_rules);
            }

            // check shift/reduce conflicts resolving
            for (&term, reduce_rules) in state.reduce_map.iter_mut() {
                if !state.shift_goto_map_term.contains_key(&term) {
                    continue;
                };
                let Some(&shift_prec) = self.precedence_levels.get(&term) else {
                    // no precedence for this shift rule
                    continue;
                };

                let mut reduces = BTreeMap::new();
                let mut remove_reduces = BTreeMap::new();
                let mut remove_shift = true;

                for &reduce_rule in reduce_rules.iter() {
                    let Some(reduce_op) = self.rules[reduce_rule].rule.precedence else {
                        // no operator for this reduce rule
                        remove_shift = false;
                        continue;
                    };
                    let Precedence::Fixed(reduce_prec) = reduce_op else {
                        // not fixed operator, so no precedence known at this time
                        remove_shift = false;
                        continue;
                    };

                    // compare precedence
                    match reduce_prec.cmp(&shift_prec) {
                        Ordering::Less => {
                            // reduce < shift => remove reduce
                            remove_reduces.insert(reduce_rule, reduce_prec);
                            remove_shift = false;
                        }
                        Ordering::Greater => {
                            // reduce > shift => remove shift, but check other reduce rules
                            reduces.insert(reduce_rule, reduce_prec);
                        }
                        Ordering::Equal => {
                            // reduce == shift => check reduce type
                            match self.precedence_types[reduce_prec] {
                                Some(ReduceType::Left) => {
                                    // reduce first => remove shift
                                    reduces.insert(reduce_rule, reduce_prec);
                                }
                                Some(ReduceType::Right) => {
                                    // shift first => remove reduce
                                    remove_reduces.insert(reduce_rule, reduce_prec);
                                    remove_shift = false;
                                }
                                None => {
                                    // no reduce type => conflict
                                    reduces.insert(reduce_rule, reduce_prec);
                                    remove_shift = false;
                                }
                            }
                        }
                    }
                }

                let shift_rules = state
                    .ruleset
                    .iter()
                    .filter_map(|rule_ref| {
                        if self.rules[rule_ref.rule].rule.rule.get(rule_ref.shifted)
                            == Some(&Token::Term(term))
                        {
                            Some(*rule_ref)
                        } else {
                            None
                        }
                    })
                    .collect();

                if remove_shift {
                    // remove rules that start with `term`
                    state.shift_goto_map_term.remove(&term);
                    state.ruleset.retain(|rule_ref| {
                        self.rules[rule_ref.rule].rule.rule.get(rule_ref.shifted)
                            != Some(&Token::Term(term))
                    });
                }

                // remove reduce rules
                for (&remove_reduce, _) in remove_reduces.iter() {
                    reduce_rules.remove(&remove_reduce);
                }

                if !remove_shift && remove_reduces.is_empty() {
                    continue;
                }

                if remove_shift {
                    diags.add_shift_reduce_resolved_reduce(term, shift_rules, shift_prec, reduces);
                } else {
                    diags.add_shift_reduce_resolved_shift(
                        term,
                        shift_rules,
                        shift_prec,
                        remove_reduces,
                    );
                }
            }
            state
                .reduce_map
                .retain(|_, reduce_rules| !reduce_rules.is_empty());
        }

        self.check_conflicts(&states, diags);

        Ok(States { states })
    }

    /// build new state with given production rules
    fn build_recursive(
        &self,
        mut rules: LookaheadRuleRefSet<Term>,
        states: &mut Vec<State<Term, NonTerm>>,
        state_map: &mut BTreeMap<LookaheadRuleRefSet<Term>, usize>,
        diags: &mut DiagnosticCollector<Term>,
    ) -> Result<usize, BuildError<Term, NonTerm>>
    where
        Term: Hash + Ord + Copy,
        NonTerm: Hash + Ord + Copy,
    {
        // expand
        self.expand(&mut rules)?;

        // check if this set of production rules already exists
        if let Some(&state_id) = state_map.get(&rules) {
            return Ok(state_id);
        }

        // new state id
        let state_id = states.len();
        state_map.insert(rules.clone(), state_id);
        states.push(State::new());
        states[state_id].ruleset = rules.rules.keys().copied().collect();

        // calculate next shifted rules and reduce rules
        // we don't care about the conflicts here
        let mut next_rules_term = BTreeMap::new();
        let mut next_rules_nonterm = BTreeMap::new();
        let mut reduce_map: BTreeMap<Term, BTreeSet<usize>> = BTreeMap::new();
        for (mut rule_ref, lookaheads) in rules.rules.into_iter() {
            let rule = &self.rules[rule_ref.rule].rule;
            match rule.rule.get(rule_ref.shifted) {
                Some(Token::Term(term)) => {
                    rule_ref.shifted += 1;
                    next_rules_term
                        .entry(*term)
                        .or_insert_with(LookaheadRuleRefSet::new)
                        .add(rule_ref, lookaheads);
                    // Duplicated rule will be handled in reduce/reduce conflict
                }
                Some(Token::NonTerm(nonterm)) => {
                    rule_ref.shifted += 1;
                    next_rules_nonterm
                        .entry(*nonterm)
                        .or_insert(LookaheadRuleRefSet::new())
                        .add(rule_ref, lookaheads);
                    // Duplicated rule will be handled in reduce/reduce conflict
                }
                None => {
                    for lookahead in lookaheads.into_iter() {
                        reduce_map
                            .entry(lookahead)
                            .or_default()
                            .insert(rule_ref.rule);
                    }
                }
            }
        }

        // remove reduce/reduce conflicts by its priority
        for reduce_rules in reduce_map.values_mut() {
            if reduce_rules.len() <= 1 {
                continue;
            }

            // check if all rules have priority, and max priority
            let max_priority = reduce_rules
                .iter()
                .map(|&rule| self.rules[rule].priority)
                .max()
                .unwrap();

            let deleted_rules: BTreeSet<_> = reduce_rules
                .iter()
                .filter(|&&rule| self.rules[rule].priority != max_priority)
                .copied()
                .collect();
            if deleted_rules.is_empty() {
                continue;
            }
            reduce_rules.retain(|rule| self.rules[*rule].priority == max_priority);

            diags.add_reduce_reduce_resolved(max_priority, reduce_rules.clone(), deleted_rules);
        }

        // check shift/reduce conflicts
        for (&term, reduce_rules) in reduce_map.iter_mut() {
            if !next_rules_term.contains_key(&term) {
                continue;
            };
            let Some(&shift_prec) = self.precedence_levels.get(&term) else {
                // no precedence for this shift rule
                continue;
            };

            // reduce_rule, reduce_prec map
            let mut reduces = BTreeMap::new();
            let mut remove_reduces = BTreeMap::new();
            let mut remove_shift = true;

            for &reduce_rule in reduce_rules.iter() {
                let Some(reduce_op) = self.rules[reduce_rule].rule.precedence else {
                    // no operator for this reduce rule
                    remove_shift = false;
                    continue;
                };
                let Precedence::Fixed(reduce_prec) = reduce_op else {
                    // not fixed operator, so no precedence known at this time
                    remove_shift = false;
                    continue;
                };

                // compare precedence
                match reduce_prec.cmp(&shift_prec) {
                    Ordering::Less => {
                        // reduce < shift => remove reduce
                        remove_reduces.insert(reduce_rule, reduce_prec);
                        remove_shift = false;
                    }
                    Ordering::Greater => {
                        // reduce > shift => remove shift, but check other reduce rules
                        reduces.insert(reduce_rule, reduce_prec);
                    }
                    Ordering::Equal => {
                        // reduce == shift => check reduce type
                        match self.precedence_types[reduce_prec] {
                            Some(ReduceType::Left) => {
                                // reduce first => remove shift
                                reduces.insert(reduce_rule, reduce_prec);
                            }
                            Some(ReduceType::Right) => {
                                // shift first => remove reduce
                                remove_reduces.insert(reduce_rule, reduce_prec);
                                remove_shift = false;
                            }
                            None => {
                                // no reduce type => conflict
                                reduces.insert(reduce_rule, reduce_prec);
                                remove_shift = false;
                            }
                        }
                    }
                }
            }

            let shift_rules = if remove_shift {
                // remove rules that start with `term`
                states[state_id].ruleset.retain(|rule_ref| {
                    self.rules[rule_ref.rule].rule.rule.get(rule_ref.shifted)
                        != Some(&Token::Term(term))
                });

                next_rules_term
                    .remove(&term)
                    .unwrap()
                    .rules
                    .into_keys()
                    .collect()
            } else {
                next_rules_term
                    .get(&term)
                    .unwrap()
                    .rules
                    .keys()
                    .copied()
                    .collect()
            };

            // remove reduce rules
            for (&remove_reduce, _) in remove_reduces.iter() {
                reduce_rules.remove(&remove_reduce);
            }

            if !remove_shift && remove_reduces.is_empty() {
                continue;
            }

            if remove_shift {
                diags.add_shift_reduce_resolved_reduce(term, shift_rules, shift_prec, reduces);
            } else {
                diags.add_shift_reduce_resolved_shift(
                    term,
                    shift_rules,
                    shift_prec,
                    remove_reduces,
                );
            }
        }
        reduce_map.retain(|_, reduce_rules| !reduce_rules.is_empty());

        // process rules that no more tokens left to shift
        // if next token is one of lookahead, add reduce action
        // if there are multiple recude rules for same lookahead, it is a reduce/reduce conflict
        // reduce_type conflict resolving
        for (lookahead, reduce_rules) in reduce_map.into_iter() {
            let state = &mut states[state_id];
            // no shift/reduce conflict
            // check for reduce/reduce conflict
            // just add this reduce action for now
            state.reduce_map.insert(lookahead, reduce_rules);
        }

        // process next rules with token
        // add shift and goto action
        for (next_term, next_rule_set) in next_rules_term.into_iter() {
            let next_state_id = self.build_recursive(next_rule_set, states, state_map, diags)?;

            states[state_id]
                .shift_goto_map_term
                .insert(next_term, next_state_id);
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            let next_state_id = self.build_recursive(next_rule_set, states, state_map, diags)?;

            states[state_id]
                .shift_goto_map_nonterm
                .insert(next_nonterm, next_state_id);
        }

        Ok(state_id)
    }
}

// impl<Term: Display, NonTerm: Display> Display for Grammar<Term, NonTerm> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         for (id, rule) in self.rules.iter().enumerate() {
//             writeln!(f, "{}: {}", id, rule)?;
//         }

//         Ok(())
//     }
// }

impl<Term, NonTerm> Default for Grammar<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
