use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;

use super::dfa::DFA;
use super::error::BuildError;
use super::state::State;
use crate::hashmap::HashMap;
use crate::rule::*;
use crate::token::Token;

/// struct that holding pre-calculated information for `expand()` function.
#[derive(Debug, Clone)]
pub struct ExpandCache<Term> {
    rule: usize,
    lookaheads: BTreeSet<Term>,
    include_origin_lookaheads: bool,
}

type ProductionRuleWithLookaheads<Term, NonTerm> =
    (ProductionRule<Term, NonTerm>, Option<BTreeSet<Term>>);

/// A struct for Context Free Grammar and DFA construction
#[derive(Debug, Clone)]
pub struct Grammar<Term, NonTerm> {
    /// set of production rules
    pub rules: Vec<ProductionRuleWithLookaheads<Term, NonTerm>>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    firsts: HashMap<NonTerm, (BTreeSet<Term>, bool)>,

    /// reduce type for each terminal symbols for resolving shift/reduce conflict
    pub reduce_types: HashMap<Term, ReduceType>,

    /// rules for each nonterminals
    rules_map: HashMap<NonTerm, Vec<usize>>,

    expand_cache: HashMap<NonTerm, Vec<ExpandCache<Term>>>,
}

impl<Term, NonTerm> Grammar<Term, NonTerm> {
    pub fn new() -> Self {
        Grammar {
            rules: Vec::new(),
            firsts: Default::default(),
            reduce_types: Default::default(),
            rules_map: Default::default(),
            expand_cache: Default::default(),
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule(&mut self, name: NonTerm, rule: Vec<Token<Term, NonTerm>>) -> usize
    where
        NonTerm: Copy + Hash + Eq,
    {
        let index = self.rules.len();
        self.rules_map.entry(name).or_default().push(index);
        let rule = ProductionRule { name, rule };
        self.rules.push((rule, None));
        index
    }
    pub fn add_rule_with_lookaheads(
        &mut self,
        name: NonTerm,
        rule: Vec<Token<Term, NonTerm>>,
        lookaheads: BTreeSet<Term>,
    ) -> usize
    where
        NonTerm: Copy + Hash + Eq,
    {
        let index = self.rules.len();
        self.rules_map.entry(name).or_default().push(index);
        let rule = ProductionRule { name, rule };
        self.rules.push((rule, Some(lookaheads)));
        index
    }

    /// error if different reduce type is assigned to same terminal symbol
    pub fn set_reduce_type(&mut self, term: Term, reduce_type: ReduceType) -> bool
    where
        Term: Hash + Eq,
    {
        if let Some(old) = self.reduce_types.insert(term, reduce_type) {
            if old != reduce_type {
                return false;
            }
        }
        true
    }

    /// build LR(1) parser table from given grammar
    pub fn build(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<DFA<Term, NonTerm>, BuildError<Term, NonTerm>>
    where
        Term: Copy + Ord + Hash,
        NonTerm: Copy + Hash + Ord,
    {
        let mut dfa = self.build_without_resolving(augmented_name)?;
        self.resolve_reduce_type(&mut dfa);
        Ok(dfa)
    }

    /// build LR(1) parser table from given grammar
    /// but do not resolve shift/reduce conflict
    pub fn build_without_resolving(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<DFA<Term, NonTerm>, BuildError<Term, NonTerm>>
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
        let mut stack = Vec::new();
        let main_state = self.build_recursive(
            augmented_rule_set,
            &mut states,
            &mut state_map,
            None,
            &mut stack,
        )?;
        if main_state != 0 {
            panic!("main state is not 0");
        }

        Ok(DFA { states })
    }

    /// build LALR(1) parser table from given grammar
    pub fn build_lalr(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<DFA<Term, NonTerm>, BuildError<Term, NonTerm>>
    where
        Term: Copy + Ord + Hash,
        NonTerm: Copy + Hash + Ord,
    {
        let mut dfa = self.build_lalr_without_resolving(augmented_name)?;
        self.resolve_reduce_type(&mut dfa);
        Ok(dfa)
    }

    /// build LALR(1) parser table from given grammar
    /// but do not resolve shift/reduce conflict
    pub fn build_lalr_without_resolving(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<DFA<Term, NonTerm>, BuildError<Term, NonTerm>>
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
        let mut stack = Vec::new();
        let main_state = self.build_recursive_lalr(
            augmented_rule_set,
            &mut states,
            &mut state_map,
            None,
            &mut stack,
        )?;
        if main_state != 0 {
            panic!("main state is not 0");
        }

        Ok(DFA { states })
    }

    /// search for every production rules with name 'name'
    fn search_rules(&self, name: NonTerm) -> Result<&Vec<usize>, BuildError<Term, NonTerm>>
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
            for (rule, _) in self.rules.iter() {
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
                    let rule = &self.rules[cur.rule].0;
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
            let (rule, _) = &self.rules[rule_ref.rule];
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
                    let lookaheads = if let Some(force_lookaheads) = self.rules[c.rule].1.as_ref() {
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

    fn resolve_reduce_type(&self, dfa: &mut DFA<Term, NonTerm>)
    where
        Term: Copy + Ord + Hash,
    {
        // reduce_type conflict resolving
        for state in dfa.states.iter_mut() {
            let mut both_in_reduce_shift = Vec::new();
            for term in state.reduce_map.keys().copied() {
                if state.shift_goto_map_term.contains_key(&term) {
                    both_in_reduce_shift.push(term);
                }
            }

            for term in both_in_reduce_shift.into_iter() {
                match self.reduce_types.get(&term) {
                    Some(ReduceType::Left) => {
                        // remove shift action
                        state.shift_goto_map_term.remove(&term);
                    }
                    Some(ReduceType::Right) => {
                        // remove reduce action
                        state.reduce_map.remove(&term);
                    }
                    None => {}
                }
            }
        }
    }

    /// build new state with given production rules
    fn build_recursive(
        &self,
        mut rules: LookaheadRuleRefSet<Term>,
        states: &mut Vec<State<Term, NonTerm>>,
        state_map: &mut BTreeMap<LookaheadRuleRefSet<Term>, usize>,
        token: Option<Token<Term, NonTerm>>,
        stack: &mut Vec<(Token<Term, NonTerm>, usize)>,
    ) -> Result<usize, BuildError<Term, NonTerm>>
    where
        Term: Hash + Ord + Copy,
        NonTerm: Hash + Ord + Copy,
    {
        // expand
        self.expand(&mut rules)?;

        // check if this set of production rules already exists
        if let Some(state_id) = state_map.get(&rules).copied() {
            if states[state_id].shortest_path.len() > stack.len() {
                states[state_id].shortest_path = stack.clone();
            }
            return Ok(state_id);
        }

        // new state id
        let state_id = states.len();
        state_map.insert(rules.clone(), state_id);
        states.push(State::new(token, stack.clone()));
        states[state_id].ruleset = rules.clone();

        let mut next_rules_term = BTreeMap::new();
        let mut next_rules_nonterm = BTreeMap::new();
        let mut reduce_map: BTreeMap<Term, BTreeSet<usize>> = BTreeMap::new();
        for (mut rule_ref, lookaheads) in rules.rules.into_iter() {
            let (rule, _) = &self.rules[rule_ref.rule];
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
            stack.push((Token::Term(next_term), state_id));
            let next_state_id = self.build_recursive(
                next_rule_set,
                states,
                state_map,
                Some(Token::Term(next_term)),
                stack,
            )?;
            stack.pop();

            states[state_id]
                .shift_goto_map_term
                .insert(next_term, next_state_id);
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            stack.push((Token::NonTerm(next_nonterm), state_id));
            let next_state_id = self.build_recursive(
                next_rule_set,
                states,
                state_map,
                Some(Token::NonTerm(next_nonterm)),
                stack,
            )?;
            stack.pop();

            states[state_id]
                .shift_goto_map_nonterm
                .insert(next_nonterm, next_state_id);
        }

        Ok(state_id)
    }

    /// build new state with given production rules
    fn build_recursive_lalr(
        &self,
        mut rules: LookaheadRuleRefSet<Term>,
        states: &mut Vec<State<Term, NonTerm>>,
        state_map: &mut BTreeMap<BTreeSet<ShiftedRuleRef>, usize>,
        token: Option<Token<Term, NonTerm>>,
        stack: &mut Vec<(Token<Term, NonTerm>, usize)>,
    ) -> Result<usize, BuildError<Term, NonTerm>>
    where
        Term: Hash + Eq + Ord + Copy,
        NonTerm: Hash + Eq + Ord + Copy,
    {
        // expand
        self.expand(&mut rules)?;

        let shifted_rules: BTreeSet<_> = rules.rules.keys().copied().collect();
        let mut newly_added = false;

        let state_id = *state_map.entry(shifted_rules).or_insert_with(|| {
            newly_added = true;
            let new_state_id = states.len();
            states.push(State::new(token, stack.clone()));
            for shifted_rule in rules.rules.keys().copied() {
                states[new_state_id]
                    .ruleset
                    .rules
                    .insert(shifted_rule, BTreeSet::new());
            }
            new_state_id
        });
        if states[state_id].shortest_path.len() > stack.len() {
            states[state_id].shortest_path = stack.clone();
        }

        let mut lookaheads_empty = true;
        let mut next_rules_term = BTreeMap::new();
        let mut next_rules_nonterm = BTreeMap::new();
        let mut reduce_map: BTreeMap<Term, BTreeSet<usize>> = BTreeMap::new();
        for ((mut rule_ref, mut lookaheads_src), (_, lookaheads_dst)) in rules
            .rules
            .into_iter()
            .zip(states[state_id].ruleset.rules.iter_mut())
        {
            let (rule, _) = &self.rules[rule_ref.rule];
            let lookaheads_diff: BTreeSet<_> =
                lookaheads_src.difference(lookaheads_dst).copied().collect();
            lookaheads_empty &= lookaheads_diff.is_empty();
            lookaheads_dst.append(&mut lookaheads_src);
            match rule.rule.get(rule_ref.shifted).copied() {
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
                    for lookahead in lookaheads_diff.into_iter() {
                        reduce_map
                            .entry(lookahead)
                            .or_default()
                            .insert(rule_ref.rule);
                    }
                }
            }
        }

        // if there are no new lookaheads, break out of recursive_build
        // Augmented rule always has empty lookahead, so filter it out with `newly_added`
        if !newly_added && lookaheads_empty {
            return Ok(state_id);
        }

        // process rules that no more tokens left to shift
        // if next token is one of lookahead, add reduce action
        // if there are multiple recude rules for same lookahead, it is a reduce/reduce conflict
        for (lookahead, mut reduce_rules) in reduce_map.into_iter() {
            let state = &mut states[state_id];
            state
                .reduce_map
                .entry(lookahead)
                .or_default()
                .append(&mut reduce_rules);
        }

        // process next rules with token
        // add shift and goto action
        // if next_token is in reduce_map, then it is a reduce/shift conflict
        for (next_term, next_rule_set) in next_rules_term.into_iter() {
            stack.push((Token::Term(next_term), state_id));
            let next_state_id = self.build_recursive_lalr(
                next_rule_set,
                states,
                state_map,
                Some(Token::Term(next_term)),
                stack,
            )?;
            stack.pop();

            states[state_id]
                .shift_goto_map_term
                .insert(next_term, next_state_id);
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            stack.push((Token::NonTerm(next_nonterm), state_id));
            let next_state_id = self.build_recursive_lalr(
                next_rule_set,
                states,
                state_map,
                Some(Token::NonTerm(next_nonterm)),
                stack,
            )?;
            stack.pop();

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
