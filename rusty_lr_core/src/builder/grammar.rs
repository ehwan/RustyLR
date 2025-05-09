use std::cmp::Ordering;
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
struct ExpandCache<Term> {
    rule: usize,
    lookaheads: BTreeSet<Term>,
    include_origin_lookaheads: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Operator<Term> {
    /// defined as normal terminal symbol
    Term(Term),
    /// defined as %prec
    Prec(Term),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ResolveDiagnostic<Term> {
    Priority {
        max_priority: usize,
        remaining: Vec<usize>,
        deleted: Vec<usize>,
    },
    Precedence {
        term: Term,
        shift_precedence: usize,
        shift_deleted: bool,
        shift_rules: Vec<ShiftedRuleRef>,

        // rule, precedence, reduce_type
        reduce_rules: Vec<(usize, Option<usize>, Option<ReduceType>)>,
        deleted_reduces: Vec<(usize, usize, Option<ReduceType>)>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ShiftReduceConflictDiag<Term> {
    pub term: Term,
    pub shift_rules: Vec<ShiftedRuleRef>,
    pub reduce_rules: BTreeSet<usize>,
}

pub struct DiagnosticCollector<Term> {
    pub enabled: bool,
    pub resolved: BTreeSet<ResolveDiagnostic<Term>>,
    pub shift_reduce_conflicts: BTreeSet<ShiftReduceConflictDiag<Term>>,
    pub reduce_reduce_conflicts: BTreeMap<BTreeSet<usize>, BTreeSet<Term>>,
}
impl<Term> DiagnosticCollector<Term> {
    pub fn new(collect: bool) -> Self {
        DiagnosticCollector {
            enabled: collect,
            resolved: BTreeSet::new(),
            shift_reduce_conflicts: BTreeSet::new(),
            reduce_reduce_conflicts: BTreeMap::new(),
        }
    }
    pub fn add_resolved(&mut self, resolved: ResolveDiagnostic<Term>)
    where
        Term: Ord,
    {
        if self.enabled {
            self.resolved.insert(resolved);
        }
    }
    pub fn add_shift_reduce_conflict(&mut self, diag: ShiftReduceConflictDiag<Term>)
    where
        Term: Ord,
    {
        if self.enabled {
            self.shift_reduce_conflicts.insert(diag);
        }
    }
    pub fn update_reduce_reduce_conflict(&mut self, reduce_rules: &BTreeSet<usize>, term: Term)
    where
        Term: Ord,
    {
        if self.enabled {
            self.reduce_reduce_conflicts
                .entry(reduce_rules.clone())
                .or_default()
                .insert(term);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Rule<Term, NonTerm> {
    pub rule: ProductionRule<Term, NonTerm>,
    pub lookaheads: Option<BTreeSet<Term>>,
    pub operator: Option<Operator<Term>>,
    /// for reduce/reduce conflict resolving
    pub priority: Option<usize>,
}

/// A struct for Context Free Grammar and DFA construction
#[derive(Debug, Clone)]
pub struct Grammar<Term, NonTerm> {
    /// set of production rules
    pub rules: Vec<Rule<Term, NonTerm>>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    pub firsts: HashMap<NonTerm, (BTreeSet<Term>, bool)>,

    /// reduce type for each terminal symbols for resolving shift/reduce conflict
    pub reduce_types: HashMap<Operator<Term>, ReduceType>,

    /// rules for each nonterminals
    rules_map: HashMap<NonTerm, Vec<usize>>,

    expand_cache: HashMap<NonTerm, Vec<ExpandCache<Term>>>,

    pub precedence_map: HashMap<Operator<Term>, usize>,
}

impl<Term, NonTerm> Grammar<Term, NonTerm> {
    pub fn new() -> Self {
        Grammar {
            rules: Vec::new(),
            firsts: Default::default(),
            reduce_types: Default::default(),
            rules_map: Default::default(),
            expand_cache: Default::default(),
            precedence_map: Default::default(),
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule(
        &mut self,
        name: NonTerm,
        rule: Vec<Token<Term, NonTerm>>,
        lookaheads: Option<BTreeSet<Term>>,
        operator: Option<Operator<Term>>,
        priority: Option<usize>,
    ) -> usize
    where
        NonTerm: Copy + Hash + Eq,
    {
        let index = self.rules.len();
        self.rules_map.entry(name).or_default().push(index);
        let rule = Rule {
            rule: ProductionRule { name, rule },
            lookaheads,
            operator,
            priority,
        };
        self.rules.push(rule);
        index
    }
    /// add empty non-terminal so that BuildError::RuleNotFound is not returned
    pub fn add_empty_rule(&mut self, name: NonTerm)
    where
        NonTerm: Copy + Hash + Eq,
    {
        self.rules_map.insert(name, Vec::new());
    }

    /// false if precedence already exists and different
    pub fn add_precedence(&mut self, term: Operator<Term>, precedence: usize) -> bool
    where
        Term: Hash + Eq,
    {
        if let Some(old) = self.precedence_map.insert(term, precedence) {
            if old != precedence {
                return false;
            }
        }
        true
    }

    /// error if different reduce type is assigned to same terminal symbol
    pub fn add_reduce_type(&mut self, op: Operator<Term>, reduce_type: ReduceType) -> bool
    where
        Term: Hash + Eq,
    {
        if let Some(old) = self.reduce_types.insert(op, reduce_type) {
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
        diags: &mut DiagnosticCollector<Term>,
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
        let main_state =
            self.build_recursive(augmented_rule_set, &mut states, &mut state_map, diags)?;
        if main_state != 0 {
            panic!("main state is not 0");
        }

        Ok(DFA { states })
    }

    /// build LALR(1) parser table from given grammar
    pub fn build_lalr(
        &mut self,
        augmented_name: NonTerm,
        diags: &mut DiagnosticCollector<Term>,
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
        let main_state =
            self.build_recursive_lalr(augmented_rule_set, &mut states, &mut state_map)?;
        if main_state != 0 {
            panic!("main state is not 0");
        }

        for state in &mut states {
            // check reduce/reduce conflicts resolving by its priority
            for reduce_rules in state.reduce_map.values_mut() {
                if reduce_rules.len() <= 1 {
                    continue;
                }

                // check if all rules have priority, and max priority
                if reduce_rules
                    .iter()
                    .any(|&rule| self.rules[rule].priority.is_none())
                {
                    continue;
                }
                let max_priority = reduce_rules
                    .iter()
                    .map(|&rule| self.rules[rule].priority)
                    .max()
                    .unwrap()
                    .unwrap();

                let deleted_rules: Vec<usize> = reduce_rules
                    .iter()
                    .filter(|&&rule| self.rules[rule].priority.unwrap() != max_priority)
                    .copied()
                    .collect();
                if deleted_rules.is_empty() {
                    continue;
                }
                let remained_rules = reduce_rules
                    .iter()
                    .filter(|&&rule| self.rules[rule].priority.unwrap() == max_priority)
                    .copied()
                    .collect();
                reduce_rules.retain(|rule| self.rules[*rule].priority.unwrap() == max_priority);

                diags.add_resolved(ResolveDiagnostic::Priority {
                    max_priority,
                    remaining: remained_rules,
                    deleted: deleted_rules,
                });
            }

            // check shift/reduce conflicts resolving
            for (&term, reduce_rules) in state.reduce_map.iter_mut() {
                if !state.shift_goto_map_term.contains_key(&term) {
                    continue;
                };
                let Some(&shift_prec) = self.precedence_map.get(&Operator::Term(term)) else {
                    // no precedence for this shift rule
                    continue;
                };

                let mut reduces = Vec::new();
                let mut remove_reduces = Vec::new();
                let mut remove_shift = true;

                for &reduce_rule in reduce_rules.iter() {
                    let Some(reduce_op) = self.rules[reduce_rule].operator else {
                        // no operator for this reduce rule
                        remove_shift = false;
                        reduces.push((reduce_rule, None, None));
                        continue;
                    };
                    let Some(&reduce_prec) = self.precedence_map.get(&reduce_op) else {
                        // no precedence for this reduce rule
                        remove_shift = false;
                        reduces.push((reduce_rule, None, None));
                        continue;
                    };

                    // compare precedence
                    match reduce_prec.cmp(&shift_prec) {
                        Ordering::Less => {
                            // reduce < shift => remove reduce
                            remove_reduces.push((reduce_rule, reduce_prec, None));
                            remove_shift = false;
                        }
                        Ordering::Greater => {
                            // reduce > shift => remove shift, but check other reduce rules
                            reduces.push((reduce_rule, Some(reduce_prec), None));
                        }
                        Ordering::Equal => {
                            // reduce == shift => check reduce type
                            let reduce_type = self.reduce_types.get(&reduce_op).copied();
                            match reduce_type {
                                Some(ReduceType::Left) => {
                                    // reduce first => remove shift
                                    reduces.push((reduce_rule, Some(reduce_prec), reduce_type));
                                }
                                Some(ReduceType::Right) => {
                                    // shift first => remove reduce
                                    remove_reduces.push((reduce_rule, reduce_prec, reduce_type));
                                    remove_shift = false;
                                }
                                None => {
                                    // no reduce type => conflict
                                    reduces.push((reduce_rule, Some(reduce_prec), None));
                                    remove_shift = false;
                                }
                            }
                        }
                    }
                }

                let shift_rules = state
                    .ruleset
                    .rules
                    .iter()
                    .filter_map(|(rule_ref, _)| {
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
                    state.ruleset.rules.retain(|rule_ref, _| {
                        self.rules[rule_ref.rule].rule.rule.get(rule_ref.shifted)
                            != Some(&Token::Term(term))
                    });
                }

                // remove reduce rules
                for &(remove_reduce, _, _) in remove_reduces.iter() {
                    reduce_rules.remove(&remove_reduce);
                }

                if !remove_shift && remove_reduces.is_empty() {
                    continue;
                }

                diags.add_resolved(ResolveDiagnostic::Precedence {
                    term,
                    shift_precedence: shift_prec,
                    shift_deleted: remove_shift,
                    shift_rules,
                    reduce_rules: reduces,
                    deleted_reduces: remove_reduces,
                });
            }
            state
                .reduce_map
                .retain(|_, reduce_rules| !reduce_rules.is_empty());
        }

        for state in &states {
            for (&term, reduce_rules) in state.reduce_map.iter() {
                if let Some(&next_shift_state) = state.shift_goto_map_term.get(&term) {
                    // shift/reduce conflict
                    let shift_rules = states[next_shift_state].unshifted_ruleset().collect();
                    let reduce_rules = reduce_rules.clone();
                    diags.add_shift_reduce_conflict(ShiftReduceConflictDiag {
                        term,
                        shift_rules,
                        reduce_rules,
                    });
                } else if reduce_rules.len() > 1 {
                    // no shift/reduce conflict
                    // check for reduce/reduce conflict
                    diags.update_reduce_reduce_conflict(reduce_rules, term);
                }
            }
        }

        Ok(DFA { states })
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
        states[state_id].ruleset = rules.clone();

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
            if reduce_rules
                .iter()
                .any(|&rule| self.rules[rule].priority.is_none())
            {
                continue;
            }
            let max_priority = reduce_rules
                .iter()
                .map(|&rule| self.rules[rule].priority)
                .max()
                .unwrap()
                .unwrap();

            let deleted_rules: Vec<usize> = reduce_rules
                .iter()
                .filter(|&&rule| self.rules[rule].priority.unwrap() != max_priority)
                .copied()
                .collect();
            if deleted_rules.is_empty() {
                continue;
            }
            let remained_rules = reduce_rules
                .iter()
                .filter(|&&rule| self.rules[rule].priority.unwrap() == max_priority)
                .copied()
                .collect();
            reduce_rules.retain(|rule| self.rules[*rule].priority.unwrap() == max_priority);

            diags.add_resolved(ResolveDiagnostic::Priority {
                max_priority,
                remaining: remained_rules,
                deleted: deleted_rules,
            });
        }

        // check shift/reduce conflicts
        for (&term, reduce_rules) in reduce_map.iter_mut() {
            if !next_rules_term.contains_key(&term) {
                continue;
            };
            let Some(&shift_prec) = self.precedence_map.get(&Operator::Term(term)) else {
                // no precedence for this shift rule
                continue;
            };

            let mut reduces = Vec::new();
            let mut remove_reduces = Vec::new();
            let mut remove_shift = true;

            for &reduce_rule in reduce_rules.iter() {
                let Some(reduce_op) = self.rules[reduce_rule].operator else {
                    // no operator for this reduce rule
                    reduces.push((reduce_rule, None, None));
                    remove_shift = false;
                    continue;
                };
                let Some(&reduce_prec) = self.precedence_map.get(&reduce_op) else {
                    // no precedence for this reduce rule
                    reduces.push((reduce_rule, None, None));
                    remove_shift = false;
                    continue;
                };

                // compare precedence
                match reduce_prec.cmp(&shift_prec) {
                    Ordering::Less => {
                        // reduce < shift => remove reduce
                        remove_reduces.push((reduce_rule, reduce_prec, None));
                        remove_shift = false;
                    }
                    Ordering::Greater => {
                        // reduce > shift => remove shift, but check other reduce rules
                        reduces.push((reduce_rule, Some(reduce_prec), None));
                    }
                    Ordering::Equal => {
                        // reduce == shift => check reduce type
                        let reduce_type = self.reduce_types.get(&reduce_op).copied();
                        match reduce_type {
                            Some(ReduceType::Left) => {
                                // reduce first => remove shift
                                reduces.push((reduce_rule, Some(reduce_prec), reduce_type));
                            }
                            Some(ReduceType::Right) => {
                                // shift first => remove reduce
                                remove_reduces.push((reduce_rule, reduce_prec, reduce_type));
                                remove_shift = false;
                            }
                            None => {
                                // no reduce type => conflict
                                reduces.push((reduce_rule, Some(reduce_prec), None));
                                remove_shift = false;
                            }
                        }
                    }
                }
            }

            let shift_rules = if remove_shift {
                // remove rules that start with `term`
                states[state_id].ruleset.rules.retain(|rule_ref, _| {
                    self.rules[rule_ref.rule].rule.rule.get(rule_ref.shifted)
                        != Some(&Token::Term(term))
                });

                next_rules_term
                    .remove(&term)
                    .unwrap()
                    .rules
                    .into_iter()
                    .map(|(rule_ref, _)| rule_ref)
                    .collect()
            } else {
                next_rules_term
                    .get(&term)
                    .unwrap()
                    .rules
                    .iter()
                    .map(|(rule_ref, _)| *rule_ref)
                    .collect()
            };

            // remove reduce rules
            for &(remove_reduce, _, _) in remove_reduces.iter() {
                reduce_rules.remove(&remove_reduce);
            }

            if !remove_shift && remove_reduces.is_empty() {
                continue;
            }

            diags.add_resolved(ResolveDiagnostic::Precedence {
                term,
                shift_precedence: shift_prec,
                shift_deleted: remove_shift,
                shift_rules,
                reduce_rules: reduces,
                deleted_reduces: remove_reduces,
            });
        }
        reduce_map.retain(|_, reduce_rules| !reduce_rules.is_empty());

        for (&term, reduce_rules) in reduce_map.iter() {
            if next_rules_term.contains_key(&term) {
                // shift/reduce conflict
                let shift_rules = next_rules_term
                    .get(&term)
                    .unwrap()
                    .rules
                    .iter()
                    .map(|(rule_ref, _)| *rule_ref)
                    .collect::<Vec<_>>();
                let reduce_rules = reduce_rules.clone();
                diags.add_shift_reduce_conflict(ShiftReduceConflictDiag {
                    term,
                    shift_rules,
                    reduce_rules: reduce_rules.clone(),
                });
            } else if reduce_rules.len() > 1 {
                // no shift/reduce conflict
                // check for reduce/reduce conflict
                diags.update_reduce_reduce_conflict(reduce_rules, term);
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
            let next_state_id = self.build_recursive(next_rule_set, states, state_map, diags)?;
            states[next_state_id].token = Some(Token::Term(next_term));

            states[state_id]
                .shift_goto_map_term
                .insert(next_term, next_state_id);
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            let next_state_id = self.build_recursive(next_rule_set, states, state_map, diags)?;
            states[next_state_id].token = Some(Token::NonTerm(next_nonterm));

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
            states.push(State::new());
            for shifted_rule in rules.rules.keys().copied() {
                states[new_state_id]
                    .ruleset
                    .rules
                    .insert(shifted_rule, BTreeSet::new());
            }
            new_state_id
        });

        let mut lookaheads_empty = true;
        let mut next_rules_term = BTreeMap::new();
        let mut next_rules_nonterm = BTreeMap::new();
        let mut reduce_map: BTreeMap<Term, BTreeSet<usize>> = BTreeMap::new();
        for ((mut rule_ref, mut lookaheads_src), (_, lookaheads_dst)) in rules
            .rules
            .into_iter()
            .zip(states[state_id].ruleset.rules.iter_mut())
        {
            let rule = &self.rules[rule_ref.rule].rule;
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

        // no conflict resolving here for LALR
        // it will be handled after all `build_recursive` calls

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
            let next_state_id = self.build_recursive_lalr(next_rule_set, states, state_map)?;
            states[next_state_id].token = Some(Token::Term(next_term));

            states[state_id]
                .shift_goto_map_term
                .insert(next_term, next_state_id);
        }

        for (next_nonterm, next_rule_set) in next_rules_nonterm.into_iter() {
            let next_state_id = self.build_recursive_lalr(next_rule_set, states, state_map)?;
            states[next_state_id].token = Some(Token::NonTerm(next_nonterm));

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
