use thiserror::Error;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Display;
use std::vec::Vec;

use crate::parser::Parser;
use crate::rule::*;
use crate::state::State;
use crate::term::TermTraitBound;
use crate::token::Token;

#[derive(Error, Debug)]
pub enum BuildError<'a, Term: TermTraitBound, NonTerm: TermTraitBound> {
    #[error("Rule not found: {0}")]
    RuleNotFound(NonTerm),

    /// reduce/reduce conflict
    #[error("Conflict in grammar: {0} and {1}")]
    ReduceReduceConflict(ProductionRule<Term, NonTerm>, ProductionRule<Term, NonTerm>),

    /// reduce/shift conflict
    #[error("Conflict in grammar:\n{0} and\n{1}")]
    ReduceShiftConflict(
        ProductionRule<Term, NonTerm>,
        LookaheadRuleRefSet<'a, Term, NonTerm>,
    ),

    #[error("Duplicated rule: {0}")]
    DuplicatedRule(LookaheadRuleRef<'a, Term, NonTerm>),

    #[error("Augmented rule cannot be in production rule; in definition of {0}")]
    AugmentedInRule(NonTerm),
}

/// A set of production rules and main entry point
#[derive(Debug, Clone)]
pub struct Grammar<Term: TermTraitBound, NonTerm: TermTraitBound> {
    /// set of production rules
    pub rules: BTreeMap<NonTerm, Vec<ProductionRule<Term, NonTerm>>>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    firsts: BTreeMap<NonTerm, (BTreeSet<Term>, bool)>,

    /// unique counter from 0 to assign uid for each production rules
    uid: usize,
}

impl<Term: TermTraitBound, NonTerm: TermTraitBound> Grammar<Term, NonTerm> {
    pub fn new() -> Self {
        Grammar {
            rules: BTreeMap::new(),
            firsts: BTreeMap::new(),
            uid: 0,
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule<'a>(
        &'a mut self,
        name: NonTerm,
        rule: Vec<Token<Term, NonTerm>>,
    ) -> &'a ProductionRule<Term, NonTerm> {
        // assign uid for each production rules in order of insertion
        let rule = ProductionRule {
            name: name.clone(),
            rule,
            uid: self.uid,
        };
        self.uid += 1;

        let rule_vec = self.rules.entry(name).or_insert(Vec::new());
        rule_vec.push(rule);

        rule_vec.last().unwrap()
    }

    /// build LR(1) parser table from given grammar
    pub fn build_main(
        &mut self,
        main_nonterminal: NonTerm,
        end_terminal: Term,
        augmented_name: NonTerm,
    ) -> Result<Parser<Term, NonTerm>, BuildError<Term, NonTerm>> {
        self.calculate_first();

        // add main augmented rule
        let augmented_rule_set = {
            self.add_rule(
                augmented_name.clone(),
                vec![Token::NonTerm(main_nonterminal), Token::Term(end_terminal)],
            );

            let augmented_rule = self.search_rules(&augmented_name).unwrap().get(0).unwrap();
            let augmented_rule = ShiftedRuleRef {
                rule: augmented_rule,
                shifted: 0,
            };
            let augmented_rule = LookaheadRuleRef {
                rule: augmented_rule,
                lookaheads: BTreeSet::new(),
            };
            LookaheadRuleRefSet {
                rules: BTreeSet::from([augmented_rule]),
            }
        };

        let mut states = Vec::new();
        let mut state_map = BTreeMap::new();
        let main_state = self.build(augmented_rule_set, &mut states, &mut state_map)?;
        Ok(Parser { states, main_state })
    }

    /// search for every production rules with name 'name'
    fn search_rules<'a>(
        &'a self,
        name: &NonTerm,
    ) -> Option<&'a Vec<ProductionRule<Term, NonTerm>>> {
        self.rules.get(name)
    }
    /// calculate first terminals for each nonterminals
    fn calculate_first(&mut self) {
        loop {
            let mut changed = false;
            for (name, rules) in self.rules.iter() {
                let (mut firsts, mut canbe_empty) = self
                    .firsts
                    .entry(name.clone())
                    .or_insert_with(|| {
                        changed = true;
                        (BTreeSet::new(), false)
                    })
                    .clone();

                let mut this_nonterm_changed = false;
                for rule in rules.iter() {
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
                }
                if this_nonterm_changed {
                    changed = true;
                    self.firsts.insert(name.clone(), (firsts, canbe_empty));
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
                    let (firsts, canbe_empty) = self.firsts.get(nonterm).unwrap();
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
    fn expand<'a>(
        &'a self,
        rules: &mut LookaheadRuleRefSet<'a, Term, NonTerm>,
    ) -> Result<(), BuildError<'a, Term, NonTerm>> {
        loop {
            let mut new_rules = Vec::new();
            for rule in rules.rules.iter() {
                if let Some(Token::NonTerm(ref nonterm_name)) = rule.rule.first() {
                    if let Some(searched_rules) = self.search_rules(nonterm_name) {
                        // calculate lookaheads
                        let lookaheads = self.lookahead(&rule.rule.rest(), &rule.lookaheads)?;

                        // init new LookaheadRule with searched rules
                        for searched_rule in searched_rules.iter() {
                            let rule_with_ahead = LookaheadRuleRef {
                                rule: ShiftedRuleRef {
                                    rule: searched_rule,
                                    shifted: 0,
                                },
                                lookaheads: lookaheads.clone(),
                            };
                            new_rules.push(rule_with_ahead);
                        }
                    } else {
                        // rule not found
                        return Err(BuildError::RuleNotFound(nonterm_name.clone()));
                    }
                }
            }

            let old_len = rules.rules.len();
            rules.rules.extend(new_rules.into_iter());
            if rules.rules.len() == old_len {
                // nothing added newly; stop loop
                return Ok(());
            }
        }
    }

    /// build new state with given production rules
    fn build<'a>(
        &'a self,
        mut rules: LookaheadRuleRefSet<'a, Term, NonTerm>,
        states: &mut Vec<State<Term, NonTerm>>,
        state_map: &mut BTreeMap<LookaheadRuleRefSet<'a, Term, NonTerm>, usize>,
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

        let mut next_rules = BTreeMap::new();
        let mut empty_rules = Vec::new();
        for mut rule in rules.rules.into_iter() {
            match rule.rule.first().cloned() {
                Some(token) => {
                    rule.rule.shifted += 1;
                    let next_rule_set = next_rules
                        .entry(token)
                        .or_insert(LookaheadRuleRefSet::new());
                    let insert_result = next_rule_set.rules.insert(rule.clone());
                    if insert_result == false {
                        return Err(BuildError::DuplicatedRule(rule));
                    }
                }
                None => {
                    empty_rules.push(rule);
                }
            }
        }

        // process rules that no more tokens left to shift
        // if next token is one of lookahead, add reduce action
        // if there are multiple recude rules for same lookahead, it is a reduce/reduce conflict
        for empty_rule in empty_rules.into_iter() {
            let lookaheads = empty_rule.lookaheads;
            let state = &mut states[state_id];
            for lookahead in lookaheads.into_iter() {
                if let Some(old) = state.reduce_map.get_mut(&lookahead) {
                    // conflict
                    return Err(BuildError::ReduceReduceConflict(
                        old.clone(),
                        empty_rule.rule.rule.clone(),
                    ));
                } else {
                    state
                        .reduce_map
                        .insert(lookahead, empty_rule.rule.rule.clone());
                }
            }
        }

        // process next rules with token
        // add shift and goto action
        for (next_token, next_rule_set) in next_rules.into_iter() {
            let next_state_id = self.build(next_rule_set.clone(), states, state_map)?;

            match next_token {
                Token::Term(term) => {
                    states[state_id]
                        .shift_goto_map_term
                        .insert(term, next_state_id);
                }
                Token::NonTerm(nonterm) => {
                    states[state_id]
                        .shift_goto_map_nonterm
                        .insert(nonterm, next_state_id);
                }
            }

            // TODO
            // check if next_token is also in reduce_map, then it is a reduce/shift conflict.
        }

        Ok(state_id)
    }
}

impl<Term: TermTraitBound + Display, NonTerm: TermTraitBound + Display> Display
    for Grammar<Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut rules = Vec::new();
        for (_name, self_rules) in self.rules.iter() {
            for rule in self_rules.iter() {
                rules.push(rule.clone());
            }
        }
        rules.sort_by_key(|rule| rule.uid);
        for rule in rules.iter() {
            write!(f, "{}: ", rule.uid)?;
            writeln!(f, "{}", rule)?;
        }

        Ok(())
    }
}
