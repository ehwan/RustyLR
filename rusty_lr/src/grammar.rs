use thiserror::Error;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::vec::Vec;

use crate::action::Action;
use crate::parser::Parser;
use crate::rule::*;
use crate::state::State;
use crate::term::TermTraitBound;
use crate::token::Token;

#[derive(Error, Debug)]
pub enum BuildError<Term: TermTraitBound, NonTerm: TermTraitBound> {
    #[error("Rule not found: {0}")]
    RuleNotFound(LookaheadRule<Term, NonTerm>),

    /// reduce/reduce conflict
    #[error("Conflict in grammar: {0} and {1}")]
    ReduceReduceConflict(
        NamedShiftedRule<Term, NonTerm>,
        NamedShiftedRule<Term, NonTerm>,
    ),

    /// reduce/shift conflict
    #[error("Conflict in grammar:\n{0} and\n{1}")]
    ReduceShiftConflict(
        NamedShiftedRule<Term, NonTerm>,
        LookaheadRuleSet<Term, NonTerm>,
    ),

    #[error("Entry rule not set; use .set_main( entry_name: &str )")]
    EntryNotSet,

    #[error("Duplicated rule: {0}")]
    DuplicatedRule(LookaheadRule<Term, NonTerm>),

    #[error("Augmented rule cannot be in production rule; in definition of {0}")]
    AugmentedInRule(NonTerm),
}

/// A set of production rules and main entry point
#[derive(Debug, Clone)]
pub struct Grammar<Term: TermTraitBound, NonTerm: TermTraitBound> {
    /// set of production rules
    pub rules: BTreeMap<NonTerm, Vec<Vec<Token<Term, NonTerm>>>>,

    /// name of main entry nonterminal rule
    pub main: Option<NonTerm>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    pub firsts: BTreeMap<NonTerm, (BTreeSet<Token<Term, NonTerm>>, bool)>,
}

impl<Term: TermTraitBound, NonTerm: TermTraitBound> Grammar<Term, NonTerm> {
    pub fn new() -> Self {
        Grammar {
            rules: BTreeMap::new(),
            main: None,
            firsts: BTreeMap::new(),
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule(&mut self, nonterm: NonTerm, rule: Vec<Token<Term, NonTerm>>) {
        self.rules.entry(nonterm).or_insert(Vec::new()).push(rule);
    }
    /// set main entry nonterminal rule
    pub fn set_main(&mut self, nonterm: NonTerm) {
        self.main = Some(nonterm);
    }

    /// build LR(1) parser table from given grammar
    pub fn build_main(
        &mut self,
        augmented_name: NonTerm,
    ) -> Result<Parser<Term, NonTerm>, BuildError<Term, NonTerm>> {
        for (_name, rules) in self.rules.iter_mut() {
            rules.sort();
        }

        self.calculate_first()?;

        if self.main.is_none() {
            return Err(BuildError::EntryNotSet);
        }

        // add main augmented rule
        let augmented_rule_set = {
            let main_rule = self.main.clone().unwrap();
            let augmented_rule = NamedShiftedRule {
                name: augmented_name,
                rule: vec![
                    Token::<Term, NonTerm>::NonTerm(main_rule),
                    Token::<Term, NonTerm>::End,
                ],
                shifted: 0,
            };
            let augmented_rule = LookaheadRule {
                rule: augmented_rule,
                lookaheads: BTreeSet::new(),
            };
            LookaheadRuleSet {
                rules: BTreeSet::from([augmented_rule]),
            }
        };

        let mut states = Vec::new();
        let mut state_map = BTreeMap::new();
        let main_state = self.build(augmented_rule_set, &mut states, &mut state_map)?;
        Ok(Parser { states, main_state })
    }

    /// search for every production rules with name 'name'
    fn search_rules<'a>(&'a self, nonterm: &NonTerm) -> Option<&'a Vec<Vec<Token<Term, NonTerm>>>> {
        self.rules.get(nonterm)
    }
    /// calculate first terminals for each nonterminals
    fn calculate_first(&mut self) -> Result<(), BuildError<Term, NonTerm>> {
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
                    'tokenfor: for token in rule.iter() {
                        match token {
                            Token::Term(_) | Token::End => {
                                let insert_result = firsts.insert(token.clone());
                                if insert_result {
                                    this_nonterm_changed = true;
                                }
                                this_rule_canbe_empty = false;
                                break 'tokenfor;
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
                                        break 'tokenfor;
                                    }
                                } else {
                                    this_rule_canbe_empty = false;
                                    break 'tokenfor;
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
        Ok(())
    }

    /// calculate lookahead tokens for given follow tokens and lookahead tokens
    /// this is equivalent to FIRST( follow_tokens lookahead )
    fn lookahead(
        &self,
        follow_tokens: &[Token<Term, NonTerm>],
        lookahead: &BTreeSet<Token<Term, NonTerm>>,
    ) -> Result<BTreeSet<Token<Term, NonTerm>>, BuildError<Term, NonTerm>> {
        let mut ret = BTreeSet::new();
        for token in follow_tokens.iter() {
            match token {
                Token::Term(_) | Token::End => {
                    ret.insert(token.clone());
                    return Ok(ret);
                }
                Token::NonTerm(nonterm) => {
                    let (firsts, canbe_empty) = self.firsts.get(nonterm).unwrap();
                    ret.extend(firsts.iter().cloned());
                    if !canbe_empty {
                        return Ok(ret);
                    }
                }
            }
        }
        ret.extend(lookahead.iter().cloned());
        Ok(ret)
    }

    /// for given set of production rules,
    /// if the first token of each rule is nonterminal, attach its production rules
    fn expand(
        &self,
        rules: &mut LookaheadRuleSet<Term, NonTerm>,
    ) -> Result<(), BuildError<Term, NonTerm>> {
        loop {
            let mut new_rules = Vec::new();
            for rule in rules.rules.iter() {
                if let Some(Token::NonTerm(ref nonterm_rule)) = rule.rule.first() {
                    if let Some(searched_rules) = self.search_rules(nonterm_rule) {
                        // calculate lookaheads
                        let lookaheads = self.lookahead(&rule.rule.rest(), &rule.lookaheads)?;

                        for searched_rule in searched_rules.iter() {
                            let rule_with_ahead = LookaheadRule {
                                rule: NamedShiftedRule {
                                    name: nonterm_rule.clone(),
                                    rule: searched_rule.clone(),
                                    shifted: 0,
                                },
                                lookaheads: lookaheads.clone(),
                            };
                            new_rules.push(rule_with_ahead);
                        }
                    } else {
                        // rule not found
                        return Err(BuildError::RuleNotFound(rule.clone()));
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
    fn build(
        &self,
        mut rules: LookaheadRuleSet<Term, NonTerm>,
        states: &mut Vec<State<Term, NonTerm>>,
        state_map: &mut BTreeMap<LookaheadRuleSet<Term, NonTerm>, usize>,
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
                    let next_rule_set = next_rules.entry(token).or_insert(LookaheadRuleSet::new());
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

        // process empty rules
        // if next token is one of lookahead, add reduce action
        // if there are multiple recude rules for same lookahead, it is a reduce/reduce conflict
        for mut empty_rule in empty_rules.into_iter() {
            empty_rule.rule.shifted = 0;
            let action = Action::Reduce(empty_rule.rule);
            let lookaheads = empty_rule.lookaheads;
            let state = &mut states[state_id];
            for lookahead in lookaheads.into_iter() {
                if let Some(old) = state.action_map.get_mut(&lookahead) {
                    // conflict
                    return Err(BuildError::ReduceReduceConflict(
                        old.clone().rule().unwrap(),
                        action.clone().rule().unwrap(),
                    ));
                } else {
                    state.action_map.insert(lookahead, action.clone());
                }
            }
        }

        // process next rules with token
        for (next_token, next_rule) in next_rules.into_iter() {
            let next_state_id = self.build(next_rule.clone(), states, state_map)?;
            let action = Action::<Term, NonTerm>::Goto(next_state_id);
            if let Some(old) = states[state_id].action_map.insert(next_token, action) {
                // reduce/shift conflict
                return Err(BuildError::ReduceShiftConflict(
                    old.clone().rule().unwrap(),
                    next_rule,
                ));
            }
        }

        Ok(state_id)
    }
}
