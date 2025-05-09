use crate::rule::LookaheadRuleRefSet;
use crate::ShiftedRuleRef;
use crate::Token;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;

/// state in DFA building
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: BTreeMap<Term, usize>,
    pub shift_goto_map_nonterm: BTreeMap<NonTerm, usize>,
    pub reduce_map: BTreeMap<Term, BTreeSet<usize>>,
    pub ruleset: LookaheadRuleRefSet<Term>,
    /// The token that shifted into this state.
    pub token: Option<Token<Term, NonTerm>>,
}
impl<Term, NonTerm> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            shift_goto_map_term: Default::default(),
            shift_goto_map_nonterm: Default::default(),
            reduce_map: Default::default(),
            ruleset: LookaheadRuleRefSet::new(),
            token: None,
        }
    }

    /// shift -= 1 for all rules in the ruleset
    pub fn unshifted_ruleset(&self) -> impl Iterator<Item = ShiftedRuleRef> + '_ {
        self.ruleset
            .rules
            .iter()
            .filter(|rule| rule.0.shifted > 0)
            .map(|(rule, _)| {
                let mut rule = *rule;
                rule.shifted -= 1;
                rule
            })
    }

    pub fn into_lr_sparse_state<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> crate::lr::SparseState<NewNonTerm>
    where
        NewNonTerm: Hash + Eq,
    {
        crate::lr::SparseState {
            shift_goto_map_class: self
                .shift_goto_map_term
                .into_iter()
                .map(|(term, state)| (term_map(term), state))
                .collect(),
            shift_goto_map_nonterm: self
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state)| (nonterm_map(nonterm), state))
                .collect(),
            reduce_map: self
                .reduce_map
                .into_iter()
                .map(|(term, rule)| (term_map(term), rule.into_iter().next().unwrap()))
                .collect(),
            ruleset: self.ruleset.rules.into_keys().collect(),
            shifted_token: self.token.map(|token| token.map(&term_map, &nonterm_map)),
        }
    }
    pub fn into_lr_dense_state<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        terms_len: usize,
    ) -> crate::lr::DenseState<NewNonTerm>
    where
        NewNonTerm: Hash + Eq,
    {
        let mut shift_goto_map_class = vec![None; terms_len];
        let mut reduce_map = vec![None; terms_len];
        for (term, state) in self.shift_goto_map_term {
            shift_goto_map_class[term_map(term)] = Some(state);
        }
        for (term, rule) in self.reduce_map {
            reduce_map[term_map(term)] = Some(rule.into_iter().next().unwrap());
        }
        crate::lr::DenseState {
            shift_goto_map_class,
            shift_goto_map_nonterm: self
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state)| (nonterm_map(nonterm), state))
                .collect(),
            reduce_map,
            ruleset: self.ruleset.rules.into_keys().collect(),
            shifted_token: self.token.map(|token| token.map(&term_map, &nonterm_map)),
        }
    }

    pub fn into_glr_sparse_state<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> crate::glr::SparseState<NewNonTerm>
    where
        NewNonTerm: Hash + Eq,
    {
        crate::glr::SparseState {
            shift_goto_map_class: self
                .shift_goto_map_term
                .into_iter()
                .map(|(term, state)| (term_map(term), state))
                .collect(),
            shift_goto_map_nonterm: self
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state)| (nonterm_map(nonterm), state))
                .collect(),
            reduce_map: self
                .reduce_map
                .into_iter()
                .map(|(term, rule)| (term_map(term), rule.into_iter().collect()))
                .collect(),
            ruleset: self.ruleset.rules.into_keys().collect(),
            shifted_token: self.token.map(|token| token.map(&term_map, &nonterm_map)),
        }
    }
    pub fn into_glr_dense_state<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        terms_len: usize,
    ) -> crate::glr::DenseState<NewNonTerm>
    where
        NewNonTerm: Hash + Eq,
    {
        let mut shift_goto_map_class = vec![None; terms_len];
        let mut reduce_map = vec![None; terms_len];
        for (term, state) in self.shift_goto_map_term {
            shift_goto_map_class[term_map(term)] = Some(state);
        }
        for (term, rule) in self.reduce_map {
            reduce_map[term_map(term)] = Some(rule.into_iter().collect());
        }
        crate::glr::DenseState {
            shift_goto_map_class,
            shift_goto_map_nonterm: self
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state)| (nonterm_map(nonterm), state))
                .collect(),
            reduce_map,
            ruleset: self.ruleset.rules.into_keys().collect(),
            shifted_token: self.token.map(|token| token.map(&term_map, &nonterm_map)),
        }
    }
}

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
