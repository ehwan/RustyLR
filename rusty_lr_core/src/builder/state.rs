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
    pub ruleset: BTreeSet<ShiftedRuleRef>,
    /// The token that shifted into this state.
    pub token: Option<Token<Term, NonTerm>>,
}
use crate::glr::state::ToUsizeList;
use smallvec::SmallVec;
type SmallVecU8 = SmallVec<[u8; 16]>;
type SmallVecU16 = SmallVec<[u16; 8]>;
type SmallVecU32 = SmallVec<[u32; 4]>;
type SmallVecU = SmallVec<[usize; 2]>;
impl ToUsizeList for SmallVecU {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().copied()
    }
}
impl ToUsizeList for SmallVecU32 {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl ToUsizeList for SmallVecU16 {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl ToUsizeList for SmallVecU8 {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl<Term, NonTerm> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            shift_goto_map_term: Default::default(),
            shift_goto_map_nonterm: Default::default(),
            reduce_map: Default::default(),
            ruleset: Default::default(),
            token: None,
        }
    }

    /// shift -= 1 for all rules in the ruleset
    pub fn unshifted_ruleset(&self) -> impl Iterator<Item = ShiftedRuleRef> + '_ {
        self.ruleset
            .iter()
            .filter(|rule| rule.shifted > 0)
            .map(|rule| {
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
            ruleset: self.ruleset.into_iter().collect(),
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
            ruleset: self.ruleset.into_iter().collect(),
        }
    }

    pub fn into_glr_sparse_state<RuleVec, NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        rule_vec_map: impl Fn(BTreeSet<usize>) -> RuleVec,
    ) -> crate::glr::SparseState<NewNonTerm, RuleVec>
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
                .map(|(term, rule)| (term_map(term), rule_vec_map(rule)))
                .collect(),
            ruleset: self.ruleset.into_iter().collect(),
        }
    }

    pub fn into_glr_sparse_state_u8<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> crate::glr::SparseState<NewNonTerm, SmallVecU8>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_sparse_state(term_map, nonterm_map, |reduce_map| {
            SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
    pub fn into_glr_sparse_state_u16<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> crate::glr::SparseState<NewNonTerm, SmallVecU16>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_sparse_state(term_map, nonterm_map, |reduce_map| {
            SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
    pub fn into_glr_sparse_state_u32<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> crate::glr::SparseState<NewNonTerm, SmallVecU32>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_sparse_state(term_map, nonterm_map, |reduce_map| {
            SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
    pub fn into_glr_sparse_state_usize<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> crate::glr::SparseState<NewNonTerm, SmallVecU>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_sparse_state(term_map, nonterm_map, SmallVecU::from_iter)
    }

    pub fn into_glr_dense_state<RuleVec: Clone, NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        terms_len: usize,
        rule_vec_map: impl Fn(BTreeSet<usize>) -> RuleVec,
    ) -> crate::glr::DenseState<NewNonTerm, RuleVec>
    where
        NewNonTerm: Hash + Eq,
    {
        let mut shift_goto_map_class = vec![None; terms_len];
        let mut reduce_map = vec![None; terms_len];
        for (term, state) in self.shift_goto_map_term {
            shift_goto_map_class[term_map(term)] = Some(state);
        }
        for (term, rule) in self.reduce_map {
            reduce_map[term_map(term)] = Some(rule_vec_map(rule));
        }
        crate::glr::DenseState {
            shift_goto_map_class,
            shift_goto_map_nonterm: self
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state)| (nonterm_map(nonterm), state))
                .collect(),
            reduce_map,
            ruleset: self.ruleset.into_iter().collect(),
        }
    }
    pub fn into_glr_dense_state_u8<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        terms_len: usize,
    ) -> crate::glr::DenseState<NewNonTerm, SmallVecU8>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_dense_state(term_map, nonterm_map, terms_len, |reduce_map| {
            SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
    pub fn into_glr_dense_state_u16<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        terms_len: usize,
    ) -> crate::glr::DenseState<NewNonTerm, SmallVecU16>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_dense_state(term_map, nonterm_map, terms_len, |reduce_map| {
            SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
    pub fn into_glr_dense_state_u32<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        terms_len: usize,
    ) -> crate::glr::DenseState<NewNonTerm, SmallVecU32>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_dense_state(term_map, nonterm_map, terms_len, |reduce_map| {
            SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
    pub fn into_glr_dense_state_usize<NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> usize,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
        terms_len: usize,
    ) -> crate::glr::DenseState<NewNonTerm, SmallVecU>
    where
        NewNonTerm: Hash + Eq,
    {
        self.into_glr_dense_state(term_map, nonterm_map, terms_len, SmallVecU::from_iter)
    }
}

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
