use std::hash::Hash;

use crate::hash::HashMap;
use crate::TerminalSymbol;

/// A trait representing a parser state.
pub trait State<NonTerm> {
    /// Get the next state for a given terminal symbol.
    fn shift_goto_class(&self, class: TerminalSymbol) -> Option<usize>;

    /// Get the next state for a given non-terminal symbol.
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq;

    /// Get the reduce rule index for a given terminal symbol.
    fn reduce(&self, class: TerminalSymbol) -> Option<impl Iterator<Item = usize> + Clone + '_>;

    /// Check if this state is an accept state.
    fn is_accept(&self) -> bool;

    /// Get the set of expected terminal classes for shift in this state
    fn expected_shift_term(&self) -> impl Iterator<Item = usize> + '_;

    /// Get the set of expected non-terminal symbols for shift in this state
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_;

    /// Get the set of expected terminal classes for reduce in this state
    fn expected_reduce_term(&self) -> impl Iterator<Item = usize> + '_;

    /// Get the set of production rule for reduce in this state
    fn expected_reduce_rule(&self) -> impl Iterator<Item = usize> + '_;

    /// Get the set of rules that this state is trying to parse
    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef];
}

/// `State` implementation for a sparse state representation using HashMap
#[derive(Debug, Clone)]
pub struct SparseState<NonTerm, RuleContainer> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: HashMap<usize, usize>,
    pub(crate) error_shift: Option<usize>,

    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,

    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: HashMap<usize, RuleContainer>,
    pub(crate) error_reduce: Option<RuleContainer>,

    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}

impl<NonTerm: Copy, RuleIndex: crate::stackvec::ToUsizeList> State<NonTerm>
    for SparseState<NonTerm, RuleIndex>
{
    fn shift_goto_class(&self, class: TerminalSymbol) -> Option<usize> {
        match class {
            TerminalSymbol::Term(class) => self.shift_goto_map_class.get(&class).copied(),
            TerminalSymbol::Error => self.error_shift,
        }
    }
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.shift_goto_map_nonterm.get(nonterm).copied()
    }
    fn reduce(&self, class: TerminalSymbol) -> Option<impl Iterator<Item = usize> + Clone + '_> {
        match class {
            TerminalSymbol::Term(class) => self
                .reduce_map
                .get(&class)
                .map(crate::stackvec::ToUsizeList::to_usize_list),
            TerminalSymbol::Error => self
                .error_reduce
                .as_ref()
                .map(crate::stackvec::ToUsizeList::to_usize_list),
        }
    }
    fn is_accept(&self) -> bool {
        self.reduce_map.is_empty()
            && self.shift_goto_map_class.is_empty()
            && self.shift_goto_map_nonterm.is_empty()
    }
    fn expected_shift_term(&self) -> impl Iterator<Item = usize> + '_ {
        self.shift_goto_map_class.keys().copied()
    }
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_ {
        self.shift_goto_map_nonterm.keys().copied()
    }
    fn expected_reduce_term(&self) -> impl Iterator<Item = usize> + '_ {
        self.reduce_map.keys().copied()
    }
    fn expected_reduce_rule(&self) -> impl Iterator<Item = usize> + '_ {
        self.reduce_map.values().flat_map(|r| r.to_usize_list())
    }
    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef] {
        &self.ruleset
    }
}

/// `State` implementation for a dense state representation using Vec
#[derive(Debug, Clone)]
pub struct DenseState<NonTerm, RuleContainer> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: Vec<Option<usize>>,
    pub(crate) error_shift: Option<usize>,

    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: Vec<Option<RuleContainer>>,
    pub(crate) error_reduce: Option<RuleContainer>,

    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}
impl<NonTerm: Copy, RuleContainer: crate::stackvec::ToUsizeList> State<NonTerm>
    for DenseState<NonTerm, RuleContainer>
{
    fn shift_goto_class(&self, class: TerminalSymbol) -> Option<usize> {
        match class {
            TerminalSymbol::Term(class) => self.shift_goto_map_class[class],
            TerminalSymbol::Error => self.error_shift,
        }
    }
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.shift_goto_map_nonterm.get(nonterm).copied()
    }
    fn reduce(&self, class: TerminalSymbol) -> Option<impl Iterator<Item = usize> + Clone + '_> {
        match class {
            TerminalSymbol::Term(class) => self
                .reduce_map
                .get(class)
                .unwrap()
                .as_ref()
                .map(crate::stackvec::ToUsizeList::to_usize_list),
            TerminalSymbol::Error => self
                .error_reduce
                .as_ref()
                .map(crate::stackvec::ToUsizeList::to_usize_list),
        }
    }
    fn is_accept(&self) -> bool {
        self.reduce_map.is_empty()
            && self.shift_goto_map_class.is_empty()
            && self.shift_goto_map_nonterm.is_empty()
    }
    fn expected_shift_term(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.shift_goto_map_class.len()).filter(|&i| self.shift_goto_map_class[i].is_some())
    }
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_ {
        self.shift_goto_map_nonterm.keys().copied()
    }
    fn expected_reduce_term(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.shift_goto_map_class.len()).filter(|&i| self.reduce_map[i].is_some())
    }
    fn expected_reduce_rule(&self) -> impl Iterator<Item = usize> + '_ {
        self.reduce_map
            .iter()
            .filter_map(|r| r.as_ref())
            .flat_map(|r| r.to_usize_list())
    }

    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef] {
        &self.ruleset
    }
}

fn builder_state_into_sparse<NonTerm, RuleContainer>(
    mut builder_state: crate::builder::State<TerminalSymbol, NonTerm>,
    rule_vec_map: impl Fn(std::collections::BTreeSet<usize>) -> RuleContainer,
) -> SparseState<NonTerm, RuleContainer>
where
    NonTerm: Hash + Eq,
{
    let error_shift = builder_state
        .shift_goto_map_term
        .remove(&TerminalSymbol::Error);
    let error_reduce = builder_state
        .reduce_map
        .remove(&TerminalSymbol::Error)
        .map(&rule_vec_map);
    SparseState {
        shift_goto_map_class: builder_state
            .shift_goto_map_term
            .into_iter()
            .map(|(term, state)| (term.to_term().unwrap(), state))
            .collect(),
        error_shift,
        shift_goto_map_nonterm: builder_state.shift_goto_map_nonterm.into_iter().collect(),
        reduce_map: builder_state
            .reduce_map
            .into_iter()
            .map(|(term, rule)| (term.to_term().unwrap(), rule_vec_map(rule)))
            .collect(),
        error_reduce,
        ruleset: builder_state.ruleset.into_iter().collect(),
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU8>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU16>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU32>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecUsize>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, crate::stackvec::SmallVecUsize::from_iter)
    }
}

fn builder_state_into_dense<NonTerm, RuleContainer: Clone>(
    mut builder_state: crate::builder::State<TerminalSymbol, NonTerm>,
    rule_vec_map: impl Fn(std::collections::BTreeSet<usize>) -> RuleContainer,
) -> DenseState<NonTerm, RuleContainer>
where
    NonTerm: Hash + Eq,
{
    let error_shift = builder_state
        .shift_goto_map_term
        .remove(&TerminalSymbol::Error);
    let error_reduce = builder_state
        .reduce_map
        .remove(&TerminalSymbol::Error)
        .map(&rule_vec_map);

    let shift_term_len = builder_state
        .shift_goto_map_term
        .keys()
        .next_back()
        .copied()
        .map(|x| x.to_term().unwrap() + 1)
        .unwrap_or(0);
    let reduce_term_len = builder_state
        .reduce_map
        .keys()
        .next_back()
        .copied()
        .map(|x| x.to_term().unwrap() + 1)
        .unwrap_or(0);
    let mut shift_goto_map_class = vec![None; shift_term_len];
    let mut reduce_map = vec![None; reduce_term_len];
    for (term, state) in builder_state.shift_goto_map_term {
        shift_goto_map_class[term.to_term().unwrap()] = Some(state);
    }
    for (term, rule) in builder_state.reduce_map {
        reduce_map[term.to_term().unwrap()] = Some(rule_vec_map(rule));
    }
    DenseState {
        shift_goto_map_class,
        error_shift,
        shift_goto_map_nonterm: builder_state.shift_goto_map_nonterm.into_iter().collect(),
        reduce_map,
        error_reduce,
        ruleset: builder_state.ruleset.into_iter().collect(),
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU8>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU16>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU32>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
}
impl<NonTerm> From<crate::builder::State<TerminalSymbol, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecUsize>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<TerminalSymbol, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, crate::stackvec::SmallVecUsize::from_iter)
    }
}
