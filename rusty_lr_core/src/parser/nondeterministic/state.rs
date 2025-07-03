use std::hash::Hash;

use crate::hash::HashMap;

/// `State` implementation for a sparse state representation using HashMap
#[derive(Debug, Clone)]
pub struct SparseState<NonTerm, RuleContainer> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: HashMap<usize, usize>,
    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: HashMap<usize, RuleContainer>,
    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}

impl<NonTerm: Copy, RuleIndex: crate::stackvec::ToUsizeList> crate::parser::State<NonTerm>
    for SparseState<NonTerm, RuleIndex>
{
    fn shift_goto_class(&self, class: usize) -> Option<usize> {
        self.shift_goto_map_class.get(&class).copied()
    }
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.shift_goto_map_nonterm.get(nonterm).copied()
    }
    fn reduce(&self, class: usize) -> Option<impl Iterator<Item = usize> + Clone + '_> {
        self.reduce_map
            .get(&class)
            .map(crate::stackvec::ToUsizeList::to_usize_list)
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
fn builder_state_into_sparse<NonTerm, RuleContainer>(
    builder_state: crate::builder::State<usize, NonTerm>,
    rule_vec_map: impl Fn(std::collections::BTreeSet<usize>) -> RuleContainer,
) -> crate::parser::nondeterministic::state::SparseState<NonTerm, RuleContainer>
where
    NonTerm: Hash + Eq,
{
    crate::parser::nondeterministic::state::SparseState {
        shift_goto_map_class: builder_state.shift_goto_map_term.into_iter().collect(),
        shift_goto_map_nonterm: builder_state.shift_goto_map_nonterm.into_iter().collect(),
        reduce_map: builder_state
            .reduce_map
            .into_iter()
            .map(|(term, rule)| (term, rule_vec_map(rule)))
            .collect(),
        ruleset: builder_state.ruleset.into_iter().collect(),
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU8>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU16>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU32>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for SparseState<NonTerm, crate::stackvec::SmallVecUsize>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_sparse(builder_state, crate::stackvec::SmallVecUsize::from_iter)
    }
}

/// `State` implementation for a dense state representation using Vec
#[derive(Debug, Clone)]
pub struct DenseState<NonTerm, RuleContainer> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: Vec<Option<usize>>,
    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: Vec<Option<RuleContainer>>,
    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}
impl<NonTerm: Copy, RuleContainer: crate::stackvec::ToUsizeList> crate::parser::State<NonTerm>
    for DenseState<NonTerm, RuleContainer>
{
    fn shift_goto_class(&self, class: usize) -> Option<usize> {
        match self.shift_goto_map_class.get(class) {
            Some(s) => *s,
            None => None,
        }
    }
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.shift_goto_map_nonterm.get(nonterm).copied()
    }
    fn reduce(&self, class: usize) -> Option<impl Iterator<Item = usize> + Clone + '_> {
        self.reduce_map
            .get(class)
            .unwrap()
            .as_ref()
            .map(crate::stackvec::ToUsizeList::to_usize_list)
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

fn builder_state_into_dense<NonTerm, RuleContainer: Clone>(
    builder_state: crate::builder::State<usize, NonTerm>,
    rule_vec_map: impl Fn(std::collections::BTreeSet<usize>) -> RuleContainer,
) -> crate::parser::nondeterministic::state::DenseState<NonTerm, RuleContainer>
where
    NonTerm: Hash + Eq,
{
    let term_len = builder_state
        .shift_goto_map_term
        .keys()
        .next_back()
        .copied()
        .map(|x| x + 1)
        .unwrap_or(0);
    let mut shift_goto_map_class = vec![None; term_len];
    let mut reduce_map = vec![None; term_len];
    for (term, state) in builder_state.shift_goto_map_term {
        shift_goto_map_class[term] = Some(state);
    }
    for (term, rule) in builder_state.reduce_map {
        reduce_map[term] = Some(rule_vec_map(rule));
    }
    crate::parser::nondeterministic::state::DenseState {
        shift_goto_map_class,
        shift_goto_map_nonterm: builder_state.shift_goto_map_nonterm.into_iter().collect(),
        reduce_map,
        ruleset: builder_state.ruleset.into_iter().collect(),
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU8>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU16>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU32>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
}
impl<NonTerm> From<crate::builder::State<usize, NonTerm>>
    for DenseState<NonTerm, crate::stackvec::SmallVecUsize>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        builder_state_into_dense(builder_state, crate::stackvec::SmallVecUsize::from_iter)
    }
}
