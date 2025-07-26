use std::hash::Hash;

use crate::hash::HashMap;
use crate::nonterminal::NonTerminal;
use crate::TerminalSymbol;

/// A trait representing a parser state.
pub trait State<NonTerm> {
    /// Get the next state for a given terminal symbol.
    fn shift_goto_class(&self, class: TerminalSymbol<usize>) -> Option<usize>;

    /// Get the next state for a given non-terminal symbol.
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq + NonTerminal;

    /// Get the reduce rule index for a given terminal symbol.
    fn reduce(
        &self,
        class: TerminalSymbol<usize>,
    ) -> Option<impl Iterator<Item = usize> + Clone + '_>;

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
pub struct SparseState<NonTerm, RuleContainer, StateIndex> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: HashMap<usize, StateIndex>,
    pub(crate) error_shift: Option<StateIndex>,
    pub(crate) eof_shift: Option<StateIndex>,

    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, StateIndex>,

    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: HashMap<usize, RuleContainer>,
    pub(crate) error_reduce: Option<RuleContainer>,
    pub(crate) eof_reduce: Option<RuleContainer>,

    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}

impl<NonTerm: Copy, RuleIndex: crate::stackvec::ToUsizeList, StateIndex: IntoUsize + Copy>
    State<NonTerm> for SparseState<NonTerm, RuleIndex, StateIndex>
{
    fn shift_goto_class(&self, class: TerminalSymbol<usize>) -> Option<usize> {
        match class {
            TerminalSymbol::Term(class) => self.shift_goto_map_class.get(&class).copied(),
            TerminalSymbol::Error => self.error_shift,
            TerminalSymbol::Eof => self.eof_shift,
        }
        .map(IntoUsize::into_usize)
    }
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.shift_goto_map_nonterm
            .get(nonterm)
            .copied()
            .map(IntoUsize::into_usize)
    }
    fn reduce(
        &self,
        class: TerminalSymbol<usize>,
    ) -> Option<impl Iterator<Item = usize> + Clone + '_> {
        match class {
            TerminalSymbol::Term(class) => self
                .reduce_map
                .get(&class)
                .map(crate::stackvec::ToUsizeList::to_usize_list),
            TerminalSymbol::Error => self
                .error_reduce
                .as_ref()
                .map(crate::stackvec::ToUsizeList::to_usize_list),
            TerminalSymbol::Eof => self
                .eof_reduce
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
pub struct DenseState<NonTerm, RuleContainer, StateIndex> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: Vec<Option<StateIndex>>,
    /// shift_goto_map_class[i] will contain i+offset 'th class's next state.
    pub(crate) shift_class_offset: usize,
    pub(crate) error_shift: Option<StateIndex>,
    pub(crate) eof_shift: Option<StateIndex>,

    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: Vec<Option<StateIndex>>,
    pub(crate) shift_nonterm_offset: usize,
    /// set of non-terminal symbols that is keys of `shift_goto_map_nonterm`
    pub(crate) shift_goto_map_nonterm_keys: Vec<NonTerm>,

    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: Vec<Option<RuleContainer>>,
    /// reduce_map[i] will contain i+offset 'th class's reduce rule.
    pub(crate) reduce_offset: usize,
    pub(crate) error_reduce: Option<RuleContainer>,
    pub(crate) eof_reduce: Option<RuleContainer>,

    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}
impl<
        NonTerm: Copy,
        RuleContainer: crate::stackvec::ToUsizeList,
        StateIndex: crate::integral::IntoUsize + Copy,
    > State<NonTerm> for DenseState<NonTerm, RuleContainer, StateIndex>
{
    fn shift_goto_class(&self, class: TerminalSymbol<usize>) -> Option<usize> {
        match class {
            TerminalSymbol::Term(class) => {
                if class < self.shift_class_offset {
                    None
                } else {
                    self.shift_goto_map_class
                        .get(class - self.shift_class_offset)
                        .copied()
                        .flatten()
                }
            }
            TerminalSymbol::Error => self.error_shift,
            TerminalSymbol::Eof => self.eof_shift,
        }
        .map(IntoUsize::into_usize)
    }
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq + NonTerminal,
    {
        let nonterm = nonterm.to_usize();
        if nonterm < self.shift_nonterm_offset {
            None
        } else {
            self.shift_goto_map_nonterm
                .get(nonterm - self.shift_nonterm_offset)
                .copied()
                .flatten()
                .map(IntoUsize::into_usize)
        }
    }
    fn reduce(
        &self,
        class: TerminalSymbol<usize>,
    ) -> Option<impl Iterator<Item = usize> + Clone + '_> {
        match class {
            TerminalSymbol::Term(class) => {
                if class < self.reduce_offset {
                    None
                } else {
                    self.reduce_map
                        .get(class - self.reduce_offset)
                        .map(|r| r.as_ref())
                        .flatten()
                        .map(crate::stackvec::ToUsizeList::to_usize_list)
                }
            }
            TerminalSymbol::Error => self
                .error_reduce
                .as_ref()
                .map(crate::stackvec::ToUsizeList::to_usize_list),
            TerminalSymbol::Eof => self
                .eof_reduce
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
        (0..self.shift_goto_map_class.len())
            .filter(|&i| self.shift_goto_map_class[i].is_some())
            .map(|i| i + self.shift_class_offset)
    }
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_ {
        self.shift_goto_map_nonterm_keys.iter().copied()
    }
    fn expected_reduce_term(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.reduce_map.len())
            .filter(|&i| self.reduce_map[i].is_some())
            .map(|i| i + self.reduce_offset)
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

use crate::integral::IntoUsize;

fn builder_state_into_sparse<NonTerm, RuleContainer, StateIndex, StateIndexTo>(
    mut builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    rule_vec_map: impl Fn(std::collections::BTreeSet<usize>) -> RuleContainer,
) -> SparseState<NonTerm, RuleContainer, StateIndexTo>
where
    NonTerm: Hash + Eq,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    let error_shift = builder_state
        .shift_goto_map_term
        .remove(&TerminalSymbol::Error);
    let eof_shift = builder_state
        .shift_goto_map_term
        .remove(&TerminalSymbol::Eof);
    let error_reduce = builder_state
        .reduce_map
        .remove(&TerminalSymbol::Error)
        .map(&rule_vec_map);
    let eof_reduce = builder_state
        .reduce_map
        .remove(&TerminalSymbol::Eof)
        .map(&rule_vec_map);
    SparseState {
        shift_goto_map_class: builder_state
            .shift_goto_map_term
            .into_iter()
            .map(|(term, state)| {
                (
                    *term.to_term().unwrap(),
                    state.try_into().expect("state conversion failed"),
                )
            })
            .collect(),
        error_shift: error_shift.map(|s| s.try_into().expect("error shift conversion failed")),
        eof_shift: eof_shift.map(|s| s.try_into().expect("eof shift conversion failed")),
        shift_goto_map_nonterm: builder_state
            .shift_goto_map_nonterm
            .into_iter()
            .map(|(nonterm, state)| {
                (
                    nonterm,
                    state
                        .try_into()
                        .expect("non-terminal state conversion failed"),
                )
            })
            .collect(),
        reduce_map: builder_state
            .reduce_map
            .into_iter()
            .map(|(term, rule)| (*term.to_term().unwrap(), rule_vec_map(rule)))
            .collect(),
        error_reduce,
        eof_reduce,
        ruleset: builder_state.ruleset.into_iter().collect(),
    }
}
impl<NonTerm, StateIndex, StateIndexTo>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for SparseState<NonTerm, usize, StateIndexTo>
where
    NonTerm: Hash + Eq,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            if reduce_map.len() != 1 {
                unreachable!("SparseState with usize reduce_map should only have one rule");
            }
            reduce_map.into_iter().next().unwrap()
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU8, StateIndexTo>
where
    NonTerm: Hash + Eq,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU16, StateIndexTo>
where
    NonTerm: Hash + Eq,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for SparseState<NonTerm, crate::stackvec::SmallVecU32, StateIndexTo>
where
    NonTerm: Hash + Eq,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_sparse(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for SparseState<NonTerm, crate::stackvec::SmallVecUsize, StateIndexTo>
where
    NonTerm: Hash + Eq,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_sparse(builder_state, crate::stackvec::SmallVecUsize::from_iter)
    }
}

fn builder_state_into_dense<NonTerm, RuleContainer: Clone, StateIndex, StateIndexTo: Copy>(
    mut builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    rule_vec_map: impl Fn(std::collections::BTreeSet<usize>) -> RuleContainer,
) -> DenseState<NonTerm, RuleContainer, StateIndexTo>
where
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    let error_shift = builder_state
        .shift_goto_map_term
        .remove(&TerminalSymbol::Error);
    let eof_shift = builder_state
        .shift_goto_map_term
        .remove(&TerminalSymbol::Eof);
    let error_reduce = builder_state
        .reduce_map
        .remove(&TerminalSymbol::Error)
        .map(&rule_vec_map);
    let eof_reduce = builder_state
        .reduce_map
        .remove(&TerminalSymbol::Eof)
        .map(&rule_vec_map);

    let (shift_min, shift_len) = {
        let mut iter = builder_state.shift_goto_map_term.keys();
        let min = iter.next().map(|x| *x.to_term().unwrap());
        let max = iter.next_back().map(|x| *x.to_term().unwrap()).or(min);

        if let (Some(min), Some(max)) = (min, max) {
            (min, max - min + 1)
        } else {
            (0, 0)
        }
    };
    let (reduce_min, reduce_len) = {
        let mut iter = builder_state.reduce_map.keys();
        let min = iter.next().map(|x| *x.to_term().unwrap());
        let max = iter.next_back().map(|x| *x.to_term().unwrap()).or(min);
        if let (Some(min), Some(max)) = (min, max) {
            (min, max - min + 1)
        } else {
            (0, 0)
        }
    };
    let (nonterm_min, nonterm_len) = {
        let mut iter = builder_state.shift_goto_map_nonterm.keys();
        let min = iter.next().map(|x| x.to_usize());
        let max = iter.next_back().map(|x| x.to_usize()).or(min);
        if let (Some(min), Some(max)) = (min, max) {
            (min, max - min + 1)
        } else {
            (0, 0)
        }
    };

    let mut shift_goto_map_class = vec![None; shift_len];
    for (term, state) in builder_state.shift_goto_map_term {
        shift_goto_map_class[*term.to_term().unwrap() - shift_min] =
            Some(state.try_into().expect("state conversion failed"));
    }

    let mut reduce_map = vec![None; reduce_len];
    for (term, rule) in builder_state.reduce_map {
        reduce_map[*term.to_term().unwrap() - reduce_min] = Some(rule_vec_map(rule));
    }

    let nonterm_keys = builder_state
        .shift_goto_map_nonterm
        .keys()
        .copied()
        .collect();
    let mut shift_goto_map_nonterm = vec![None; nonterm_len];
    for (nonterm, state) in builder_state.shift_goto_map_nonterm {
        shift_goto_map_nonterm[nonterm.to_usize() - nonterm_min] = Some(
            state
                .try_into()
                .expect("non-terminal state conversion failed"),
        );
    }

    DenseState {
        shift_goto_map_class,
        shift_class_offset: shift_min,
        error_shift: error_shift.map(|s| s.try_into().expect("error shift conversion failed")),
        eof_shift: eof_shift.map(|s| s.try_into().expect("EOF shift conversion failed")),
        shift_goto_map_nonterm,
        shift_goto_map_nonterm_keys: nonterm_keys,
        shift_nonterm_offset: nonterm_min,
        reduce_map,
        reduce_offset: reduce_min,
        error_reduce,
        eof_reduce,
        ruleset: builder_state.ruleset.into_iter().collect(),
    }
}
impl<NonTerm, StateIndex, StateIndexTo: Copy>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for DenseState<NonTerm, usize, StateIndexTo>
where
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            if reduce_map.len() != 1 {
                unreachable!("DenseState with usize reduce_map should only have one rule");
            }
            reduce_map.into_iter().next().unwrap()
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo: Copy>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU8, StateIndexTo>
where
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU8::from_iter(reduce_map.into_iter().map(|x| x as u8))
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo: Copy>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU16, StateIndexTo>
where
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU16::from_iter(reduce_map.into_iter().map(|x| x as u16))
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo: Copy>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for DenseState<NonTerm, crate::stackvec::SmallVecU32, StateIndexTo>
where
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_dense(builder_state, |reduce_map| {
            crate::stackvec::SmallVecU32::from_iter(reduce_map.into_iter().map(|x| x as u32))
        })
    }
}
impl<NonTerm, StateIndex, StateIndexTo: Copy>
    From<crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>>
    for DenseState<NonTerm, crate::stackvec::SmallVecUsize, StateIndexTo>
where
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
{
    fn from(
        builder_state: crate::builder::State<TerminalSymbol<usize>, NonTerm, StateIndex>,
    ) -> Self {
        builder_state_into_dense(builder_state, crate::stackvec::SmallVecUsize::from_iter)
    }
}
