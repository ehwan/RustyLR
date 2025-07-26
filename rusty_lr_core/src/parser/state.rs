use std::hash::Hash;

use crate::hash::HashMap;
use crate::nonterminal::NonTerminal;
use crate::TerminalSymbol;

/// For state, terminal and class indices, we use the most compact integer type that can hold the maximum value.
/// This trait defines the conversion between {u8, u16, u32, usize} <-> usize.
pub trait Index {
    fn into_usize(self) -> usize;
    fn from_usize_unchecked(value: usize) -> Self;
}
impl Index for usize {
    fn into_usize(self) -> usize {
        self
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value
    }
}
impl Index for u8 {
    fn into_usize(self) -> usize {
        self as usize
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value as u8
    }
}
impl Index for u16 {
    fn into_usize(self) -> usize {
        self as usize
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value as u16
    }
}
impl Index for u32 {
    fn into_usize(self) -> usize {
        self as usize
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value as u32
    }
}

/// Since non-deterministic parsers can have multiple reduce rules for a single terminal,
/// we need to handle the set of reduce rules efficiently, usually 2~3 items.
/// this trait implements the stack-allocated vector for this purpose.
pub trait ReduceRules {
    type RuleIndex: Index;

    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone;
    fn from_set<RuleIndexFrom: TryInto<Self::RuleIndex>>(
        set: std::collections::BTreeSet<RuleIndexFrom>,
    ) -> Self;
}

/// For deterministic parser behavior
impl<Integral: Index + Copy> ReduceRules for Integral {
    type RuleIndex = Integral;

    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        std::iter::once(self.into_usize())
    }
    fn from_set<RuleIndexFrom: TryInto<Self::RuleIndex>>(
        set: std::collections::BTreeSet<RuleIndexFrom>,
    ) -> Self {
        debug_assert!(set.len() == 1, "Expected a single element set");
        set.into_iter().next().unwrap().try_into().ok().unwrap()
    }
}
impl<Arr: smallvec::Array> ReduceRules for smallvec::SmallVec<Arr>
where
    Arr::Item: Index + Copy,
{
    type RuleIndex = Arr::Item;

    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x.into_usize())
    }
    fn from_set<RuleIndexFrom: TryInto<Self::RuleIndex>>(
        set: std::collections::BTreeSet<RuleIndexFrom>,
    ) -> Self {
        set.into_iter()
            .map(|value| value.try_into().ok().unwrap())
            .collect()
    }
}

pub type SmallVecU8 = smallvec::SmallVec<[u8; 16]>;
pub type SmallVecU16 = smallvec::SmallVec<[u16; 8]>;
pub type SmallVecU32 = smallvec::SmallVec<[u32; 4]>;
pub type SmallVecUsize = smallvec::SmallVec<[usize; 2]>;

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
pub struct SparseState<Term, NonTerm, RuleContainer, StateIndex> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: HashMap<Term, StateIndex>,
    pub(crate) error_shift: Option<StateIndex>,
    pub(crate) eof_shift: Option<StateIndex>,

    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, StateIndex>,

    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: HashMap<Term, RuleContainer>,
    pub(crate) error_reduce: Option<RuleContainer>,
    pub(crate) eof_reduce: Option<RuleContainer>,

    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}

impl<
        Term: Into<usize> + Index + Copy + Hash + Eq,
        NonTerm: Copy,
        RuleContainer: ReduceRules,
        StateIndex: Into<usize> + Copy,
    > State<NonTerm> for SparseState<Term, NonTerm, RuleContainer, StateIndex>
{
    fn shift_goto_class(&self, class: TerminalSymbol<usize>) -> Option<usize> {
        match class {
            TerminalSymbol::Term(class) => self
                .shift_goto_map_class
                .get(&Term::from_usize_unchecked(class))
                .copied(),
            TerminalSymbol::Error => self.error_shift,
            TerminalSymbol::Eof => self.eof_shift,
        }
        .map(Into::into)
    }
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq,
    {
        self.shift_goto_map_nonterm
            .get(nonterm)
            .copied()
            .map(Into::into)
    }
    fn reduce(
        &self,
        class: TerminalSymbol<usize>,
    ) -> Option<impl Iterator<Item = usize> + Clone + '_> {
        match class {
            TerminalSymbol::Term(class) => self.reduce_map.get(&Term::from_usize_unchecked(class)),
            TerminalSymbol::Error => self.error_reduce.as_ref(),
            TerminalSymbol::Eof => self.eof_reduce.as_ref(),
        }
        .map(ReduceRules::to_usize_list)
    }
    fn is_accept(&self) -> bool {
        self.reduce_map.is_empty()
            && self.shift_goto_map_class.is_empty()
            && self.shift_goto_map_nonterm.is_empty()
    }
    fn expected_shift_term(&self) -> impl Iterator<Item = usize> + '_ {
        self.shift_goto_map_class.keys().copied().map(Into::into)
    }
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_ {
        self.shift_goto_map_nonterm.keys().copied()
    }
    fn expected_reduce_term(&self) -> impl Iterator<Item = usize> + '_ {
        self.reduce_map.keys().copied().map(Into::into)
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
pub struct DenseState<Term, NonTerm, RuleContainer, StateIndex> {
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

    _phantom: std::marker::PhantomData<Term>,
}
impl<Term, NonTerm: Copy, RuleContainer: ReduceRules, StateIndex: Into<usize> + Copy> State<NonTerm>
    for DenseState<Term, NonTerm, RuleContainer, StateIndex>
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
        .map(Into::into)
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
                .map(Into::into)
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
                }
            }
            TerminalSymbol::Error => self.error_reduce.as_ref(),
            TerminalSymbol::Eof => self.eof_reduce.as_ref(),
        }
        .map(ReduceRules::to_usize_list)
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

impl<Term, TermTo, NonTerm, RuleContainer, StateIndex, StateIndexTo, RuleIndex>
    From<crate::builder::State<TerminalSymbol<Term>, NonTerm, StateIndex, RuleIndex>>
    for SparseState<TermTo, NonTerm, RuleContainer, StateIndexTo>
where
    Term: Ord + TryInto<TermTo>,
    Term::Error: std::fmt::Debug,
    TermTo: Hash + Eq,
    NonTerm: Hash + Eq,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
    RuleContainer: ReduceRules,
    RuleContainer::RuleIndex: TryFrom<RuleIndex>,
{
    fn from(
        mut builder_state: crate::builder::State<
            TerminalSymbol<Term>,
            NonTerm,
            StateIndex,
            RuleIndex,
        >,
    ) -> Self {
        let error_shift = builder_state
            .shift_goto_map_term
            .remove(&TerminalSymbol::Error);
        let eof_shift = builder_state
            .shift_goto_map_term
            .remove(&TerminalSymbol::Eof);
        let error_reduce = builder_state
            .reduce_map
            .remove(&TerminalSymbol::Error)
            .map(|reduce_rules| RuleContainer::from_set(reduce_rules));
        let eof_reduce = builder_state
            .reduce_map
            .remove(&TerminalSymbol::Eof)
            .map(|reduce_rules| RuleContainer::from_set(reduce_rules));
        SparseState {
            shift_goto_map_class: builder_state
                .shift_goto_map_term
                .into_iter()
                .map(|(term, state)| {
                    (
                        term.into_term()
                            .unwrap()
                            .try_into()
                            .expect("term conversion failed"),
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
                .map(|(term, rule)| {
                    (
                        term.into_term()
                            .unwrap()
                            .try_into()
                            .expect("term conversion failed"),
                        RuleContainer::from_set(rule),
                    )
                })
                .collect(),
            error_reduce,
            eof_reduce,
            ruleset: builder_state.ruleset.into_iter().collect(),
        }
    }
}
impl<Term, TermTo, NonTerm, RuleContainer, StateIndex, StateIndexTo: Copy, RuleIndex>
    From<crate::builder::State<TerminalSymbol<Term>, NonTerm, StateIndex, RuleIndex>>
    for DenseState<TermTo, NonTerm, RuleContainer, StateIndexTo>
where
    Term: Ord + Into<usize> + Copy,
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: TryInto<StateIndexTo>,
    StateIndex::Error: std::fmt::Debug,
    RuleContainer: Clone + ReduceRules,
    RuleContainer::RuleIndex: TryFrom<RuleIndex>,
{
    fn from(
        mut builder_state: crate::builder::State<
            TerminalSymbol<Term>,
            NonTerm,
            StateIndex,
            RuleIndex,
        >,
    ) -> Self {
        let error_shift = builder_state
            .shift_goto_map_term
            .remove(&TerminalSymbol::Error);
        let eof_shift = builder_state
            .shift_goto_map_term
            .remove(&TerminalSymbol::Eof);
        let error_reduce = builder_state
            .reduce_map
            .remove(&TerminalSymbol::Error)
            .map(|reduce_map| RuleContainer::from_set(reduce_map));
        let eof_reduce = builder_state
            .reduce_map
            .remove(&TerminalSymbol::Eof)
            .map(|reduce_map| RuleContainer::from_set(reduce_map));

        let (shift_min, shift_len) = {
            let mut iter = builder_state.shift_goto_map_term.keys();
            let min: Option<usize> = iter.next().map(|x| x.into_term().unwrap().into());
            let max: Option<usize> = iter
                .next_back()
                .map(|x| x.into_term().unwrap().into())
                .or(min);

            if let (Some(min), Some(max)) = (min, max) {
                (min, max - min + 1)
            } else {
                (0, 0)
            }
        };
        let (reduce_min, reduce_len) = {
            let mut iter = builder_state.reduce_map.keys();
            let min: Option<usize> = iter.next().map(|x| x.into_term().unwrap().into());
            let max: Option<usize> = iter
                .next_back()
                .map(|x| x.into_term().unwrap().into())
                .or(min);
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
            shift_goto_map_class[term.into_term().unwrap().into() - shift_min] =
                Some(state.try_into().expect("state conversion failed"));
        }

        let mut reduce_map = vec![None; reduce_len];
        for (term, rule) in builder_state.reduce_map {
            reduce_map[term.into_term().unwrap().into() - reduce_min] =
                Some(RuleContainer::from_set(rule));
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
            _phantom: std::marker::PhantomData,
        }
    }
}
