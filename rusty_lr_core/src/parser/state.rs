use std::hash::Hash;

use crate::hash::HashMap;
use crate::parser::nonterminal::NonTerminal;
use crate::parser::terminalclass::TerminalClass;

#[derive(Debug, Clone, Copy)]
pub struct ShiftTarget<StateIndex> {
    pub state: StateIndex,
    /// true if the data should be pushed, false if data should not be pushed
    pub push: bool,
}
impl<StateIndex> ShiftTarget<StateIndex> {
    pub fn new(state: StateIndex, push: bool) -> Self {
        ShiftTarget { state, push }
    }
}

/// This intermediate state is a common structure to convert from generated code and grammar builder
/// into various types of parser states (SparseState, DenseState, ...).
pub struct IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex> {
    pub shift_goto_map_term: Vec<(TermClass, ShiftTarget<StateIndex>)>, // must be sorted
    pub shift_goto_map_nonterm: Vec<(NonTerm, ShiftTarget<StateIndex>)>, // must be sorted
    pub reduce_map: Vec<(TermClass, Vec<RuleIndex>)>,                   // must be sorted
    pub ruleset: Vec<crate::rule::ShiftedRuleRef>,
}

/// For state, terminal and class indices, we use the most compact integer type that can hold the maximum value.
/// This trait defines the conversion between {u8, u16, u32, usize} <-> usize.
pub trait Index: Copy {
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
    const CAP: usize;
    type RuleIndex: Index;

    fn to_iter(&self) -> impl Iterator<Item = Self::RuleIndex> + Clone;
    fn from_set<RuleIndexFrom: TryInto<Self::RuleIndex>>(set: Vec<RuleIndexFrom>) -> Self;
}

/// For deterministic parser behavior
impl<Integral: Index + Copy> ReduceRules for Integral {
    const CAP: usize = 1;
    type RuleIndex = Integral;

    fn to_iter(&self) -> impl Iterator<Item = Self::RuleIndex> + Clone {
        std::iter::once(*self)
    }
    fn from_set<RuleIndexFrom: TryInto<Self::RuleIndex>>(set: Vec<RuleIndexFrom>) -> Self {
        debug_assert!(set.len() == 1, "Expected a single element set");
        set.into_iter().next().unwrap().try_into().ok().unwrap()
    }
}

pub use arrayvec::ArrayVec;
impl<T: Index, const CAP: usize> ReduceRules for ArrayVec<T, CAP> {
    const CAP: usize = CAP;
    type RuleIndex = T;

    fn to_iter(&self) -> impl Iterator<Item = Self::RuleIndex> + Clone {
        self.iter().copied()
    }
    fn from_set<RuleIndexFrom: TryInto<Self::RuleIndex>>(set: Vec<RuleIndexFrom>) -> Self {
        set.into_iter()
            .map(|value| value.try_into().ok().unwrap())
            .collect()
    }
}

/// A trait representing a parser state.
pub trait State {
    type TermClass: TerminalClass;
    type NonTerm: NonTerminal;
    type ReduceRules: ReduceRules;
    type StateIndex: Index;

    /// Get the next state for a given terminal symbol.
    fn shift_goto_class(&self, class: Self::TermClass) -> Option<ShiftTarget<Self::StateIndex>>;

    /// Get the next state for a given non-terminal symbol.
    fn shift_goto_nonterm(&self, nonterm: Self::NonTerm) -> Option<ShiftTarget<Self::StateIndex>>;
    /// Get the reduce rule index for a given terminal symbol.
    fn reduce(&self, class: Self::TermClass) -> Option<&Self::ReduceRules>;

    /// Check if this state is an accept state.
    fn is_accept(&self) -> bool;

    /// Get the set of expected terminal classes for shift in this state
    fn expected_shift_term(&self) -> impl Iterator<Item = Self::TermClass> + '_;

    /// Get the set of expected non-terminal symbols for shift in this state
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = Self::NonTerm> + '_;

    /// Get the set of production rule for reduce in this state
    fn expected_reduce_rule(&self) -> impl Iterator<Item = impl Index> + '_;

    /// Get the set of rules that this state is trying to parse
    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef];
}

/// `State` implementation for a sparse state representation using HashMap
#[derive(Debug, Clone)]
pub struct SparseState<TermClass, NonTerm, RuleContainer, StateIndex> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: HashMap<TermClass, ShiftTarget<StateIndex>>,

    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, ShiftTarget<StateIndex>>,

    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: HashMap<TermClass, RuleContainer>,

    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}

impl<
        TermClass: TerminalClass + Hash + Eq,
        NonTerm: NonTerminal + Hash + Eq,
        RuleContainer: ReduceRules,
        StateIndex: Index,
    > State for SparseState<TermClass, NonTerm, RuleContainer, StateIndex>
{
    type TermClass = TermClass;
    type NonTerm = NonTerm;
    type ReduceRules = RuleContainer;
    type StateIndex = StateIndex;

    fn shift_goto_class(&self, class: Self::TermClass) -> Option<ShiftTarget<Self::StateIndex>> {
        self.shift_goto_map_class.get(&class).copied()
    }
    fn shift_goto_nonterm(&self, nonterm: Self::NonTerm) -> Option<ShiftTarget<Self::StateIndex>> {
        self.shift_goto_map_nonterm.get(&nonterm).copied()
    }
    fn reduce(&self, class: Self::TermClass) -> Option<&Self::ReduceRules> {
        self.reduce_map.get(&class)
    }
    fn is_accept(&self) -> bool {
        self.reduce_map.is_empty()
            && self.shift_goto_map_class.is_empty()
            && self.shift_goto_map_nonterm.is_empty()
    }
    fn expected_shift_term(&self) -> impl Iterator<Item = Self::TermClass> + '_ {
        self.shift_goto_map_class.keys().copied()
    }
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = Self::NonTerm> + '_ {
        self.shift_goto_map_nonterm.keys().copied()
    }
    fn expected_reduce_rule(&self) -> impl Iterator<Item = impl Index> + '_ {
        self.reduce_map.values().flat_map(RuleContainer::to_iter)
    }
    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef] {
        &self.ruleset
    }
}

/// `State` implementation for a dense state representation using Vec
#[derive(Debug, Clone)]
pub struct DenseState<TermClass, NonTerm, RuleContainer, StateIndex> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: Vec<Option<ShiftTarget<StateIndex>>>,
    /// shift_goto_map_class[i] will contain i+offset 'th class's next state.
    pub(crate) shift_class_offset: usize,
    /// set of terminal classes that is keys of `shift_goto_map_class`
    pub(crate) shift_goto_map_class_keys: Vec<TermClass>,

    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: Vec<Option<ShiftTarget<StateIndex>>>,
    pub(crate) shift_nonterm_offset: usize,
    /// set of non-terminal symbols that is keys of `shift_goto_map_nonterm`
    pub(crate) shift_goto_map_nonterm_keys: Vec<NonTerm>,

    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: Vec<Option<RuleContainer>>,
    /// reduce_map[i] will contain i+offset 'th class's reduce rule.
    pub(crate) reduce_offset: usize,

    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,

    _phantom: std::marker::PhantomData<TermClass>,
}
impl<
        TermClass: TerminalClass,
        NonTerm: NonTerminal,
        RuleContainer: ReduceRules,
        StateIndex: Index,
    > State for DenseState<TermClass, NonTerm, RuleContainer, StateIndex>
{
    type TermClass = TermClass;
    type NonTerm = NonTerm;
    type ReduceRules = RuleContainer;
    type StateIndex = StateIndex;

    fn shift_goto_class(&self, class: Self::TermClass) -> Option<ShiftTarget<Self::StateIndex>> {
        self.shift_goto_map_class
            .get(class.to_usize().wrapping_sub(self.shift_class_offset))
            .copied()
            .flatten()
    }
    fn shift_goto_nonterm(&self, nonterm: Self::NonTerm) -> Option<ShiftTarget<Self::StateIndex>> {
        self.shift_goto_map_nonterm
            .get(nonterm.to_usize().wrapping_sub(self.shift_nonterm_offset))
            .copied()
            .flatten()
    }
    fn reduce(&self, class: Self::TermClass) -> Option<&Self::ReduceRules> {
        self.reduce_map
            .get(class.to_usize().wrapping_sub(self.reduce_offset))
            .and_then(|r| r.as_ref())
    }
    fn is_accept(&self) -> bool {
        self.reduce_map.is_empty()
            && self.shift_goto_map_class.is_empty()
            && self.shift_goto_map_nonterm.is_empty()
    }
    fn expected_shift_term(&self) -> impl Iterator<Item = Self::TermClass> + '_ {
        self.shift_goto_map_class_keys.iter().copied()
    }
    fn expected_shift_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_ {
        self.shift_goto_map_nonterm_keys.iter().copied()
    }
    fn expected_reduce_rule(&self) -> impl Iterator<Item = impl Index> + '_ {
        self.reduce_map
            .iter()
            .filter_map(|r| r.as_ref())
            .flat_map(RuleContainer::to_iter)
    }

    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef] {
        &self.ruleset
    }
}

impl<TermClass: TerminalClass, NonTerm: NonTerminal, RuleContainer, StateIndex, RuleIndex>
    From<IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex>>
    for SparseState<TermClass, NonTerm, RuleContainer, StateIndex>
where
    TermClass: Ord + Hash,
    NonTerm: Hash + Eq,
    RuleContainer: ReduceRules,
    RuleContainer::RuleIndex: TryFrom<RuleIndex>,
{
    fn from(builder_state: IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex>) -> Self {
        // TerminalSymbol::Term(_) < TerminalSymbol::Error < TerminalSymbol::Eof
        // since maps are sorted, eof and error should be at the end of the array

        // make sure the order is preserved
        #[cfg(debug_assertions)]
        {
            let keys = builder_state
                .shift_goto_map_term
                .iter()
                .map(|(term, _)| term)
                .collect::<Vec<_>>();
            debug_assert!(keys.is_sorted());

            let keys = builder_state
                .reduce_map
                .iter()
                .map(|(term, _)| term)
                .collect::<Vec<_>>();
            debug_assert!(keys.is_sorted());
        }
        SparseState {
            shift_goto_map_class: builder_state.shift_goto_map_term.into_iter().collect(),
            shift_goto_map_nonterm: builder_state.shift_goto_map_nonterm.into_iter().collect(),
            reduce_map: builder_state
                .reduce_map
                .into_iter()
                .map(|(term, rule)| {
                    (
                        term.try_into().expect("term conversion failed"),
                        RuleContainer::from_set(rule),
                    )
                })
                .collect(),
            ruleset: builder_state.ruleset.into_iter().collect(),
        }
    }
}
impl<TermClass: TerminalClass, NonTerm: NonTerminal, RuleContainer, StateIndex, RuleIndex>
    From<IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex>>
    for DenseState<TermClass, NonTerm, RuleContainer, StateIndex>
where
    TermClass: Ord + Copy,
    NonTerm: Hash + Eq + Copy + NonTerminal,
    StateIndex: Copy,
    RuleContainer: Clone + ReduceRules,
    RuleContainer::RuleIndex: TryFrom<RuleIndex>,
{
    fn from(builder_state: IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex>) -> Self {
        // TerminalSymbol::Term(_) < TerminalSymbol::Error < TerminalSymbol::Eof
        // since maps are sorted, eof and error should be at the end of the array

        // make sure the order is preserved
        #[cfg(debug_assertions)]
        {
            let keys = builder_state
                .shift_goto_map_term
                .iter()
                .map(|(term, _)| term)
                .collect::<Vec<_>>();
            debug_assert!(keys.is_sorted());

            let keys = builder_state
                .reduce_map
                .iter()
                .map(|(term, _)| term)
                .collect::<Vec<_>>();
            debug_assert!(keys.is_sorted());
        }

        let (shift_min, shift_len) = {
            let mut iter = builder_state
                .shift_goto_map_term
                .iter()
                .map(|(term, _)| term);
            let min: Option<usize> = iter.next().map(|x| x.to_usize());
            let max: Option<usize> = iter.next_back().map(|x| x.to_usize()).or(min);

            if let (Some(min), Some(max)) = (min, max) {
                (min, max - min + 1)
            } else {
                (0, 0)
            }
        };
        let (reduce_min, reduce_len) = {
            let mut iter = builder_state.reduce_map.iter().map(|(term, _)| term);
            let min: Option<usize> = iter.next().map(|x| x.to_usize());
            let max: Option<usize> = iter.next_back().map(|x| x.to_usize()).or(min);
            if let (Some(min), Some(max)) = (min, max) {
                (min, max - min + 1)
            } else {
                (0, 0)
            }
        };
        let (nonterm_min, nonterm_len) = {
            let mut iter = builder_state
                .shift_goto_map_nonterm
                .iter()
                .map(|(nonterm, _)| nonterm);
            let min = iter.next().map(|x| x.to_usize());
            let max = iter.next_back().map(|x| x.to_usize()).or(min);
            if let (Some(min), Some(max)) = (min, max) {
                (min, max - min + 1)
            } else {
                (0, 0)
            }
        };

        let shift_term_keys = builder_state
            .shift_goto_map_term
            .iter()
            .map(|(term, _)| *term)
            .collect();
        let mut shift_goto_map_class = vec![None; shift_len];
        for (term, state) in builder_state.shift_goto_map_term {
            shift_goto_map_class[term.to_usize() - shift_min] = Some(state);
        }

        let mut reduce_map = vec![None; reduce_len];
        for (term, rule) in builder_state.reduce_map {
            reduce_map[term.to_usize() - reduce_min] = Some(RuleContainer::from_set(rule));
        }

        let nonterm_keys = builder_state
            .shift_goto_map_nonterm
            .iter()
            .map(|(nonterm, _)| *nonterm)
            .collect();
        let mut shift_goto_map_nonterm = vec![None; nonterm_len];
        for (nonterm, state) in builder_state.shift_goto_map_nonterm {
            shift_goto_map_nonterm[nonterm.to_usize() - nonterm_min] = Some(state);
        }

        DenseState {
            shift_goto_map_class,
            shift_class_offset: shift_min,
            shift_goto_map_class_keys: shift_term_keys,
            shift_goto_map_nonterm,
            shift_goto_map_nonterm_keys: nonterm_keys,
            shift_nonterm_offset: nonterm_min,
            reduce_map,
            reduce_offset: reduce_min,
            ruleset: builder_state.ruleset.into_iter().collect(),
            _phantom: std::marker::PhantomData,
        }
    }
}
