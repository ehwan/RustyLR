use std::hash::Hash;

use crate::hash::HashMap;
use crate::parser::nonterminal::NonTerminal;
use crate::parser::terminalclass::TerminalClass;
use crate::TriState;

#[derive(Debug, Clone, Copy)]
pub struct ShiftTarget<StateIndex> {
    pub state: StateIndex,
    /// true if the data should be pushed, false if data should not be pushed (so `Empty` tag will be pushed)
    pub push: bool,
}
impl<StateIndex> ShiftTarget<StateIndex> {
    #[inline]
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
    pub can_accept_error: TriState,
}

/// Hot-path information for a production rule.
///
/// The full production token list is useful for diagnostics and debug output, but parsing only
/// needs the left-hand side, right-hand-side length, and precedence.
#[derive(Debug, Clone, Copy)]
pub struct RuleInfo<NonTerm> {
    pub name: NonTerm,
    pub len: usize,
    pub precedence: Option<crate::rule::Precedence>,
}

/// Intermediate flat parser table data used by generated code before converting to a concrete
/// dense or sparse runtime layout.
pub struct IntermediateTables<TermClass, NonTerm, StateIndex, RuleIndex> {
    pub states: Vec<IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex>>,
    pub rules: Vec<RuleInfo<NonTerm>>,
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

#[derive(Debug, Clone)]
pub enum TermAction<RuleContainer, StateIndex> {
    Shift(ShiftTarget<StateIndex>),
    Reduce(RuleContainer),
    ShiftReduce(ShiftTarget<StateIndex>, RuleContainer),
}

#[derive(Debug)]
pub enum TermActionRef<'a, RuleContainer, StateIndex> {
    Shift(ShiftTarget<StateIndex>),
    Reduce(&'a RuleContainer),
    ShiftReduce(ShiftTarget<StateIndex>, &'a RuleContainer),
}

impl<'a, RuleContainer, StateIndex: Copy> Copy
    for TermActionRef<'a, RuleContainer, StateIndex>
{
}

impl<'a, RuleContainer, StateIndex: Copy> Clone
    for TermActionRef<'a, RuleContainer, StateIndex>
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, RuleContainer, StateIndex: Copy> TermActionRef<'a, RuleContainer, StateIndex> {
    #[inline]
    pub fn shift(self) -> Option<ShiftTarget<StateIndex>> {
        match self {
            TermActionRef::Shift(shift) | TermActionRef::ShiftReduce(shift, _) => Some(shift),
            TermActionRef::Reduce(_) => None,
        }
    }

    #[inline]
    pub fn reduce(self) -> Option<&'a RuleContainer> {
        match self {
            TermActionRef::Reduce(reduce) | TermActionRef::ShiftReduce(_, reduce) => Some(reduce),
            TermActionRef::Shift(_) => None,
        }
    }
}

impl<RuleContainer, StateIndex: Copy> TermAction<RuleContainer, StateIndex> {
    #[inline]
    fn as_ref(&self) -> TermActionRef<'_, RuleContainer, StateIndex> {
        match self {
            TermAction::Shift(shift) => TermActionRef::Shift(*shift),
            TermAction::Reduce(reduce) => TermActionRef::Reduce(reduce),
            TermAction::ShiftReduce(shift, reduce) => TermActionRef::ShiftReduce(*shift, reduce),
        }
    }
}

/// A trait representing the whole parser table.
pub trait ParserTables {
    type TermClass: TerminalClass;
    type NonTerm: NonTerminal;
    type ReduceRules: ReduceRules;
    type StateIndex: Index;

    fn term_action(
        &self,
        state: usize,
        class: Self::TermClass,
    ) -> Option<TermActionRef<'_, Self::ReduceRules, Self::StateIndex>>;

    fn shift_goto_class(
        &self,
        state: usize,
        class: Self::TermClass,
    ) -> Option<ShiftTarget<Self::StateIndex>> {
        self.term_action(state, class).and_then(TermActionRef::shift)
    }

    fn shift_goto_nonterm(
        &self,
        state: usize,
        nonterm: Self::NonTerm,
    ) -> Option<ShiftTarget<Self::StateIndex>>;

    fn reduce(&self, state: usize, class: Self::TermClass) -> Option<&Self::ReduceRules> {
        self.term_action(state, class)
            .and_then(TermActionRef::reduce)
    }

    fn is_accept(&self, state: usize) -> bool;

    fn expected_shift_term(&self, state: usize) -> impl Iterator<Item = Self::TermClass> + '_;

    fn expected_shift_nonterm(&self, state: usize) -> impl Iterator<Item = Self::NonTerm> + '_;

    fn expected_reduce_rule(&self, state: usize) -> impl Iterator<Item = impl Index> + '_;

    fn can_accept_error(&self, state: usize) -> TriState;

    fn rule(&self, rule: usize) -> &RuleInfo<Self::NonTerm>;

    fn state_count(&self) -> usize;

    fn rule_count(&self) -> usize;
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

    fn can_accept_error(&self) -> TriState;
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

    pub(crate) can_accept_error: TriState,
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
    fn can_accept_error(&self) -> TriState {
        self.can_accept_error
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

    pub(crate) can_accept_error: TriState,
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

    fn can_accept_error(&self) -> TriState {
        self.can_accept_error
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
            can_accept_error: builder_state.can_accept_error,
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
            can_accept_error: builder_state.can_accept_error,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SparseFlatTables<TermClass, NonTerm, RuleContainer, StateIndex> {
    term_offsets: Vec<usize>,
    term_actions: Vec<(TermClass, TermAction<RuleContainer, StateIndex>)>,
    nonterm_offsets: Vec<usize>,
    nonterm_goto: Vec<(NonTerm, ShiftTarget<StateIndex>)>,
    can_accept_error: Vec<TriState>,
    rules: Vec<RuleInfo<NonTerm>>,
}

impl<TermClass, NonTerm, RuleContainer, StateIndex, RuleIndex>
    From<IntermediateTables<TermClass, NonTerm, StateIndex, RuleIndex>>
    for SparseFlatTables<TermClass, NonTerm, RuleContainer, StateIndex>
where
    TermClass: TerminalClass + Ord,
    NonTerm: NonTerminal + Ord,
    RuleContainer: ReduceRules,
    RuleContainer::RuleIndex: TryFrom<RuleIndex>,
{
    fn from(intermediate: IntermediateTables<TermClass, NonTerm, StateIndex, RuleIndex>) -> Self {
        let mut term_offsets = Vec::with_capacity(intermediate.states.len() + 1);
        let mut term_actions = Vec::new();
        let mut nonterm_offsets = Vec::with_capacity(intermediate.states.len() + 1);
        let mut nonterm_goto = Vec::new();
        let mut can_accept_error = Vec::with_capacity(intermediate.states.len());

        term_offsets.push(0);
        nonterm_offsets.push(0);

        for state in intermediate.states {
            let mut shifts = state.shift_goto_map_term.into_iter().peekable();
            let mut reduces = state.reduce_map.into_iter().peekable();

            while shifts.peek().is_some() || reduces.peek().is_some() {
                match (shifts.peek(), reduces.peek()) {
                    (Some((shift_term, _)), Some((reduce_term, _))) => {
                        use std::cmp::Ordering;
                        match shift_term.cmp(reduce_term) {
                            Ordering::Less => {
                                let (term, shift) = shifts.next().unwrap();
                                term_actions.push((term, TermAction::Shift(shift)));
                            }
                            Ordering::Equal => {
                                let (term, shift) = shifts.next().unwrap();
                                let (_, rules) = reduces.next().unwrap();
                                term_actions.push((
                                    term,
                                    TermAction::ShiftReduce(
                                        shift,
                                        RuleContainer::from_set(rules),
                                    ),
                                ));
                            }
                            Ordering::Greater => {
                                let (term, rules) = reduces.next().unwrap();
                                term_actions.push((
                                    term,
                                    TermAction::Reduce(RuleContainer::from_set(rules)),
                                ));
                            }
                        }
                    }
                    (Some(_), None) => {
                        let (term, shift) = shifts.next().unwrap();
                        term_actions.push((term, TermAction::Shift(shift)));
                    }
                    (None, Some(_)) => {
                        let (term, rules) = reduces.next().unwrap();
                        term_actions.push((
                            term,
                            TermAction::Reduce(RuleContainer::from_set(rules)),
                        ));
                    }
                    (None, None) => unreachable!(),
                }
            }
            term_offsets.push(term_actions.len());

            nonterm_goto.extend(state.shift_goto_map_nonterm);
            nonterm_offsets.push(nonterm_goto.len());
            can_accept_error.push(state.can_accept_error);
        }

        SparseFlatTables {
            term_offsets,
            term_actions,
            nonterm_offsets,
            nonterm_goto,
            can_accept_error,
            rules: intermediate.rules,
        }
    }
}

impl<TermClass, NonTerm, RuleContainer, StateIndex> ParserTables
    for SparseFlatTables<TermClass, NonTerm, RuleContainer, StateIndex>
where
    TermClass: TerminalClass + Ord,
    NonTerm: NonTerminal + Ord,
    RuleContainer: ReduceRules,
    StateIndex: Index,
{
    type TermClass = TermClass;
    type NonTerm = NonTerm;
    type ReduceRules = RuleContainer;
    type StateIndex = StateIndex;

    fn term_action(
        &self,
        state: usize,
        class: Self::TermClass,
    ) -> Option<TermActionRef<'_, Self::ReduceRules, Self::StateIndex>> {
        let row = &self.term_actions[self.term_offsets[state]..self.term_offsets[state + 1]];
        if row.len() <= 8 {
            row.iter()
                .find(|(term, _)| *term == class)
                .map(|(_, action)| action.as_ref())
        } else {
            row.binary_search_by_key(&class, |(term, _)| *term)
                .ok()
                .map(|idx| row[idx].1.as_ref())
        }
    }

    fn shift_goto_nonterm(
        &self,
        state: usize,
        nonterm: Self::NonTerm,
    ) -> Option<ShiftTarget<Self::StateIndex>> {
        let row = &self.nonterm_goto[self.nonterm_offsets[state]..self.nonterm_offsets[state + 1]];
        if row.len() <= 8 {
            row.iter()
                .find(|(key, _)| *key == nonterm)
                .map(|(_, target)| *target)
        } else {
            row.binary_search_by_key(&nonterm, |(key, _)| *key)
                .ok()
                .map(|idx| row[idx].1)
        }
    }

    fn is_accept(&self, state: usize) -> bool {
        self.term_offsets[state] == self.term_offsets[state + 1]
            && self.nonterm_offsets[state] == self.nonterm_offsets[state + 1]
    }

    fn expected_shift_term(&self, state: usize) -> impl Iterator<Item = Self::TermClass> + '_ {
        self.term_actions[self.term_offsets[state]..self.term_offsets[state + 1]]
            .iter()
            .filter_map(|(term, action)| match action {
                TermAction::Shift(_) | TermAction::ShiftReduce(_, _) => Some(*term),
                TermAction::Reduce(_) => None,
            })
    }

    fn expected_shift_nonterm(&self, state: usize) -> impl Iterator<Item = Self::NonTerm> + '_ {
        self.nonterm_goto[self.nonterm_offsets[state]..self.nonterm_offsets[state + 1]]
            .iter()
            .map(|(nonterm, _)| *nonterm)
    }

    fn expected_reduce_rule(&self, state: usize) -> impl Iterator<Item = impl Index> + '_ {
        self.term_actions[self.term_offsets[state]..self.term_offsets[state + 1]]
            .iter()
            .filter_map(|(_, action)| match action {
                TermAction::Reduce(rules) | TermAction::ShiftReduce(_, rules) => Some(rules),
                TermAction::Shift(_) => None,
            })
            .flat_map(RuleContainer::to_iter)
    }

    fn can_accept_error(&self, state: usize) -> TriState {
        self.can_accept_error[state]
    }

    fn rule(&self, rule: usize) -> &RuleInfo<Self::NonTerm> {
        &self.rules[rule]
    }

    fn state_count(&self) -> usize {
        self.can_accept_error.len()
    }

    fn rule_count(&self) -> usize {
        self.rules.len()
    }
}

#[derive(Debug, Clone)]
pub struct DenseFlatTables<TermClass, NonTerm, RuleContainer, StateIndex> {
    term_offsets: Vec<usize>,
    term_mins: Vec<usize>,
    term_actions: Vec<Option<TermAction<RuleContainer, StateIndex>>>,
    term_keys_offsets: Vec<usize>,
    term_keys: Vec<TermClass>,
    nonterm_offsets: Vec<usize>,
    nonterm_mins: Vec<usize>,
    nonterm_goto: Vec<Option<ShiftTarget<StateIndex>>>,
    nonterm_keys_offsets: Vec<usize>,
    nonterm_keys: Vec<NonTerm>,
    can_accept_error: Vec<TriState>,
    rules: Vec<RuleInfo<NonTerm>>,
}

impl<TermClass, NonTerm, RuleContainer, StateIndex, RuleIndex>
    From<IntermediateTables<TermClass, NonTerm, StateIndex, RuleIndex>>
    for DenseFlatTables<TermClass, NonTerm, RuleContainer, StateIndex>
where
    TermClass: TerminalClass + Ord,
    NonTerm: NonTerminal + Ord,
    StateIndex: Copy,
    RuleContainer: Clone + ReduceRules,
    RuleContainer::RuleIndex: TryFrom<RuleIndex>,
{
    fn from(intermediate: IntermediateTables<TermClass, NonTerm, StateIndex, RuleIndex>) -> Self {
        let mut term_offsets = Vec::with_capacity(intermediate.states.len() + 1);
        let mut term_mins = Vec::with_capacity(intermediate.states.len());
        let mut term_actions = Vec::new();
        let mut term_keys_offsets = Vec::with_capacity(intermediate.states.len() + 1);
        let mut term_keys = Vec::new();

        let mut nonterm_offsets = Vec::with_capacity(intermediate.states.len() + 1);
        let mut nonterm_mins = Vec::with_capacity(intermediate.states.len());
        let mut nonterm_goto = Vec::new();
        let mut nonterm_keys_offsets = Vec::with_capacity(intermediate.states.len() + 1);
        let mut nonterm_keys = Vec::new();
        let mut can_accept_error = Vec::with_capacity(intermediate.states.len());

        term_offsets.push(0);
        term_keys_offsets.push(0);
        nonterm_offsets.push(0);
        nonterm_keys_offsets.push(0);

        for state in intermediate.states {
            let term_min = state
                .shift_goto_map_term
                .first()
                .map(|(term, _)| term.to_usize())
                .into_iter()
                .chain(state.reduce_map.first().map(|(term, _)| term.to_usize()))
                .min();
            let term_max = state
                .shift_goto_map_term
                .last()
                .map(|(term, _)| term.to_usize())
                .into_iter()
                .chain(state.reduce_map.last().map(|(term, _)| term.to_usize()))
                .max();
            let term_min = term_min.unwrap_or(0);
            let term_len = term_max.map(|max| max - term_min + 1).unwrap_or(0);
            let term_base = term_actions.len();
            term_mins.push(term_min);
            term_actions.resize_with(term_base + term_len, || None);

            let mut shifts = state.shift_goto_map_term.into_iter().peekable();
            let mut reduces = state.reduce_map.into_iter().peekable();
            while shifts.peek().is_some() || reduces.peek().is_some() {
                match (shifts.peek(), reduces.peek()) {
                    (Some((shift_term, _)), Some((reduce_term, _))) => {
                        use std::cmp::Ordering;
                        match shift_term.cmp(reduce_term) {
                            Ordering::Less => {
                                let (term, shift) = shifts.next().unwrap();
                                let idx = term_base + term.to_usize() - term_min;
                                term_actions[idx] = Some(TermAction::Shift(shift));
                                term_keys.push(term);
                            }
                            Ordering::Equal => {
                                let (term, shift) = shifts.next().unwrap();
                                let (_, rules) = reduces.next().unwrap();
                                let idx = term_base + term.to_usize() - term_min;
                                term_actions[idx] = Some(TermAction::ShiftReduce(
                                    shift,
                                    RuleContainer::from_set(rules),
                                ));
                                term_keys.push(term);
                            }
                            Ordering::Greater => {
                                let (term, rules) = reduces.next().unwrap();
                                let idx = term_base + term.to_usize() - term_min;
                                term_actions[idx] =
                                    Some(TermAction::Reduce(RuleContainer::from_set(rules)));
                            }
                        }
                    }
                    (Some(_), None) => {
                        let (term, shift) = shifts.next().unwrap();
                        let idx = term_base + term.to_usize() - term_min;
                        term_actions[idx] = Some(TermAction::Shift(shift));
                        term_keys.push(term);
                    }
                    (None, Some(_)) => {
                        let (term, rules) = reduces.next().unwrap();
                        let idx = term_base + term.to_usize() - term_min;
                        term_actions[idx] =
                            Some(TermAction::Reduce(RuleContainer::from_set(rules)));
                    }
                    (None, None) => unreachable!(),
                }
            }
            term_offsets.push(term_actions.len());
            term_keys_offsets.push(term_keys.len());

            let nonterm_min = state
                .shift_goto_map_nonterm
                .first()
                .map(|(nonterm, _)| nonterm.to_usize())
                .unwrap_or(0);
            let nonterm_len = state
                .shift_goto_map_nonterm
                .last()
                .map(|(nonterm, _)| nonterm.to_usize() - nonterm_min + 1)
                .unwrap_or(0);
            let nonterm_base = nonterm_goto.len();
            nonterm_mins.push(nonterm_min);
            nonterm_goto.resize_with(nonterm_base + nonterm_len, || None);
            for (nonterm, target) in state.shift_goto_map_nonterm {
                let idx = nonterm_base + nonterm.to_usize() - nonterm_min;
                nonterm_goto[idx] = Some(target);
                nonterm_keys.push(nonterm);
            }
            nonterm_offsets.push(nonterm_goto.len());
            nonterm_keys_offsets.push(nonterm_keys.len());

            can_accept_error.push(state.can_accept_error);
        }

        DenseFlatTables {
            term_offsets,
            term_mins,
            term_actions,
            term_keys_offsets,
            term_keys,
            nonterm_offsets,
            nonterm_mins,
            nonterm_goto,
            nonterm_keys_offsets,
            nonterm_keys,
            can_accept_error,
            rules: intermediate.rules,
        }
    }
}

impl<TermClass, NonTerm, RuleContainer, StateIndex> ParserTables
    for DenseFlatTables<TermClass, NonTerm, RuleContainer, StateIndex>
where
    TermClass: TerminalClass + Ord,
    NonTerm: NonTerminal + Ord,
    RuleContainer: ReduceRules,
    StateIndex: Index,
{
    type TermClass = TermClass;
    type NonTerm = NonTerm;
    type ReduceRules = RuleContainer;
    type StateIndex = StateIndex;

    fn term_action(
        &self,
        state: usize,
        class: Self::TermClass,
    ) -> Option<TermActionRef<'_, Self::ReduceRules, Self::StateIndex>> {
        let idx = class.to_usize().wrapping_sub(self.term_mins[state]);
        let row_len = self.term_offsets[state + 1] - self.term_offsets[state];
        if idx >= row_len {
            return None;
        }
        self.term_actions[self.term_offsets[state] + idx]
            .as_ref()
            .map(TermAction::as_ref)
    }

    fn shift_goto_nonterm(
        &self,
        state: usize,
        nonterm: Self::NonTerm,
    ) -> Option<ShiftTarget<Self::StateIndex>> {
        let idx = nonterm.to_usize().wrapping_sub(self.nonterm_mins[state]);
        let row_len = self.nonterm_offsets[state + 1] - self.nonterm_offsets[state];
        if idx >= row_len {
            return None;
        }
        self.nonterm_goto[self.nonterm_offsets[state] + idx]
    }

    fn is_accept(&self, state: usize) -> bool {
        self.term_offsets[state] == self.term_offsets[state + 1]
            && self.nonterm_offsets[state] == self.nonterm_offsets[state + 1]
    }

    fn expected_shift_term(&self, state: usize) -> impl Iterator<Item = Self::TermClass> + '_ {
        self.term_keys[self.term_keys_offsets[state]..self.term_keys_offsets[state + 1]]
            .iter()
            .copied()
    }

    fn expected_shift_nonterm(&self, state: usize) -> impl Iterator<Item = Self::NonTerm> + '_ {
        self.nonterm_keys[self.nonterm_keys_offsets[state]..self.nonterm_keys_offsets[state + 1]]
            .iter()
            .copied()
    }

    fn expected_reduce_rule(&self, state: usize) -> impl Iterator<Item = impl Index> + '_ {
        self.term_actions[self.term_offsets[state]..self.term_offsets[state + 1]]
            .iter()
            .filter_map(|action| match action.as_ref()? {
                TermAction::Reduce(rules) | TermAction::ShiftReduce(_, rules) => Some(rules),
                TermAction::Shift(_) => None,
            })
            .flat_map(RuleContainer::to_iter)
    }

    fn can_accept_error(&self, state: usize) -> TriState {
        self.can_accept_error[state]
    }

    fn rule(&self, rule: usize) -> &RuleInfo<Self::NonTerm> {
        &self.rules[rule]
    }

    fn state_count(&self) -> usize {
        self.can_accept_error.len()
    }

    fn rule_count(&self) -> usize {
        self.rules.len()
    }
}
