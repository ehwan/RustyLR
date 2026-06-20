//! Runtime parser table layouts and access traits.
//!
//! The `state` module keeps state-level primitives and the older per-state representations.
//! This module is the public home for whole-table layouts used by generated parsers.

use crate::parser::nonterminal::NonTerminal;
use crate::parser::state::Index;
use crate::parser::state::IntermediateState;
use crate::parser::state::ShiftTarget;
use crate::parser::terminalclass::TerminalClass;
use crate::TriState;

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
    /// One decoded transition/action row per LR state.
    pub state_rows: Vec<IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex>>,
    pub rules: Vec<RuleInfo<NonTerm>>,
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

/// Terminal action stored in the runtime action table.
///
/// Shift and reduce entries are merged into one lookup key because the parser hot path always asks
/// the same `(state, terminal_class)` question before deciding whether to reduce or shift. Keeping
/// both actions together avoids probing two independent maps for the common LR step.
#[derive(Debug, Clone)]
pub enum TermAction<RuleContainer, StateIndex> {
    /// Shift the lookahead terminal into the target state.
    Shift(ShiftTarget<StateIndex>),
    /// Reduce by one or more production rules.
    Reduce(RuleContainer),
    /// A real shift/reduce conflict remains after table construction and is resolved at runtime
    /// with precedence or, for GLR, by branching.
    ShiftReduce(ShiftTarget<StateIndex>, RuleContainer),
}

/// Borrowed view of a terminal action.
///
/// The action table owns reduce-rule containers, but parsing only needs to inspect them while the
/// table is borrowed. This lightweight view lets the context obtain both shift and reduce sides
/// from a single table lookup without cloning the reduce container.
#[derive(Debug)]
pub enum TermActionRef<'a, RuleContainer, StateIndex> {
    Shift(ShiftTarget<StateIndex>),
    Reduce(&'a RuleContainer),
    ShiftReduce(ShiftTarget<StateIndex>, &'a RuleContainer),
}

impl<'a, RuleContainer, StateIndex: Copy> Copy for TermActionRef<'a, RuleContainer, StateIndex> {}

impl<'a, RuleContainer, StateIndex: Copy> Clone for TermActionRef<'a, RuleContainer, StateIndex> {
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
///
/// Contexts keep a `&'static ParserTables` reference initialized in `Context::new`, so feeding
/// tokens does not repeatedly pass through `Parser::get_tables()` or `OnceLock`. Implementations are
/// free to choose dense or sparse storage while presenting the same action/goto/rule accessors.
pub trait ParserTables {
    type TermClass: TerminalClass;
    type NonTerm: NonTerminal;
    type ReduceRules: ReduceRules;
    type StateIndex: Index;

    /// Returns the merged terminal action for `(state, terminal_class)`.
    ///
    /// This is the primary hot-path accessor. Callers that need both shift and reduce information
    /// should use this once instead of calling `shift_goto_class` and `reduce` separately.
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
        self.term_action(state, class)
            .and_then(TermActionRef::shift)
    }

    /// Returns the goto transition after reducing to `nonterm`.
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

    /// Returns compact rule metadata used while reducing.
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

#[derive(Debug, Clone)]
pub struct SparseFlatTables<TermClass, NonTerm, RuleContainer, StateIndex> {
    /// Row boundaries into `term_actions`; row `s` is `term_offsets[s]..term_offsets[s + 1]`.
    term_offsets: Vec<usize>,
    /// Sorted `(terminal_class, action)` pairs for all states concatenated together.
    ///
    /// Sparse rows avoid allocating empty slots for terminal classes that never appear in a state.
    /// Small rows use linear search because LR rows are often tiny; larger rows use binary search.
    term_actions: Vec<(TermClass, TermAction<RuleContainer, StateIndex>)>,
    /// Row boundaries into `nonterm_goto`.
    nonterm_offsets: Vec<usize>,
    /// Sorted `(nonterminal, goto)` pairs for all states concatenated together.
    nonterm_goto: Vec<(NonTerm, ShiftTarget<StateIndex>)>,
    /// Error-recovery capability for each state.
    can_accept_error: Vec<TriState>,
    /// Compact rule metadata indexed by reduce rule id.
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
        let mut term_offsets = Vec::with_capacity(intermediate.state_rows.len() + 1);
        let mut term_actions = Vec::new();
        let mut nonterm_offsets = Vec::with_capacity(intermediate.state_rows.len() + 1);
        let mut nonterm_goto = Vec::new();
        let mut can_accept_error = Vec::with_capacity(intermediate.state_rows.len());

        term_offsets.push(0);
        nonterm_offsets.push(0);

        for state in intermediate.state_rows {
            // Merge the separately serialized shift and reduce maps into one sorted action row.
            // The generated source stays compact, while the runtime table answers the hot-path
            // terminal lookup with a single probe.
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
                                    TermAction::ShiftReduce(shift, RuleContainer::from_set(rules)),
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
                        term_actions
                            .push((term, TermAction::Reduce(RuleContainer::from_set(rules))));
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
    /// Row boundaries into `term_actions`.
    term_offsets: Vec<usize>,
    /// Minimum terminal-class index represented by each dense terminal row.
    term_mins: Vec<usize>,
    /// Dense terminal action slots for all rows concatenated together.
    ///
    /// Each row only spans its own `min..=max` terminal range, preserving the existing auto-layout
    /// memory heuristic while still avoiding one allocation per state.
    term_actions: Vec<Option<TermAction<RuleContainer, StateIndex>>>,
    /// Row boundaries into `term_keys`, used for diagnostics and expected-token reporting.
    term_keys_offsets: Vec<usize>,
    /// Terminal classes that have a shift side in each row.
    term_keys: Vec<TermClass>,
    /// Row boundaries into `nonterm_goto`.
    nonterm_offsets: Vec<usize>,
    /// Minimum nonterminal index represented by each dense goto row.
    nonterm_mins: Vec<usize>,
    /// Dense nonterminal goto slots for all rows concatenated together.
    nonterm_goto: Vec<Option<ShiftTarget<StateIndex>>>,
    /// Row boundaries into `nonterm_keys`.
    nonterm_keys_offsets: Vec<usize>,
    /// Nonterminal keys present in each goto row.
    nonterm_keys: Vec<NonTerm>,
    /// Error-recovery capability for each state.
    can_accept_error: Vec<TriState>,
    /// Compact rule metadata indexed by reduce rule id.
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
        let mut term_offsets = Vec::with_capacity(intermediate.state_rows.len() + 1);
        let mut term_mins = Vec::with_capacity(intermediate.state_rows.len());
        let mut term_actions = Vec::new();
        let mut term_keys_offsets = Vec::with_capacity(intermediate.state_rows.len() + 1);
        let mut term_keys = Vec::new();

        let mut nonterm_offsets = Vec::with_capacity(intermediate.state_rows.len() + 1);
        let mut nonterm_mins = Vec::with_capacity(intermediate.state_rows.len());
        let mut nonterm_goto = Vec::new();
        let mut nonterm_keys_offsets = Vec::with_capacity(intermediate.state_rows.len() + 1);
        let mut nonterm_keys = Vec::new();
        let mut can_accept_error = Vec::with_capacity(intermediate.state_rows.len());

        term_offsets.push(0);
        term_keys_offsets.push(0);
        nonterm_offsets.push(0);
        nonterm_keys_offsets.push(0);

        for state in intermediate.state_rows {
            // Build a per-row dense terminal span, then append it to the shared backing vector.
            // This keeps direct indexing inside a row without paying for a full
            // `states * terminal_classes` matrix.
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
