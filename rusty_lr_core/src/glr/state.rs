use std::hash::Hash;

use crate::hash::HashMap;

/// A trait representing a parser state.
pub trait State<NonTerm> {
    /// Get the next state for a given terminal symbol.
    fn shift_goto_class(&self, class: usize) -> Option<usize>;

    /// Get the next state for a given non-terminal symbol.
    fn shift_goto_nonterm(&self, nonterm: &NonTerm) -> Option<usize>
    where
        NonTerm: Hash + Eq;

    /// Get the reduce rule index for a given terminal symbol.
    fn reduce(&self, class: usize) -> Option<impl Iterator<Item = usize> + Clone + '_>;

    /// Check if this state is an accept state.
    fn is_accept(&self) -> bool;

    /// Get the set of expected classes for this state
    fn expected(&self) -> impl Iterator<Item = usize> + '_;

    /// Get the set of expected non-terminal symbols for this state
    fn expected_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_;

    /// Get the set of rules that this state is trying to parse
    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef];
}

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

impl<NonTerm: Copy, RuleIndex: crate::stackvec::ToUsizeList> State<NonTerm>
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
    fn expected(&self) -> impl Iterator<Item = usize> + '_ {
        self.shift_goto_map_class
            .keys()
            .chain(self.reduce_map.keys())
            .copied()
    }
    fn expected_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_ {
        self.shift_goto_map_nonterm.keys().copied()
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
    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: Vec<Option<RuleContainer>>,
    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}
impl<NonTerm: Copy, RuleContainer: crate::stackvec::ToUsizeList> State<NonTerm>
    for DenseState<NonTerm, RuleContainer>
{
    fn shift_goto_class(&self, class: usize) -> Option<usize> {
        self.shift_goto_map_class[class]
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
    fn expected(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.shift_goto_map_class.len())
            .filter(|&i| self.shift_goto_map_class[i].is_some() || self.reduce_map[i].is_some())
    }
    fn expected_nonterm(&self) -> impl Iterator<Item = NonTerm> + '_ {
        self.shift_goto_map_nonterm.keys().copied()
    }
    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef] {
        &self.ruleset
    }
}
