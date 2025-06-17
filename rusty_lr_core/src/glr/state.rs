use std::hash::Hash;

use crate::HashMap;
use crate::ShiftedRuleRef;

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
    fn expected_nonterm<'a>(&'a self) -> impl Iterator<Item = &'a NonTerm> + 'a
    where
        NonTerm: 'a;

    /// Get the set of rules that this state is trying to parse
    fn get_rules(&self) -> &[ShiftedRuleRef];
}

pub trait ToUsizeList {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone;
}
use smallvec::SmallVec;
impl ToUsizeList for SmallVec<[usize; 2]> {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().copied()
    }
}
impl ToUsizeList for SmallVec<[u32; 4]> {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl ToUsizeList for SmallVec<[u16; 8]> {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl ToUsizeList for SmallVec<[u8; 16]> {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}

/// `State` implementation for a sparse state representation using HashMap
#[derive(Debug, Clone)]
pub struct SparseState<NonTerm, RuleVec> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: HashMap<usize, usize>,
    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: HashMap<usize, RuleVec>,
    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<ShiftedRuleRef>,
}
impl<NonTerm, RuleIndex: ToUsizeList> State<NonTerm> for SparseState<NonTerm, RuleIndex> {
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
        self.reduce_map.get(&class).map(ToUsizeList::to_usize_list)
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
    fn expected_nonterm<'a>(&'a self) -> impl Iterator<Item = &'a NonTerm> + 'a
    where
        NonTerm: 'a,
    {
        self.shift_goto_map_nonterm.keys()
    }
    fn get_rules(&self) -> &[ShiftedRuleRef] {
        &self.ruleset
    }
}

/// `State` implementation for a dense state representation using Vec
#[derive(Debug, Clone)]
pub struct DenseState<NonTerm, RuleVec> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: Vec<Option<usize>>,
    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: Vec<Option<RuleVec>>,
    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<ShiftedRuleRef>,
}
impl<NonTerm, RuleVec: ToUsizeList> State<NonTerm> for DenseState<NonTerm, RuleVec> {
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
            .map(ToUsizeList::to_usize_list)
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
    fn expected_nonterm<'a>(&'a self) -> impl Iterator<Item = &'a NonTerm> + 'a
    where
        NonTerm: 'a,
    {
        self.shift_goto_map_nonterm.keys()
    }
    fn get_rules(&self) -> &[ShiftedRuleRef] {
        &self.ruleset
    }
}
