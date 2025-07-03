use std::hash::Hash;

use crate::hash::HashMap;

/// `State` implementation for a sparse state representation using HashMap
#[derive(Debug, Clone)]
pub struct SparseState<NonTerm> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: HashMap<usize, usize>,
    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: HashMap<usize, usize>,
    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}
impl<NonTerm: Copy> crate::parser::State<NonTerm> for SparseState<NonTerm> {
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
        self.reduce_map.get(&class).copied().map(std::iter::once)
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
        self.reduce_map.values().copied()
    }

    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef] {
        &self.ruleset
    }
}

/// `State` implementation for a dense state representation using Vec
#[derive(Debug, Clone)]
pub struct DenseState<NonTerm> {
    /// terminal symbol -> next state
    pub(crate) shift_goto_map_class: Vec<Option<usize>>,
    /// non-terminal symbol -> next state
    pub(crate) shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    /// terminal symbol -> reduce rule index
    pub(crate) reduce_map: Vec<Option<usize>>,
    /// set of rules that this state is trying to parse
    pub(crate) ruleset: Vec<crate::rule::ShiftedRuleRef>,
}
impl<NonTerm: Copy> crate::parser::State<NonTerm> for DenseState<NonTerm> {
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
        self.reduce_map[class].map(std::iter::once)
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
        self.reduce_map.iter().filter_map(|&r| r)
    }

    fn get_rules(&self) -> &[crate::rule::ShiftedRuleRef] {
        &self.ruleset
    }
}
