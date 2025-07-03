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

impl<NonTerm> From<crate::builder::State<usize, NonTerm>> for SparseState<NonTerm>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        crate::parser::deterministic::state::SparseState {
            shift_goto_map_class: builder_state.shift_goto_map_term.into_iter().collect(),
            shift_goto_map_nonterm: builder_state.shift_goto_map_nonterm.into_iter().collect(),
            reduce_map: builder_state
                .reduce_map
                .into_iter()
                .map(|(term, rule)| (term, rule.into_iter().next().unwrap()))
                .collect(),
            ruleset: builder_state.ruleset.into_iter().collect(),
        }
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
        self.shift_goto_map_class.get(class).copied().flatten()
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
            .copied()
            .flatten()
            .map(std::iter::once)
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
impl<NonTerm> From<crate::builder::State<usize, NonTerm>> for DenseState<NonTerm>
where
    NonTerm: Hash + Eq,
{
    fn from(builder_state: crate::builder::State<usize, NonTerm>) -> Self {
        let shift_term_len = builder_state
            .shift_goto_map_term
            .keys()
            .next_back()
            .copied()
            .map(|x| x + 1)
            .unwrap_or(0);
        let mut shift_goto_map_class = vec![None; shift_term_len];

        let reduce_term_len = builder_state
            .reduce_map
            .keys()
            .next_back()
            .copied()
            .map(|x| x + 1)
            .unwrap_or(0);
        let mut reduce_map = vec![None; reduce_term_len];
        for (term, state) in builder_state.shift_goto_map_term {
            shift_goto_map_class[term] = Some(state);
        }
        for (term, rule) in builder_state.reduce_map {
            reduce_map[term] = Some(rule.into_iter().next().unwrap());
        }
        crate::parser::deterministic::state::DenseState {
            shift_goto_map_class,
            shift_goto_map_nonterm: builder_state.shift_goto_map_nonterm.into_iter().collect(),
            reduce_map,
            ruleset: builder_state.ruleset.into_iter().collect(),
        }
    }
}
