use crate::rule::ShiftedRuleRef;

use std::collections::BTreeMap;
use std::collections::BTreeSet;

pub struct DiagnosticCollector<Term> {
    pub enabled: bool,
    pub reduce_reduce_resolved: BTreeSet<(usize, BTreeSet<usize>, BTreeSet<usize>)>,
    pub shift_reduce_resolved_shift: BTreeMap<
        (Term, Vec<ShiftedRuleRef>),
        (
            usize, // shift precedence
            BTreeMap<usize, usize>,
            // (rule, reduce precedence)
        ),
    >,
    pub shift_reduce_resolved_reduce: BTreeMap<
        (Term, Vec<ShiftedRuleRef>),
        (
            usize, // shift precedence
            BTreeMap<usize, usize>,
            // (rule, reduce precedence)
        ),
    >,
    pub reduce_reduce_conflicts: BTreeMap<Vec<(usize, Vec<ShiftedRuleRef>)>, BTreeSet<Term>>,
    pub shift_reduce_conflicts: BTreeMap<
        (Term, Vec<ShiftedRuleRef>, Vec<ShiftedRuleRef>),
        BTreeMap<usize, Vec<ShiftedRuleRef>>,
    >,
}
impl<Term> DiagnosticCollector<Term> {
    pub fn new(collect: bool) -> Self {
        DiagnosticCollector {
            enabled: collect,
            reduce_reduce_resolved: BTreeSet::new(),
            shift_reduce_resolved_shift: BTreeMap::new(),
            shift_reduce_resolved_reduce: BTreeMap::new(),
            shift_reduce_conflicts: BTreeMap::new(),
            reduce_reduce_conflicts: BTreeMap::new(),
        }
    }
    pub fn add_reduce_reduce_resolved(
        &mut self,
        max_priority: usize,
        reduce_rules: BTreeSet<usize>,
        removed_rules: BTreeSet<usize>,
    ) where
        Term: Ord,
    {
        if self.enabled {
            self.reduce_reduce_resolved
                .insert((max_priority, reduce_rules, removed_rules));
        }
    }
    pub fn add_shift_reduce_resolved_shift(
        &mut self,
        term: Term,
        shift_rules: Vec<ShiftedRuleRef>,
        shift_precedence: usize,
        mut reduce_rules: BTreeMap<usize, usize>,
    ) where
        Term: Ord,
    {
        if self.enabled {
            let value = self
                .shift_reduce_resolved_shift
                .entry((term, shift_rules))
                .or_default();
            value.0 = shift_precedence;
            value.1.append(&mut reduce_rules);
        }
    }
    pub fn add_shift_reduce_resolved_reduce(
        &mut self,
        term: Term,
        shift_rules: Vec<ShiftedRuleRef>,
        shift_precedence: usize,
        mut reduce_rules: BTreeMap<usize, usize>,
    ) where
        Term: Ord,
    {
        if self.enabled {
            let value = self
                .shift_reduce_resolved_reduce
                .entry((term, shift_rules))
                .or_default();
            value.0 = shift_precedence;
            value.1.append(&mut reduce_rules);
        }
    }
    pub fn add_shift_reduce_conflict(
        &mut self,
        term: Term,
        shift_rules: Vec<ShiftedRuleRef>,
        shift_rules_backtrace: Vec<ShiftedRuleRef>,
        mut reduce_rules: BTreeMap<usize, Vec<ShiftedRuleRef>>,
    ) where
        Term: Ord,
    {
        if self.enabled {
            self.shift_reduce_conflicts
                .entry((term, shift_rules, shift_rules_backtrace))
                .or_default()
                .append(&mut reduce_rules);
        }
    }
    pub fn update_reduce_reduce_conflict(
        &mut self,
        reduce_rules: Vec<(usize, Vec<ShiftedRuleRef>)>,
        term: Term,
    ) where
        Term: Ord,
    {
        if self.enabled {
            self.reduce_reduce_conflicts
                .entry(reduce_rules)
                .or_default()
                .insert(term);
        }
    }
}
