use crate::ReduceType;
use crate::ShiftedRuleRef;

use std::collections::BTreeMap;
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ResolveDiagnostic<Term> {
    Priority {
        max_priority: usize,
        remaining: Vec<usize>,
        deleted: Vec<usize>,
    },
    Precedence {
        term: Term,
        shift_precedence: usize,
        shift_deleted: bool,
        shift_rules: Vec<ShiftedRuleRef>,

        // rule, precedence, reduce_type
        reduce_rules: Vec<(usize, Option<usize>, Option<ReduceType>)>,
        deleted_reduces: Vec<(usize, usize, Option<ReduceType>)>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ShiftReduceConflictDiag<Term> {
    pub term: Term,
    pub shift_rules: Vec<ShiftedRuleRef>,
    pub reduce_rules: Vec<(usize, Vec<ShiftedRuleRef>)>,
}

pub struct DiagnosticCollector<Term> {
    pub enabled: bool,
    pub resolved: BTreeSet<ResolveDiagnostic<Term>>,
    pub shift_reduce_conflicts: BTreeSet<ShiftReduceConflictDiag<Term>>,
    pub reduce_reduce_conflicts: BTreeMap<Vec<(usize, Vec<ShiftedRuleRef>)>, BTreeSet<Term>>,
}
impl<Term> DiagnosticCollector<Term> {
    pub fn new(collect: bool) -> Self {
        DiagnosticCollector {
            enabled: collect,
            resolved: BTreeSet::new(),
            shift_reduce_conflicts: BTreeSet::new(),
            reduce_reduce_conflicts: BTreeMap::new(),
        }
    }
    pub fn add_resolved(&mut self, resolved: ResolveDiagnostic<Term>)
    where
        Term: Ord,
    {
        if self.enabled {
            self.resolved.insert(resolved);
        }
    }
    pub fn add_shift_reduce_conflict(&mut self, diag: ShiftReduceConflictDiag<Term>)
    where
        Term: Ord,
    {
        if self.enabled {
            self.shift_reduce_conflicts.insert(diag);
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
