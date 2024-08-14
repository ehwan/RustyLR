use crate::rule::LookaheadRuleRefSet;

/// Error type for building grammar
pub enum BuildError<Term, NonTerm> {
    RuleNotFound(NonTerm),

    ReduceReduceConflict {
        lookahead: Term,
        rule1: usize,
        rule2: usize,
    },

    /// shift/reduce conflict
    ShiftReduceConflict {
        reduce: usize,
        shift: LookaheadRuleRefSet<Term>,
        term: Term,
    },

    NoAugmented,
}
