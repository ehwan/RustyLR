use super::State;
use crate::ProductionRule;

/// A trait for Parser that holds the entire parser table.
pub trait Parser {
    type Term;
    type NonTerm;

    /// Get list of production rules
    fn get_rules(&self) -> &[ProductionRule<Self::Term, Self::NonTerm>];
    /// Get list of states
    fn get_states(&self) -> &[State<Self::Term, Self::NonTerm>];
}
