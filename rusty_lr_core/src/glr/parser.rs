use crate::glr::State;
use crate::ProductionRule;

/// Parser trait for GLR parser
pub trait Parser {
    type Term;
    type NonTerm;

    /// get list of production rules
    fn get_rules(&self) -> &[ProductionRule<Self::Term, Self::NonTerm>];
    /// get list of states
    fn get_states(&self) -> &[State<Self::Term, Self::NonTerm>];
}
