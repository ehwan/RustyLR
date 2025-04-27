use crate::glr::State;
use crate::ProductionRule;

/// A trait for Parser that holds the entire parser table.
pub trait Parser {
    type Term;
    type NonTerm;

    /// Get list of production rules
    fn get_rules(&self) -> &[ProductionRule<usize, Self::NonTerm>];
    /// Get list of states
    fn get_states(&self) -> &[State<usize, Self::NonTerm>];
    /// Get set of terminals for i'th terminal class
    fn get_terminals(&self, i: usize) -> Option<impl Iterator<Item = &Self::Term>>;
    /// Get the terminal class of the given terminal
    fn to_terminal_class(&self, terminal: &Self::Term) -> usize;
}
