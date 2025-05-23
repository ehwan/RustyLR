use super::State;
use crate::ProductionRule;

/// A trait for Parser that holds the entire parser table.
pub trait Parser {
    type Term;
    type NonTerm;
    type State: State<Self::NonTerm>;

    /// A type to represent single element in a terminal class.
    /// RangeInclusive<char> if the `Term` is a `char`, RangeInclusive<u8> if the `Term` is a `u8`.
    /// `&'static str` otherwise.
    type TerminalClassElement;

    /// Get list of production rules
    fn get_rules(&self) -> &[ProductionRule<&'static str, Self::NonTerm>];
    /// Get list of states
    fn get_states(&self) -> &[Self::State];
    /// Get set of terminals for i'th terminal class
    fn get_terminals(
        &self,
        i: usize,
    ) -> Option<impl IntoIterator<Item = Self::TerminalClassElement> + '_>;
    /// Get the terminal class of the given terminal
    fn to_terminal_class(&self, terminal: &Self::Term) -> usize;

    fn get_error_nonterm(&self) -> Option<Self::NonTerm>;
}
