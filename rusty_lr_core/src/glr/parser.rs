use super::State;
use crate::ProductionRule;

/// A trait for Parser that holds the entire parser table.
pub trait Parser {
    type Term;
    type NonTerm;
    type State: State<Self::NonTerm>;

    /// either `Term` or `&Term` for return type
    type TermRet<'a>
    where
        Self: 'a;

    /// Get list of production rules
    fn get_rules(&self) -> &[ProductionRule<usize, Self::NonTerm>];
    /// Get list of states
    fn get_states(&self) -> &[Self::State];
    /// Get set of terminals for i'th terminal class
    fn get_terminals<'a>(
        &'a self,
        i: usize,
    ) -> Option<impl IntoIterator<Item = Self::TermRet<'a>> + 'a>;
    /// Get the terminal class of the given terminal
    fn to_terminal_class(&self, terminal: &Self::Term) -> usize;

    fn get_error_nonterm(&self) -> Option<Self::NonTerm>;
}
