/// Core parser functionality for deterministic parsers
pub mod deterministic;

/// Core parser functionality for non-deterministic parsers
pub mod nondeterministic;

pub mod state;
pub use state::State;

/// A trait for Parser that holds the entire parser table.
/// This trait will be automatically implemented by rusty_lr
pub trait Parser {
    /// The type of terminal symbols.
    type Term;
    /// The type of non-terminal symbols.
    type NonTerm;
    /// The type of the parser state.
    type State: State<Self::NonTerm>;

    /// A type to represent single element in a terminal class.
    ///
    /// `RangeInclusive<char>` if the `Term` is a `char`, `RangeInclusive<u8>` if the `Term` is a `u8`.
    /// `&'static str` otherwise.
    type TerminalClassElement;

    /// Get list of production rules
    fn get_rules(&self) -> &[crate::rule::ProductionRule<&'static str, Self::NonTerm>];
    /// Get list of states
    fn get_states(&self) -> &[Self::State];
    /// Get set of terminals for i'th terminal class
    fn get_terminals(
        &self,
        i: usize,
    ) -> Option<impl IntoIterator<Item = Self::TerminalClassElement> + '_>;
    /// Get the terminal class of the given terminal
    fn to_terminal_class(&self, terminal: &Self::Term) -> usize;

    /// Get the type of precedence for i'th level.
    /// `None` if i'th level was defined as %precedence (no reduce type).
    fn precedence_types(&self, level: usize) -> Option<crate::builder::ReduceType>;

    /// Get the precedence level (priority) of the given terminal class
    /// `None` if the terminal class has no precedence defined.
    fn class_precedence(&self, class: crate::TerminalSymbol<usize>) -> Option<usize>;

    /// whether the `error` token was used in the grammar.
    fn error_used() -> bool;
}
