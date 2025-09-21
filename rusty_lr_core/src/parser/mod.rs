/// Core parser functionality for deterministic parsers
pub mod deterministic;

/// Core parser functionality for non-deterministic parsers
pub mod nondeterministic;

pub mod data_stack;

/// module for auto-generated types of non-terminals representation
pub mod nonterminal;

pub mod terminalclass;

pub mod state;
pub use state::State;

#[derive(Clone, Copy)]
pub struct Precedence(u8);

impl Precedence {
    #[inline]
    pub fn none() -> Self {
        Precedence(u8::MAX)
    }
    #[inline]
    pub fn new(level: u8) -> Self {
        debug_assert!(level < u8::MAX);
        Precedence(level)
    }
    #[inline]
    pub fn is_some(&self) -> bool {
        self.0 < u8::MAX
    }

    pub fn unwrap(self) -> u8 {
        debug_assert!(self.0 < u8::MAX);
        self.0
    }
}

/// A trait for Parser that holds the entire parser table.
/// This trait will be automatically implemented by rusty_lr
pub trait Parser {
    /// whether the `error` token was used in the grammar.
    const ERROR_USED: bool;

    /// The type of terminal symbols.
    type Term;
    /// The type of terminal classes.
    type TermClass: terminalclass::TerminalClass<Term = Self::Term>;
    /// The type of non-terminal symbols.
    type NonTerm: nonterminal::NonTerminal;
    /// The type of the parser state.
    type State: State<TermClass = Self::TermClass, NonTerm = Self::NonTerm>;

    /// A type to represent single element in a terminal class.
    ///
    /// `RangeInclusive<char>` if the `Term` is a `char`, `RangeInclusive<u8>` if the `Term` is a `u8`.
    /// `&'static str` otherwise.
    type TerminalClassElement;

    /// Get list of production rules
    fn get_rules(&self) -> &[crate::rule::ProductionRule<Self::TermClass, Self::NonTerm>];
    /// Get list of states
    fn get_states(&self) -> &[Self::State];
    /// Get the type of precedence for i'th level.
    /// `None` if i'th level was defined as %precedence (no reduce type).
    fn precedence_types(&self, level: u8) -> Option<crate::rule::ReduceType>;
}
