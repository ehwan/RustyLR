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
    type Term: 'static;
    /// The type of terminal classes.
    type TermClass: terminalclass::TerminalClass<Term = Self::Term> + 'static;
    /// The type of non-terminal symbols.
    type NonTerm: nonterminal::NonTerminal + 'static;
    /// The type of the parser state.
    type State: State<TermClass = Self::TermClass, NonTerm = Self::NonTerm> + 'static;

    /// Get list of production rules. Returns a static slice since rules are generated at compile time.
    fn get_rules() -> &'static [crate::rule::ProductionRule<Self::TermClass, Self::NonTerm>];
    /// Get list of states. Returns a static slice since states are generated at compile time.
    fn get_states() -> &'static [Self::State];
    /// Get the type of precedence for i'th level.
    /// `None` if i'th level was defined as %precedence (no reduce type).
    /// Determined entirely at compile time and is static.
    fn precedence_types(level: u8) -> Option<crate::rule::ReduceType>;
}
