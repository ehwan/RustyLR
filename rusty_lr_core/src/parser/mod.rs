/// Core parser functionality for deterministic parsers
pub mod deterministic;

/// Core parser functionality for non-deterministic parsers
pub mod nondeterministic;

pub mod data_stack;

/// module for auto-generated types of non-terminals representation
pub mod nonterminal;

pub mod terminalclass;

pub mod state;
pub mod table;

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
    /// The compact integer type used for state indices.
    type StateIndex: table::Index + 'static;
    /// The reduce-rule container for a terminal action.
    type ReduceRules: table::ReduceRules + 'static;
    /// The type of the parser table.
    type Tables: table::ParserTables<
            TermClass = Self::TermClass,
            NonTerm = Self::NonTerm,
            StateIndex = Self::StateIndex,
            ReduceRules = Self::ReduceRules,
        > + 'static;

    /// Get the decoded runtime parser tables.
    fn get_tables() -> &'static Self::Tables;
}
