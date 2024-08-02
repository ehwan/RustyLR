//! # RustyLR
//! LR(1) Parser generator in Rust
//!
//! ## Features
//!  - pure Rust implementation
//!  - compile-time DFA construction from CFGs ( with proc-macro )
//!  - customizable reducing action
//!  - resolving conflicts of ambiguous grammar
//!  - tracing parser action with callback, also error handling
//!  - readable error messages, both for grammar building and parsing

pub(crate) mod grammar;
pub(crate) mod parser;
pub(crate) mod rule;
pub(crate) mod state;
pub(crate) mod token;

// reexport

/// A struct for production rule
pub use rule::ProductionRule;

pub use rule::LookaheadRuleRefSet;
pub use rule::ShiftedRuleRef;

/// An enum for resolving shift/reduce conflict
pub use rule::ReduceType;
/// A struct for state in DFA
pub use state::State;
/// An enum for terminal and non-terminal symbols
pub use token::Token;

/// Error type for LR(1) DFA construction
pub use grammar::error::BuildError;

/// A struct for Context Free Grammar and DFA construction
pub use grammar::grammar::Grammar;

/// callback trait for tracing the parsing process
pub use parser::callback::Callback;
/// default callback that does nothing
pub use parser::callback::DefaultCallback;
/// A struct for parsing context
pub use parser::context::Context;
/// Error type for parsing slice
pub use parser::error::ParseError;
/// A struct for LR parser
pub use parser::parser::Parser;
