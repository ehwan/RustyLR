//! # RustyLR
//! LR(1) Parser generator in Rust
//!
//! ## Features
//!  - pure Rust implementation.
//!  - DFA construction from CFG.
//!  - conflict resolution
//!  - tracing parser action with callback, also error handling.
//!  - construct Tree from parsing result.

pub(crate) mod grammar;
pub(crate) mod parser;
pub(crate) mod rule;
pub(crate) mod state;
pub(crate) mod token;

// reexport

/// An enum for resolving shift/reduce conflict
pub use rule::ReduceType;
/// A struct for state in DFA
pub use state::State;
/// An enum for terminal and non-terminal symbols
pub use token::Token;

/// Error type for LR(1) DFA construction
pub use grammar::grammar::BuildError;

/// A struct for Context Free Grammar and DFA construction
pub use grammar::grammar::Grammar;

/// callback trait for tracing the parsing process
pub use parser::callback::Callback;
/// callback trait for tracing the parsing process (for `&str`)
pub use parser::callback::CallbackStr;
/// A struct for parsing context
pub use parser::context::Context;
/// A struct for parsing context (for `&str`)
pub use parser::context::ContextStr;
/// Error type for parsing
pub use parser::parser::ParseError;
/// A struct for LR parser
pub use parser::parser::Parser;
/// A tree struct for result of parsing
pub use parser::tree::Tree;
/// A tree struct for result of parsing (for `&str`)
pub use parser::tree::TreeStr;
