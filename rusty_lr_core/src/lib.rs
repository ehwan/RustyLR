//! Core module for the Rusty LR parser.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

pub(crate) mod error;
pub(crate) mod hashmap;
pub(crate) mod rule;
pub(crate) mod state;
pub(crate) mod token;

/// module for build DFA tables from CFG
#[cfg(feature = "builder")]
pub mod builder;

pub use hashmap::HashMap;
pub use hashmap::HashSet;

pub use error::GetContext;
pub use error::GetParser;

pub use rule::ProductionRule;

pub use rule::LookaheadRule;
pub use rule::LookaheadRuleRefSet;
pub use rule::ShiftedRule;
pub use rule::ShiftedRuleRef;

pub use rule::ReduceType;
pub use state::State;
pub use token::Token;

pub use error::DefaultReduceActionError;
pub use error::InvalidTerminalError;
pub use error::ParseError;
