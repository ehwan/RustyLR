/// Core parser functionality for deterministic parsers
pub mod deterministic;

/// Core parser functionality for non-deterministic parsers
pub mod nondeterministic;

mod state;
pub use state::State;
