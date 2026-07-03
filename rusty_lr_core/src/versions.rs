//! Centralized RustyLR compatibility version declarations.

pub type Version = (usize, usize, usize);

/// The `rusty_lr` runtime version that generated parsers should target.
pub const COMPATIBLE_RUSTY_LR_VERSION: Version = (4, 5, 0);

/// The `rustylr` executable version that should emit compatible parser code.
pub const COMPATIBLE_RUSTYLR_VERSION: Version = (1, 36, 0);

/// The internal parser generator version expected by this runtime.
pub const EXPECTED_RUSTY_LR_PARSER_VERSION: Version = (4, 5, 0);

/// The runtime version reported by code generators for generated output.
pub const TARGET_RUSTY_LR_VERSION: Version = COMPATIBLE_RUSTY_LR_VERSION;
