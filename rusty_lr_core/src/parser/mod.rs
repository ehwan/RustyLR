/// Core parser functionality for deterministic parsers
pub mod deterministic;

/// Core parser functionality for non-deterministic parsers
pub mod nondeterministic;

pub mod semantic_value;

/// module for auto-generated types of non-terminals representation
pub mod nonterminal;

pub mod terminalclass;

pub mod state;
pub mod table;

/// Gets the `rusty_lr_parser` version expected by this runtime crate.
///
/// Generated parsers are compatible when the major and minor components match.
#[doc(hidden)]
pub fn __expected_rusty_lr_parser_version() -> (usize, usize, usize) {
    crate::versions::EXPECTED_RUSTY_LR_PARSER_VERSION
}

/// Asserts that generated code was emitted by a compatible `rusty_lr_parser`.
#[doc(hidden)]
pub fn __assert_rusty_lr_parser_version_compatible(
    emitted_parser: (usize, usize, usize),
    emitted_rustylr: (usize, usize, usize),
    emitted_rusty_lr: (usize, usize, usize),
) {
    let expected = __expected_rusty_lr_parser_version();
    if emitted_parser.0 != expected.0 || emitted_parser.1 != expected.1 {
        let rusty_lr = crate::versions::COMPATIBLE_RUSTY_LR_VERSION;
        let rustylr = crate::versions::COMPATIBLE_RUSTYLR_VERSION;
        if emitted_parser == (0, 0, 0) {
            panic!(
                "RustyLR generated parser version mismatch: this generated parser does not \
                 report a compatible generator version. Version compatibility is not satisfied. \
                 Use rusty_lr {}.{}.x, or re-emit the parser with rustylr {}.{}.x.",
                rusty_lr.0, rusty_lr.1, rustylr.0, rustylr.1
            );
        }
        if emitted_rustylr != (0, 0, 0) && emitted_rusty_lr != (0, 0, 0) {
            panic!(
                "RustyLR generated parser version mismatch: this generated parser was emitted \
                 by rustylr {}.{}.{}, targeting rusty_lr {}.{}.{}, but this runtime is \
                 compatible with parsers emitted by rustylr {}.{}.x. Version compatibility is \
                 not satisfied. Use rusty_lr {}.{}.x, or re-emit the parser with rustylr {}.{}.x.",
                emitted_rustylr.0,
                emitted_rustylr.1,
                emitted_rustylr.2,
                emitted_rusty_lr.0,
                emitted_rusty_lr.1,
                emitted_rusty_lr.2,
                rustylr.0,
                rustylr.1,
                emitted_rusty_lr.0,
                emitted_rusty_lr.1,
                rustylr.0,
                rustylr.1
            );
        }
        panic!(
            "RustyLR generated parser version mismatch: this generated parser was emitted \
             by an incompatible rustylr version. Version compatibility is not satisfied. \
             Use rusty_lr {}.{}.x, or re-emit the parser with rustylr {}.{}.x.",
            rusty_lr.0, rusty_lr.1, rustylr.0, rustylr.1
        );
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

    /// Gets the `rusty_lr_parser` version that emitted this parser.
    #[doc(hidden)]
    fn __rusty_lr_parser_version() -> (usize, usize, usize) {
        (0, 0, 0)
    }

    /// Gets the `rustylr` version compatible with the generator that emitted this parser.
    #[doc(hidden)]
    fn __rustylr_version() -> (usize, usize, usize) {
        (0, 0, 0)
    }

    /// Gets the `rusty_lr` version targeted by this generated parser.
    #[doc(hidden)]
    fn __rusty_lr_version() -> (usize, usize, usize) {
        (0, 0, 0)
    }

    /// Asserts that this generated parser is compatible with the current runtime.
    #[doc(hidden)]
    fn __assert_rusty_lr_parser_version_compatible() {
        __assert_rusty_lr_parser_version_compatible(
            Self::__rusty_lr_parser_version(),
            Self::__rustylr_version(),
            Self::__rusty_lr_version(),
        );
    }
}

#[cfg(test)]
mod tests {
    fn assert_compatible(
        emitted_parser: (usize, usize, usize),
        emitted_rustylr: (usize, usize, usize),
        emitted_rusty_lr: (usize, usize, usize),
    ) {
        super::__assert_rusty_lr_parser_version_compatible(
            emitted_parser,
            emitted_rustylr,
            emitted_rusty_lr,
        );
    }

    fn panic_message(
        emitted_parser: (usize, usize, usize),
        emitted_rustylr: (usize, usize, usize),
        emitted_rusty_lr: (usize, usize, usize),
    ) -> String {
        let result = std::panic::catch_unwind(|| {
            assert_compatible(emitted_parser, emitted_rustylr, emitted_rusty_lr)
        });
        let payload = result.expect_err("version check should panic");
        if let Some(message) = payload.downcast_ref::<String>() {
            message.clone()
        } else if let Some(message) = payload.downcast_ref::<&'static str>() {
            message.to_string()
        } else {
            panic!("unexpected panic payload type");
        }
    }

    #[test]
    fn parser_version_check_accepts_exact_match() {
        let expected = super::__expected_rusty_lr_parser_version();
        assert_compatible(
            expected,
            crate::versions::COMPATIBLE_RUSTYLR_VERSION,
            crate::versions::COMPATIBLE_RUSTY_LR_VERSION,
        );
    }

    #[test]
    fn parser_version_check_accepts_patch_difference() {
        let (major, minor, patch) = super::__expected_rusty_lr_parser_version();
        assert_compatible(
            (major, minor, patch + 1),
            crate::versions::COMPATIBLE_RUSTYLR_VERSION,
            crate::versions::COMPATIBLE_RUSTY_LR_VERSION,
        );
    }

    #[test]
    fn parser_version_check_accepts_matching_parser_with_missing_public_versions() {
        let expected = super::__expected_rusty_lr_parser_version();
        assert_compatible(expected, (0, 0, 0), (0, 0, 0));
    }

    #[test]
    fn parser_version_check_rejects_mismatched_major() {
        let (major, minor, patch) = super::__expected_rusty_lr_parser_version();
        let message = panic_message(
            (major + 1, minor, patch),
            crate::versions::COMPATIBLE_RUSTYLR_VERSION,
            crate::versions::COMPATIBLE_RUSTY_LR_VERSION,
        );
        assert!(message.contains("Version compatibility is not satisfied"));
        assert!(message.contains("targeting rusty_lr"));
    }

    #[test]
    fn parser_version_check_rejects_mismatched_minor() {
        let (major, minor, patch) = super::__expected_rusty_lr_parser_version();
        let message = panic_message(
            (major, minor + 1, patch),
            crate::versions::COMPATIBLE_RUSTYLR_VERSION,
            crate::versions::COMPATIBLE_RUSTY_LR_VERSION,
        );
        assert!(message.contains("Version compatibility is not satisfied"));
        assert!(message.contains("re-emit the parser with rustylr"));
    }

    #[test]
    fn parser_version_check_reports_emitted_public_versions() {
        let (major, minor, patch) = super::__expected_rusty_lr_parser_version();
        let emitted_rustylr = (1, 33, 0);
        let emitted_rusty_lr = (4, 2, 0);
        let message = panic_message((major, minor + 1, patch), emitted_rustylr, emitted_rusty_lr);
        assert!(message.contains("emitted by rustylr 1.33.0"));
        assert!(message.contains("targeting rusty_lr 4.2.0"));
        assert!(message.contains("Use rusty_lr 4.2.x"));
        assert!(message.contains("re-emit the parser with rustylr"));
    }

    #[test]
    fn parser_version_check_reports_missing_parser_version() {
        let message = panic_message((0, 0, 0), (0, 0, 0), (0, 0, 0));
        assert!(message.contains("does not report a compatible generator version"));
        assert!(message.contains("Use rusty_lr"));
        assert!(message.contains("re-emit the parser with rustylr"));
    }

    #[test]
    fn parser_version_check_handles_missing_public_versions_on_mismatch() {
        let (major, minor, patch) = super::__expected_rusty_lr_parser_version();
        let message = panic_message((major, minor + 1, patch), (0, 0, 0), (0, 0, 0));
        assert!(message.contains("incompatible rustylr version"));
        assert!(message.contains("Use rusty_lr"));
        assert!(message.contains("re-emit the parser with rustylr"));
    }
}
