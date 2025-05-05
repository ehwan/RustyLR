// constants and utility functions for macro-generation

use super::error::ParseError;
use proc_macro2::Ident;

pub static AUGMENTED_NAME: &str = "Augmented";
pub static EOF_NAME: &str = "eof";
pub static ERROR_NAME: &str = "error";
pub static USER_DATA_PARAMETER_NAME: &str = "data";
pub static TERMINAL_STACK_NAME: &str = "__rustylr_generated_terminal_stack";
pub static OTHERS_TERMINAL_NAME: &str = "__rustylr_other_terminals";

/// check if the given identifier is reserved name
pub(crate) fn check_reserved_name(ident: &Ident) -> Result<(), ParseError> {
    if ident == AUGMENTED_NAME {
        return Err(ParseError::ReservedName(ident.clone()));
    }
    if ident == EOF_NAME {
        return Err(ParseError::ReservedName(ident.clone()));
    }
    if ident == ERROR_NAME {
        return Err(ParseError::ReservedName(ident.clone()));
    }
    Ok(())
}
