// constants and utility functions for macro-generation

use super::error::ParseError;
use proc_macro2::Ident;

pub static AUGMENTED_NAME: &str = "Augmented";
pub static EOF_NAME: &str = "eof";
pub static USER_DATA_PARAMETER_NAME: &str = "data";
pub static TERMINAL_STACK_NAME: &str = "__rustylr_generated_terminal_stack";
pub static AUTO_GENERATED_RULE_PREFIX: &str = "_RustyLRGenerated";

/// for non-terminal symbol 'rule_name', generate the name of the enum
pub(crate) fn generate_enum_name(rule_name: &Ident) -> Ident {
    Ident::new(&format!("{}NonTerminals", rule_name), rule_name.span())
}

/// check if the given identifier is reserved name
pub(crate) fn check_reserved_name(ident: &Ident) -> Result<(), ParseError> {
    if ident == AUGMENTED_NAME {
        return Err(ParseError::AugmentedDefined(ident.clone()));
    }
    if ident == EOF_NAME {
        return Err(ParseError::EofDefined(ident.clone()));
    }
    if ident.to_string().starts_with(AUTO_GENERATED_RULE_PREFIX) {
        return Err(ParseError::ReservedName(ident.clone()));
    }
    Ok(())
}
