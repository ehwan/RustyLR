// constants and utility functions for macro-generation

use super::error::ParseError;
use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;

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

pub(crate) fn tokenstream_contains_ident(stream: TokenStream, ident: &Ident) -> bool {
    for t in stream {
        match t {
            proc_macro2::TokenTree::Ident(i) if &i == ident => return true,
            proc_macro2::TokenTree::Group(g) => {
                if tokenstream_contains_ident(g.stream(), ident) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

pub(crate) fn location_variable_name(varname: &Ident) -> Ident {
    format_ident!("__rustylr_location_{}", varname)
}
