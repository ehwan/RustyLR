// constants and utility functions for macro-generation

use crate::parser::location::Location;
use proc_macro2::Ident;

pub static AUGMENTED_NAME: &str = "Augmented";
pub static EOF_NAME: &str = "eof";
pub static ERROR_NAME: &str = "error";
pub static USER_DATA_PARAMETER_NAME: &str = "data";
pub static TERMINAL_STACK_NAME: &str = "__rustylr_generated_terminal_stack";
pub static OTHERS_TERMINAL_NAME: &str = "__rustylr_other_terminals";
pub static LOOKAHEAD_PARAMETER_NAME: &str = "lookahead";

pub(crate) fn location_variable_name(varname: &str) -> String {
    format!("__rustylr_location_{}", varname)
}

/// Build an Ident from a string and Location using the SpanManager.
pub(crate) fn ident_from_located(
    name: &str,
    location: &Location,
    span_manager: &crate::parser::location::SpanManager,
) -> Ident {
    let span = span_manager.get_span_in_location(location);
    Ident::new(name, span)
}

pub(crate) fn has_box_prefix(ts: &proc_macro2::TokenStream) -> bool {
    let mut iter = ts.clone().into_iter();
    if let Some(proc_macro2::TokenTree::Ident(ident)) = iter.next() {
        ident.to_string() == "box" && iter.next().is_some()
    } else {
        false
    }
}

pub(crate) fn strip_box_prefix(ts: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    if has_box_prefix(&ts) {
        let mut iter = ts.into_iter();
        let _ = iter.next(); // Consume 'box'
        iter.collect()
    } else {
        ts
    }
}

pub(crate) fn remove_whitespaces(s: String) -> String {
    s.chars().filter(|c| !c.is_whitespace()).collect()
}

