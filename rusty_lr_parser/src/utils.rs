// constants and utility functions for macro-generation

use proc_macro2::Ident;

pub static AUGMENTED_NAME: &str = "Augmented";
pub static EOF_NAME: &str = "eof";

/// for non-terminal symbol 'rule_name', generate the name of the stack
pub fn generate_stack_name(rule_name: &Ident) -> Ident {
    Ident::new(
        &format!("rustylr_macro_generated_{}_stack", rule_name),
        rule_name.span(),
    )
}

/// for non-terminal symbol 'rule_name', generate the name of the enum
pub fn generate_enum_name(rule_name: &Ident) -> Ident {
    Ident::new(&format!("{}NonTerminals", rule_name), rule_name.span())
}

pub fn generate_plus_rule_name(rule_name: &Ident) -> Ident {
    Ident::new(&format!("{}Plus", rule_name), rule_name.span())
}
pub fn generate_star_rule_name(rule_name: &Ident) -> Ident {
    Ident::new(&format!("{}Star", rule_name), rule_name.span())
}
pub fn generate_question_rule_name(rule_name: &Ident) -> Ident {
    Ident::new(&format!("{}Question", rule_name), rule_name.span())
}
