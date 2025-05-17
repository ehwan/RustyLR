//! This crate provides a procedural macro to generate a parser from a grammar.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

use proc_macro::TokenStream;
use quote::quote;

/// Build a LR(1) Deterministic Finite Automaton (DFA) parser.
///
/// This macro will generate a `Parser` and `Context` structs.
#[proc_macro]
pub fn lr1(input: TokenStream) -> TokenStream {
    let input = input.into();
    use rusty_lr_parser::grammar::Grammar;
    let mut grammar_args = match Grammar::parse_args(input) {
        Ok(grammar_args) => grammar_args,
        Err(e) => return e.to_compile_error().into(),
    };
    match Grammar::arg_check_error(&mut grammar_args) {
        Ok(_) => {}
        Err(e) => return e.to_compile_error().into(),
    }
    let mut grammar = match Grammar::from_grammar_args(grammar_args) {
        Ok(grammar) => grammar,
        Err(e) => return e.to_compile_error().into(),
    };
    if grammar.optimize {
        grammar.optimize(5);
    }
    grammar.builder = grammar.create_builder();
    let diags = grammar.build_grammar();
    if !grammar.glr {
        if let Some(((term, shift_rules, _), reduce_rules)) =
            diags.shift_reduce_conflicts.into_iter().next()
        {
            let class_mapper = |term| grammar.class_pretty_name_list(term, 5);
            let nonterm_mapper = |term| grammar.nonterm_pretty_name(term);
            let term = class_mapper(term);
            let (reduce_rule, _) = reduce_rules.into_iter().next().unwrap();
            let reduce_rule = grammar.builder.rules[reduce_rule]
                .rule
                .clone()
                .map(class_mapper, nonterm_mapper);
            let shift_rules = shift_rules
                .into_iter()
                .map(|rule| {
                    format!(
                        "\n>>> {}",
                        grammar.builder.rules[rule.rule]
                            .rule
                            .clone()
                            .map(class_mapper, nonterm_mapper)
                            .into_shifted(rule.shifted)
                    )
                })
                .collect::<Vec<_>>()
                .join("");

            let message = format!(
                "Shift-Reduce conflict with terminal symbol: {}\n>>> Reduce: {}\n>>> Shifts: {}",
                term, reduce_rule, shift_rules
            );
            return quote! {
                compile_error!(#message);
            }
            .into();
        }
        if let Some((reduce_rules, reduce_terms)) = diags.reduce_reduce_conflicts.into_iter().next()
        {
            let class_mapper = |term| grammar.class_pretty_name_list(term, 5);
            let nonterm_mapper = |term| grammar.nonterm_pretty_name(term);
            let terms = reduce_terms
                .into_iter()
                .map(&class_mapper)
                .collect::<Vec<_>>()
                .join(", ");
            let reduce_rules = reduce_rules
                .into_iter()
                .map(|(rule, _)| {
                    format!(
                        "\n>>> {}",
                        grammar.builder.rules[rule]
                            .rule
                            .clone()
                            .map(class_mapper, nonterm_mapper)
                    )
                })
                .collect::<Vec<_>>()
                .join("");

            let message = format!(
                "Reduce-Reduce conflict with terminal symbols: {}\n>>> Reduce: {}",
                terms, reduce_rules
            );
            return quote! {
                compile_error!(#message);
            }
            .into();
        }
    }

    grammar.emit_compiletime().into()
}
