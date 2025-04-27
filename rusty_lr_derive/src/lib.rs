//! This crate provides a procedural macro to generate a parser from a grammar.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

use proc_macro::TokenStream;

/// Build a LR(1) Deterministic Finite Automaton (DFA) parser.
///
/// This macro will generate a `Parser` and `Context` structs.
#[proc_macro]
pub fn lr1(input: TokenStream) -> TokenStream {
    let input = input.into();
    use rusty_lr_parser::grammar::Grammar;
    let grammar_args = match Grammar::parse_args(input) {
        Ok(grammar_args) => grammar_args,
        Err(e) => return e.to_compile_error().into(),
    };
    match Grammar::arg_check_error(&grammar_args) {
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
    grammar.build_grammar_without_resolve();
    grammar.resolve_precedence();
    if let Err(e) = grammar.conflict() {
        return e.to_compile_error().into();
    }
    grammar.emit_compiletime().into()
}
