//! This crate provides a procedural macro to generate a parser from a grammar.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

use proc_macro::TokenStream;

/// Generate new structs for LR(1) or LALR(1) parser.
#[proc_macro]
pub fn lr1(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let g = match rusty_lr_parser::grammar::Grammar::parse(input) {
        Ok(grammar) => grammar,
        Err(e) => {
            return e.into();
        }
    };

    match g.emit_compiletime() {
        Ok(parser) => parser.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
