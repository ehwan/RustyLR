use proc_macro::TokenStream;

mod callback;
mod emit;
mod error;
mod grammar;
mod rule;
mod term;
mod token;
mod tokenizer;

/// build a lr1 Deterministic Finite Automaton (DFA) parser
#[proc_macro]
pub fn lr1(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let g = match grammar::Grammar::parse(input) {
        Ok(grammar) => grammar,
        Err(e) => return e.compile_error().into(),
    };

    match g.emit(false) {
        Ok(parser) => parser.into(),
        Err(e) => e.compile_error().into(),
    }
}

/// build a lalr1 Deterministic Finite Automaton (DFA) parser
#[proc_macro]
pub fn lalr1(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let g = match grammar::Grammar::parse(input) {
        Ok(grammar) => grammar,
        Err(e) => return e.compile_error().into(),
    };

    match g.emit(true) {
        Ok(parser) => parser.into(),
        Err(e) => e.compile_error().into(),
    }
}
