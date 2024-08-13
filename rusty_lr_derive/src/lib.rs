use proc_macro::TokenStream;

/// build a lr1 Deterministic Finite Automaton (DFA) parser at compile time
#[proc_macro]
pub fn lr1(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let g = match rusty_lr_parser::grammar::Grammar::parse(input) {
        Ok(grammar) => grammar,
        Err(e) => {
            return e.into();
        }
    };

    match g.emit_compiletime(false) {
        Ok(parser) => parser.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// build a lalr1 Deterministic Finite Automaton (DFA) parser at compile time
#[proc_macro]
pub fn lalr1(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let g = match rusty_lr_parser::grammar::Grammar::parse(input) {
        Ok(grammar) => grammar,
        Err(e) => {
            return e.into();
        }
    };

    match g.emit_compiletime(true) {
        Ok(parser) => parser.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// build a lr1 Deterministic Finite Automaton (DFA) parser at runtime
#[proc_macro]
pub fn lr1_runtime(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let g = match rusty_lr_parser::grammar::Grammar::parse(input) {
        Ok(grammar) => grammar,
        Err(e) => {
            return e.into();
        }
    };

    match g.emit_runtime(false) {
        Ok(parser) => parser.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// build a lalr1 Deterministic Finite Automaton (DFA) parser at runtime
#[proc_macro]
pub fn lalr1_runtime(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let g = match rusty_lr_parser::grammar::Grammar::parse(input) {
        Ok(grammar) => grammar,
        Err(e) => {
            return e.into();
        }
    };

    match g.emit_runtime(true) {
        Ok(parser) => parser.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
