use error::ParseError;
use proc_macro::TokenStream;
use proc_macro2;

mod emit;
mod error;
mod grammar;
mod rule;
mod token;

/// build a lr1 Deterministic Finite Automaton (DFA) parser for Slice of TokenStream
#[proc_macro]
pub fn lr1(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let tokens = match grammar::Grammar::tokenize(input) {
        Ok(tokens) => tokens,
        Err(err) => {
            return err.compile_error().into();
        }
    };

    let parser = match grammar::Grammar::build_parser() {
        Ok(parser) => parser,
        Err(err) => {
            let message = format!("{}", err);
            return ParseError::InternalGrammarBuildError(message)
                .compile_error()
                .into();
        }
    };
    let res = match parser.parse(&tokens, grammar::TermType::Eof) {
        Ok(res) => res,
        Err(err) => {
            let message = format!("{}", err);
            return ParseError::InternalGrammarParseError(message)
                .compile_error()
                .into();
        }
    };

    let grammar = match grammar::Grammar::parse_tree(&res, &tokens, &parser) {
        Ok(grammar) => grammar,
        Err(err) => {
            return err.compile_error().into();
        }
    };

    let emit = match grammar.emit() {
        Ok(emit) => emit,
        Err(err) => {
            return err.compile_error().into();
        }
    };

    emit.into()
}

/// build a lr1 Deterministic Finite Automaton (DFA) parser for &str
#[proc_macro]
pub fn lr1_str(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let tokens = match grammar::Grammar::tokenize(input) {
        Ok(tokens) => tokens,
        Err(err) => {
            return err.compile_error().into();
        }
    };

    let parser = match grammar::Grammar::build_parser() {
        Ok(parser) => parser,
        Err(err) => {
            let message = format!("{}", err);
            return ParseError::InternalGrammarBuildError(message)
                .compile_error()
                .into();
        }
    };
    let res = match parser.parse(&tokens, grammar::TermType::Eof) {
        Ok(res) => res,
        Err(err) => {
            let message = format!("{}", err);
            return ParseError::InternalGrammarParseError(message)
                .compile_error()
                .into();
        }
    };

    let grammar = match grammar::Grammar::parse_tree(&res, &tokens, &parser) {
        Ok(grammar) => grammar,
        Err(err) => {
            return err.compile_error().into();
        }
    };

    let emit = match grammar.emit_str() {
        Ok(emit) => emit,
        Err(err) => {
            return err.compile_error().into();
        }
    };

    emit.into()
}
