use proc_macro2::Ident;
use proc_macro2::Punct;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::quote_spanned;

use std::fmt::Display;

#[derive(Debug)]
pub enum ParseError {
    MultipleTokenDefinition(Span, Ident, TokenStream, TokenStream),
    MultipleStartDefinition(Span, Ident, Ident),
    MultipleTokenTypeDefinition(Span, TokenStream, TokenStream),
    MultipleEofDefinition(Span, TokenStream, TokenStream),
    MultipleUserDataDefinition(Span, TokenStream, TokenStream),
    MultipleRuleDefinition(Span, String),

    // same name for terminal and non-terminal exists
    TermNonTermConflict(Span, String),

    TerminalNotDefined(Ident),
    NonTerminalNotDefined(Ident),

    StartNotDefined,
    EofNotDefined,
    TokenTypeNotDefined,

    InvalidPunct(Punct),
    GrammarBuildError(String),

    // building the grammar for parsing production rules failed
    InternalGrammar(Span, String),
}

impl ParseError {
    pub fn compile_error(&self) -> TokenStream {
        match self {
            ParseError::MultipleTokenDefinition(span, ident, tokens0, tokens1) => {
                let message = format!(
                    "Multiple token definition: {} ->\n{}\nAND\n{}",
                    ident, tokens0, tokens1
                );
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleStartDefinition(span, start1, start2) => {
                let message = format!("Multiple start definition: {} AND {}", start1, start2);
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleTokenTypeDefinition(span, stream1, stream2) => {
                let message = format!(
                    "Multiple token type definition: {} AND {}",
                    stream1, stream2
                );
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleEofDefinition(span, stream1, stream2) => {
                let message = format!("Multiple eof definition: {} AND {}", stream1, stream2);
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleUserDataDefinition(span, ident, tokens) => {
                let message = format!("Multiple user data definition: {} AND {}", ident, tokens);
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleRuleDefinition(span, name) => {
                let message = format!("Multiple rule definition: {}", name);
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
            ParseError::TermNonTermConflict(span, name) => {
                let message = format!(
                    "Same token name for Terminal and Non-Terminal symbol exists: {}",
                    name
                );
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
            ParseError::StartNotDefined => {
                quote! {
                    compile_error!("Start production rule not defined;\n>> %start <RuleName>;");
                }
            }
            ParseError::TokenTypeNotDefined => {
                quote! {
                    compile_error!("Token type not defined for slice Terminal;\n>> %tokentype <RustType>;");
                }
            }
            ParseError::EofNotDefined => {
                quote! {
                    compile_error!("Eof not defined;\n>> %eof <Terminal>;");
                }
            }
            ParseError::TerminalNotDefined(ident) => {
                let message = format!("Terminal not defined: {}", ident);
                quote_spanned! {
                    ident.span() =>
                    compile_error!(#message);
                }
            }
            ParseError::NonTerminalNotDefined(ident) => {
                let message = format!("Non-terminal not defined: {}", ident);
                quote_spanned! {
                    ident.span() =>
                    compile_error!(#message);
                }
            }
            ParseError::InvalidPunct(punct) => {
                let message = format!("Invalid punctuation: {}", punct);
                quote_spanned! {
                    punct.span() =>
                    compile_error!(#message);
                }
            }
            ParseError::GrammarBuildError(message) => {
                quote! {
                    compile_error!(#message);
                }
            }
            ParseError::InternalGrammar(span, message) => {
                quote_spanned! {
                    span.clone() =>
                    compile_error!(#message);
                }
            }
        }
    }
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.compile_error().to_string().fmt(f)
    }
}
