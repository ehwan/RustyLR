use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
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
    MultipleErrorDefinition(Span, TokenStream, TokenStream),
    MultipleReduceDefinition(Span, String),

    InvalidRuletypeDelimiter(Span),
    InvalidReduceActionDelimiter(Span),

    // same name for terminal and non-terminal exists
    TermNonTermConflict(Span, String),

    TerminalNotDefined(Ident),
    NonTerminalNotDefined(Ident),
    EofDefined(Ident),

    StartNotDefined,
    EofNotDefined,
    TokenTypeNotDefined,

    GrammarBuildError(String),

    // building the grammar for parsing production rules failed
    InternalGrammar(Span, String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::MultipleTokenDefinition(_, ident, tokens0, tokens1) => {
                write!(
                    f,
                    "Multiple token definition: {} ->\n{}\nAND\n{}",
                    ident, tokens0, tokens1
                )
            }
            ParseError::MultipleStartDefinition(_, start1, start2) => {
                write!(f, "Multiple start definition: {} AND {}", start1, start2)
            }
            ParseError::MultipleTokenTypeDefinition(_, stream1, stream2) => {
                write!(
                    f,
                    "Multiple token type definition: {} AND {}",
                    stream1, stream2
                )
            }
            ParseError::MultipleEofDefinition(_, stream1, stream2) => {
                write!(f, "Multiple eof definition: {} AND {}", stream1, stream2)
            }
            ParseError::MultipleUserDataDefinition(_, ident, tokens) => {
                write!(f, "Multiple user data definition: {} AND {}", ident, tokens)
            }
            ParseError::MultipleRuleDefinition(_, name) => {
                write!(f, "Multiple rule definition: {}", name)
            }
            ParseError::MultipleErrorDefinition(_, err1, err2) => {
                write!(f, "Multiple error type definition: {} AND {}", err1, err2)
            }
            ParseError::MultipleReduceDefinition(_, name) => {
                write!(f, "Multiple reduce type definition: {}", name)
            }
            ParseError::InvalidRuletypeDelimiter(_) => {
                write!(f, "rule type must be enclosed with '(' and ')'")
            }
            ParseError::InvalidReduceActionDelimiter(_) => {
                write!(f, "reduce action must be enclosed with '{{' and '}}'")
            }
            ParseError::TermNonTermConflict(_, name) => {
                write!(
                    f,
                    "Same token name for Terminal and Non-Terminal symbol exists: {}",
                    name
                )
            }
            ParseError::StartNotDefined => {
                write!(
                    f,
                    "Start production rule not defined;\n>> %start <RuleName>;"
                )
            }
            ParseError::TokenTypeNotDefined => {
                write!(
                    f,
                    "Token type not defined for Terminal;\n>> %tokentype <RustType>;"
                )
            }
            ParseError::EofNotDefined => {
                write!(f, "Eof not defined;\n>> %eof <RustCode>;")
            }
            ParseError::TerminalNotDefined(ident) => {
                write!(f, "Terminal not defined: {}", ident)
            }
            ParseError::NonTerminalNotDefined(ident) => {
                write!(f, "Non-terminal not defined: {}", ident)
            }
            ParseError::EofDefined(_) => {
                write!(f, "'eof' is reserved name for terminal symbol")
            }
            ParseError::GrammarBuildError(message) => {
                write!(f, "{}", message)
            }
            ParseError::InternalGrammar(_, message) => write!(f, "{}", message),
        }
    }
}
impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::MultipleTokenDefinition(span, _, _, _) => span.clone(),
            ParseError::MultipleStartDefinition(span, _, _) => span.clone(),
            ParseError::MultipleTokenTypeDefinition(span, _, _) => span.clone(),
            ParseError::MultipleEofDefinition(span, _, _) => span.clone(),
            ParseError::MultipleUserDataDefinition(span, _, _) => span.clone(),
            ParseError::MultipleRuleDefinition(span, _) => span.clone(),
            ParseError::MultipleErrorDefinition(span, _, _) => span.clone(),
            ParseError::MultipleReduceDefinition(span, _) => span.clone(),
            ParseError::InvalidRuletypeDelimiter(span) => span.clone(),
            ParseError::InvalidReduceActionDelimiter(span) => span.clone(),
            ParseError::TermNonTermConflict(span, _) => span.clone(),
            ParseError::StartNotDefined => Span::call_site(),
            ParseError::TokenTypeNotDefined => Span::call_site(),
            ParseError::EofNotDefined => Span::call_site(),
            ParseError::TerminalNotDefined(ident) => ident.span(),
            ParseError::NonTerminalNotDefined(ident) => ident.span(),
            ParseError::EofDefined(ident) => ident.span(),
            ParseError::GrammarBuildError(_) => Span::call_site(),
            ParseError::InternalGrammar(span, _) => span.clone(),
        }
    }
    pub fn compile_error(&self) -> TokenStream {
        let message = format!("{}", self);
        quote_spanned! {
            self.span() => compile_error!(#message);
        }
    }
}
