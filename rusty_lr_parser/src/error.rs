use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote_spanned;

use std::fmt::Display;

use super::utils;

#[non_exhaustive]
#[derive(Debug)]
pub enum ParseError {
    MultipleTokenDefinition(Ident),
    MultipleStartDefinition(Span, Ident, Ident),
    MultipleTokenTypeDefinition(Span, TokenStream, TokenStream),
    MultipleEofDefinition(Span, TokenStream, TokenStream),
    MultipleUserDataDefinition(Span, TokenStream, TokenStream),
    MultipleRuleDefinition(Ident),
    MultipleErrorDefinition(Span, TokenStream, TokenStream),
    /// different reduce type applied to the same terminal symbol
    MultipleReduceDefinition(Ident),

    // same name for terminal and non-terminal exists
    TermNonTermConflict(Ident),

    InvalidTerminalRange(Ident, Ident),

    TerminalNotDefined(Ident),
    NonTerminalNotDefined(Ident),
    EofDefined(Ident),
    AugmentedDefined(Ident),
    ReservedNonTerminal(Ident),

    RuleTypeDefinedButActionNotDefined(Ident),

    StartNotDefined,
    EofNotDefined,
    TokenTypeNotDefined,

    GrammarBuildError(String),

    // building the grammar for parsing production rules failed
    InternalGrammar(Span, String),

    /// error parsing TerminalSet
    TerminalSetParse(Span, String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::MultipleTokenDefinition(ident) => {
                write!(f, "Multiple token definition: {}", ident)
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
            ParseError::MultipleRuleDefinition(name) => {
                write!(f, "Multiple rule definition: {}", name)
            }
            ParseError::MultipleErrorDefinition(_, err1, err2) => {
                write!(f, "Multiple error type definition: {} AND {}", err1, err2)
            }
            ParseError::MultipleReduceDefinition(name) => {
                write!(f, "Multiple reduce type definition: {}", name)
            }
            ParseError::InvalidTerminalRange(ident1, ident2) => {
                write!(f, "Invalid terminal range: {} - {}", ident1, ident2)
            }
            ParseError::TermNonTermConflict(name) => {
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
                write!(
                    f,
                    "\"{}\" is reserved name for terminal symbol",
                    utils::EOF_NAME
                )
            }
            ParseError::AugmentedDefined(_) => {
                write!(
                    f,
                    "\"{}\" is reserved name for non-terminal symbol",
                    utils::AUGMENTED_NAME
                )
            }
            ParseError::ReservedNonTerminal(ident) => {
                write!(f, "\"{}\" is reserved name for non-terminal symbol", ident)
            }
            ParseError::RuleTypeDefinedButActionNotDefined(ident) => {
                write!(
                    f,
                    "<RuleType> is defined but <ReduceAction> is not defined: {}",
                    ident
                )
            }
            ParseError::GrammarBuildError(message) => {
                write!(f, "{}", message)
            }
            ParseError::InternalGrammar(_, message) => write!(f, "{}", message),

            ParseError::TerminalSetParse(_, message) => {
                write!(f, "Error parsing TerminalSet:\n{}", message)
            }
        }
    }
}
impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::MultipleTokenDefinition(ident) => ident.span(),
            ParseError::MultipleStartDefinition(span, _, _) => *span,
            ParseError::MultipleTokenTypeDefinition(span, _, _) => *span,
            ParseError::MultipleEofDefinition(span, _, _) => *span,
            ParseError::MultipleUserDataDefinition(span, _, _) => *span,
            ParseError::MultipleRuleDefinition(ident) => ident.span(),
            ParseError::MultipleErrorDefinition(span, _, _) => *span,
            ParseError::MultipleReduceDefinition(ident) => ident.span(),
            ParseError::InvalidTerminalRange(ident1, _ident2) => ident1.span(),
            ParseError::TermNonTermConflict(ident) => ident.span(),
            ParseError::StartNotDefined => Span::call_site(),
            ParseError::TokenTypeNotDefined => Span::call_site(),
            ParseError::EofNotDefined => Span::call_site(),
            ParseError::TerminalNotDefined(ident) => ident.span(),
            ParseError::NonTerminalNotDefined(ident) => ident.span(),
            ParseError::EofDefined(ident) => ident.span(),
            ParseError::AugmentedDefined(ident) => ident.span(),
            ParseError::ReservedNonTerminal(ident) => ident.span(),
            ParseError::RuleTypeDefinedButActionNotDefined(ident) => ident.span(),
            ParseError::GrammarBuildError(_) => Span::call_site(),
            ParseError::InternalGrammar(span, _) => *span,
            ParseError::TerminalSetParse(span, _) => *span,
        }
    }
    pub fn compile_error(&self) -> TokenStream {
        let message = format!("{}", self);
        quote_spanned! {
            self.span() => compile_error!(#message);
        }
    }
}
