use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::quote;

#[derive(Debug)]
pub enum ParseError {
    MultipleTokenDefinition(Ident, TokenStream),
    MultipleStartDefinition(Ident, Ident),
    MultipleAugmentedDefinition(Ident, Ident),
    MultipleTokenTypeDefinition(TokenStream, TokenStream),
    MultipleUserDataDefinition(TokenStream, TokenStream),

    StartNotDefined,
    TokenTypeNotDefined,
    AugmentedNotDefined,

    InvalidPunct(char),
    GrammarBuildError(String),

    // building the grammar for parsing production rules failed
    InternalGrammarParseError(String),
    // UnexpectedToken(String),
    // MultipleDefinition(String),
    // UnknownReduceType(String),
    // NonTermNameExistInTerminals(String),
    // GrammarParseError(String),
}

impl ParseError {
    pub fn compile_error(&self) -> TokenStream {
        match self {
            ParseError::MultipleTokenDefinition(ident, tokens) => {
                quote! {
                    compile_error!(concat!("Multiple token definition: ", #ident, " ", #tokens));
                }
            }
            ParseError::MultipleStartDefinition(ident, start) => {
                quote! {
                    compile_error!(concat!("Multiple start definition: ", #ident, " ", #start));
                }
            }
            ParseError::MultipleAugmentedDefinition(ident, aug) => {
                quote! {
                    compile_error!(concat!("Multiple augmented definition: ", #ident, " ", #aug));
                }
            }
            ParseError::MultipleTokenTypeDefinition(ident, tokens) => {
                quote! {
                    compile_error!(concat!("Multiple token type definition: ", #ident, " ", #tokens));
                }
            }
            ParseError::MultipleUserDataDefinition(ident, tokens) => {
                quote! {
                    compile_error!(concat!("Multiple user data definition: ", #ident, " ", #tokens));
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
            ParseError::AugmentedNotDefined => {
                quote! {
                    compile_error!("Augmented production rule not defined;\n>> %augmented <RuleName>;");
                }
            }
            ParseError::InvalidPunct(punct) => {
                let message = format!("Invalid punctuation: {}", punct);
                quote! {
                    compile_error!(#message);
                }
            }
            ParseError::GrammarBuildError(message) => {
                quote! {
                    compile_error!(#message);
                }
            }
            ParseError::InternalGrammarParseError(message) => {
                quote! {
                    compile_error!(#message);
                }
            }
        }
    }
}
