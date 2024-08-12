use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote;
use quote::quote_spanned;

use rusty_lr_core::ProductionRule;
use rusty_lr_core::ShiftedRule;

use crate::utils;

#[non_exhaustive]
#[derive(Debug)]
pub enum ParseError {
    MultipleModulePrefixDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleUserDataDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleErrorDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleTokenTypeDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleEofDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleStartDefinition(Ident, Ident),
    MultipleRuleDefinition(Ident, Ident),

    /// different reduce type applied to the same terminal symbol
    MultipleReduceDefinition(Ident),

    /// same name for terminal and non-terminal exists
    TermNonTermConflict {
        name: Ident,
        terminal: Ident,
        non_terminal: Ident,
    },

    InvalidTerminalRange((Ident, usize, TokenStream), (Ident, usize, TokenStream)),

    /// name given to %start not defined
    StartNonTerminalNotDefined(Ident),

    /// unknown terminal symbol name
    TerminalNotDefined(Ident),

    /// multiple %token definition
    MultipleTokenDefinition(Ident, Ident),

    /// 'eof' is reserved name
    EofDefined(Ident),
    /// 'Augmented' is reserved name
    AugmentedDefined(Ident),

    StartNotDefined,
    EofNotDefined,
    TokenTypeNotDefined,

    /// feed() failed
    MacroLineParse {
        span: Span,
        message: String,
    },
    // feed(eof) failed
    MacroLineParseEnd {
        message: String,
    },

    ////////////////////////////////////
    //// Below errors are for emit()
    ////////////////////////////////////
    /// ReduceAction must be defined but not defined
    RuleTypeDefinedButActionNotDefined {
        name: Ident,
        rule_local_id: usize,
    },

    /// error building given CFG
    ShiftReduceConflict {
        term: Ident,
        reduce_rule: (usize, ProductionRule<Ident, Ident>),
        shift_rules: Vec<(usize, ShiftedRule<Ident, Ident>)>,
    },
    /// error building given CFG
    ReduceReduceConflict {
        lookahead: Ident,
        rule1: (usize, ProductionRule<Ident, Ident>),
        rule2: (usize, ProductionRule<Ident, Ident>),
    },
}
#[allow(unused)]
impl ParseError {
    pub fn to_compile_error(&self) -> TokenStream {
        match self {
            ParseError::MultipleModulePrefixDefinition(
                (span1, tokenstream1),
                (span2, tokenstream2),
            ) => {
                let span = *span2;
                let message = "Multiple %moduleprefix definition";
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleUserDataDefinition(
                (span1, tokenstream1),
                (span2, tokenstream2),
            ) => {
                let span = *span2;
                let message = "Multiple %userdata definition";
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleErrorDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                let span = *span2;
                let message = "Multiple %error definition";
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleTokenTypeDefinition(
                (span1, tokenstream1),
                (span2, tokenstream2),
            ) => {
                let span = *span2;
                let message = "Multiple %tokentype definition";
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleEofDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                let span = *span2;
                let message = "Multiple %eof definition";
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleStartDefinition(old, new) => {
                let span = new.span();
                let message = format!("Multiple %start definition: {} and {}", old, new);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::MultipleRuleDefinition(old, new) => {
                let span = new.span();
                let message = format!("Multiple rule definition with same name: {}", old);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::MultipleReduceDefinition(term) => {
                let span = term.span();
                let message = format!("Differnt reduce type (%left and %right) applied to the same terminal symbol: {}", term);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::TermNonTermConflict {
                name,
                terminal,
                non_terminal,
            } => {
                let span = name.span();
                let message = format!("Same name for terminal and non-terminal exists: {}", name);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::InvalidTerminalRange((first, first_index, _), (last, last_index, _)) => {
                let span = first.span();
                let message = format!(
                    "Invalid terminal range: [{}({}) - {}({})]",
                    first, first_index, last, last_index
                );
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::StartNonTerminalNotDefined(ident) => {
                let span = ident.span();
                let message = format!("Name given to %start not defined: {}", ident);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::TerminalNotDefined(ident) => {
                let span = ident.span();
                let message = format!("Unknown terminal symbol name: {}", ident);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::MultipleTokenDefinition(old, new) => {
                let span = new.span();
                let message = format!("Multiple %token definition with same name: {}", old);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::EofDefined(ident) => {
                let span = ident.span();
                let message = format!("'{}' is reserved name", utils::EOF_NAME);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::AugmentedDefined(ident) => {
                let span = ident.span();
                let message = format!("'{}' is reserved name", utils::AUGMENTED_NAME);
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }

            ParseError::StartNotDefined => {
                quote! {
                    compile_error!("Start rule not defined\n>>> %start <rule_name>;");
                }
            }
            ParseError::EofNotDefined => {
                quote! {
                    compile_error!("Eof not defined\n>>> %eof <eof_token_value>;");
                }
            }
            ParseError::TokenTypeNotDefined => {
                quote! {
                    compile_error!("Token type not defined\n>>> %tokentype <token_type_name>;");
                }
            }

            ParseError::MacroLineParse { span, message } => {
                quote_spanned! {
                    *span=>
                    compile_error!(#message);
                }
            }
            ParseError::MacroLineParseEnd { message } => {
                quote! {
                    compile_error!(#message);
                }
            }

            ParseError::RuleTypeDefinedButActionNotDefined {
                name,
                rule_local_id,
            } => {
                let span = name.span();
                quote_spanned! {
                    span=>
                    compile_error!("ReduceAction must be defined for this rule");
                }
            }

            ParseError::ShiftReduceConflict {
                term,
                reduce_rule: (ruleid, rule),
                shift_rules,
            } => {
                let span = term.span();
                let message = format!(
                    "Shift-Reduce conflict with terminal symbol: {}\n>>> Reduce: {}\n>>> Shifts: {}",
                    term,
                    rule,
                    shift_rules
                        .iter()
                        .map(|(ruleid, rule)| format!("{}", rule))
                        .collect::<Vec<_>>()
                        .join("\n>>>")
                );
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
            ParseError::ReduceReduceConflict {
                lookahead,
                rule1: (ruleid1, rule1),
                rule2: (ruleid2, rule2),
            } => {
                let span = lookahead.span();
                let message = format!(
                    "Reduce-Reduce conflict with lookahead symbol: {}\n>>> Rule1: {}\n>>> Rule2: {}",
                    lookahead, rule1, rule2
                );
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            }
        }
    }
}
