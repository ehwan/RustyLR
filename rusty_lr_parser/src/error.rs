use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote_spanned;

use rusty_lr_core::ProductionRule;
use rusty_lr_core::ShiftedRule;

use crate::utils;

/// failed to feed() the token
#[non_exhaustive]
#[derive(Debug)]
pub enum ParseArgError {
    /// feed() failed
    MacroLineParse {
        span: Span,
        message: String,
    },
    // feed(eof) failed
    MacroLineParseEnd {
        message: String,
    },
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ArgError {
    MultipleModulePrefixDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleUserDataDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleErrorDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleTokenTypeDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleEofDefinition((Span, TokenStream), (Span, TokenStream)),
    MultipleStartDefinition(Ident, Ident),

    StartNotDefined,
    EofNotDefined,
    TokenTypeNotDefined,
}

#[non_exhaustive]
#[derive(Debug)]
pub enum EmitError {
    /// ReduceAction must be defined but not defined
    RuleTypeDefinedButActionNotDefined { name: Ident, rule_local_id: usize },

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

#[non_exhaustive]
#[derive(Debug)]
pub enum ParseError {
    MultipleRuleDefinition(Ident, Ident),

    /// different reduce type applied to the same terminal symbol
    MultipleReduceDefinition {
        terminal: Ident,
        old: (Span, Span, rusty_lr_core::ReduceType),
        new: (Span, Span, rusty_lr_core::ReduceType),
    },

    /// multiple %token definition
    MultipleTokenDefinition(Ident, Ident),

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

    /// 'eof' is reserved name
    EofDefined(Ident),
    /// 'Augmented' is reserved name
    AugmentedDefined(Ident),
}
#[allow(unused)]
impl ArgError {
    pub fn to_compile_error(&self) -> TokenStream {
        let span = self.span();
        let message = self.short_message();
        quote_spanned! {
            span=>
            compile_error!(#message);
        }
    }

    pub fn span(&self) -> Span {
        match self {
            ArgError::MultipleModulePrefixDefinition(
                (span1, tokenstream1),
                (span2, tokenstream2),
            ) => *span2,
            ArgError::MultipleUserDataDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                *span2
            }
            ArgError::MultipleErrorDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                *span2
            }
            ArgError::MultipleTokenTypeDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                *span2
            }
            ArgError::MultipleEofDefinition((span1, tokenstream1), (span2, tokenstream2)) => *span2,
            ArgError::MultipleStartDefinition(old, new) => new.span(),

            ArgError::StartNotDefined => Span::call_site(),
            ArgError::EofNotDefined => Span::call_site(),
            ArgError::TokenTypeNotDefined => Span::call_site(),
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ArgError::MultipleModulePrefixDefinition(
                (span1, tokenstream1),
                (span2, tokenstream2),
            ) => "Multiple %moduleprefix definition".into(),
            ArgError::MultipleUserDataDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                "Multiple %userdata definition".into()
            }
            ArgError::MultipleErrorDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                "Multiple %error definition".into()
            }
            ArgError::MultipleTokenTypeDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                "Multiple %tokentype definition".into()
            }
            ArgError::MultipleEofDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                "Multiple %eof definition".into()
            }
            ArgError::MultipleStartDefinition(old, new) => {
                format!("Multiple %start definition: {} and {}", old, new)
            }

            ArgError::StartNotDefined => "Start rule not defined\n>>> %start <rule_name>;".into(),
            ArgError::EofNotDefined => "Eof not defined\n>>> %eof <eof_token_value>;".into(),
            ArgError::TokenTypeNotDefined => {
                "Token type not defined\n>>> %tokentype <token_type_name>;".into()
            }
        }
    }
}
#[allow(unused)]
impl ParseArgError {
    pub fn to_compile_error(&self) -> TokenStream {
        let span = self.span();
        let message = self.short_message();
        quote_spanned! {
            span=>
            compile_error!(#message);
        }
    }

    pub fn span(&self) -> Span {
        match self {
            ParseArgError::MacroLineParse { span, message } => *span,
            ParseArgError::MacroLineParseEnd { message } => Span::call_site(),
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ParseArgError::MacroLineParse { span, message } => message.clone(),
            ParseArgError::MacroLineParseEnd { message } => message.clone(),
        }
    }
}

#[allow(unused)]
impl ParseError {
    pub fn to_compile_error(&self) -> TokenStream {
        let span = self.span();
        let message = self.short_message();
        quote_spanned! {
            span=>
            compile_error!(#message);
        }
    }

    pub fn span(&self) -> Span {
        match self {
            ParseError::MultipleRuleDefinition(old, new) => new.span(),

            ParseError::MultipleReduceDefinition { terminal, old, new } => terminal.span(),

            ParseError::TermNonTermConflict {
                name,
                terminal,
                non_terminal,
            } => name.span(),

            ParseError::InvalidTerminalRange((first, first_index, _), (last, last_index, _)) => {
                first.span()
            }

            ParseError::StartNonTerminalNotDefined(ident) => ident.span(),

            ParseError::TerminalNotDefined(ident) => ident.span(),

            ParseError::MultipleTokenDefinition(old, new) => new.span(),

            ParseError::EofDefined(ident) => ident.span(),
            ParseError::AugmentedDefined(ident) => ident.span(),
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ParseError::MultipleRuleDefinition(old, new) => {
                format!("Multiple rule definition with same name: {}", old)
            }

            ParseError::MultipleReduceDefinition { terminal, old, new } => {
                format!("Differnt reduce type (%left and %right) applied to the same terminal symbol: {}", terminal)
            }

            ParseError::TermNonTermConflict {
                name,
                terminal,
                non_terminal,
            } => {
                format!("Same name for terminal and non-terminal exists: {}", name)
            }

            ParseError::InvalidTerminalRange((first, first_index, _), (last, last_index, _)) => {
                format!(
                    "Invalid terminal range: [{}({}) - {}({})]",
                    first, first_index, last, last_index
                )
            }

            ParseError::StartNonTerminalNotDefined(ident) => {
                format!("Name given to %start not defined: {}", ident)
            }

            ParseError::TerminalNotDefined(ident) => {
                format!("Unknown terminal symbol name: {}", ident)
            }

            ParseError::MultipleTokenDefinition(old, new) => {
                format!("Multiple %token definition with same name: {}", old)
            }

            ParseError::EofDefined(ident) => {
                format!("'{}' is reserved name", utils::EOF_NAME)
            }
            ParseError::AugmentedDefined(ident) => {
                format!("'{}' is reserved name", utils::AUGMENTED_NAME)
            }
        }
    }
}

#[allow(unused)]
impl EmitError {
    pub fn to_compile_error(&self) -> TokenStream {
        let span = self.span();
        let message = self.short_message();
        quote_spanned! {
            span=>
            compile_error!(#message);
        }
    }

    pub fn span(&self) -> Span {
        match self {
            EmitError::RuleTypeDefinedButActionNotDefined {
                name,
                rule_local_id,
            } => name.span(),

            EmitError::ShiftReduceConflict {
                term,
                reduce_rule: (ruleid, rule),
                shift_rules,
            } => term.span(),
            EmitError::ReduceReduceConflict {
                lookahead,
                rule1: (ruleid1, rule1),
                rule2: (ruleid2, rule2),
            } => Span::call_site(),
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            EmitError::RuleTypeDefinedButActionNotDefined {
                name,
                rule_local_id,
            } => "ReduceAction must be defined for this rule".into(),

            EmitError::ShiftReduceConflict {
                term,
                reduce_rule: (ruleid, rule),
                shift_rules,
            } => {
                format!(
                    "Shift-Reduce conflict with terminal symbol: {}\n>>> Reduce: {}\n>>> Shifts: {}",
                    term,
                    rule,
                    shift_rules
                        .iter()
                        .map(|(ruleid, rule)| format!("{}", rule))
                        .collect::<Vec<_>>()
                        .join("\n>>>")
                )
            }
            EmitError::ReduceReduceConflict {
                lookahead,
                rule1: (ruleid1, rule1),
                rule2: (ruleid2, rule2),
            } => {
                format!(
                    "Reduce-Reduce conflict with lookahead symbol: {}\n>>> Rule1: {}\n>>> Rule2: {}",
                    lookahead, rule1, rule2
                )
            }
        }
    }
}
