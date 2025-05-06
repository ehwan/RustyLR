use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote_spanned;

use rusty_lr_core::ProductionRule;
use rusty_lr_core::ShiftedRule;

use crate::parser::args::IdentOrLiteral;

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

    /// multiple %prec in the same rule
    MultiplePrecDefinition(Span),
    /// multiple %dprec in the same rule
    MultipleDPrecDefinition(Span),
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ConflictError {
    /// error building given CFG
    ShiftReduceConflict {
        term: String,
        reduce_rule: (usize, ProductionRule<String, String>),
        shift_rules: Vec<(usize, ShiftedRule<String, String>)>,
    },
    /// error building given CFG
    ReduceReduceConflict {
        lookahead: String,
        rule1: (usize, ProductionRule<String, String>),
        rule2: (usize, ProductionRule<String, String>),
    },
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ParseError {
    MultipleRuleDefinition(Ident, Ident),

    /// different reduce type applied to the same terminal symbol
    MultipleReduceDefinition {
        terminal: String,
        old: (Span, rusty_lr_core::ReduceType),
        new: (Span, rusty_lr_core::ReduceType),
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

    /// can't use reserved keyword as token name
    ReservedName(Ident),

    /// not supported literal type
    UnsupportedLiteralType(TokenStream),

    /// range in literal terminal set is not valid
    InvalidLiteralRange(Literal, Literal),

    /// TokenType in Literal mode is not supported
    TokenInLiteralMode(Span),

    MultiplePrecedenceOrderDefinition {
        cur: IdentOrLiteral,
        old: Span,
    },

    /// ReduceAction must be defined but not defined
    RuleTypeDefinedButActionNotDefined {
        name: Ident,
        span: (Span, Span),
    },

    /// Only terminal or terminal set is allowed
    OnlyTerminalSet(Span, Span),

    /// unknown non-terminal symbol name
    NonTerminalNotDefined(Ident),

    /// only 'usize' literal is allowed for %dprec
    OnlyUsizeLiteral(Span),
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

            ArgError::MultiplePrecDefinition(span) => *span,
            ArgError::MultipleDPrecDefinition(span) => *span,
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

            ArgError::MultiplePrecDefinition(span) => "Multiple %prec definition".into(),
            ArgError::MultipleDPrecDefinition(span) => "Multiple %dprec definition".into(),
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

            ParseError::MultipleReduceDefinition { terminal, old, new } => new.0,

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

            ParseError::ReservedName(ident) => ident.span(),

            ParseError::UnsupportedLiteralType(stream) => {
                stream.clone().into_iter().next().unwrap().span()
            }

            ParseError::InvalidLiteralRange(first, last) => first.span(),

            ParseError::TokenInLiteralMode(open_span) => *open_span,

            ParseError::MultiplePrecedenceOrderDefinition { cur, old } => cur.span(),

            ParseError::RuleTypeDefinedButActionNotDefined { name, span } => span.0,
            ParseError::OnlyTerminalSet(span_begin, span_end) => *span_begin,
            ParseError::NonTerminalNotDefined(ident) => ident.span(),
            ParseError::OnlyUsizeLiteral(span) => *span,
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

            ParseError::ReservedName(ident) => {
                format!("'{}' is reserved name", ident)
            }

            ParseError::UnsupportedLiteralType(literal) => {
                format!("Not supported literal type: {}", literal)
            }

            ParseError::InvalidLiteralRange(first, last) => {
                format!(
                    "Range in literal terminal set is not valid: [{} - {}]",
                    first, last
                )
            }

            ParseError::TokenInLiteralMode(_) => {
                "%token with %tokentype `char` or `u8` is not supported. Use 'a' or b'a' instead"
                    .to_string()
            }

            ParseError::MultiplePrecedenceOrderDefinition { cur, old } => {
                format!("Conflicts with precedence definition: {}", cur)
            }

            ParseError::RuleTypeDefinedButActionNotDefined { name, span } => {
                "ReduceAction must be defined for this rule".into()
            }
            ParseError::OnlyTerminalSet(_, _) => "Only terminal or terminal set is allowed".into(),
            ParseError::NonTerminalNotDefined(ident) => {
                format!("Unknown non-terminal symbol name: {}", ident)
            }
            ParseError::OnlyUsizeLiteral(_) => "Only 'usize' literal is allowed for %dprec".into(),
        }
    }
}

#[allow(unused)]
impl ConflictError {
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
            ConflictError::ShiftReduceConflict {
                term,
                reduce_rule: (ruleid, rule),
                shift_rules,
            } => Span::call_site(),
            ConflictError::ReduceReduceConflict {
                lookahead,
                rule1: (ruleid1, rule1),
                rule2: (ruleid2, rule2),
            } => Span::call_site(),
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ConflictError::ShiftReduceConflict {
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
            ConflictError::ReduceReduceConflict {
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
