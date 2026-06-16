use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote_spanned;

use crate::parser::args::IdentOrLiteral;
use crate::parser::location::Location;

/// failed to feed() the token
#[non_exhaustive]
#[derive(Debug)]
pub enum ParseArgError {
    /// feed() failed; `span` is the byte range `[start, end)` in the source
    MacroLineParse {
        location: Location,
        message: String,
    },
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ArgError {
    MultipleModulePrefixDefinition(Vec<Location>),
    MultipleUserDataDefinition(Vec<Location>),
    MultipleErrorDefinition(Vec<Location>),
    MultipleTokenTypeDefinition(Vec<Location>),
    MultipleEofDefinition(Vec<Location>),
    MultipleStartDefinition(Vec<Location>),
    /// multiple %prec in the same rule
    MultiplePrecDefinition(Vec<Location>),
    /// multiple %dprec in the same rule
    MultipleDPrecDefinition(Vec<Location>),

    StartNotDefined,
    EofNotDefined,
    TokenTypeNotDefined,
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ConflictError {
    /// error building given CFG
    ShiftReduceConflict {
        term: String,
        reduce_rule: (usize, rusty_lr_core::rule::ProductionRule<String, String>),
        shift_rules: Vec<(usize, rusty_lr_core::rule::ShiftedRule<String, String>)>,
    },
    /// error building given CFG
    ReduceReduceConflict {
        lookahead: String,
        rule1: (usize, rusty_lr_core::rule::ProductionRule<String, String>),
        rule2: (usize, rusty_lr_core::rule::ProductionRule<String, String>),
    },
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ParseError {
    MultipleRuleDefinition(Ident, Ident),

    /// different reduce type applied to the same terminal symbol
    MultipleReduceDefinition {
        terminal: String,
        old: (Location, rusty_lr_core::rule::ReduceType),
        new: (Location, rusty_lr_core::rule::ReduceType),
    },

    /// multiple %token definition
    MultipleTokenDefinition(Ident, Ident),

    /// same name for terminal and non-terminal exists
    TermNonTermConflict{term: Location, nonterm: Location},

    InvalidTerminalRange{ location: Location, start: (Ident, usize), end: (Ident, usize) },

    /// name given to %start not defined
    StartNonTerminalNotDefined(Ident),

    /// unknown terminal symbol name
    TerminalNotDefined(Ident),

    /// can't use reserved keyword as token name
    ReservedName(Ident),

    /// not supported literal type
    UnsupportedLiteralType(Location),

    /// range in literal terminal set is not valid; [first, last] with first > last
    InvalidLiteralRange(Location),

    /// TokenType in Literal mode is not supported
    TokenInLiteralMode(Location),

    /// conflicts in precedence definition
    MultiplePrecedenceOrderDefinition (Vec<Location>),

    /// Precedence not defined for the given token
    PrecedenceNotDefined(IdentOrLiteral),

    /// All production rules in this non-terminal must have %prec defined
    NonTerminalPrecedenceNotDefined(Location, usize),

    /// ReduceAction must be defined but not defined
    RuleTypeDefinedButActionNotDefined {
        nonterm: Location,
        rule: Location
    },

    /// Only terminal or terminal set is allowed
    OnlyTerminalSet(Location),

    /// unknown non-terminal symbol name
    NonTerminalNotDefined(Ident),

    /// only 'usize' literal is allowed for %dprec
    OnlyUsizeLiteral(Location),
}
#[allow(unused)]
impl ArgError {
    pub fn to_compile_error(&self, span_manager: &crate::parser::location::SpanManager) -> TokenStream {
        let mut output = TokenStream::new();
        let message = self.short_message();
        for loc in self.locations() {
            for span in span_manager.get_spans_in_location(&loc) {
                output.extend(quote_spanned! {
                    span=>
                    compile_error!(#message);
                });
            }
        }
        output
    }

    pub fn locations(&self) -> Vec<Location> {
        match self {
            ArgError::MultipleModulePrefixDefinition(locs)
            | ArgError::MultipleUserDataDefinition(locs)
            | ArgError::MultipleErrorDefinition(locs)
            | ArgError::MultipleTokenTypeDefinition(locs)
            | ArgError::MultipleEofDefinition(locs)
            | ArgError::MultipleStartDefinition(locs)
            | ArgError::MultiplePrecDefinition(locs)
            | ArgError::MultipleDPrecDefinition(locs) => locs.clone(),
            _ => vec![Location::call_site()],
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ArgError::MultipleModulePrefixDefinition(_) => {
                "Multiple %moduleprefix definition".into()
            }
            ArgError::MultipleUserDataDefinition(_) => "Multiple %userdata definition".into(),
            ArgError::MultipleErrorDefinition(_) => "Multiple %error definition".into(),
            ArgError::MultipleTokenTypeDefinition(_) => "Multiple %tokentype definition".into(),
            ArgError::MultipleEofDefinition(_) => "Multiple %eof definition".into(),
            ArgError::MultipleStartDefinition(_) => "Multiple %start definition".into(),
            ArgError::MultiplePrecDefinition(_) => "Multiple %prec definition".into(),
            ArgError::MultipleDPrecDefinition(_) => "Multiple %dprec definition".into(),
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
    pub fn to_compile_error(&self, span_manager: &crate::parser::location::SpanManager) -> TokenStream {
        let mut output = TokenStream::new();
        let message = self.short_message();
        let location = self.location();
        for span in span_manager.get_spans_in_location(&location) {
            output.extend(
                quote_spanned! {
                    span=>
                    compile_error!(#message);
                }
            );
        }
        output
    }

    /// Returns the byte range `[start, end)` of the error location in the source.
    pub fn location(&self) -> Location {
        match self {
            ParseArgError::MacroLineParse { location, message } => location.clone(),
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ParseArgError::MacroLineParse { message, .. } => message.clone(),
        }
    }
}

#[allow(unused)]
impl ParseError {
    pub fn to_compile_error(&self, span_manager: &crate::parser::location::SpanManager) -> TokenStream {
        let mut output = TokenStream::new();
        let message = self.short_message();
        for loc in self.locations() {
            for span in span_manager.get_spans_in_location(&loc) {
                output.extend(quote_spanned! {
                    span=>
                    compile_error!(#message);
                });
            }
        }
        output
    }

    pub fn locations(&self) -> Vec<Location> {
        match self {
            ParseError::MultipleRuleDefinition(old, new) => vec![old.span().into(), new.span().into()],

            ParseError::MultipleReduceDefinition { terminal, old, new } => vec![old.0.clone(), new.0.clone()],

            ParseError::TermNonTermConflict { term, nonterm } => vec![term.clone(), nonterm.clone()],
            ParseError::InvalidTerminalRange { location: range, start, end } => vec![range.clone()],

            ParseError::StartNonTerminalNotDefined(ident) => vec![ident.span().into()],

            ParseError::TerminalNotDefined(ident) => vec![ident.span().into()],

            ParseError::MultipleTokenDefinition(old, new) => vec![old.span().into(), new.span().into()],

            ParseError::ReservedName(ident) => vec![ident.span().into()],

            ParseError::UnsupportedLiteralType(loc) => vec![loc.clone()],

            ParseError::InvalidLiteralRange(loc) => vec![loc.clone()],

            ParseError::TokenInLiteralMode(loc) => vec![loc.clone()],

            ParseError::MultiplePrecedenceOrderDefinition(locations) => locations.clone(),
            ParseError::PrecedenceNotDefined(name) => vec![name.location().clone()],
            ParseError::NonTerminalPrecedenceNotDefined(loc, _) => vec![loc.clone()],

            ParseError::RuleTypeDefinedButActionNotDefined { nonterm, rule} => vec![nonterm.clone(), rule.clone()],
            ParseError::OnlyTerminalSet(location) => vec![location.clone()],
            ParseError::NonTerminalNotDefined(ident) => vec![ident.span().into()],
            ParseError::OnlyUsizeLiteral(loc) => vec![loc.clone()],
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

            ParseError::TermNonTermConflict { .. } =>
                "Duplicated name for terminal and non-terminal".to_string(),

            ParseError::InvalidTerminalRange { location: range, start, end } => {
                format!(
                    "Invalid terminal range: [{}({}) - {}({})]",
                    start.0, start.1, end.0, end.1
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

            ParseError::UnsupportedLiteralType(_) => {
                format!("This literal type is not supported. Use string literal (e.g. 'a', \"abc\", b'a' or b\"abc\") instead")
            }

            ParseError::InvalidLiteralRange(_) => 
                    "Invalid literal range: [first, last] with first > last".to_string(),

            ParseError::TokenInLiteralMode(_) => {
                "%token with %tokentype `char` or `u8` is not supported. Use character literal (e.g. 'a' or b'a') instead"
                    .to_string()
            }

            ParseError::MultiplePrecedenceOrderDefinition(locations) =>
                "Multiple precedence order definition for the same token".to_string(),
            ParseError::PrecedenceNotDefined(name) => {
                format!("Precedence not defined for the given token: {}", name)
            }
            ParseError::NonTerminalPrecedenceNotDefined(span, nonterm_idx) => {
                "All production rules in this non-terminal must have %prec defined".into()
            }

            ParseError::RuleTypeDefinedButActionNotDefined { .. } => {
                "ReduceAction must be defined for this rule".into()
            }
            ParseError::OnlyTerminalSet(_) => "Only terminal or terminal set is allowed".into(),
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
