use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote_spanned;

use crate::parser::args::IdentOrLiteral;
use crate::parser::location::Located;
use crate::parser::location::Location;
use rusty_lr_core::TerminalSymbol;

/// failed to feed() the token
#[derive(Debug)]
pub enum ParseArgError {
    /// feed() failed; `span` is the byte range `[start, end)` in the source
    MacroLineParse { location: Location, message: String },
}

#[derive(Debug)]
pub enum ArgError {
    /// multiple %moduleprefix in the same grammar
    MultipleModulePrefixDefinition(Vec<Location>),
    /// multiple %userdata in the same grammar
    MultipleUserDataDefinition(Vec<Location>),
    /// multiple %error in the same grammar
    MultipleErrorDefinition(Vec<Location>),
    /// multiple %tokentype in the same grammar
    MultipleTokenTypeDefinition(Vec<Location>),
    /// multiple %start in the same grammar
    MultipleStartDefinition(Vec<Location>),
    /// multiple %prec in the same rule
    MultiplePrecDefinition(Vec<Location>),
    /// multiple %dprec in the same rule
    MultipleDPrecDefinition(Vec<Location>),
    /// multiple %location in the same grammar
    MultipleLocationDefinition(Vec<Location>),

    StartNotDefined,
    TokenTypeNotDefined,

    /// duplicated name for terminal symbols or non-terminal symbols
    MultipleNameDefinition(String, Vec<Location>),

    /// can't use reserved keyword as token name
    ReservedName(Vec<Located<String>>),
}

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

#[derive(Debug)]
pub enum ParseError {
    /// different reduce type applied to the same terminal symbol
    MultipleReduceDefinition(Vec<Located<rusty_lr_core::rule::ReduceType>>),

    InvalidTerminalRange {
        location: Location,
        start: (Located<String>, usize),
        end: (Located<String>, usize),
    },

    /// name given to %start not defined
    StartNonTerminalNotDefined(Location),

    /// unknown terminal symbol name
    TerminalNotDefined(Location),

    /// not supported literal type
    UnsupportedLiteralType(Location),

    /// range in literal terminal set is not valid; [first, last] with first > last
    InvalidLiteralRange(Location),

    /// TokenType in Literal mode is not supported
    TokenInLiteralMode(Vec<Location>),

    /// conflicts in precedence definition
    MultiplePrecedenceOrderDefinition(Vec<Location>),

    /// Precedence not defined for the given token
    PrecedenceNotDefined(IdentOrLiteral),

    /// ReduceAction must be defined but not defined
    RuleTypeDefinedButActionNotDefined { nonterm: Location, rule: Location },

    /// Only terminal or terminal set is allowed
    OnlyTerminalSet(Location),

    /// unknown non-terminal symbol name
    NonTerminalNotDefined(Location),

    /// only 'usize' literal is allowed for %dprec
    OnlyUsizeLiteral(Location),

    /// bison variable $0 is not supported
    BisonVariableZero(Location),

    /// bison variable is out of range
    BisonVariableOutOfRange {
        location: Location,
        name: String,
        max: usize,
    },

    /// type inference failed for NonTerminal's ruletype placeholder '_'
    TypeInferenceFailed(Location),

    /// Circular dependency detected in variable substitution
    CircularDependency {
        location: Location,
        path: Vec<String>,
    },

    /// Maximum variable substitution depth exceeded
    MaxSubstitutionDepthExceeded {
        location: Location,
        max_depth: usize,
    },

    /// unknown diagnostic name allowed
    InvalidAllowDiagnostic { location: Location, name: String },
}
impl ArgError {
    pub fn to_compile_error(
        &self,
        span_manager: &crate::parser::location::SpanManager,
    ) -> TokenStream {
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
            | ArgError::MultipleLocationDefinition(locs)
            | ArgError::MultipleStartDefinition(locs)
            | ArgError::MultiplePrecDefinition(locs)
            | ArgError::MultipleDPrecDefinition(locs) => locs.clone(),
            ArgError::MultipleNameDefinition(_, locs) => locs.clone(),
            ArgError::ReservedName(names) => names.iter().map(|name| name.location()).collect(),
            _ => vec![Location::default()],
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
            ArgError::MultipleLocationDefinition(_) => "Multiple %location definition".into(),
            ArgError::MultipleStartDefinition(_) => "Multiple %start definition".into(),
            ArgError::MultiplePrecDefinition(_) => "Multiple %prec definition".into(),
            ArgError::MultipleDPrecDefinition(_) => "Multiple %dprec definition".into(),
            ArgError::StartNotDefined => "Start rule not defined\n>>> %start <rule_name>;".into(),
            ArgError::TokenTypeNotDefined => {
                "Token type not defined\n>>> %tokentype <token_type_name>;".into()
            }
            ArgError::MultipleNameDefinition(name, _) => {
                format!("Duplicated name for terminal or non-terminal: {}", name)
            }
            ArgError::ReservedName(_) => "This name is reserved and cannot be used".into(),
        }
    }
}
impl ParseArgError {
    pub fn to_compile_error(
        &self,
        span_manager: &crate::parser::location::SpanManager,
    ) -> TokenStream {
        let mut output = TokenStream::new();
        let message = self.short_message();
        let location = self.location();
        for span in span_manager.get_spans_in_location(&location) {
            output.extend(quote_spanned! {
                span=>
                compile_error!(#message);
            });
        }
        output
    }

    /// Returns the byte range `[start, end)` of the error location in the source.
    pub fn location(&self) -> Location {
        match self {
            ParseArgError::MacroLineParse { location, .. } => *location,
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ParseArgError::MacroLineParse { message, .. } => message.clone(),
        }
    }
}

impl ParseError {
    pub fn to_compile_error(
        &self,
        span_manager: &crate::parser::location::SpanManager,
    ) -> TokenStream {
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
            ParseError::MultipleReduceDefinition(locations) => {
                locations.iter().map(Located::location).collect()
            }

            ParseError::InvalidTerminalRange { location, .. } => vec![*location],

            ParseError::StartNonTerminalNotDefined(loc) => vec![*loc],

            ParseError::TerminalNotDefined(loc) => vec![*loc],

            ParseError::UnsupportedLiteralType(loc) => vec![*loc],

            ParseError::InvalidLiteralRange(loc) => vec![*loc],

            ParseError::TokenInLiteralMode(locs) => locs.clone(),

            ParseError::MultiplePrecedenceOrderDefinition(locations) => locations.clone(),
            ParseError::PrecedenceNotDefined(name) => vec![name.location()],

            ParseError::RuleTypeDefinedButActionNotDefined { nonterm, rule } => {
                vec![*nonterm, *rule]
            }
            ParseError::OnlyTerminalSet(location) => vec![*location],
            ParseError::NonTerminalNotDefined(loc) => vec![*loc],
            ParseError::OnlyUsizeLiteral(loc) => vec![*loc],
            ParseError::BisonVariableZero(loc) => vec![*loc],
            ParseError::BisonVariableOutOfRange { location, .. } => vec![*location],
            ParseError::TypeInferenceFailed(location) => vec![*location],
            ParseError::CircularDependency { location, .. } => vec![*location],
            ParseError::MaxSubstitutionDepthExceeded { location, .. } => vec![*location],
            ParseError::InvalidAllowDiagnostic { location, .. } => vec![*location],
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ParseError::MultipleReduceDefinition(_) => {
                "Different reduce type (%left and %right) applied to the same terminal symbol".to_string()
            }

            ParseError::InvalidTerminalRange { location: _, start, end } => {
                format!(
                    "Invalid terminal range: [{}({}) - {}({})]",
                    start.0, start.1, end.0, end.1
                )
            }

            ParseError::StartNonTerminalNotDefined(_) => {
                "Name given to %start not defined".to_string()
            }

            ParseError::TerminalNotDefined(_) => {
                "Unknown terminal symbol name".to_string()
            }

            ParseError::UnsupportedLiteralType(_) => {

                format!("This literal type is not supported. Use string literal (e.g. 'a', \"abc\", b'a' or b\"abc\") instead")
            }

            ParseError::InvalidLiteralRange(_)=>"Invalid literal range: [first, last] with first > last".to_string(),


            ParseError::TokenInLiteralMode(_) => {
                "%token with %tokentype `char` or `u8` is not supported. Use character literal (e.g. 'a' or b'a') instead"
                    .to_string()
            }

            ParseError::MultiplePrecedenceOrderDefinition(_) => {
                "Multiple precedence order definition for the same token".to_string()
            }

            ParseError::PrecedenceNotDefined(_) => {
                "Precedence not defined for the given token".to_string()
            }


            ParseError::RuleTypeDefinedButActionNotDefined { .. } => {
                "ReduceAction must be defined for this rule".into()
            }
            ParseError::OnlyTerminalSet(_) => "Only terminal or terminal set is allowed".into(),
            ParseError::NonTerminalNotDefined(_) => {
                "Unknown non-terminal symbol name".to_string()
            }
            ParseError::OnlyUsizeLiteral(_) => "Only 'usize' literal is allowed for %dprec".into(),
            ParseError::BisonVariableZero(_) => "bison variable $0 is not supported".into(),
            ParseError::BisonVariableOutOfRange { name, max, .. } => {
                format!("bison variable {} is out of range (max: {})", name, max)
            }
            ParseError::TypeInferenceFailed(_) => {
                "Type inference failed for NonTerminal rule type".to_string()
            }
            ParseError::CircularDependency { path, .. } => {
                format!(
                    "Circular dependency detected in variable substitutions: {}",
                    path.join(" -> ")
                )
            }
            ParseError::MaxSubstitutionDepthExceeded { max_depth, .. } => {
                format!(
                    "Maximum variable substitution depth ({}) exceeded",
                    max_depth
                )
            }
            ParseError::InvalidAllowDiagnostic { name, .. } => {
                format!("unknown diagnostic name: `{}`", name)
            }
        }
    }
}

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
            ConflictError::ShiftReduceConflict { .. } => Span::call_site(),
            ConflictError::ReduceReduceConflict { .. } => Span::call_site(),
        }
    }

    pub fn short_message(&self) -> String {
        match self {
            ConflictError::ShiftReduceConflict {
                term,
                reduce_rule: (_, rule),
                shift_rules,
            } => {
                format!(
                    "Shift-Reduce conflict with terminal symbol: {}\n>>> Reduce: {}\n>>> Shifts: {}",
                    term,
                    rule,
                    shift_rules
                        .iter()
                        .map(|(_, rule)| format!("{}", rule))
                        .collect::<Vec<_>>()
                        .join("\n>>>")
                )
            }
            ConflictError::ReduceReduceConflict {
                lookahead,
                rule1: (_, rule1),
                rule2: (_, rule2),
            } => {
                format!(
                    "Reduce-Reduce conflict with lookahead symbol: {}\n>>> Rule1: {}\n>>> Rule2: {}",
                    lookahead, rule1, rule2
                )
            }
        }
    }
}

/// Represents various compilation warnings encountered during grammar analysis
/// and parser generation (e.g., unused symbols, cycles, etc.).
#[derive(Debug, Clone)]
pub enum Warning {
    NonTermNotUsed { nonterm_name: Located<String> },
    Cycle { nonterm_name: Located<String> },
    NonTermDataNotUsed { nonterm_name: Located<String> },
    UnusedTerminals { class_idx: usize },
}

impl Warning {
    pub fn name(&self) -> &'static str {
        match self {
            Warning::NonTermNotUsed { .. } => "nonterm_not_used",
            Warning::Cycle { .. } => "cycle",
            Warning::NonTermDataNotUsed { .. } => "nonterm_data_not_used",
            Warning::UnusedTerminals { .. } => "unused_terminals",
        }
    }

    pub fn suggestion(&self, grammar: &crate::grammar::Grammar) -> String {
        match self {
            Warning::NonTermNotUsed { nonterm_name }
            | Warning::Cycle { nonterm_name }
            | Warning::NonTermDataNotUsed { nonterm_name } => {
                let name = nonterm_name.value();
                format!("%allow {}({});", self.name(), name)
            }
            Warning::UnusedTerminals { class_idx } => {
                let class_name = grammar.class_pretty_name_abbr(*class_idx);
                format!("%allow {}({});", self.name(), class_name)
            }
        }
    }

    /// Translates the warning into a `TokenStream` containing a compiler warning.
    /// Since Rust does not have a stable `compile_warning!` macro, this leverages
    /// a dummy deprecated struct definition mapped to the source code span
    /// where the warning originated, allowing stable Rust compilers to emit a diagnostic warning.
    pub fn to_compile_warning(
        &self,
        grammar: &crate::grammar::Grammar,
        span_manager: &crate::parser::location::SpanManager,
    ) -> TokenStream {
        if grammar.is_warning_allowed(self) {
            return TokenStream::new();
        }
        let mut output = TokenStream::new();
        let message = format!(
            "{} (to ignore, add `{}` to the grammar)",
            self.short_message(grammar),
            self.suggestion(grammar)
        );
        let locs = self.locations();
        if locs.is_empty() {
            let span = Span::call_site();
            output.extend(quote_spanned! {
                span=>
                const _: () = {
                    #[deprecated(since = "0.0.0", note = #message)]
                    struct Warning;
                    let _ = Warning;
                };
            });
        } else {
            for loc in locs {
                for span in span_manager.get_spans_in_location(&loc) {
                    output.extend(quote_spanned! {
                        span=>
                        const _: () = {
                            #[deprecated(since = "0.0.0", note = #message)]
                            struct Warning;
                            let _ = Warning;
                        };
                    });
                }
            }
        }
        output
    }

    /// Retrieves all source code locations associated with this warning.
    pub fn locations(&self) -> Vec<Location> {
        match self {
            Warning::NonTermNotUsed { nonterm_name } => vec![nonterm_name.location()],
            Warning::Cycle { nonterm_name } => vec![nonterm_name.location()],
            Warning::NonTermDataNotUsed { nonterm_name } => vec![nonterm_name.location()],
            Warning::UnusedTerminals { .. } => Vec::new(),
        }
    }

    /// Formats a short diagnostic message describing the warning.
    pub fn short_message(&self, grammar: &crate::grammar::Grammar) -> String {
        match self {
            Warning::NonTermNotUsed { nonterm_name } => {
                let name = nonterm_name.value();
                format!("Non-terminal `{name}` is not used in the grammar")
            }
            Warning::Cycle { nonterm_name } => {
                let name = nonterm_name.value();
                format!("Cycle detected: non-terminal `{name}` is involved in a bad cycle")
            }
            Warning::NonTermDataNotUsed { nonterm_name } => {
                let name = nonterm_name.value();
                format!("Non-terminal `{name}`'s data type is not used in any reduce action")
            }
            Warning::UnusedTerminals { class_idx } => {
                let class_name = grammar.class_pretty_name_abbr(*class_idx);
                let terminals = grammar.terminal_classes[*class_idx]
                    .terminals
                    .iter()
                    .map(|&term| grammar.term_pretty_name(term))
                    .collect::<Vec<_>>();
                format!(
                    "These terminals are not used: {class_name}: {}",
                    terminals.join(", ")
                )
            }
        }
    }
}

/// Represents informational diagnostics and details collected during parser construction
/// and conflict resolution (e.g. terminals merged, conflicts resolved, backtraces for GLR).
#[derive(Debug, Clone)]
pub enum Info {
    TerminalsMerged {
        class_idx: usize,
    },
    TerminalClassRuleMerge {
        rule_location: Location,
    },
    SingleNonTerminalRule {
        nonterm_name: Located<String>,
        rule_location: Location,
    },
    ReduceReduceConflictResolved {
        max_priority: usize,
        reduce_rules: Vec<usize>,
        deleted_rules: Vec<usize>,
    },
    ShiftReduceConflictResolvedShift {
        term: TerminalSymbol<usize>,
        shift_prec: usize,
        shift_rules: Vec<usize>,
        reduce_rules: Vec<(usize, usize)>,
    },
    ShiftReduceConflictResolvedReduce {
        term: TerminalSymbol<usize>,
        shift_prec: usize,
        shift_rules: Vec<usize>,
        reduce_rules: Vec<(usize, usize)>,
    },
    ShiftReduceConflictGLR {
        term: TerminalSymbol<usize>,
        shift_rules: Vec<usize>,
        shift_rules_backtrace: Vec<String>,
        reduce_rules: Vec<(usize, Vec<String>)>,
    },
    ReduceReduceConflictGLR {
        terms: Vec<TerminalSymbol<usize>>,
        reduce_rules: Vec<(usize, Vec<String>)>,
    },
}

impl Info {
    pub fn name(&self) -> &'static str {
        match self {
            Info::TerminalsMerged { .. } => "terminals_merged",
            Info::TerminalClassRuleMerge { .. } => "terminal_class_rule_merge",
            Info::SingleNonTerminalRule { .. } => "single_non_terminal_rule",
            Info::ReduceReduceConflictResolved { .. } => "reduce_reduce_conflict_resolved",
            Info::ShiftReduceConflictResolvedShift { .. } => "shift_reduce_conflict_resolved",
            Info::ShiftReduceConflictResolvedReduce { .. } => "shift_reduce_conflict_resolved",
            Info::ShiftReduceConflictGLR { .. } => "shift_reduce_conflict_glr",
            Info::ReduceReduceConflictGLR { .. } => "reduce_reduce_conflict_glr",
        }
    }

    pub fn suggestion(&self, grammar: &crate::grammar::Grammar) -> String {
        match self {
            Info::SingleNonTerminalRule { nonterm_name, .. } => {
                let name = nonterm_name.value();
                format!("%allow {}({});", self.name(), name)
            }
            Info::TerminalClassRuleMerge { rule_location } => {
                let mut name = String::new();
                for nonterm in &grammar.nonterminals {
                    for rule in &nonterm.rules {
                        if rule.location() == *rule_location {
                            name = nonterm.name.value().clone();
                            break;
                        }
                    }
                }
                if name.is_empty() {
                    format!("%allow {};", self.name())
                } else {
                    format!("%allow {}({});", self.name(), name)
                }
            }
            Info::TerminalsMerged { class_idx } => {
                let class_name = format!(
                    "TerminalClass{}",
                    grammar.terminal_classes[*class_idx].multiterm_counter
                );
                format!("%allow {}({});", self.name(), class_name)
            }
            Info::ReduceReduceConflictResolved { reduce_rules, .. } => {
                if let Some(&r) = reduce_rules.first() {
                    if let Some((nonterm, _)) = grammar.get_rule_by_id(r) {
                        return format!("%allow {}({});", self.name(), nonterm.name.value());
                    }
                }
                format!("%allow {};", self.name())
            }
            Info::ShiftReduceConflictResolvedShift { term, .. }
            | Info::ShiftReduceConflictResolvedReduce { term, .. }
            | Info::ShiftReduceConflictGLR { term, .. } => {
                let term_name = match term {
                    TerminalSymbol::Term(t) => grammar.class_pretty_name_abbr(*t),
                    TerminalSymbol::Error => "error".to_string(),
                    TerminalSymbol::Eof => "$".to_string(),
                };
                format!("%allow {}({});", self.name(), term_name)
            }
            Info::ReduceReduceConflictGLR { terms, .. } => {
                if let Some(term) = terms.first() {
                    let term_name = match term {
                        TerminalSymbol::Term(t) => grammar.class_pretty_name_abbr(*t),
                        TerminalSymbol::Error => "error".to_string(),
                        TerminalSymbol::Eof => "$".to_string(),
                    };
                    return format!("%allow {}({});", self.name(), term_name);
                }
                format!("%allow {};", self.name())
            }
        }
    }
}
