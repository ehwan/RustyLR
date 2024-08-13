use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use rusty_lr_core::ReduceType;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::pattern::Pattern;
use crate::terminalset::TerminalSet;
use crate::utils;

/// parsed arguments for reduce type def
pub enum ReduceTypeArgs {
    Ident(Ident),
    TerminalSet(TerminalSet),
}

/// parsed arguments for pattern
pub enum PatternArgs {
    /// span of the ident
    Ident(Ident, Span),
    /// span of '+' or '*' or '?' after the pattern
    Plus(Box<PatternArgs>, Span),
    Star(Box<PatternArgs>, Span),
    Question(Box<PatternArgs>, Span),
    /// span of '[' and ']' containing terminal set
    /// a group delimited by '[' and ']' containing terminal set
    TerminalSet(TerminalSet),
}

impl PatternArgs {
    pub fn to_pattern(&self, grammar: &Grammar) -> Result<Pattern, ParseError> {
        match self {
            PatternArgs::Ident(ident, _) => {
                utils::check_reserved_name(ident)?;
                Ok(Pattern::Ident(ident.clone()))
            }
            PatternArgs::Plus(pattern, _) => {
                Ok(Pattern::Plus(Box::new(pattern.to_pattern(grammar)?)))
            }
            PatternArgs::Star(pattern, _) => {
                Ok(Pattern::Star(Box::new(pattern.to_pattern(grammar)?)))
            }
            PatternArgs::Question(pattern, _) => {
                Ok(Pattern::Question(Box::new(pattern.to_pattern(grammar)?)))
            }
            PatternArgs::TerminalSet(terminal_set) => {
                Ok(Pattern::TerminalSet(terminal_set.to_terminal_set(grammar)?))
            }
        }
    }
    pub fn span_pair(&self) -> (Span, Span) {
        match self {
            PatternArgs::Ident(_, span) => (*span, *span),
            PatternArgs::Plus(base, span) => (base.span_pair().0, *span),
            PatternArgs::Star(base, span) => (base.span_pair().0, *span),
            PatternArgs::Question(base, span) => (base.span_pair().0, *span),
            PatternArgs::TerminalSet(terminal_set) => {
                (terminal_set.open_span, terminal_set.close_span)
            }
        }
    }
}

/// parsed arguments for single line of a rule separated by '|'
pub struct RuleLineArgs {
    /// mapto '=' pattern
    pub tokens: Vec<(Option<Ident>, PatternArgs)>,
    pub reduce_action: Option<TokenStream>,
    /// span of ':' or '|' in front of this rule line
    pub separator_span: Span,
}

/// parsed arguments for multiple lines of a rule
pub struct RuleDefArgs {
    pub name: Ident,
    pub typename: Option<TokenStream>,
    pub rule_lines: Vec<RuleLineArgs>,
}

/// parsed arguments for the grammar
#[derive(Default)]
pub struct GrammarArgs {
    pub module_prefix: Vec<(Span, TokenStream)>,
    pub token_typename: Vec<(Span, TokenStream)>,
    pub userdata_typename: Vec<(Span, TokenStream)>,
    pub start_rule_name: Vec<Ident>,
    pub eof: Vec<(Span, TokenStream)>,
    pub error_typename: Vec<(Span, TokenStream)>,
    pub terminals: Vec<(Ident, TokenStream)>,
    pub reduce_types: Vec<(ReduceTypeArgs, ReduceType)>,
    pub rules: Vec<RuleDefArgs>,
}
