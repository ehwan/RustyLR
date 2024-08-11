use proc_macro2::Ident;
use proc_macro2::TokenStream;
use rusty_lr_core::ReduceType;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::pattern::Pattern;
use crate::terminalset::TerminalSet;

/// parsed arguments for reduce type def
pub(crate) enum ReduceTypeArgs {
    Ident(Ident),
    TerminalSet(TerminalSet),
}

/// parsed arguments for pattern
pub(crate) enum PatternArgs {
    Ident(Ident),
    Plus(Box<PatternArgs>),
    Star(Box<PatternArgs>),
    Question(Box<PatternArgs>),
    /// a group delimited by '[' and ']' containing terminal set
    TerminalSet(TerminalSet),
}

impl PatternArgs {
    pub fn to_pattern(&self, grammar: &Grammar) -> Result<Pattern, ParseError> {
        match self {
            PatternArgs::Ident(ident) => Ok(Pattern::Ident(ident.clone())),
            PatternArgs::Plus(pattern) => Ok(Pattern::Plus(Box::new(pattern.to_pattern(grammar)?))),
            PatternArgs::Star(pattern) => Ok(Pattern::Star(Box::new(pattern.to_pattern(grammar)?))),
            PatternArgs::Question(pattern) => {
                Ok(Pattern::Question(Box::new(pattern.to_pattern(grammar)?)))
            }
            PatternArgs::TerminalSet(terminal_set) => {
                Ok(Pattern::TerminalSet(terminal_set.to_terminal_set(grammar)?))
            }
        }
    }
}

/// parsed arguments for single line of a rule separated by '|'
pub(crate) struct RuleLineArgs {
    /// mapto '=' pattern
    pub tokens: Vec<(Option<Ident>, PatternArgs)>,
    pub reduce_action: Option<TokenStream>,
}

/// parsed arguments for multiple lines of a rule
pub(crate) struct RuleDefArgs {
    pub name: Ident,
    pub typename: Option<TokenStream>,
    pub rule_lines: Vec<RuleLineArgs>,
}

/// parsed arguments for the grammar
#[derive(Default)]
pub(crate) struct GrammarArgs {
    pub module_prefix: Option<TokenStream>,
    pub token_typename: Option<TokenStream>,
    pub userdata_typename: Option<TokenStream>,
    pub start_rule_name: Option<Ident>,
    pub eof: Option<TokenStream>,
    pub error_typename: Option<TokenStream>,
    pub terminals: Vec<(Ident, TokenStream)>,
    pub reduce_types: Vec<(ReduceTypeArgs, ReduceType)>,
    pub rules: Vec<RuleDefArgs>,
}
