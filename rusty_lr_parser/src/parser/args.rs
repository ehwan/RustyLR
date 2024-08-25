use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use std::collections::BTreeSet;

use rusty_lr_core::ReduceType;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::pattern::Pattern;
use crate::terminalset::TerminalSet;
use crate::utils;

/// parsed arguments for reduce type def
pub enum TerminalSetOrIdent {
    Ident(Ident),
    TerminalSet(TerminalSet),
}

impl TerminalSetOrIdent {
    pub fn to_terminal_set(&self, grammar: &Grammar) -> Result<BTreeSet<Ident>, ParseError> {
        match self {
            TerminalSetOrIdent::Ident(ident) => {
                if grammar.terminals.contains_key(ident) {
                    Ok(BTreeSet::from([ident.clone()]))
                } else {
                    Err(ParseError::TerminalNotDefined(ident.clone()))
                }
            }
            TerminalSetOrIdent::TerminalSet(terminal_set) => terminal_set.to_terminal_set(grammar),
        }
    }
    pub fn span_pair(&self) -> (Span, Span) {
        match self {
            TerminalSetOrIdent::Ident(ident) => (ident.span(), ident.span()),
            TerminalSetOrIdent::TerminalSet(terminal_set) => {
                (terminal_set.open_span, terminal_set.close_span)
            }
        }
    }
}

/// parsed arguments for pattern
pub enum PatternArgs {
    Ident(Ident),

    /// span of punctuation('+', '*', ...) after the pattern
    Plus(Box<PatternArgs>, Span),
    Star(Box<PatternArgs>, Span),
    Question(Box<PatternArgs>, Span),
    Exclamation(Box<PatternArgs>, Span),

    /// span of '[' and ']' containing terminal set
    /// a group delimited by '[' and ']' containing terminal set
    TerminalSet(TerminalSet),

    /// force lookahead tokens for this pattern.
    /// lookaheads will not be consumed.
    /// span of the rightmost of this pattern
    Lookaheads(Box<PatternArgs>, TerminalSetOrIdent),

    /// ( Pattern+ )
    /// span of '(' and ')'
    Group(Vec<PatternArgs>, Span, Span),
}

impl PatternArgs {
    /// When converting to Pattern, if any exclamation mark `!` is present, put it in the most inner pattern.
    /// e.g. Pattern like `A+?!` will be converted to `A!+?`
    /// Since `A+` or `A?` will generate new rule with RuleType Vec<T> or Option<T>,
    /// it is more efficient to put the exclamation mark inside the pattern.
    pub fn into_pattern(
        self,
        grammar: &Grammar,
        put_exclamation: bool,
    ) -> Result<Pattern, ParseError> {
        match self {
            PatternArgs::Ident(ident) => {
                utils::check_reserved_name(&ident)?;
                let pattern = Pattern::Ident(ident);
                if put_exclamation {
                    Ok(Pattern::Exclamation(Box::new(pattern)))
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Plus(pattern, _) => Ok(Pattern::Plus(Box::new(
                pattern.into_pattern(grammar, put_exclamation)?,
            ))),
            PatternArgs::Star(pattern, _) => Ok(Pattern::Star(Box::new(
                pattern.into_pattern(grammar, put_exclamation)?,
            ))),
            PatternArgs::Question(pattern, _) => Ok(Pattern::Question(Box::new(
                pattern.into_pattern(grammar, put_exclamation)?,
            ))),
            PatternArgs::Exclamation(pattern, _) => pattern.into_pattern(grammar, true),
            PatternArgs::TerminalSet(terminal_set) => {
                let pattern = Pattern::TerminalSet(terminal_set.to_terminal_set(grammar)?);
                if put_exclamation {
                    Ok(Pattern::Exclamation(Box::new(pattern)))
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Lookaheads(pattern, terminalset) => {
                let terminal_set = terminalset.to_terminal_set(grammar)?;
                let pattern = Pattern::Lookaheads(
                    Box::new(pattern.into_pattern(grammar, put_exclamation)?),
                    terminal_set,
                );
                Ok(pattern)
            }
            PatternArgs::Group(group, _, _) => {
                if group.len() == 1 {
                    return group
                        .into_iter()
                        .next()
                        .unwrap()
                        .into_pattern(grammar, put_exclamation);
                }
                let mut patterns = Vec::with_capacity(group.len());
                for pattern in group.into_iter() {
                    patterns.push(pattern.into_pattern(grammar, put_exclamation)?);
                }
                Ok(Pattern::Group(patterns))
            }
        }
    }
    pub fn span_pair(&self) -> (Span, Span) {
        match self {
            PatternArgs::Ident(ident) => {
                let span = ident.span();
                (span, span)
            }
            PatternArgs::Plus(base, span) => (base.span_pair().0, *span),
            PatternArgs::Star(base, span) => (base.span_pair().0, *span),
            PatternArgs::Question(base, span) => (base.span_pair().0, *span),
            PatternArgs::Exclamation(base, span) => (base.span_pair().0, *span),
            PatternArgs::TerminalSet(terminal_set) => {
                (terminal_set.open_span, terminal_set.close_span)
            }
            PatternArgs::Lookaheads(base, terminal_set) => {
                (base.span_pair().0, terminal_set.span_pair().1)
            }
            PatternArgs::Group(_, open, close) => (*open, *close),
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
    /// user assigned id for this rule line
    pub id: usize,
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
    pub reduce_types: Vec<(TerminalSetOrIdent, ReduceType)>,
    pub derives: Vec<TokenStream>,
    pub rules: Vec<RuleDefArgs>,
    pub glr: bool,
}
