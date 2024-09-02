use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use std::collections::BTreeSet;

use rusty_lr_core::ReduceType;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::pattern::Pattern;
use crate::pattern::PatternType;
use crate::terminalset::TerminalSet;
use crate::utils;

/// parsed arguments for reduce type def
pub enum TerminalSetOrIdent {
    Ident(Ident),
    TerminalSet(TerminalSet),
}

impl TerminalSetOrIdent {
    pub fn to_terminal_set(&self, grammar: &Grammar) -> Result<BTreeSet<usize>, ParseError> {
        match self {
            TerminalSetOrIdent::Ident(ident) => {
                if let Some(idx) = grammar.terminals_index.get(ident) {
                    Ok(BTreeSet::from([*idx]))
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

impl std::fmt::Display for TerminalSetOrIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TerminalSetOrIdent::Ident(ident) => write!(f, "{}", ident),
            TerminalSetOrIdent::TerminalSet(terminal_set) => write!(f, "{}", terminal_set),
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

impl std::fmt::Display for PatternArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PatternArgs::Ident(ident) => write!(f, "{}", ident),
            PatternArgs::Plus(base, _) => write!(f, "{}+", base),
            PatternArgs::Star(base, _) => write!(f, "{}*", base),
            PatternArgs::Question(base, _) => write!(f, "{}?", base),
            PatternArgs::Exclamation(base, _) => write!(f, "{}", base),
            PatternArgs::TerminalSet(terminal_set) => write!(f, "{}", terminal_set),
            PatternArgs::Lookaheads(base, terminal_set) => {
                write!(f, "{}/{}", base, terminal_set)
            }
            PatternArgs::Group(group, _, _) => {
                write!(
                    f,
                    "({})",
                    group
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
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
        let pretty_name = format!("{}", self);
        match self {
            PatternArgs::Ident(ident) => {
                utils::check_reserved_name(&ident)?;
                let pattern = Pattern {
                    pattern_type: PatternType::Ident(ident),
                    pretty_name: pretty_name.clone(),
                };
                if put_exclamation {
                    Ok(Pattern {
                        pattern_type: PatternType::Exclamation(Box::new(pattern)),
                        pretty_name,
                    })
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Plus(pattern, _) => Ok(Pattern {
                pattern_type: PatternType::Plus(Box::new(
                    pattern.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Star(pattern, _) => Ok(Pattern {
                pattern_type: PatternType::Star(Box::new(
                    pattern.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Question(pattern, _) => Ok(Pattern {
                pattern_type: PatternType::Question(Box::new(
                    pattern.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Exclamation(pattern, _) => pattern.into_pattern(grammar, true),
            PatternArgs::TerminalSet(terminal_set) => {
                let terminal_set = terminal_set.to_terminal_set(grammar)?;
                let pattern = Pattern {
                    pattern_type: PatternType::TerminalSet(terminal_set),
                    pretty_name: pretty_name.clone(),
                };
                if put_exclamation {
                    Ok(Pattern {
                        pattern_type: PatternType::Exclamation(Box::new(pattern)),
                        pretty_name,
                    })
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Lookaheads(pattern, terminalset) => {
                let terminal_set = terminalset.to_terminal_set(grammar)?;
                let pattern = Pattern {
                    pattern_type: PatternType::Lookaheads(
                        Box::new(pattern.into_pattern(grammar, put_exclamation)?),
                        terminal_set,
                    ),
                    pretty_name,
                };
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
                Ok(Pattern {
                    pattern_type: PatternType::Group(patterns),
                    pretty_name,
                })
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
    /// user assigned id for this rule line, currently not in use
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
    pub rules: Vec<RuleDefArgs>,
    pub lalr: bool,
    pub glr: bool,
}
