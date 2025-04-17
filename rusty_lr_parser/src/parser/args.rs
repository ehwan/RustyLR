use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;

use std::collections::BTreeSet;

use rusty_lr_core::ReduceType;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::pattern::Pattern;
use crate::pattern::PatternType;
use crate::terminalset::TerminalSet;
use crate::utils;

/// parsed arguments for reduce type def
pub enum TerminalOrTerminalSet {
    Ident(Ident),
    Literal(Literal),
    TerminalSet(TerminalSet),
}

impl TerminalOrTerminalSet {
    pub fn to_terminal_set(
        &self,
        grammar: &mut Grammar,
        include_eof: bool,
    ) -> Result<BTreeSet<usize>, ParseError> {
        match self {
            TerminalOrTerminalSet::Ident(ident) => {
                if let Some(idx) = grammar.terminals_index.get(ident) {
                    Ok(BTreeSet::from([*idx]))
                } else {
                    Err(ParseError::TerminalNotDefined(ident.clone()))
                }
            }
            TerminalOrTerminalSet::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2::<syn::Lit>");

                if grammar.is_char {
                    if !matches!(&lit, syn::Lit::Char(_)) {
                        return Err(ParseError::UnsupportedLiteralType(literal.clone()));
                    }
                } else if grammar.is_u8 {
                    if !matches!(&lit, syn::Lit::Byte(_)) {
                        return Err(ParseError::UnsupportedLiteralType(literal.clone()));
                    }
                } else {
                    return Err(ParseError::UnsupportedLiteralType(literal.clone()));
                }

                let idx = grammar.add_or_get_literal_character(lit, None).unwrap();
                Ok(BTreeSet::from([idx]))
            }
            TerminalOrTerminalSet::TerminalSet(terminal_set) => {
                terminal_set.to_terminal_set(grammar, include_eof)
            }
        }
    }
    pub fn span_pair(&self) -> (Span, Span) {
        match self {
            TerminalOrTerminalSet::Ident(ident) => (ident.span(), ident.span()),
            TerminalOrTerminalSet::Literal(literal) => (literal.span(), literal.span()),
            TerminalOrTerminalSet::TerminalSet(terminal_set) => {
                (terminal_set.open_span, terminal_set.close_span)
            }
        }
    }
}

impl std::fmt::Display for TerminalOrTerminalSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TerminalOrTerminalSet::Ident(ident) => write!(f, "{}", ident),
            TerminalOrTerminalSet::Literal(literal) => write!(f, "{}", literal),
            TerminalOrTerminalSet::TerminalSet(terminal_set) => write!(f, "{}", terminal_set),
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
    Lookaheads(Box<PatternArgs>, TerminalOrTerminalSet),

    /// ( Pattern+ )
    /// span of '(' and ')'
    Group(Vec<PatternArgs>, Span, Span),

    /// 'a', b'a', "abc", b"abc"
    Literal(Literal),
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
            PatternArgs::Literal(literal) => {
                write!(f, "{}", literal)
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
        grammar: &mut Grammar,
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
                let terminal_set = terminal_set.to_terminal_set(grammar, false)?;
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
                let terminal_set = terminalset.to_terminal_set(grammar, true)?;
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
            PatternArgs::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2::<syn::Lit>");
                if grammar.is_char {
                    if !matches!(&lit, syn::Lit::Char(_) | syn::Lit::Str(_)) {
                        return Err(ParseError::UnsupportedLiteralType(literal.clone()));
                    }
                } else if grammar.is_u8 {
                    if !matches!(&lit, syn::Lit::Byte(_) | syn::Lit::ByteStr(_)) {
                        return Err(ParseError::UnsupportedLiteralType(literal.clone()));
                    }
                } else {
                    return Err(ParseError::UnsupportedLiteralType(literal.clone()));
                }
                if !matches!(
                    &lit,
                    syn::Lit::Char(_) | syn::Lit::Byte(_) | syn::Lit::Str(_) | syn::Lit::ByteStr(_)
                ) {
                    return Err(ParseError::UnsupportedLiteralType(literal.clone()));
                }

                let pattern = Pattern {
                    pattern_type: PatternType::Literal(lit),
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
            PatternArgs::Literal(literal) => {
                let span = literal.span();
                (span, span)
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
    pub reduce_types: Vec<(TerminalOrTerminalSet, ReduceType)>,
    pub rules: Vec<RuleDefArgs>,
    pub lalr: bool,
    pub glr: bool,
}
