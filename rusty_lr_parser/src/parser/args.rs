use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::parser::location::Location;
use crate::pattern::Pattern;
use crate::pattern::PatternInternal;
use crate::terminal_info::TerminalName;
use crate::terminalset::TerminalSet;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum IdentOrU32 {
    Ident(Ident),
    U32(u32),
}

#[derive(Debug, Clone)]
pub enum IdentOrLiteral {
    Ident(Ident),
    Literal(Literal),
}
impl IdentOrLiteral {
    pub fn span(&self) -> Span {
        match self {
            IdentOrLiteral::Ident(ident) => ident.span(),
            IdentOrLiteral::Literal(literal) => literal.span(),
        }
    }
    pub fn into_ident_or_u32(self, grammar: &Grammar) -> Result<IdentOrU32, ParseError> {
        match self {
            Self::Ident(ident) => Ok(IdentOrU32::Ident(ident)),
            Self::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2::<syn::Lit>");
                let val = grammar.get_char_value(&lit)?;
                Ok(IdentOrU32::U32(val))
            }
        }
    }
    pub fn range_resolve(&self, grammar: &mut Grammar) -> Result<(), ParseError> {
        match self {
            IdentOrLiteral::Ident(_) => {}
            IdentOrLiteral::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2::<syn::Lit>");
                let val = grammar.get_char_value(&lit)?;
                grammar.range_resolver.insert(val, val);
            }
        }
        Ok(())
    }
}
impl std::fmt::Display for IdentOrLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            IdentOrLiteral::Ident(ident) => write!(f, "{}", ident),
            IdentOrLiteral::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

/// parsed arguments for pattern
#[derive(Clone)]
pub enum PatternArgs {
    Ident(Ident),

    /// location of punctuation('+', '*', ...) after the pattern
    Plus {
        base: Box<PatternArgs>,
        op_location: Location,
    },
    Star {
        base: Box<PatternArgs>,
        op_location: Location,
    },
    Question {
        base: Box<PatternArgs>,
        op_location: Location,
    },
    Exclamation {
        base: Box<PatternArgs>,
        op_location: Location,
    },

    /// a group delimited by '[' and ']' containing terminal set
    TerminalSet(TerminalSet),

    /// force lookahead tokens for this pattern.
    /// lookaheads will not be consumed.
    Lookaheads {
        pattern: Box<PatternArgs>,
        lookaheads: Box<PatternArgs>,
    },

    /// ( Pattern+ )
    /// alternatives is a list of alternatives (separated by '|'),
    /// each alternative is a list of patterns.
    Group {
        alternatives: Vec<Vec<PatternArgs>>,
        open_location: Location,
        close_location: Location,
    },

    /// 'a', b'a', "abc", b"abc"
    Literal(Literal),

    /// Pattern - Terminals exclusion
    Minus {
        base: Box<PatternArgs>,
        exclude: Box<PatternArgs>,
    },

    Sep {
        base: Box<PatternArgs>,
        delimiter: Box<PatternArgs>,
        /// if true, use '+' (at least one); otherwise '*' (zero or more)
        at_least_one: bool,
        /// location of the whole $sep(...)
        location: Location,
    },
}

impl std::fmt::Display for PatternArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PatternArgs::Ident(ident) => write!(f, "{}", ident),
            PatternArgs::Plus { base, .. } => write!(f, "{}+", base),
            PatternArgs::Star { base, .. } => write!(f, "{}*", base),
            PatternArgs::Question { base, .. } => write!(f, "{}?", base),
            PatternArgs::Exclamation { base, .. } => write!(f, "{}", base),
            PatternArgs::TerminalSet(terminal_set) => write!(f, "{}", terminal_set),
            PatternArgs::Lookaheads {
                pattern,
                lookaheads,
            } => {
                write!(f, "{}/{}", pattern, lookaheads)
            }
            PatternArgs::Group { alternatives, .. } => {
                write!(
                    f,
                    "({})",
                    alternatives
                        .iter()
                        .map(|p| p
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", "))
                        .collect::<Vec<_>>()
                        .join(" | ")
                )
            }
            PatternArgs::Literal(literal) => {
                write!(f, "{}", literal)
            }
            PatternArgs::Minus { base, exclude } => {
                write!(f, "{}-{}", base, exclude)
            }
            PatternArgs::Sep {
                base,
                delimiter,
                at_least_one,
                ..
            } => {
                write!(
                    f,
                    "$sep({base}, {delimiter}, {})",
                    if *at_least_one { '+' } else { '*' }
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
        grammar: &mut Grammar,
        put_exclamation: bool,
    ) -> Result<Pattern, ParseError> {
        let pretty_name = format!("{}", self);
        match self {
            PatternArgs::Ident(ident) => {
                // utils::check_reserved_name(&ident)?;
                let pattern = Pattern {
                    internal: PatternInternal::Ident(ident),
                    pretty_name: pretty_name.clone(),
                };
                if put_exclamation {
                    Ok(Pattern {
                        internal: PatternInternal::Exclamation(Box::new(pattern)),
                        pretty_name,
                    })
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Plus { base, .. } => Ok(Pattern {
                internal: PatternInternal::Plus(Box::new(
                    base.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Star { base, .. } => Ok(Pattern {
                internal: PatternInternal::Star(Box::new(
                    base.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Question { base, .. } => Ok(Pattern {
                internal: PatternInternal::Question(Box::new(
                    base.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Exclamation { base, .. } => base.into_pattern(grammar, true),
            PatternArgs::TerminalSet(terminal_set) => {
                let (negate, terminal_set) = terminal_set.to_terminal_set(grammar)?;
                let pattern = Pattern {
                    internal: PatternInternal::TerminalSet(negate, terminal_set),
                    pretty_name: pretty_name.clone(),
                };
                if put_exclamation {
                    Ok(Pattern {
                        internal: PatternInternal::Exclamation(Box::new(pattern)),
                        pretty_name,
                    })
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Lookaheads {
                pattern,
                lookaheads,
            } => {
                let (negate, terminal_set) = lookaheads.to_terminal_set(grammar)?;
                let pattern = Pattern {
                    internal: PatternInternal::Lookaheads(
                        Box::new(pattern.into_pattern(grammar, put_exclamation)?),
                        negate,
                        terminal_set,
                    ),
                    pretty_name,
                };
                Ok(pattern)
            }
            PatternArgs::Group { alternatives, .. } => {
                if alternatives.len() == 1 && alternatives[0].len() == 1 {
                    let line = alternatives.into_iter().next().unwrap();
                    return line
                        .into_iter()
                        .next()
                        .unwrap()
                        .into_pattern(grammar, put_exclamation);
                }

                let patterns = alternatives
                    .into_iter()
                    .map(|line| {
                        line.into_iter()
                            .map(|p| p.into_pattern(grammar, put_exclamation))
                            .collect::<Result<_, _>>()
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Pattern {
                    internal: PatternInternal::Group(patterns),
                    pretty_name,
                })
            }
            PatternArgs::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2::<syn::Lit>");
                if grammar.is_char {
                    if !matches!(&lit, syn::Lit::Char(_) | syn::Lit::Str(_)) {
                        return Err(ParseError::UnsupportedLiteralType(
                            literal.to_token_stream(),
                        ));
                    }
                } else if grammar.is_u8 {
                    if !matches!(&lit, syn::Lit::Byte(_) | syn::Lit::ByteStr(_)) {
                        return Err(ParseError::UnsupportedLiteralType(
                            literal.to_token_stream(),
                        ));
                    }
                } else {
                    return Err(ParseError::UnsupportedLiteralType(
                        literal.to_token_stream(),
                    ));
                }
                if !matches!(
                    &lit,
                    syn::Lit::Char(_) | syn::Lit::Byte(_) | syn::Lit::Str(_) | syn::Lit::ByteStr(_)
                ) {
                    return Err(ParseError::UnsupportedLiteralType(
                        literal.to_token_stream(),
                    ));
                }

                let pattern = Pattern {
                    internal: PatternInternal::Literal(lit),
                    pretty_name: pretty_name.clone(),
                };
                if put_exclamation {
                    Ok(Pattern {
                        internal: PatternInternal::Exclamation(Box::new(pattern)),
                        pretty_name,
                    })
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Minus { .. } => {
                let (negate, terminal_set) = self.to_terminal_set(grammar)?;
                let pattern = Pattern {
                    internal: PatternInternal::TerminalSet(negate, terminal_set),
                    pretty_name: pretty_name.clone(),
                };
                if put_exclamation {
                    Ok(Pattern {
                        internal: PatternInternal::Exclamation(Box::new(pattern)),
                        pretty_name,
                    })
                } else {
                    Ok(pattern)
                }
            }
            PatternArgs::Sep {
                base,
                delimiter,
                at_least_one,
                ..
            } => {
                let base = base.into_pattern(grammar, put_exclamation)?;
                let delimiter = delimiter.into_pattern(grammar, false)?;
                let pattern = Pattern {
                    internal: PatternInternal::Sep(
                        Box::new(base),
                        Box::new(delimiter),
                        at_least_one,
                    ),
                    pretty_name: pretty_name.clone(),
                };
                Ok(pattern)
            }
        }
    }

    /// if this pattern consists of single-length terminals, returns the set of terminals
    pub fn to_terminal_set(
        &self,
        grammar: &mut Grammar,
    ) -> Result<(bool, BTreeSet<usize>), ParseError> {
        match self {
            PatternArgs::TerminalSet(terminal_set) => terminal_set.to_terminal_set(grammar),
            PatternArgs::Ident(ident) => {
                if let Some(idx) = grammar
                    .terminals_index
                    .get(&TerminalName::Ident(ident.clone()))
                {
                    Ok((false, BTreeSet::from([*idx])))
                } else {
                    Err(ParseError::TerminalNotDefined(ident.clone()))
                }
            }
            PatternArgs::Plus {
                base,
                op_location: op_span,
            } => Err(ParseError::OnlyTerminalSet(base.location(), *op_span)),
            PatternArgs::Star {
                base,
                op_location: op_span,
            } => Err(ParseError::OnlyTerminalSet(base.location(), *op_span)),
            PatternArgs::Question {
                base,
                op_location: op_span,
            } => Err(ParseError::OnlyTerminalSet(base.location(), *op_span)),
            PatternArgs::Exclamation { base, .. } => base.to_terminal_set(grammar),
            PatternArgs::Lookaheads { pattern, .. } => {
                let sp = pattern.location();
                Err(ParseError::OnlyTerminalSet(sp, sp))
            }
            PatternArgs::Group {
                alternatives,
                open_location: open_span,
                close_location: close_span,
            } => {
                if alternatives.len() == 1 && alternatives[0].len() == 1 {
                    alternatives[0][0].to_terminal_set(grammar)
                } else {
                    Err(ParseError::OnlyTerminalSet(*open_span, *close_span))
                }
            }
            PatternArgs::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2::<syn::Lit>");
                let val = grammar.get_char_value(&lit)?;
                let name: TerminalName = (val, val).into();
                let idx = *grammar.terminals_index.get(&name).unwrap();
                Ok((false, BTreeSet::from([idx])))
            }
            PatternArgs::Minus { base, exclude } => {
                let (negate_lhs, mut lhs_set) = base.to_terminal_set(grammar)?;
                let (negate_rhs, mut rhs_set) = exclude.to_terminal_set(grammar)?;

                // A - B = A and B^c

                match (negate_lhs, negate_rhs) {
                    // L and R^c
                    (false, false) => Ok((false, lhs_set.difference(&rhs_set).copied().collect())),
                    // L^c and R^c = (L or R)^c
                    (true, false) => {
                        lhs_set.append(&mut rhs_set);
                        Ok((true, lhs_set))
                    }
                    // L and R
                    (false, true) => Ok((true, lhs_set.intersection(&rhs_set).copied().collect())),

                    // L^c and R
                    (true, true) => Ok((false, rhs_set.difference(&lhs_set).copied().collect())),
                }
            }
            PatternArgs::Sep { location: span, .. } => {
                Err(ParseError::OnlyTerminalSet(*span, *span))
            }
        }
    }
    pub fn location(&self) -> Location {
        match self {
            PatternArgs::Ident(ident) => ident.span().into(),
            PatternArgs::Plus {
                base,
                op_location: op_span,
            } => base.location().merge(op_span),
            PatternArgs::Star {
                base,
                op_location: op_span,
            } => base.location().merge(op_span),
            PatternArgs::Question {
                base,
                op_location: op_span,
            } => base.location().merge(op_span),
            PatternArgs::Exclamation {
                base,
                op_location: op_span,
            } => base.location().merge(op_span),
            PatternArgs::TerminalSet(terminal_set) => terminal_set.location(),
            PatternArgs::Lookaheads {
                pattern,
                lookaheads,
            } => pattern.location().merge(&lookaheads.location()),
            PatternArgs::Group {
                open_location: open_span,
                close_location: close_span,
                ..
            } => open_span.merge(close_span),
            PatternArgs::Literal(literal) => literal.span().into(),
            PatternArgs::Minus { base, exclude } => base.location().merge(&exclude.location()),
            PatternArgs::Sep { location: span, .. } => *span,
        }
    }

    pub fn range_resolve(&self, grammar: &mut Grammar) -> Result<(), ParseError> {
        match self {
            PatternArgs::Ident(_) => Ok(()),
            PatternArgs::Plus { base, .. } => base.range_resolve(grammar),
            PatternArgs::Star { base, .. } => base.range_resolve(grammar),
            PatternArgs::Question { base, .. } => base.range_resolve(grammar),
            PatternArgs::Exclamation { base, .. } => base.range_resolve(grammar),
            PatternArgs::TerminalSet(terminal_set) => terminal_set.range_resolve(grammar),
            PatternArgs::Lookaheads {
                pattern,
                lookaheads,
            } => {
                pattern.range_resolve(grammar)?;
                lookaheads.range_resolve(grammar)?;
                Ok(())
            }
            PatternArgs::Group { alternatives, .. } => {
                for line in alternatives {
                    for pattern in line {
                        pattern.range_resolve(grammar)?;
                    }
                }
                Ok(())
            }
            PatternArgs::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2::<syn::Lit>");
                match lit {
                    syn::Lit::Char(lit) => {
                        let val = lit.value() as u32;
                        grammar.range_resolver.insert(val, val);
                        Ok(())
                    }
                    syn::Lit::Byte(lit) => {
                        let val = lit.value() as u32;
                        grammar.range_resolver.insert(val, val);
                        Ok(())
                    }
                    syn::Lit::Str(s) => {
                        for ch in s.value().chars() {
                            let val = ch as u32;
                            grammar.range_resolver.insert(val, val);
                        }
                        Ok(())
                    }
                    syn::Lit::ByteStr(s) => {
                        for ch in s.value() {
                            let val = ch as u32;
                            grammar.range_resolver.insert(val, val);
                        }
                        Ok(())
                    }
                    _ => Err(ParseError::UnsupportedLiteralType(
                        literal.to_token_stream(),
                    )),
                }
            }
            PatternArgs::Minus { base, exclude } => {
                base.range_resolve(grammar)?;
                exclude.range_resolve(grammar)?;
                Ok(())
            }
            PatternArgs::Sep {
                base, delimiter, ..
            } => {
                base.range_resolve(grammar)?;
                delimiter.range_resolve(grammar)?;
                Ok(())
            }
        }
    }
}

/// parsed arguments for single line of a rule separated by '|'
#[derive(Clone)]
pub struct RuleLineArgs {
    /// mapto '=' pattern
    pub tokens: Vec<(Option<Ident>, PatternArgs)>,
    pub reduce_action: Option<TokenStream>,
    /// location of ':' or '|' in front of this rule line
    pub separator_location: Location,
    /// %prec or %dprec, allow duplicates here, return error later
    pub precs: Vec<PrecDPrecArgs>,

    /// %prec
    pub prec: Option<IdentOrLiteral>,
    /// %dprec
    pub dprec: Option<Literal>,
}

/// For %prec and %dprec at the end of a rule line
#[derive(Clone)]
pub enum PrecDPrecArgs {
    Prec(IdentOrLiteral),
    DPrec(Literal),
    None,
}

/// parsed arguments for multiple lines of a rule
#[derive(Clone)]
pub struct RuleDefArgs {
    pub name: Ident,
    pub typename: Option<TokenStream>,
    pub rule_lines: Vec<RuleLineArgs>,
}

pub struct RecoveredError {
    pub message: String,
    pub link: String,
    pub location: Location,
}

/// parsed arguments for the grammar
pub struct GrammarArgs {
    pub module_prefix: Vec<(Span, TokenStream)>,
    pub token_typename: Vec<(Span, TokenStream)>,
    pub userdata_typename: Vec<(Span, TokenStream)>,
    pub start_rule_name: Vec<Ident>,
    pub error_typename: Vec<(Span, TokenStream)>,
    pub terminals: Vec<(Ident, TokenStream)>,
    pub precedences: Vec<(
        Span,                                    // span of %left, %right, %precedence
        Option<rusty_lr_core::rule::ReduceType>, // actual definition of precedence type
        Vec<IdentOrLiteral>,                     // items
    )>,
    pub rules: Vec<RuleDefArgs>,
    pub lalr: bool,
    pub glr: bool,
    pub no_optim: bool,
    pub dense: bool,
    pub traces: Vec<Ident>,
    pub filter: Option<TokenStream>,
    pub location_typename: Option<TokenStream>,

    pub error_recovered: Vec<RecoveredError>,
}

impl Default for GrammarArgs {
    fn default() -> Self {
        GrammarArgs {
            module_prefix: Vec::new(),
            token_typename: Vec::new(),
            userdata_typename: Vec::new(),
            start_rule_name: Vec::new(),
            error_typename: Vec::new(),
            terminals: Vec::new(),
            precedences: Vec::new(),
            rules: Vec::new(),
            lalr: false,
            glr: false,
            no_optim: false,
            dense: false,
            traces: Vec::new(),
            filter: None,
            location_typename: None,
            error_recovered: Vec::new(),
        }
    }
}
