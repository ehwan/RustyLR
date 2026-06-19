use proc_macro2::TokenStream;
use quote::ToTokens;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::parser::location::Located;
use crate::parser::location::Location;
use crate::pattern::Pattern;
use crate::pattern::PatternInternal;
use crate::terminal_info::TerminalName;
use crate::terminalset::TerminalSet;

#[derive(Debug, Clone)]
pub enum IdentOrLiteral {
    Ident(Located<String>),
    Byte(Located<u8>),
    Char(Located<char>),
}
impl PartialEq for IdentOrLiteral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (IdentOrLiteral::Ident(ident1), IdentOrLiteral::Ident(ident2)) => ident1 == ident2,
            (IdentOrLiteral::Byte(b1), IdentOrLiteral::Byte(b2)) => b1 == b2,
            (IdentOrLiteral::Char(c1), IdentOrLiteral::Char(c2)) => c1 == c2,
            _ => false,
        }
    }
}
impl Eq for IdentOrLiteral {}
impl std::hash::Hash for IdentOrLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            IdentOrLiteral::Ident(ident) => {
                ident.hash(state);
            }
            IdentOrLiteral::Byte(b) => {
                b.hash(state);
            }
            IdentOrLiteral::Char(c) => {
                c.hash(state);
            }
        }
    }
}
impl IdentOrLiteral {
    pub fn location(&self) -> Location {
        match self {
            IdentOrLiteral::Ident(ident) => ident.location(),
            IdentOrLiteral::Byte(l) => l.location(),
            IdentOrLiteral::Char(l) => l.location(),
        }
    }
    pub fn range_resolve(&self, grammar: &mut Grammar) -> Result<(), ParseError> {
        match self {
            IdentOrLiteral::Ident(_) => {}
            IdentOrLiteral::Byte(b) => {
                let val = *b.value() as u32;
                grammar.range_resolver.insert(val, val);
            }
            IdentOrLiteral::Char(c) => {
                let val = *c.value() as u32;
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
            IdentOrLiteral::Byte(literal) => write!(f, "{}", literal.to_token_stream()),
            IdentOrLiteral::Char(literal) => write!(f, "{}", literal.to_token_stream()),
        }
    }
}

/// parsed arguments for pattern
#[derive(Clone)]
pub enum PatternArgs {
    Ident(Located<String>),

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

    /// b'a'
    Byte(Located<u8>),
    /// b"abc"
    ByteString(Located<Vec<u8>>),
    /// 'a'
    Char(Located<char>),
    /// "abc"
    String(Located<String>),

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
            PatternArgs::Byte(b) => write!(f, "{}", b.to_token_stream()),
            PatternArgs::ByteString(s) => {
                write!(
                    f,
                    "{}",
                    syn::LitByteStr::new(s.value(), proc_macro2::Span::call_site())
                        .to_token_stream()
                )
            }
            PatternArgs::Char(c) => write!(f, "{}", c.to_token_stream()),
            PatternArgs::String(s) => write!(f, "{}", s.to_token_stream()),

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
            PatternArgs::Byte(l) => {
                if !grammar.is_u8 {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let pattern = Pattern {
                    internal: PatternInternal::Byte(l),
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
            PatternArgs::ByteString(l) => {
                if !grammar.is_u8 {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let pattern = Pattern {
                    internal: PatternInternal::ByteString(l),
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
            PatternArgs::Char(l) => {
                if !grammar.is_char {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let pattern = Pattern {
                    internal: PatternInternal::Char(l),
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
            PatternArgs::String(l) => {
                if !grammar.is_char {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let pattern = Pattern {
                    internal: PatternInternal::String(l),
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
                    .get(&TerminalName::Ident(ident.value().clone()))
                {
                    Ok((false, BTreeSet::from([*idx])))
                } else {
                    Err(ParseError::TerminalNotDefined(ident.location()))
                }
            }
            PatternArgs::Plus { .. } => Err(ParseError::OnlyTerminalSet(self.location())),
            PatternArgs::Star { .. } => Err(ParseError::OnlyTerminalSet(self.location())),
            PatternArgs::Question { .. } => Err(ParseError::OnlyTerminalSet(self.location())),
            PatternArgs::Exclamation { base, .. } => base.to_terminal_set(grammar),
            PatternArgs::Lookaheads { pattern, .. } => {
                Err(ParseError::OnlyTerminalSet(pattern.location()))
            }
            PatternArgs::Group { alternatives, .. } => {
                if alternatives.len() == 1 && alternatives[0].len() == 1 {
                    alternatives[0][0].to_terminal_set(grammar)
                } else {
                    Err(ParseError::OnlyTerminalSet(self.location()))
                }
            }
            PatternArgs::Byte(l) => {
                if !grammar.is_u8 {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let val = *l.value();
                let name: TerminalName = (val, val).into();
                let idx = *grammar.terminals_index.get(&name).unwrap();
                Ok((false, BTreeSet::from([idx])))
            }
            PatternArgs::Char(l) => {
                if !grammar.is_char {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let val = *l.value();
                let name: TerminalName = (val, val).into();
                let idx = *grammar.terminals_index.get(&name).unwrap();
                Ok((false, BTreeSet::from([idx])))
            }
            PatternArgs::ByteString(_) => Err(ParseError::OnlyTerminalSet(self.location())),
            PatternArgs::String(_) => Err(ParseError::OnlyTerminalSet(self.location())),
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
            PatternArgs::Sep { location, .. } => Err(ParseError::OnlyTerminalSet(*location)),
        }
    }
    pub fn location(&self) -> Location {
        match self {
            PatternArgs::Ident(ident) => ident.location(),
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
            PatternArgs::Byte(l) => l.location(),
            PatternArgs::ByteString(l) => l.location(),
            PatternArgs::Char(l) => l.location(),
            PatternArgs::String(l) => l.location(),
            PatternArgs::Minus { base, exclude } => base.location().merge(&exclude.location()),
            PatternArgs::Sep { location, .. } => *location,
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
            PatternArgs::Byte(l) => {
                if !grammar.is_u8 {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let val = *l.value() as u32;
                grammar.range_resolver.insert(val, val);
                Ok(())
            }
            PatternArgs::ByteString(l) => {
                if !grammar.is_u8 {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                for &ch in l.value() {
                    let val = ch as u32;
                    grammar.range_resolver.insert(val, val);
                }
                Ok(())
            }
            PatternArgs::Char(l) => {
                if !grammar.is_char {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                let val = *l.value() as u32;
                grammar.range_resolver.insert(val, val);
                Ok(())
            }
            PatternArgs::String(l) => {
                if !grammar.is_char {
                    return Err(ParseError::UnsupportedLiteralType(l.location()));
                }
                for ch in l.value().chars() {
                    let val = ch as u32;
                    grammar.range_resolver.insert(val, val);
                }
                Ok(())
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
    pub tokens: Vec<(Option<Located<String>>, PatternArgs)>,
    pub reduce_action: Option<TokenStream>,
    /// location of ':' or '|' in front of this rule line
    pub separator_location: Location,
    /// %prec or %dprec, allow duplicates here, return error later
    pub precs: Vec<PrecDPrecArgs>,
}

impl RuleLineArgs {
    pub(crate) fn precs(&self) -> impl Iterator<Item = &'_ IdentOrLiteral> + '_ {
        self.precs.iter().filter_map(|p| match p {
            PrecDPrecArgs::Prec(prec) => Some(prec),
            _ => None,
        })
    }
    pub(crate) fn dprecs(&self) -> impl Iterator<Item = &'_ Located<syn::LitInt>> + '_ {
        self.precs.iter().filter_map(|p| match p {
            PrecDPrecArgs::DPrec(dprec) => Some(dprec),
            _ => None,
        })
    }
}

/// For %prec and %dprec at the end of a rule line
#[derive(Clone)]
pub enum PrecDPrecArgs {
    Prec(IdentOrLiteral),
    DPrec(Located<syn::LitInt>),
    /// for error recovery
    None,
}

/// parsed arguments for multiple lines of a rule
#[derive(Clone)]
pub struct RuleDefArgs {
    pub name: Located<String>,
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
    pub module_prefix: Vec<(Location, TokenStream)>,
    pub token_typename: Vec<(Location, TokenStream)>,
    pub userdata_typename: Vec<(Location, TokenStream)>,
    pub start_rule_name: Vec<Located<String>>,
    pub error_typename: Vec<(Location, TokenStream)>,
    pub terminals: Vec<(Located<String>, TokenStream)>,
    pub precedences: Vec<(
        Location,                                // location of %left, %right, %precedence
        Option<rusty_lr_core::rule::ReduceType>, // actual definition of precedence type
        Vec<IdentOrLiteral>,                     // items
    )>,
    pub rules: Vec<RuleDefArgs>,
    pub lalr: bool,
    pub glr: bool,
    pub no_optim: bool,
    pub dense: bool,
    pub location_typename: Vec<(Location, TokenStream)>,

    pub error_recovered: Vec<RecoveredError>,
    pub span_manager: crate::parser::location::SpanManager,
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
            location_typename: Vec::new(),
            error_recovered: Vec::new(),
            span_manager: crate::parser::location::SpanManager::default(),
        }
    }
}
