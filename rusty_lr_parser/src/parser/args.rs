use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::parser::span_pair::SpanPair;
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
    Lookaheads(Box<PatternArgs>, Box<PatternArgs>),

    /// ( Pattern+ )
    /// span of '(' and ')'
    Group(Vec<Vec<PatternArgs>>, Span, Span),

    /// 'a', b'a', "abc", b"abc"
    Literal(Literal),

    /// Pattern - Terminals exclusion
    Minus(Box<PatternArgs>, Box<PatternArgs>),

    Sep(Box<PatternArgs>, Box<PatternArgs>, bool, SpanPair),
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
            PatternArgs::Minus(base, terminal_set) => {
                write!(f, "{}-{}", base, terminal_set)
            }
            PatternArgs::Sep(base, del, one, _) => {
                write!(f, "$sep({base}, {del}, {})", if *one { '+' } else { '*' })
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
            PatternArgs::Plus(pattern, _) => Ok(Pattern {
                internal: PatternInternal::Plus(Box::new(
                    pattern.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Star(pattern, _) => Ok(Pattern {
                internal: PatternInternal::Star(Box::new(
                    pattern.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Question(pattern, _) => Ok(Pattern {
                internal: PatternInternal::Question(Box::new(
                    pattern.into_pattern(grammar, put_exclamation)?,
                )),
                pretty_name,
            }),
            PatternArgs::Exclamation(pattern, _) => pattern.into_pattern(grammar, true),
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
            PatternArgs::Lookaheads(pattern, lookaheads) => {
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
            PatternArgs::Group(group, _, _) => {
                if group.len() == 1 && group[0].len() == 1 {
                    let line = group.into_iter().next().unwrap();
                    return line
                        .into_iter()
                        .next()
                        .unwrap()
                        .into_pattern(grammar, put_exclamation);
                }

                let patterns = group
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
            PatternArgs::Minus(_, _) => {
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
            PatternArgs::Sep(base, del, one, _) => {
                let base = base.into_pattern(grammar, put_exclamation)?;
                let del = del.into_pattern(grammar, false)?;
                let pattern = Pattern {
                    internal: PatternInternal::Sep(Box::new(base), Box::new(del), one),
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
            PatternArgs::Plus(base, span) => {
                Err(ParseError::OnlyTerminalSet(base.span_pair().0, *span))
            }
            PatternArgs::Star(base, span) => {
                Err(ParseError::OnlyTerminalSet(base.span_pair().0, *span))
            }
            PatternArgs::Question(base, span) => {
                Err(ParseError::OnlyTerminalSet(base.span_pair().0, *span))
            }
            PatternArgs::Exclamation(base, _) => base.to_terminal_set(grammar),
            PatternArgs::Lookaheads(base, _) => {
                let (span_begin, span_end) = base.span_pair();
                Err(ParseError::OnlyTerminalSet(span_begin, span_end))
            }
            PatternArgs::Group(group, span_begin, span_end) => {
                if group.len() == 1 && group[0].len() == 1 {
                    group[0][0].to_terminal_set(grammar)
                } else {
                    Err(ParseError::OnlyTerminalSet(*span_begin, *span_end))
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
            PatternArgs::Minus(base, exclude) => {
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
            PatternArgs::Sep(_, _, _, span) => {
                let (first, last) = span.pair.unwrap();
                Err(ParseError::OnlyTerminalSet(first, last))
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
            PatternArgs::Minus(base, terminal_set) => {
                (base.span_pair().0, terminal_set.span_pair().1)
            }
            PatternArgs::Sep(_, _, _, span) => span.pair.unwrap(),
        }
    }

    pub fn range_resolve(&self, grammar: &mut Grammar) -> Result<(), ParseError> {
        match self {
            PatternArgs::Ident(_) => Ok(()),
            PatternArgs::Plus(base, _) => base.range_resolve(grammar),
            PatternArgs::Star(base, _) => base.range_resolve(grammar),
            PatternArgs::Question(base, _) => base.range_resolve(grammar),
            PatternArgs::Exclamation(base, _) => base.range_resolve(grammar),
            PatternArgs::TerminalSet(terminal_set) => terminal_set.range_resolve(grammar),
            PatternArgs::Lookaheads(base, terminal_set) => {
                base.range_resolve(grammar)?;
                terminal_set.range_resolve(grammar)?;
                Ok(())
            }
            PatternArgs::Group(groups, _, _) => {
                for line in groups {
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
            PatternArgs::Minus(base, terminal_set) => {
                base.range_resolve(grammar)?;
                terminal_set.range_resolve(grammar)?;
                Ok(())
            }
            PatternArgs::Sep(base, del, _, _) => {
                base.range_resolve(grammar)?;
                del.range_resolve(grammar)?;
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
    /// span of ':' or '|' in front of this rule line
    pub separator_span: Span,
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
    pub span: SpanPair,
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
        Span,                                       // span of %left, %right, %precedence
        Option<rusty_lr_core::builder::ReduceType>, // actual definition of precedence type
        Vec<IdentOrLiteral>,                        // items
    )>,
    pub rules: Vec<RuleDefArgs>,
    pub lalr: bool,
    pub glr: bool,
    pub no_optim: bool,
    pub dense: bool,
    pub traces: Vec<Ident>,
    pub filter: Option<TokenStream>,
    pub compiled: bool,
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
            compiled: true,
            location_typename: None,
            error_recovered: Vec::new(),
        }
    }
}
