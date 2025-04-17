use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use quote::ToTokens;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::utils;

#[derive(Debug, Clone)]
pub enum TerminalSetItem {
    Terminal(Ident),
    Range(Ident, Ident),
    Literal(Literal),
    LiteralRange(Literal, Literal),
}

impl std::fmt::Display for TerminalSetItem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TerminalSetItem::Terminal(ident) => write!(f, "{}", ident),
            TerminalSetItem::Range(first, last) => write!(f, "{}-{}", first, last),
            TerminalSetItem::Literal(literal) => write!(f, "{}", literal),
            TerminalSetItem::LiteralRange(first, last) => write!(f, "{}-{}", first, last),
        }
    }
}

impl TerminalSetItem {
    pub fn to_terminal_set(&self, grammar: &mut Grammar) -> Result<BTreeSet<usize>, ParseError> {
        match self {
            TerminalSetItem::Terminal(terminal) => {
                if let Some(idx) = grammar.terminals_index.get(terminal) {
                    Ok(BTreeSet::from([*idx]))
                } else {
                    Err(ParseError::TerminalNotDefined(terminal.clone()))
                }
            }
            TerminalSetItem::Range(first, last) => {
                let first_index = match grammar.terminals_index.get(first) {
                    Some(f) => f,
                    None => return Err(ParseError::TerminalNotDefined(first.clone())),
                };
                let last_index = match grammar.terminals_index.get(last) {
                    Some(l) => l,
                    None => return Err(ParseError::TerminalNotDefined(last.clone())),
                };
                if last_index < first_index {
                    return Err(ParseError::InvalidTerminalRange(
                        (
                            first.clone(),
                            *first_index,
                            grammar.terminals[*first_index].body.clone(),
                        ),
                        (
                            last.clone(),
                            *last_index,
                            grammar.terminals[*last_index].body.clone(),
                        ),
                    ));
                }
                Ok((*first_index..=*last_index).collect())
            }
            TerminalSetItem::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2");
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
            TerminalSetItem::LiteralRange(first_l, last_l) => {
                let first = syn::parse2::<syn::Lit>(first_l.to_token_stream())
                    .expect("failed on syn::parse2");
                if grammar.is_char {
                    if !matches!(&first, syn::Lit::Char(_)) {
                        return Err(ParseError::UnsupportedLiteralType(first_l.clone()));
                    }
                } else if grammar.is_u8 {
                    if !matches!(&first, syn::Lit::Byte(_)) {
                        return Err(ParseError::UnsupportedLiteralType(first_l.clone()));
                    }
                } else {
                    return Err(ParseError::UnsupportedLiteralType(first_l.clone()));
                }
                let first_ch = match &first {
                    syn::Lit::Char(lit) => lit.value(),
                    syn::Lit::Byte(lit) => lit.value() as char,
                    _ => unreachable!(),
                };

                let last = syn::parse2::<syn::Lit>(last_l.to_token_stream())
                    .expect("failed on syn::parse2");
                if grammar.is_char {
                    if !matches!(&last, syn::Lit::Char(_)) {
                        return Err(ParseError::UnsupportedLiteralType(last_l.clone()));
                    }
                } else if grammar.is_u8 {
                    if !matches!(&last, syn::Lit::Byte(_)) {
                        return Err(ParseError::UnsupportedLiteralType(last_l.clone()));
                    }
                } else {
                    return Err(ParseError::UnsupportedLiteralType(last_l.clone()));
                }
                let last_ch = match &last {
                    syn::Lit::Char(lit) => lit.value(),
                    syn::Lit::Byte(lit) => lit.value() as char,
                    _ => unreachable!(),
                };
                if first_ch > last_ch {
                    return Err(ParseError::InvalidLiteralRange(
                        first_l.clone(),
                        last_l.clone(),
                    ));
                }

                Ok((first_ch..=last_ch)
                    .map(|ch| {
                        let lit = syn::Lit::Char(syn::LitChar::new(ch, Span::call_site()));
                        let idx = grammar.add_or_get_literal_character(lit, None).unwrap();
                        idx
                    })
                    .collect())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TerminalSet {
    pub negate: bool,
    pub items: Vec<TerminalSetItem>,
    // '['
    pub open_span: Span,
    // ']'
    pub close_span: Span,
}
impl TerminalSet {
    pub fn to_terminal_set(
        &self,
        grammar: &mut Grammar,
        include_eof: bool,
    ) -> Result<BTreeSet<usize>, ParseError> {
        let mut terminal_set = BTreeSet::new();
        for item in &self.items {
            let mut item_set = item.to_terminal_set(grammar)?;
            terminal_set.append(&mut item_set);
        }
        if self.negate {
            // cannot use negation in str mode
            // because we have to cover all characters in `char` or `u8`
            if matches!(grammar.token_typename.to_string().as_str(), "char" | "u8") {
                return Err(ParseError::NegateInLiteralMode(
                    self.open_span,
                    self.close_span,
                ));
            }
            let full_terminals: BTreeSet<usize> = (0..grammar.terminals.len()).collect();
            terminal_set = full_terminals.difference(&terminal_set).cloned().collect();
        }

        if !include_eof {
            // include eof when TerminalSet is used in a lookahead set
            let eof_idx = grammar
                .terminals_index
                .get(&Ident::new(utils::EOF_NAME, Span::call_site()))
                .unwrap();
            terminal_set.remove(eof_idx);
        }
        Ok(terminal_set)
    }
}

impl std::fmt::Display for TerminalSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        if self.negate {
            write!(f, "^")?;
        }
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, "]")
    }
}
