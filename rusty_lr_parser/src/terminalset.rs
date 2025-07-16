use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use quote::ToTokens;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::terminal_info::TerminalName;

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
                if let Some(idx) = grammar
                    .terminals_index
                    .get(&TerminalName::Ident(terminal.clone()))
                {
                    Ok(BTreeSet::from([*idx]))
                } else {
                    Err(ParseError::TerminalNotDefined(terminal.clone()))
                }
            }
            TerminalSetItem::Range(first, last) => {
                let first_index = match grammar
                    .terminals_index
                    .get(&TerminalName::Ident(first.clone()))
                {
                    Some(f) => f,
                    None => return Err(ParseError::TerminalNotDefined(first.clone())),
                };
                let last_index = match grammar
                    .terminals_index
                    .get(&TerminalName::Ident(last.clone()))
                {
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
                let val = grammar.get_char_value(&lit)?;
                let name: TerminalName = (val, val).into();
                let idx = *grammar.terminals_index.get(&name).unwrap();
                Ok(BTreeSet::from([idx]))
            }
            TerminalSetItem::LiteralRange(first_l, last_l) => {
                let first = syn::parse2::<syn::Lit>(first_l.to_token_stream())
                    .expect("failed on syn::parse2");
                let first_ch = grammar.get_char_value(&first)?;

                let last = syn::parse2::<syn::Lit>(last_l.to_token_stream())
                    .expect("failed on syn::parse2");
                let last_ch = grammar.get_char_value(&last)?;
                if first_ch > last_ch {
                    return Err(ParseError::InvalidLiteralRange(
                        first_l.clone(),
                        last_l.clone(),
                    ));
                }

                let set: BTreeSet<usize> = grammar
                    .get_terminal_indices_from_char_range(first_ch, last_ch)
                    .collect();
                Ok(set)
            }
        }
    }
    pub fn range_resolve(&self, grammar: &mut Grammar) -> Result<(), ParseError> {
        match self {
            TerminalSetItem::Terminal(_) => Ok(()),
            TerminalSetItem::Range(_, _) => Ok(()),
            TerminalSetItem::Literal(literal) => {
                let lit = syn::parse2::<syn::Lit>(literal.to_token_stream())
                    .expect("failed on syn::parse2");
                let val = grammar.get_char_value(&lit)?;
                grammar.range_resolver.insert(val, val);
                Ok(())
            }
            TerminalSetItem::LiteralRange(first_l, last_l) => {
                let first = syn::parse2::<syn::Lit>(first_l.to_token_stream())
                    .expect("failed on syn::parse2");
                let first_ch = grammar.get_char_value(&first)?;

                let last = syn::parse2::<syn::Lit>(last_l.to_token_stream())
                    .expect("failed on syn::parse2");
                let last_ch = grammar.get_char_value(&last)?;
                if first_ch > last_ch {
                    return Err(ParseError::InvalidLiteralRange(
                        first_l.clone(),
                        last_l.clone(),
                    ));
                }
                grammar.range_resolver.insert(first_ch, last_ch);
                Ok(())
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
    // in case of negation, `include_eof` is true if the final terminal set contains eof
    pub fn to_terminal_set(
        &self,
        grammar: &mut Grammar,
    ) -> Result<(bool, BTreeSet<usize>), ParseError> {
        let mut terminal_set = BTreeSet::new();
        for item in &self.items {
            let mut item_set = item.to_terminal_set(grammar)?;
            terminal_set.append(&mut item_set);
        }
        Ok((self.negate, terminal_set))
    }
    pub fn range_resolve(&self, grammar: &mut Grammar) -> Result<(), ParseError> {
        for item in &self.items {
            item.range_resolve(grammar)?;
        }
        Ok(())
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
