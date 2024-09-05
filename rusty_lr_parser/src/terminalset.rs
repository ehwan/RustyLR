use proc_macro2::Ident;
use proc_macro2::Span;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::utils;

#[derive(Debug, Clone)]
pub enum TerminalSetItem {
    Terminal(Ident),
    Range(Ident, Ident),
}

impl std::fmt::Display for TerminalSetItem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TerminalSetItem::Terminal(ident) => write!(f, "{}", ident),
            TerminalSetItem::Range(first, last) => write!(f, "{}-{}", first, last),
        }
    }
}

impl TerminalSetItem {
    pub fn to_terminal_set(&self, grammar: &Grammar) -> Result<BTreeSet<usize>, ParseError> {
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
        grammar: &Grammar,
        include_eof: bool,
    ) -> Result<BTreeSet<usize>, ParseError> {
        let mut terminal_set = BTreeSet::new();
        for item in &self.items {
            let mut item_set = item.to_terminal_set(grammar)?;
            terminal_set.append(&mut item_set);
        }
        if self.negate {
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
