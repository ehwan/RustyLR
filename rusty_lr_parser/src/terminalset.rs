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
    pub fn to_terminal_set(&self, grammar: &Grammar) -> Result<BTreeSet<Ident>, ParseError> {
        match self {
            TerminalSetItem::Terminal(terminal) => {
                if grammar.terminals.contains_key(terminal) {
                    Ok(BTreeSet::from([terminal.clone()]))
                } else {
                    Err(ParseError::TerminalNotDefined(terminal.clone()))
                }
            }
            TerminalSetItem::Range(first, last) => {
                let (first_index, first_stream) = match grammar.terminals.get(first) {
                    Some(f) => f,
                    None => return Err(ParseError::TerminalNotDefined(first.clone())),
                };
                let (last_index, last_stream) = match grammar.terminals.get(last) {
                    Some(l) => l,
                    None => return Err(ParseError::TerminalNotDefined(last.clone())),
                };
                if last_index < first_index {
                    return Err(ParseError::InvalidTerminalRange(
                        (first.clone(), *first_index, first_stream.clone()),
                        (last.clone(), *last_index, last_stream.clone()),
                    ));
                }
                Ok(grammar.terminals_index[*first_index..=*last_index]
                    .iter()
                    .cloned()
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
    pub fn to_terminal_set(&self, grammar: &Grammar) -> Result<BTreeSet<Ident>, ParseError> {
        let mut terminal_set = BTreeSet::new();
        for item in &self.items {
            let mut item_set = item.to_terminal_set(grammar)?;
            terminal_set.append(&mut item_set);
        }
        if self.negate {
            let full_terminals: BTreeSet<_> = grammar.terminals.keys().cloned().collect();
            terminal_set = full_terminals.difference(&terminal_set).cloned().collect();
        }

        // exclude eof on any case
        terminal_set.remove(&Ident::new(utils::EOF_NAME, Span::call_site()));
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
