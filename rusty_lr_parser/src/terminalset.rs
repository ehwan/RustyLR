use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::parser::lexer::Lexed;
use crate::parser::lexer::Lexer;
use crate::parser::terminalset_expanded::TerminalSetParser;
use crate::utils;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TerminalSetItem {
    Terminal(Ident),
    Range(Ident, Ident),
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
                let first_index = match grammar.terminals.get(first) {
                    Some((index, _)) => *index,
                    None => return Err(ParseError::TerminalNotDefined(first.clone())),
                };
                let last_index = match grammar.terminals.get(last) {
                    Some((index, _)) => *index,
                    None => return Err(ParseError::TerminalNotDefined(last.clone())),
                };
                if last_index < first_index {
                    return Err(ParseError::InvalidTerminalRange(
                        first.clone(),
                        last.clone(),
                    ));
                }
                Ok(grammar.terminals_index[first_index..=last_index]
                    .iter()
                    .cloned()
                    .collect())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TerminalSet {
    pub negate: bool,
    pub items: Vec<TerminalSetItem>,
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

    /// input: internal TokenStream between '[' and ']'
    pub fn parse(input: TokenStream, parser: &TerminalSetParser) -> Result<Self, ParseError> {
        let mut context = parser.begin();
        let mut lexer = Lexer::new(input);
        while let Some(lexed) = lexer.next_token() {
            let span = lexed.span().unwrap();
            match parser.feed(&mut context, lexed) {
                Ok(_) => {}
                Err(err) => {
                    let message = format!("{}", err);
                    return Err(ParseError::TerminalSetParse(span, message));
                }
            }
        }
        match parser.feed(&mut context, Lexed::Eof) {
            Ok(_) => {}
            Err(err) => {
                let message = format!("{}", err);
                let span = Span::call_site();
                return Err(ParseError::TerminalSetParse(span, message));
            }
        }

        let terminal_set = context.accept();
        Ok(terminal_set)
    }
}
