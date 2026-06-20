use quote::ToTokens;

use std::collections::BTreeSet;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::grammar::Terminal;
use crate::parser::location::Located;
use crate::parser::location::Location;
use crate::terminal_info::TerminalName;

#[derive(Debug, Clone)]
pub enum TerminalSetItem {
    Terminal(Located<String>),
    Range(Located<String>, Located<String>),
    Byte(Located<u8>),
    ByteRange(Located<u8>, Located<u8>),
    Char(Located<char>),
    CharRange(Located<char>, Located<char>),
}

impl std::fmt::Display for TerminalSetItem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TerminalSetItem::Terminal(ident) => write!(f, "{}", ident),
            TerminalSetItem::Range(first, last) => write!(f, "{}-{}", first, last),
            TerminalSetItem::Byte(literal) => write!(f, "{}", literal.to_token_stream()),
            TerminalSetItem::ByteRange(first, last) => {
                write!(f, "{}-{}", first.to_token_stream(), last.to_token_stream())
            }
            TerminalSetItem::Char(literal) => write!(f, "{}", literal.to_token_stream()),
            TerminalSetItem::CharRange(first, last) => {
                write!(f, "{}-{}", first.to_token_stream(), last.to_token_stream())
            }
        }
    }
}

impl TerminalSetItem {
    pub fn location(&self) -> Location {
        match self {
            TerminalSetItem::Terminal(ident) => ident.location(),
            TerminalSetItem::Range(first, last) => first.location().merge(&last.location()),
            TerminalSetItem::Byte(literal) => literal.location(),
            TerminalSetItem::ByteRange(first, last) => first.location().merge(&last.location()),
            TerminalSetItem::Char(literal) => literal.location(),
            TerminalSetItem::CharRange(first, last) => first.location().merge(&last.location()),
        }
    }
    pub fn to_terminal_set(&self, grammar: &mut Grammar) -> Result<BTreeSet<Terminal>, ParseError> {
        match self {
            TerminalSetItem::Terminal(terminal) => {
                if let Some(idx) = grammar
                    .terminals_index
                    .get(&TerminalName::Ident(terminal.value().clone()))
                {
                    Ok(BTreeSet::from([*idx]))
                } else {
                    Err(ParseError::TerminalNotDefined(terminal.location()))
                }
            }
            TerminalSetItem::Range(first, last) => {
                let first_index = match grammar
                    .terminals_index
                    .get(&TerminalName::Ident(first.value().clone()))
                {
                    Some(f) => f,
                    None => return Err(ParseError::TerminalNotDefined(first.location())),
                };
                let last_index = match grammar
                    .terminals_index
                    .get(&TerminalName::Ident(last.value().clone()))
                {
                    Some(l) => l,
                    None => return Err(ParseError::TerminalNotDefined(last.location())),
                };
                if last_index < first_index {
                    return Err(ParseError::InvalidTerminalRange {
                        location: self.location(),
                        start: (first.clone(), *first_index),
                        end: (last.clone(), *last_index),
                    });
                }
                Ok((*first_index..=*last_index).collect())
            }
            TerminalSetItem::Byte(l) => {
                let val = *l.value();
                let name: TerminalName = (val, val).into();
                let idx = *grammar.terminals_index.get(&name).unwrap();
                Ok(BTreeSet::from([idx]))
            }
            TerminalSetItem::ByteRange(first_l, last_l) => {
                let first_val = *first_l.value();
                let last_val = *last_l.value();

                if first_val > last_val {
                    return Err(ParseError::InvalidLiteralRange(self.location()));
                }

                let set: BTreeSet<Terminal> = grammar
                    .get_terminal_indices_from_char_range(first_val as char, last_val as char)
                    .collect();
                Ok(set)
            }
            TerminalSetItem::Char(l) => {
                let val = *l.value();
                let name: TerminalName = (val, val).into();
                let idx = *grammar.terminals_index.get(&name).unwrap();
                Ok(BTreeSet::from([idx]))
            }
            TerminalSetItem::CharRange(first_l, last_l) => {
                let first_val = *first_l.value();
                let last_val = *last_l.value();
                if first_val > last_val {
                    return Err(ParseError::InvalidLiteralRange(self.location()));
                }
                let set: BTreeSet<Terminal> = grammar
                    .get_terminal_indices_from_char_range(first_val, last_val)
                    .collect();
                Ok(set)
            }
        }
    }
    pub fn range_resolve(&self, grammar: &mut Grammar) -> Result<(), ParseError> {
        match self {
            TerminalSetItem::Terminal(_) => Ok(()),
            TerminalSetItem::Range(_, _) => Ok(()),
            TerminalSetItem::Byte(l) => {
                let val = *l.value() as u32;
                grammar.range_resolver.insert(val, val);
                Ok(())
            }
            TerminalSetItem::ByteRange(first_l, last_l) => {
                let first_val = *first_l.value() as u32;
                let last_val = *last_l.value() as u32;
                if first_val > last_val {
                    return Err(ParseError::InvalidLiteralRange(self.location()));
                }
                grammar.range_resolver.insert(first_val, last_val);
                Ok(())
            }
            TerminalSetItem::Char(l) => {
                let val = *l.value() as u32;
                grammar.range_resolver.insert(val, val);
                Ok(())
            }
            TerminalSetItem::CharRange(first_l, last_l) => {
                let first_val = *first_l.value() as u32;
                let last_val = *last_l.value() as u32;
                if first_val > last_val {
                    return Err(ParseError::InvalidLiteralRange(self.location()));
                }
                grammar.range_resolver.insert(first_val, last_val);
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TerminalSet {
    pub negate: bool,
    pub items: Vec<TerminalSetItem>,
    // location of '[' or '.'
    pub open_location: Location,
    // location of ']' or '.'
    pub close_location: Location,
}
impl TerminalSet {
    // in case of negation, `include_eof` is true if the final terminal set contains eof
    pub fn to_terminal_set(
        &self,
        grammar: &mut Grammar,
    ) -> Result<(bool, BTreeSet<Terminal>), ParseError> {
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

    pub fn location(&self) -> Location {
        self.open_location.merge(&self.close_location)
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
