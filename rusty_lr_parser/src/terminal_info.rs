use proc_macro2::Ident;
use proc_macro2::Span;

use proc_macro2::TokenStream;
use quote::ToTokens;
use rusty_lr_core::ReduceType;

pub struct ReduceTypeInfo {
    pub reduce_type: ReduceType,

    // the span of this reduce type definition
    pub source: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TerminalName {
    /// defined in %token
    Ident(Ident),

    /// defined as literal anywhere in the grammar
    Char(char),
}
impl TerminalName {
    pub fn ident(&self) -> Option<&Ident> {
        match self {
            TerminalName::Ident(ident) => Some(ident),
            TerminalName::Char(_) => None,
        }
    }
    pub fn into_ident(self) -> Option<Ident> {
        match self {
            TerminalName::Ident(ident) => Some(ident),
            TerminalName::Char(_) => None,
        }
    }
    pub fn char(&self) -> Option<char> {
        match self {
            TerminalName::Ident(_) => None,
            TerminalName::Char(c) => Some(*c),
        }
    }
    pub fn name(self) -> Ident {
        match self {
            TerminalName::Ident(name) => name,
            TerminalName::Char(c) => {
                let s = format!("_Terminal{}", c as u32);
                Ident::new(&s, Span::call_site())
            }
        }
    }
    pub fn pretty_name(&self, is_char: bool, is_u8: bool) -> String {
        match self {
            TerminalName::Ident(ident) => ident.to_string(),
            TerminalName::Char(c) => {
                if is_char {
                    format!("'{}'", c)
                } else if is_u8 {
                    format!(
                        "{}",
                        syn::LitByte::new(*c as u8, Span::call_site()).to_token_stream()
                    )
                } else {
                    format!(
                        "{}",
                        syn::LitChar::new(*c, Span::call_site()).to_token_stream()
                    )
                }
            }
        }
    }
}
impl From<Ident> for TerminalName {
    fn from(ident: Ident) -> Self {
        TerminalName::Ident(ident)
    }
}
impl From<char> for TerminalName {
    fn from(c: char) -> Self {
        TerminalName::Char(c)
    }
}

pub struct TerminalInfo {
    pub name: TerminalName,

    /// any %left of %right set for this terminal
    pub reduce_type: Option<ReduceTypeInfo>,

    /// the actual Rust expr to be emitted
    pub body: TokenStream,
}
