use proc_macro2::Span;

use crate::parser::location::Located;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::ToTokens;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TerminalName {
    /// defined in %token
    Ident(String),

    /// defined as literal anywhere in the grammar
    CharRange(char, char),
}
impl TerminalName {
    pub fn count(&self) -> usize {
        match self {
            TerminalName::Ident(_) => 1,
            TerminalName::CharRange(s, l) => {
                let s = *s as usize;
                let l = *l as usize;
                l + 1 - s
            }
        }
    }
    pub fn ident_str(&self) -> Option<&str> {
        match self {
            TerminalName::Ident(s) => Some(s.as_str()),
            TerminalName::CharRange(_, _) => None,
        }
    }
    pub fn name_ident(&self, span: Span) -> proc_macro2::Ident {
        match self {
            TerminalName::Ident(s) => format_ident!("{}", s, span = span),
            TerminalName::CharRange(c, _) => {
                let s = format!("_Terminal{}", *c as u32);
                proc_macro2::Ident::new(&s, span)
            }
        }
    }
    pub fn into_string(self) -> Option<String> {
        match self {
            TerminalName::Ident(s) => Some(s),
            TerminalName::CharRange(_, _) => None,
        }
    }
    pub fn pretty_name(&self, is_char: bool, is_u8: bool) -> String {
        match self {
            TerminalName::Ident(s) => s.clone(),
            TerminalName::CharRange(start, last) => {
                if is_char {
                    let start_tok = syn::LitChar::new(*start, Span::call_site()).to_token_stream();
                    let last_tok = syn::LitChar::new(*last, Span::call_site()).to_token_stream();
                    if start == last {
                        format!("{start_tok}")
                    } else {
                        format!("{start_tok}-{last_tok}")
                    }
                } else if is_u8 {
                    let start_tok =
                        syn::LitByte::new(*start as u8, Span::call_site()).to_token_stream();
                    let last_tok =
                        syn::LitByte::new(*last as u8, Span::call_site()).to_token_stream();
                    if start == last {
                        format!("{start_tok}")
                    } else {
                        format!("{start_tok}-{last_tok}")
                    }
                } else {
                    unreachable!("unexpected char type")
                }
            }
        }
    }
}
impl From<String> for TerminalName {
    fn from(s: String) -> Self {
        TerminalName::Ident(s)
    }
}
impl From<(char, char)> for TerminalName {
    fn from(c: (char, char)) -> Self {
        TerminalName::CharRange(c.0, c.1)
    }
}
impl From<(u8, u8)> for TerminalName {
    fn from(c: (u8, u8)) -> Self {
        TerminalName::CharRange(c.0 as char, c.1 as char)
    }
}

pub struct TerminalInfo {
    pub name: TerminalName,

    /// the precedence level of this terminal
    pub precedence: Option<Located<usize>>,

    /// the actual Rust expr to be emitted
    pub body: TokenStream,
}
