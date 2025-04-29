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
    pub fn ident(&self) -> Option<&Ident> {
        match self {
            TerminalName::Ident(ident) => Some(ident),
            TerminalName::CharRange(_, _) => None,
        }
    }
    pub fn into_ident(self) -> Option<Ident> {
        match self {
            TerminalName::Ident(ident) => Some(ident),
            TerminalName::CharRange(_, _) => None,
        }
    }
    // pub fn char(&self) -> Option<char> {
    //     match self {
    //         TerminalName::Ident(_) => None,
    //         TerminalName::Char(c) => Some(*c),
    //     }
    // }
    pub fn name(self) -> Ident {
        match self {
            TerminalName::Ident(name) => name,
            TerminalName::CharRange(c, _) => {
                let s = format!("_Terminal{}", c as u32);
                Ident::new(&s, Span::call_site())
            }
        }
    }
    pub fn pretty_name(&self, is_char: bool, is_u8: bool) -> String {
        match self {
            TerminalName::Ident(ident) => ident.to_string(),
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
impl From<Ident> for TerminalName {
    fn from(ident: Ident) -> Self {
        TerminalName::Ident(ident)
    }
}
impl From<(char, char)> for TerminalName {
    fn from(c: (char, char)) -> Self {
        TerminalName::CharRange(c.0, c.1)
    }
}
impl From<(u32, u32)> for TerminalName {
    fn from(c: (u32, u32)) -> Self {
        let s = unsafe { char::from_u32_unchecked(c.0) };
        let l = unsafe { char::from_u32_unchecked(c.1) };
        TerminalName::CharRange(s, l)
    }
}

pub struct TerminalInfo {
    pub name: TerminalName,

    /// any %left of %right set for this terminal
    pub reduce_type: Option<ReduceTypeInfo>,

    /// the actual Rust expr to be emitted
    pub body: TokenStream,
}
