use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use super::term::TermType;

pub struct Tokenizer {
    iter: std::iter::Peekable<proc_macro2::token_stream::IntoIter>,
}
impl Tokenizer {
    pub fn new(input: TokenStream) -> Self {
        Tokenizer {
            iter: input.into_iter().peekable(),
        }
    }
    pub fn next_token(&mut self) -> Option<TermType> {
        if let Some(next) = self.iter.next() {
            match next {
                TokenTree::Ident(ident) => Some(TermType::Ident(Some(ident))),
                TokenTree::Punct(punct) => match punct.as_char() {
                    ':' => Some(TermType::Colon(Some(punct))),
                    ';' => Some(TermType::Semicolon(Some(punct))),
                    '|' => Some(TermType::Pipe(Some(punct))),
                    '+' => Some(TermType::Plus(Some(punct))),
                    '*' => Some(TermType::Star(Some(punct))),
                    '?' => Some(TermType::Question(Some(punct))),
                    '%' => {
                        let (ret, shift) = match self.iter.peek() {
                            Some(TokenTree::Ident(ident)) => match ident.to_string().as_str() {
                                "left" | "l" | "reduce" => {
                                    (Some(TermType::Left(Some((punct, ident.clone())))), true)
                                }
                                "right" | "r" | "shift" => {
                                    (Some(TermType::Right(Some((punct, ident.clone())))), true)
                                }
                                "token" => {
                                    (Some(TermType::Token(Some((punct, ident.clone())))), true)
                                }
                                "start" => {
                                    (Some(TermType::Start(Some((punct, ident.clone())))), true)
                                }
                                "eof" => {
                                    (Some(TermType::EofDef(Some((punct, ident.clone())))), true)
                                }
                                "tokentype" => (
                                    Some(TermType::TokenType(Some((punct, ident.clone())))),
                                    true,
                                ),
                                "userdata" => {
                                    (Some(TermType::UserData(Some((punct, ident.clone())))), true)
                                }
                                "err" | "error" => (
                                    Some(TermType::ErrorType(Some((punct, ident.clone())))),
                                    true,
                                ),
                                "moduleprefix" => (
                                    Some(TermType::ModulePrefix(Some((punct, ident.clone())))),
                                    true,
                                ),
                                _ => (Some(TermType::Percent(Some(punct))), false),
                            },
                            _ => (Some(TermType::Percent(Some(punct))), false),
                        };
                        if shift {
                            self.iter.next();
                        }
                        ret
                    }
                    '=' => Some(TermType::Equal(Some(punct))),

                    _ => Some(TermType::OtherPunct(Some(punct))),
                },
                TokenTree::Group(group) => Some(TermType::Group(Some(group))),
                TokenTree::Literal(literal) => Some(TermType::Literal(Some(literal))),
            }
        } else {
            None
        }
    }
}
