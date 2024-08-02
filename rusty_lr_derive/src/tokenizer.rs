use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use super::error::ParseError;
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
    pub fn next_token(&mut self) -> Result<Option<TermType>, ParseError> {
        if let Some(next) = self.iter.next() {
            match next {
                TokenTree::Ident(ident) => Ok(Some(TermType::Ident(Some(ident)))),
                TokenTree::Punct(punct) => match punct.as_char() {
                    ':' => Ok(Some(TermType::Colon(Some(punct)))),
                    ';' => Ok(Some(TermType::Semicolon(Some(punct)))),
                    '|' => Ok(Some(TermType::Pipe(Some(punct)))),
                    '%' => {
                        let (ret, shift) = match self.iter.peek() {
                            Some(TokenTree::Ident(ident)) => match ident.to_string().as_str() {
                                "left" | "l" | "reduce" => {
                                    (Ok(Some(TermType::Left(Some((punct, ident.clone()))))), true)
                                }
                                "right" | "r" | "shift" => (
                                    Ok(Some(TermType::Right(Some((punct, ident.clone()))))),
                                    true,
                                ),
                                "token" => (
                                    Ok(Some(TermType::Token(Some((punct, ident.clone()))))),
                                    true,
                                ),
                                "start" => (
                                    Ok(Some(TermType::Start(Some((punct, ident.clone()))))),
                                    true,
                                ),
                                "eof" => (
                                    Ok(Some(TermType::EofDef(Some((punct, ident.clone()))))),
                                    true,
                                ),
                                "tokentype" => (
                                    Ok(Some(TermType::TokenType(Some((punct, ident.clone()))))),
                                    true,
                                ),
                                "userdata" => (
                                    Ok(Some(TermType::UserData(Some((punct, ident.clone()))))),
                                    true,
                                ),
                                _ => (Ok(Some(TermType::Percent(Some(punct)))), false),
                            },
                            _ => (Ok(Some(TermType::Percent(Some(punct)))), false),
                        };
                        if shift {
                            self.iter.next();
                        }
                        ret
                    }

                    _ => Err(ParseError::InvalidPunct(punct.clone())),
                },
                TokenTree::Group(group) => Ok(Some(TermType::Group(Some(group)))),
                TokenTree::Literal(literal) => Ok(Some(TermType::Literal(Some(literal)))),
            }
        } else {
            Ok(None)
        }
    }
}
