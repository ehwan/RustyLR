use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Punct;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use quote::quote;
use quote::ToTokens;

use super::parser_expanded::GrammarContext;
use super::parser_expanded::GrammarNonTerminals;
use super::parser_expanded::GrammarParser;
use crate::error::ParseError;

#[derive(Clone, Debug)]
pub enum Lexed {
    Ident(Option<Ident>),
    Colon(Option<Punct>),
    Semicolon(Option<Punct>),
    Pipe(Option<Punct>),
    Percent(Option<Punct>),
    Literal(Option<Literal>),
    Equal(Option<Punct>),
    Plus(Option<Punct>),
    Star(Option<Punct>),
    Question(Option<Punct>),
    Caret(Option<Punct>),
    Minus(Option<Punct>),
    OtherPunct(Option<Punct>),

    ParenGroup(Option<Group>),
    BraceGroup(Option<Group>),
    BracketGroup(Option<Group>),
    NoneGroup(Option<Group>),
    LParen(Option<Span>),
    RParen(Option<Span>),
    LBrace(Option<Span>),
    RBrace(Option<Span>),
    LBracket(Option<Span>),
    RBracket(Option<Span>),

    Left(Option<(Punct, Ident)>),         // %left, %l, %reduce
    Right(Option<(Punct, Ident)>),        // %right, %r, %shift
    Token(Option<(Punct, Ident)>),        // %token
    Start(Option<(Punct, Ident)>),        // %start
    EofDef(Option<(Punct, Ident)>),       // %eof
    TokenType(Option<(Punct, Ident)>),    // %tokentype
    UserData(Option<(Punct, Ident)>),     // %userdata
    ErrorType(Option<(Punct, Ident)>),    // %err %error
    ModulePrefix(Option<(Punct, Ident)>), // %moduleprefix
    Eof,
}
impl Lexed {
    pub fn stream(self) -> TokenStream {
        match self {
            Lexed::Ident(ident) => ident.unwrap().to_token_stream(),
            Lexed::Colon(punct) => punct.unwrap().to_token_stream(),
            Lexed::Semicolon(punct) => punct.unwrap().to_token_stream(),
            Lexed::Pipe(punct) => punct.unwrap().to_token_stream(),
            Lexed::Percent(punct) => punct.unwrap().to_token_stream(),
            Lexed::Literal(lit) => lit.unwrap().to_token_stream(),
            Lexed::Equal(punct) => punct.unwrap().to_token_stream(),
            Lexed::Plus(punct) => punct.unwrap().to_token_stream(),
            Lexed::Star(punct) => punct.unwrap().to_token_stream(),
            Lexed::Question(punct) => punct.unwrap().to_token_stream(),
            Lexed::Caret(punct) => punct.unwrap().to_token_stream(),
            Lexed::Minus(punct) => punct.unwrap().to_token_stream(),
            Lexed::OtherPunct(punct) => punct.unwrap().to_token_stream(),

            Lexed::ParenGroup(group) => group.unwrap().to_token_stream(),
            Lexed::BraceGroup(group) => group.unwrap().to_token_stream(),
            Lexed::BracketGroup(group) => group.unwrap().to_token_stream(),
            Lexed::NoneGroup(group) => group.unwrap().to_token_stream(),

            Lexed::LParen(_) => unreachable!("LParen::stream()"),
            Lexed::RParen(_) => unreachable!("RParen::stream()"),
            Lexed::LBrace(_) => unreachable!("LBrace::stream()"),
            Lexed::RBrace(_) => unreachable!("RBrace::stream()"),
            Lexed::LBracket(_) => unreachable!("LBracket::stream()"),
            Lexed::RBracket(_) => unreachable!("RBracket::stream()"),

            Lexed::Left(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::Right(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::Token(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::Start(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::EofDef(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::TokenType(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::UserData(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::ErrorType(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            Lexed::ModulePrefix(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }

            Lexed::Eof => unreachable!("Eof should not be converted to TokenStream"),
        }
    }
    #[allow(unused_variables)]
    pub fn span(&self) -> Option<Span> {
        match self {
            Lexed::Ident(ident) => ident.as_ref().map(|i| i.span()),
            Lexed::Colon(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Semicolon(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Pipe(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Percent(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Literal(lit) => lit.as_ref().map(|l| l.span()),
            Lexed::Equal(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Plus(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Star(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Question(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Caret(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Minus(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::OtherPunct(punct) => punct.as_ref().map(|p| p.span()),

            Lexed::ParenGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::BraceGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::BracketGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::NoneGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::LParen(span) => span.as_ref().map(|s| *s),
            Lexed::RParen(span) => span.as_ref().map(|s| *s),
            Lexed::LBrace(span) => span.as_ref().map(|s| *s),
            Lexed::RBrace(span) => span.as_ref().map(|s| *s),
            Lexed::LBracket(span) => span.as_ref().map(|s| *s),
            Lexed::RBracket(span) => span.as_ref().map(|s| *s),

            Lexed::Left(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::Right(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::Token(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::Start(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::EofDef(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::TokenType(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::UserData(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::ErrorType(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::ModulePrefix(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),

            Lexed::Eof => None,
        }
    }
}
impl std::fmt::Display for Lexed {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Lexed::Ident(_) => write!(f, "<Ident>"),
            Lexed::Colon(_) => write!(f, "':'"),
            Lexed::Semicolon(_) => write!(f, "';'"),
            Lexed::Pipe(_) => write!(f, "'|'"),
            Lexed::Percent(_) => write!(f, "'%'"),
            Lexed::Literal(_) => write!(f, "<Literal>"),
            Lexed::Equal(_) => write!(f, "'='"),
            Lexed::Plus(_) => write!(f, "'+'"),
            Lexed::Star(_) => write!(f, "'*'"),
            Lexed::Question(_) => write!(f, "'?'"),
            Lexed::Caret(_) => write!(f, "'^'"),
            Lexed::Minus(_) => write!(f, "'-'"),
            Lexed::OtherPunct(_) => write!(f, "<Punct>"),

            Lexed::ParenGroup(_) => write!(f, "<ParenGroup>"),
            Lexed::BraceGroup(_) => write!(f, "<BraceGroup>"),
            Lexed::BracketGroup(_) => write!(f, "<BracketGroup>"),
            Lexed::NoneGroup(_) => write!(f, "<NoneGroup>"),
            Lexed::LParen(_) => write!(f, "'('"),
            Lexed::RParen(_) => write!(f, "')'"),
            Lexed::LBrace(_) => write!(f, "'{{'"),
            Lexed::RBrace(_) => write!(f, "'}}'"),
            Lexed::LBracket(_) => write!(f, "'['"),
            Lexed::RBracket(_) => write!(f, "'['"),

            Lexed::Left(_) => write!(f, "%left"),
            Lexed::Right(_) => write!(f, "%right"),
            Lexed::Token(_) => write!(f, "%token"),
            Lexed::Start(_) => write!(f, "%start"),
            Lexed::EofDef(_) => write!(f, "%eof"),
            Lexed::TokenType(_) => write!(f, "%tokentype"),
            Lexed::UserData(_) => write!(f, "%userdata"),
            Lexed::ErrorType(_) => write!(f, "%error"),
            Lexed::ModulePrefix(_) => write!(f, "%moduleprefix"),

            Lexed::Eof => write!(f, "<eof>"),
        }
    }
}
impl std::hash::Hash for Lexed {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
    }
}
impl PartialEq for Lexed {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self).eq(&std::mem::discriminant(other))
    }
}
impl Eq for Lexed {}

/// lex & feed stream to parser
/// For '%' directives and 'Group' variants,
/// First tries to feed the Compound token
/// if it failed, then feed the internal splitted tokens recursively
pub fn feed_recursive<'a>(
    input: TokenStream,
    parser: &'a GrammarParser,
    context: &mut GrammarContext,
) -> Result<
    (),
    (
        Span, // span of the error point
        rusty_lr_core::ParseError<'a, Lexed, GrammarNonTerminals, u8, ParseError>,
    ),
> {
    let mut input = input.into_iter().peekable();

    while let Some(next) = input.next() {
        let lexed = match next {
            TokenTree::Ident(ident) => Lexed::Ident(Some(ident)),
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' => Lexed::Colon(Some(punct)),
                ';' => Lexed::Semicolon(Some(punct)),
                '|' => Lexed::Pipe(Some(punct)),
                '+' => Lexed::Plus(Some(punct)),
                '*' => Lexed::Star(Some(punct)),
                '?' => Lexed::Question(Some(punct)),
                '^' => Lexed::Caret(Some(punct)),
                '-' => Lexed::Minus(Some(punct)),
                '=' => Lexed::Equal(Some(punct)),
                '%' => match input.peek().cloned() {
                    Some(TokenTree::Ident(ident)) => match ident.to_string().as_str() {
                        "left" | "l" | "reduce" => {
                            input.next();
                            Lexed::Left(Some((punct, ident)))
                        }
                        "right" | "r" | "shift" => {
                            input.next();
                            Lexed::Right(Some((punct, ident)))
                        }
                        "token" => {
                            input.next();
                            Lexed::Token(Some((punct, ident)))
                        }
                        "start" => {
                            input.next();
                            Lexed::Start(Some((punct, ident)))
                        }
                        "eof" => {
                            input.next();
                            Lexed::EofDef(Some((punct, ident)))
                        }
                        "tokentype" => {
                            input.next();
                            Lexed::TokenType(Some((punct, ident)))
                        }
                        "userdata" => {
                            input.next();
                            Lexed::UserData(Some((punct, ident)))
                        }
                        "err" | "error" => {
                            input.next();
                            Lexed::ErrorType(Some((punct, ident)))
                        }
                        "moduleprefix" => {
                            input.next();
                            Lexed::ModulePrefix(Some((punct, ident)))
                        }
                        _ => Lexed::Percent(Some(punct)),
                    },
                    _ => Lexed::Percent(Some(punct)),
                },

                _ => Lexed::OtherPunct(Some(punct)),
            },
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => Lexed::ParenGroup(Some(group)),
                Delimiter::Brace => Lexed::BraceGroup(Some(group)),
                Delimiter::Bracket => Lexed::BracketGroup(Some(group)),
                _ => Lexed::NoneGroup(Some(group)),
            },
            TokenTree::Literal(literal) => Lexed::Literal(Some(literal)),
        };

        let l0 = lexed.clone();

        match parser.feed(context, lexed) {
            Ok(_) => {}
            Err(e) => match l0 {
                Lexed::ParenGroup(Some(group)) => {
                    parser
                        .feed(context, Lexed::LParen(Some(group.span_open())))
                        .map_err(|e| (group.span_open(), e))?;
                    feed_recursive(group.stream(), parser, context)?;
                    parser
                        .feed(context, Lexed::RParen(Some(group.span_close())))
                        .map_err(|e| (group.span_close(), e))?;
                }
                Lexed::BraceGroup(Some(group)) => {
                    parser
                        .feed(context, Lexed::LBrace(Some(group.span_open())))
                        .map_err(|e| (group.span_open(), e))?;
                    feed_recursive(group.stream(), parser, context)?;
                    parser
                        .feed(context, Lexed::RBrace(Some(group.span_close())))
                        .map_err(|e| (group.span_close(), e))?;
                }
                Lexed::BracketGroup(Some(group)) => {
                    parser
                        .feed(context, Lexed::LBracket(Some(group.span_open())))
                        .map_err(|e| (group.span_open(), e))?;
                    feed_recursive(group.stream(), parser, context)?;
                    parser
                        .feed(context, Lexed::RBracket(Some(group.span_close())))
                        .map_err(|e| (group.span_close(), e))?;
                }
                Lexed::NoneGroup(Some(group)) => {
                    feed_recursive(group.stream(), parser, context)?;
                }
                Lexed::Left(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::Right(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::Token(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::Start(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::EofDef(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::TokenType(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::UserData(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::ErrorType(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                Lexed::ModulePrefix(Some((punct, ident))) => {
                    let ps = punct.span();
                    parser
                        .feed(context, Lexed::Percent(Some(punct)))
                        .map_err(|e| (ps, e))?;
                    let is = ident.span();
                    parser
                        .feed(context, Lexed::Ident(Some(ident)))
                        .map_err(|e| (is, e))?;
                }
                _ => return Err((l0.span().unwrap(), e)),
            },
        }
    }
    Ok(())
}
