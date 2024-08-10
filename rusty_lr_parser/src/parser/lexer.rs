use proc_macro2::Delimiter;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use quote::quote;
use quote::ToTokens;

#[derive(Clone, Debug)]
pub enum Lexed {
    Ident(Option<proc_macro2::Ident>),
    Colon(Option<proc_macro2::Punct>),
    Semicolon(Option<proc_macro2::Punct>),
    Pipe(Option<proc_macro2::Punct>),
    Percent(Option<proc_macro2::Punct>),
    Left(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %left, %l, %reduce
    Right(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %right, %r, %shift
    Token(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %token
    Start(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %start
    EofDef(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %eof
    TokenType(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %tokentype
    UserData(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %userdata
    ErrorType(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %err %error
    ModulePrefix(Option<(proc_macro2::Punct, proc_macro2::Ident)>),
    ParenGroup(Option<proc_macro2::Group>),
    BraceGroup(Option<proc_macro2::Group>),
    BracketGroup(Option<proc_macro2::Group>),
    OtherGroup(Option<proc_macro2::Group>),
    Literal(Option<proc_macro2::Literal>),
    Equal(Option<proc_macro2::Punct>),
    Plus(Option<proc_macro2::Punct>),
    Star(Option<proc_macro2::Punct>),
    Question(Option<proc_macro2::Punct>),
    Caret(Option<proc_macro2::Punct>),
    Minus(Option<proc_macro2::Punct>),
    OtherPunct(Option<proc_macro2::Punct>),
    Eof,
}
impl Lexed {
    pub fn enum_index(&self) -> usize {
        match self {
            Lexed::Ident(_) => 0,
            Lexed::Colon(_) => 1,
            Lexed::Semicolon(_) => 2,
            Lexed::Pipe(_) => 3,
            Lexed::Percent(_) => 4,
            Lexed::Left(_) => 5,
            Lexed::Right(_) => 6,
            Lexed::Token(_) => 8,
            Lexed::Start(_) => 9,
            Lexed::EofDef(_) => 10,
            Lexed::TokenType(_) => 11,
            Lexed::UserData(_) => 12,
            Lexed::ErrorType(_) => 13,
            Lexed::ModulePrefix(_) => 14,
            Lexed::ParenGroup(_) => 15,
            Lexed::BraceGroup(_) => 16,
            Lexed::BracketGroup(_) => 17,
            Lexed::OtherGroup(_) => 18,
            Lexed::Literal(_) => 19,
            Lexed::Equal(_) => 20,
            Lexed::Plus(_) => 21,
            Lexed::Star(_) => 22,
            Lexed::Question(_) => 23,
            Lexed::Caret(_) => 24,
            Lexed::Minus(_) => 25,
            Lexed::OtherPunct(_) => 26,
            Lexed::Eof => 27,
        }
    }
    pub fn stream(self) -> TokenStream {
        match self {
            Lexed::Ident(ident) => ident.unwrap().to_token_stream(),
            Lexed::Colon(punct) => punct.unwrap().to_token_stream(),
            Lexed::Semicolon(punct) => punct.unwrap().to_token_stream(),
            Lexed::Pipe(punct) => punct.unwrap().to_token_stream(),
            Lexed::Percent(punct) => punct.unwrap().to_token_stream(),
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
            Lexed::ParenGroup(group) => group.unwrap().to_token_stream(),
            Lexed::BraceGroup(group) => group.unwrap().to_token_stream(),
            Lexed::BracketGroup(group) => group.unwrap().to_token_stream(),
            Lexed::OtherGroup(group) => group.unwrap().to_token_stream(),
            Lexed::Literal(lit) => lit.unwrap().to_token_stream(),
            Lexed::Equal(punct) => punct.unwrap().to_token_stream(),
            Lexed::Plus(punct) => punct.unwrap().to_token_stream(),
            Lexed::Star(punct) => punct.unwrap().to_token_stream(),
            Lexed::Question(punct) => punct.unwrap().to_token_stream(),
            Lexed::Caret(punct) => punct.unwrap().to_token_stream(),
            Lexed::Minus(punct) => punct.unwrap().to_token_stream(),
            Lexed::OtherPunct(punct) => punct.unwrap().to_token_stream(),
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
            Lexed::Left(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::Right(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::Token(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::Start(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::EofDef(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::TokenType(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::UserData(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::ErrorType(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::ModulePrefix(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            Lexed::ParenGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::BraceGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::BracketGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::OtherGroup(group) => group.as_ref().map(|g| g.span()),
            Lexed::Literal(lit) => lit.as_ref().map(|l| l.span()),
            Lexed::Equal(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Plus(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Star(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Question(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Caret(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::Minus(punct) => punct.as_ref().map(|p| p.span()),
            Lexed::OtherPunct(punct) => punct.as_ref().map(|p| p.span()),
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
            Lexed::Left(_) => write!(f, "%left"),
            Lexed::Right(_) => write!(f, "%right"),
            Lexed::Token(_) => write!(f, "%token"),
            Lexed::Start(_) => write!(f, "%start"),
            Lexed::EofDef(_) => write!(f, "%eof"),
            Lexed::TokenType(_) => write!(f, "%tokentype"),
            Lexed::UserData(_) => write!(f, "%userdata"),
            Lexed::ErrorType(_) => write!(f, "%error"),
            Lexed::ModulePrefix(_) => write!(f, "%moduleprefix"),
            Lexed::ParenGroup(_) => write!(f, "<ParenGroup>"),
            Lexed::BraceGroup(_) => write!(f, "<BraceGroup>"),
            Lexed::BracketGroup(_) => write!(f, "<BracketGroup>"),
            Lexed::OtherGroup(_) => write!(f, "<OtherGroup>"),
            Lexed::Literal(_) => write!(f, "<Literal>"),
            Lexed::Equal(_) => write!(f, "'='"),
            Lexed::Plus(_) => write!(f, "'+'"),
            Lexed::Star(_) => write!(f, "'*'"),
            Lexed::Question(_) => write!(f, "'?'"),
            Lexed::Caret(_) => write!(f, "'^'"),
            Lexed::Minus(_) => write!(f, "'-'"),
            Lexed::OtherPunct(_) => write!(f, "<Punct>"),
            Lexed::Eof => write!(f, "<eof>"),
        }
    }
}
impl std::hash::Hash for Lexed {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.enum_index().hash(state);
    }
}
impl PartialEq for Lexed {
    fn eq(&self, other: &Self) -> bool {
        self.enum_index() == other.enum_index()
    }
}
impl Eq for Lexed {}
impl std::cmp::PartialOrd for Lexed {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for Lexed {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.enum_index().cmp(&other.enum_index())
    }
}

pub struct Lexer {
    iter: std::iter::Peekable<proc_macro2::token_stream::IntoIter>,
}
impl Lexer {
    pub fn new(input: TokenStream) -> Self {
        Lexer {
            iter: input.into_iter().peekable(),
        }
    }
    pub fn next_token(&mut self) -> Option<Lexed> {
        if let Some(next) = self.iter.next() {
            match next {
                TokenTree::Ident(ident) => Some(Lexed::Ident(Some(ident))),
                TokenTree::Punct(punct) => match punct.as_char() {
                    ':' => Some(Lexed::Colon(Some(punct))),
                    ';' => Some(Lexed::Semicolon(Some(punct))),
                    '|' => Some(Lexed::Pipe(Some(punct))),
                    '+' => Some(Lexed::Plus(Some(punct))),
                    '*' => Some(Lexed::Star(Some(punct))),
                    '?' => Some(Lexed::Question(Some(punct))),
                    '^' => Some(Lexed::Caret(Some(punct))),
                    '-' => Some(Lexed::Minus(Some(punct))),
                    '%' => {
                        let (ret, shift) = match self.iter.peek() {
                            Some(TokenTree::Ident(ident)) => match ident.to_string().as_str() {
                                "left" | "l" | "reduce" => {
                                    (Some(Lexed::Left(Some((punct, ident.clone())))), true)
                                }
                                "right" | "r" | "shift" => {
                                    (Some(Lexed::Right(Some((punct, ident.clone())))), true)
                                }
                                "token" => (Some(Lexed::Token(Some((punct, ident.clone())))), true),
                                "start" => (Some(Lexed::Start(Some((punct, ident.clone())))), true),
                                "eof" => (Some(Lexed::EofDef(Some((punct, ident.clone())))), true),
                                "tokentype" => {
                                    (Some(Lexed::TokenType(Some((punct, ident.clone())))), true)
                                }
                                "userdata" => {
                                    (Some(Lexed::UserData(Some((punct, ident.clone())))), true)
                                }
                                "err" | "error" => {
                                    (Some(Lexed::ErrorType(Some((punct, ident.clone())))), true)
                                }
                                "moduleprefix" => (
                                    Some(Lexed::ModulePrefix(Some((punct, ident.clone())))),
                                    true,
                                ),
                                _ => (Some(Lexed::Percent(Some(punct))), false),
                            },
                            _ => (Some(Lexed::Percent(Some(punct))), false),
                        };
                        if shift {
                            self.iter.next();
                        }
                        ret
                    }
                    '=' => Some(Lexed::Equal(Some(punct))),

                    _ => Some(Lexed::OtherPunct(Some(punct))),
                },
                TokenTree::Group(group) => match group.delimiter() {
                    Delimiter::Parenthesis => Some(Lexed::ParenGroup(Some(group))),
                    Delimiter::Brace => Some(Lexed::BraceGroup(Some(group))),
                    Delimiter::Bracket => Some(Lexed::BracketGroup(Some(group))),
                    _ => Some(Lexed::OtherGroup(Some(group))),
                },
                TokenTree::Literal(literal) => Some(Lexed::Literal(Some(literal))),
            }
        } else {
            None
        }
    }
}
