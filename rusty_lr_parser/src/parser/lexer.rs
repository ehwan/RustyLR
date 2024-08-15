use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Punct;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use quote::TokenStreamExt;

use super::parser_expanded::GrammarContext;
use super::parser_expanded::GrammarParseError;
use super::parser_expanded::GrammarParser;

#[derive(Clone, Debug)]
pub enum Lexed {
    Ident(Ident),
    Colon(Punct),
    Semicolon(Punct),
    Pipe(Punct),
    Percent(Punct),
    Equal(Punct),
    Plus(Punct),
    Star(Punct),
    Question(Punct),
    Caret(Punct),
    Minus(Punct),
    Exclamation(Punct),
    OtherPunct(Punct),

    Literal(Option<Literal>),

    ParenGroup(Option<Group>),
    BraceGroup(Option<Group>),
    BracketGroup(Option<Group>),
    NoneGroup(Option<Group>),
    LParen(Span),
    RParen(Span),
    LBrace(Span),
    RBrace(Span),
    LBracket(Span),
    RBracket(Span),

    Left(Punct, Ident),         // %left, %l, %reduce
    Right(Punct, Ident),        // %right, %r, %shift
    Token(Punct, Ident),        // %token
    Start(Punct, Ident),        // %start
    EofDef(Punct, Ident),       // %eof
    TokenType(Punct, Ident),    // %tokentype
    UserData(Punct, Ident),     // %userdata
    ErrorType(Punct, Ident),    // %err %error
    ModulePrefix(Punct, Ident), // %moduleprefix
    Eof,
}
impl Lexed {
    pub fn append_to_stream(self, stream: &mut TokenStream) {
        match self {
            Lexed::Ident(ident) => stream.append(ident),
            Lexed::Colon(punct) => stream.append(punct),
            Lexed::Semicolon(punct) => stream.append(punct),
            Lexed::Pipe(punct) => stream.append(punct),
            Lexed::Percent(punct) => stream.append(punct),
            Lexed::Equal(punct) => stream.append(punct),
            Lexed::Plus(punct) => stream.append(punct),
            Lexed::Star(punct) => stream.append(punct),
            Lexed::Question(punct) => stream.append(punct),
            Lexed::Caret(punct) => stream.append(punct),
            Lexed::Minus(punct) => stream.append(punct),
            Lexed::Exclamation(punct) => stream.append(punct),
            Lexed::OtherPunct(punct) => stream.append(punct),

            Lexed::Literal(lit) => stream.append(lit.unwrap()),

            Lexed::ParenGroup(group) => stream.append(group.unwrap()),
            Lexed::BraceGroup(group) => stream.append(group.unwrap()),
            Lexed::BracketGroup(group) => stream.append(group.unwrap()),
            Lexed::NoneGroup(group) => stream.append(group.unwrap()),

            Lexed::LParen(_) => unreachable!("LParen::stream()"),
            Lexed::RParen(_) => unreachable!("RParen::stream()"),
            Lexed::LBrace(_) => unreachable!("LBrace::stream()"),
            Lexed::RBrace(_) => unreachable!("RBrace::stream()"),
            Lexed::LBracket(_) => unreachable!("LBracket::stream()"),
            Lexed::RBracket(_) => unreachable!("RBracket::stream()"),

            Lexed::Left(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Right(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Token(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Start(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::EofDef(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::TokenType(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::UserData(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::ErrorType(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::ModulePrefix(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }

            Lexed::Eof => unreachable!("Eof::stream()"),
        }
    }
    #[allow(unused_variables)]
    pub fn span(&self) -> Span {
        match self {
            Lexed::Ident(ident) => ident.span(),
            Lexed::Colon(punct) => punct.span(),
            Lexed::Semicolon(punct) => punct.span(),
            Lexed::Pipe(punct) => punct.span(),
            Lexed::Percent(punct) => punct.span(),
            Lexed::Equal(punct) => punct.span(),
            Lexed::Plus(punct) => punct.span(),
            Lexed::Star(punct) => punct.span(),
            Lexed::Question(punct) => punct.span(),
            Lexed::Caret(punct) => punct.span(),
            Lexed::Minus(punct) => punct.span(),
            Lexed::Exclamation(punct) => punct.span(),
            Lexed::OtherPunct(punct) => punct.span(),

            Lexed::Literal(lit) => lit.as_ref().unwrap().span(),

            Lexed::ParenGroup(group) => group.as_ref().unwrap().span(),
            Lexed::BraceGroup(group) => group.as_ref().unwrap().span(),
            Lexed::BracketGroup(group) => group.as_ref().unwrap().span(),
            Lexed::NoneGroup(group) => group.as_ref().unwrap().span(),
            Lexed::LParen(span) => *span,
            Lexed::RParen(span) => *span,
            Lexed::LBrace(span) => *span,
            Lexed::RBrace(span) => *span,
            Lexed::LBracket(span) => *span,
            Lexed::RBracket(span) => *span,

            Lexed::Left(punct, ident) => ident.span(),
            Lexed::Right(punct, ident) => ident.span(),
            Lexed::Token(punct, ident) => ident.span(),
            Lexed::Start(punct, ident) => ident.span(),
            Lexed::EofDef(punct, ident) => ident.span(),
            Lexed::TokenType(punct, ident) => ident.span(),
            Lexed::UserData(punct, ident) => ident.span(),
            Lexed::ErrorType(punct, ident) => ident.span(),
            Lexed::ModulePrefix(punct, ident) => ident.span(),

            Lexed::Eof => Span::call_site(),
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
            Lexed::Exclamation(_) => write!(f, "'!'"),
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

            Lexed::Left(_, _) => write!(f, "%left"),
            Lexed::Right(_, _) => write!(f, "%right"),
            Lexed::Token(_, _) => write!(f, "%token"),
            Lexed::Start(_, _) => write!(f, "%start"),
            Lexed::EofDef(_, _) => write!(f, "%eof"),
            Lexed::TokenType(_, _) => write!(f, "%tokentype"),
            Lexed::UserData(_, _) => write!(f, "%userdata"),
            Lexed::ErrorType(_, _) => write!(f, "%error"),
            Lexed::ModulePrefix(_, _) => write!(f, "%moduleprefix"),

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

fn ident_to_keyword(percent: Punct, ident: Ident) -> Option<Lexed> {
    match ident.to_string().as_str() {
        "left" | "l" | "reduce" => Some(Lexed::Left(percent, ident)),
        "right" | "r" | "shift" => Some(Lexed::Right(percent, ident)),
        "token" => Some(Lexed::Token(percent, ident)),
        "start" => Some(Lexed::Start(percent, ident)),
        "eof" => Some(Lexed::EofDef(percent, ident)),
        "tokentype" => Some(Lexed::TokenType(percent, ident)),
        "userdata" => Some(Lexed::UserData(percent, ident)),
        "err" | "error" => Some(Lexed::ErrorType(percent, ident)),
        "moduleprefix" => Some(Lexed::ModulePrefix(percent, ident)),
        _ => None,
    }
}

/// lex & feed stream to parser
/// For '%' directives and 'Group' variants,
/// First tries to feed the Compound token
/// if it failed, then feed the internal splitted tokens recursively
pub fn feed_recursive(
    input: TokenStream,
    parser: &GrammarParser,
    context: &mut GrammarContext,
) -> Result<(), GrammarParseError> {
    let mut input = input.into_iter().peekable();

    while let Some(next) = input.next() {
        match next {
            TokenTree::Ident(ident) => {
                parser.feed(context, Lexed::Ident(ident))?;
            }
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' => parser.feed(context, Lexed::Colon(punct))?,
                ';' => parser.feed(context, Lexed::Semicolon(punct))?,
                '|' => parser.feed(context, Lexed::Pipe(punct))?,
                '+' => parser.feed(context, Lexed::Plus(punct))?,
                '*' => parser.feed(context, Lexed::Star(punct))?,
                '?' => parser.feed(context, Lexed::Question(punct))?,
                '^' => parser.feed(context, Lexed::Caret(punct))?,
                '-' => parser.feed(context, Lexed::Minus(punct))?,
                '=' => parser.feed(context, Lexed::Equal(punct))?,
                '!' => parser.feed(context, Lexed::Exclamation(punct))?,
                '%' => {
                    // check next is ident, and is a keyword
                    let next_ident = input.peek().cloned();
                    if let Some(TokenTree::Ident(next_ident)) = next_ident {
                        if let Some(keyword_compound) =
                            ident_to_keyword(punct.clone(), next_ident.clone())
                        {
                            input.next();
                            // feed the compound token
                            if parser.feed(context, keyword_compound).is_err() {
                                // compound token failed
                                // feed the splitted tokens
                                parser.feed(context, Lexed::Percent(punct))?;
                                parser.feed(context, Lexed::Ident(next_ident))?;
                            }
                        } else {
                            parser.feed(context, Lexed::Percent(punct))?;
                        }
                    } else {
                        parser.feed(context, Lexed::Percent(punct))?;
                    }
                }

                _ => parser.feed(context, Lexed::OtherPunct(punct))?,
            },
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {
                    // feed the compound token
                    if parser
                        .feed(context, Lexed::ParenGroup(Some(group.clone())))
                        .is_err()
                    {
                        // compound token failed
                        // feed the splitted tokens
                        parser.feed(context, Lexed::LParen(group.span_open()))?;
                        feed_recursive(group.stream(), parser, context)?;
                        parser.feed(context, Lexed::RParen(group.span_close()))?;
                    }
                }
                Delimiter::Brace => {
                    // feed the compound token
                    if parser
                        .feed(context, Lexed::BraceGroup(Some(group.clone())))
                        .is_err()
                    {
                        // compound token failed
                        // feed the splitted tokens
                        parser.feed(context, Lexed::LBrace(group.span_open()))?;
                        feed_recursive(group.stream(), parser, context)?;
                        parser.feed(context, Lexed::RBrace(group.span_close()))?;
                    }
                }
                Delimiter::Bracket => {
                    // feed the compound token
                    if parser
                        .feed(context, Lexed::BracketGroup(Some(group.clone())))
                        .is_err()
                    {
                        // compound token failed
                        // feed the splitted tokens
                        parser.feed(context, Lexed::LBracket(group.span_open()))?;
                        feed_recursive(group.stream(), parser, context)?;
                        parser.feed(context, Lexed::RBracket(group.span_close()))?;
                    }
                }
                _ => {
                    // feed the compound token
                    if parser
                        .feed(context, Lexed::NoneGroup(Some(group.clone())))
                        .is_err()
                    {
                        // compound token failed
                        // feed the splitted tokens
                        feed_recursive(group.stream(), parser, context)?;
                    }
                }
            },
            TokenTree::Literal(literal) => parser.feed(context, Lexed::Literal(Some(literal)))?,
        };
    }
    Ok(())
}
