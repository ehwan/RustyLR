use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Punct;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use quote::TokenStreamExt;

use super::args::GrammarArgs;
use super::parser_expanded::GrammarContext;
use super::parser_expanded::GrammarParseError;
use super::parser_expanded::GrammarParser;

#[allow(dead_code)]
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
    Slash(Punct),
    Dot(Punct),
    Dollar(Punct),
    Comma(Punct),
    OtherPunct(Punct),

    Literal(Literal),

    ParenGroup(Group),
    BraceGroup(Group),
    BracketGroup(Group),
    NoneGroup(Group),
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Left(Ident),         // %left, %l, %reduce
    Right(Ident),        // %right, %r, %shift
    Token(Ident),        // %token
    Start(Ident),        // %start
    TokenType(Ident),    // %tokentype
    UserData(Ident),     // %userdata
    ErrorType(Ident),    // %err %error
    ModulePrefix(Ident), // %moduleprefix
    Lalr(Ident),         // %lalr
    Glr(Ident),          // %glr
    Prec(Ident),         // %prec
    Precedence(Ident),   // %precedence
    NoOptim(Ident),      // %nooptim
    Dense(Ident),        // %dense
    Trace(Ident),        // %trace
    DPrec(Ident),        // %dprec
    Filter(Ident),       // %filter
    Location(Ident),     // %location
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
            Lexed::Slash(punct) => stream.append(punct),
            Lexed::Dot(punct) => stream.append(punct),
            Lexed::Dollar(punct) => stream.append(punct),
            Lexed::Comma(punct) => stream.append(punct),
            Lexed::OtherPunct(punct) => stream.append(punct),

            Lexed::Literal(lit) => stream.append(lit),

            Lexed::ParenGroup(group) => stream.append(group),
            Lexed::BraceGroup(group) => stream.append(group),
            Lexed::BracketGroup(group) => stream.append(group),
            Lexed::NoneGroup(group) => stream.append(group),

            Lexed::LParen => unreachable!("LParen::stream()"),
            Lexed::RParen => unreachable!("RParen::stream()"),
            Lexed::LBrace => unreachable!("LBrace::stream()"),
            Lexed::RBrace => unreachable!("RBrace::stream()"),
            Lexed::LBracket => unreachable!("LBracket::stream()"),
            Lexed::RBracket => unreachable!("RBracket::stream()"),

            Lexed::Left(ident) => {
                stream.append(ident);
            }
            Lexed::Right(ident) => {
                stream.append(ident);
            }
            Lexed::Token(ident) => {
                stream.append(ident);
            }
            Lexed::Start(ident) => {
                stream.append(ident);
            }
            Lexed::TokenType(ident) => {
                stream.append(ident);
            }
            Lexed::UserData(ident) => {
                stream.append(ident);
            }
            Lexed::ErrorType(ident) => {
                stream.append(ident);
            }
            Lexed::ModulePrefix(ident) => {
                stream.append(ident);
            }
            Lexed::Lalr(ident) => {
                stream.append(ident);
            }
            Lexed::Glr(ident) => {
                stream.append(ident);
            }
            Lexed::Prec(ident) => {
                stream.append(ident);
            }
            Lexed::Precedence(ident) => {
                stream.append(ident);
            }
            Lexed::NoOptim(ident) => {
                stream.append(ident);
            }
            Lexed::Dense(ident) => {
                stream.append(ident);
            }
            Lexed::Trace(ident) => {
                stream.append(ident);
            }
            Lexed::DPrec(ident) => {
                stream.append(ident);
            }
            Lexed::Filter(ident) => {
                stream.append(ident);
            }
            Lexed::Location(ident) => {
                stream.append(ident);
            }
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
            Lexed::Slash(_) => write!(f, "'/'"),
            Lexed::Dot(_) => write!(f, "'.'"),
            Lexed::Dollar(_) => write!(f, "'$'"),
            Lexed::Comma(_) => write!(f, "','"),
            Lexed::OtherPunct(p) => write!(f, "'{}'", p.as_char()),

            Lexed::ParenGroup(_) => write!(f, "<ParenGroup>"),
            Lexed::BraceGroup(_) => write!(f, "<BraceGroup>"),
            Lexed::BracketGroup(_) => write!(f, "<BracketGroup>"),
            Lexed::NoneGroup(_) => write!(f, "<NoneGroup>"),
            Lexed::LParen => write!(f, "'('"),
            Lexed::RParen => write!(f, "')'"),
            Lexed::LBrace => write!(f, "'{{'"),
            Lexed::RBrace => write!(f, "'}}'"),
            Lexed::LBracket => write!(f, "'['"),
            Lexed::RBracket => write!(f, "'['"),

            Lexed::Left(_) => write!(f, "left"),
            Lexed::Right(_) => write!(f, "right"),
            Lexed::Token(_) => write!(f, "token"),
            Lexed::Start(_) => write!(f, "start"),
            Lexed::TokenType(_) => write!(f, "tokentype"),
            Lexed::UserData(_) => write!(f, "userdata"),
            Lexed::ErrorType(_) => write!(f, "error"),
            Lexed::ModulePrefix(_) => write!(f, "moduleprefix"),
            Lexed::Lalr(_) => write!(f, "lalr"),
            Lexed::Glr(_) => write!(f, "glr"),
            Lexed::Prec(_) => write!(f, "prec"),
            Lexed::Precedence(_) => write!(f, "precedence"),
            Lexed::NoOptim(_) => write!(f, "nooptim"),
            Lexed::Dense(_) => write!(f, "dense"),
            Lexed::Trace(_) => write!(f, "trace"),
            Lexed::DPrec(_) => write!(f, "dprec"),
            Lexed::Filter(_) => write!(f, "filter"),
            Lexed::Location(_) => write!(f, "location"),
        }
    }
}

fn ident_to_keyword(ident: Ident) -> Option<Lexed> {
    match ident.to_string().as_str() {
        "left" | "l" | "reduce" => Some(Lexed::Left(ident)),
        "right" | "r" | "shift" => Some(Lexed::Right(ident)),
        "token" => Some(Lexed::Token(ident)),
        "start" => Some(Lexed::Start(ident)),
        "tokentype" => Some(Lexed::TokenType(ident)),
        "userdata" => Some(Lexed::UserData(ident)),
        "err" | "error" => Some(Lexed::ErrorType(ident)),
        "moduleprefix" => Some(Lexed::ModulePrefix(ident)),
        "lalr" => Some(Lexed::Lalr(ident)),
        "glr" => Some(Lexed::Glr(ident)),
        "prec" => Some(Lexed::Prec(ident)),
        "precedence" => Some(Lexed::Precedence(ident)),
        "nooptim" => Some(Lexed::NoOptim(ident)),
        "dense" => Some(Lexed::Dense(ident)),
        "trace" => Some(Lexed::Trace(ident)),
        "dprec" => Some(Lexed::DPrec(ident)),
        "filter" => Some(Lexed::Filter(ident)),
        "location" => Some(Lexed::Location(ident)),
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
    grammar_args: &mut GrammarArgs,
) -> Result<(), GrammarParseError> {
    use super::span_pair::SpanPair;
    let mut input = input.into_iter().peekable();

    while let Some(next) = input.next() {
        let location = SpanPair::new_single(next.span());
        match next {
            TokenTree::Ident(ident) => {
                if let Some(keyword) = ident_to_keyword(ident.clone()) {
                    if context.can_feed(parser, &keyword) {
                        context.feed_location(parser, keyword, grammar_args, location)?;
                    } else {
                        context.feed_location(
                            parser,
                            Lexed::Ident(ident),
                            grammar_args,
                            location,
                        )?;
                    }
                } else {
                    context.feed_location(parser, Lexed::Ident(ident), grammar_args, location)?;
                }
            }
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' => {
                    context.feed_location(parser, Lexed::Colon(punct), grammar_args, location)?
                }
                ';' => context.feed_location(
                    parser,
                    Lexed::Semicolon(punct),
                    grammar_args,
                    location,
                )?,
                '|' => context.feed_location(parser, Lexed::Pipe(punct), grammar_args, location)?,
                '+' => context.feed_location(parser, Lexed::Plus(punct), grammar_args, location)?,
                '*' => context.feed_location(parser, Lexed::Star(punct), grammar_args, location)?,
                '?' => {
                    context.feed_location(parser, Lexed::Question(punct), grammar_args, location)?
                }
                '^' => {
                    context.feed_location(parser, Lexed::Caret(punct), grammar_args, location)?
                }
                '-' => {
                    context.feed_location(parser, Lexed::Minus(punct), grammar_args, location)?
                }
                '=' => {
                    context.feed_location(parser, Lexed::Equal(punct), grammar_args, location)?
                }
                '!' => context.feed_location(
                    parser,
                    Lexed::Exclamation(punct),
                    grammar_args,
                    location,
                )?,
                '/' => {
                    context.feed_location(parser, Lexed::Slash(punct), grammar_args, location)?
                }
                '.' => context.feed_location(parser, Lexed::Dot(punct), grammar_args, location)?,
                '%' => {
                    context.feed_location(parser, Lexed::Percent(punct), grammar_args, location)?
                }
                '$' => {
                    context.feed_location(parser, Lexed::Dollar(punct), grammar_args, location)?
                }
                ',' => {
                    context.feed_location(parser, Lexed::Comma(punct), grammar_args, location)?
                }
                _ => context.feed_location(
                    parser,
                    Lexed::OtherPunct(punct),
                    grammar_args,
                    location,
                )?,
            },
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {
                    let token = Lexed::ParenGroup(group);
                    if context.can_feed(parser, &token) {
                        context.feed_location(parser, token, grammar_args, location)?;
                    } else {
                        let Lexed::ParenGroup(group) = token else {
                            unreachable!();
                        };
                        // feed the splitted tokens
                        context.feed_location(
                            parser,
                            Lexed::LParen,
                            grammar_args,
                            SpanPair::new_single(group.span_open()),
                        )?;
                        feed_recursive(group.stream(), parser, context, grammar_args)?;
                        context.feed_location(
                            parser,
                            Lexed::RParen,
                            grammar_args,
                            SpanPair::new_single(group.span_close()),
                        )?;
                    }
                }
                Delimiter::Brace => {
                    // for now, splitted for brace is not in syntax, so ignore it
                    context.feed_location(
                        parser,
                        Lexed::BraceGroup(group),
                        grammar_args,
                        location,
                    )?;
                }
                Delimiter::Bracket => {
                    let token = Lexed::BracketGroup(group);
                    if context.can_feed(parser, &token) {
                        context.feed_location(parser, token, grammar_args, location)?;
                    } else {
                        let Lexed::BracketGroup(group) = token else {
                            unreachable!();
                        };
                        // feed the splitted tokens
                        context.feed_location(
                            parser,
                            Lexed::LBracket,
                            grammar_args,
                            SpanPair::new_single(group.span_open()),
                        )?;
                        feed_recursive(group.stream(), parser, context, grammar_args)?;
                        context.feed_location(
                            parser,
                            Lexed::RBracket,
                            grammar_args,
                            SpanPair::new_single(group.span_close()),
                        )?;
                    }
                }
                _ => {
                    // for now, compound for nonegroup is not in syntax, so ignore it
                    context.feed_location(
                        parser,
                        Lexed::NoneGroup(group),
                        grammar_args,
                        location,
                    )?;
                }
            },
            TokenTree::Literal(literal) => {
                context.feed_location(parser, Lexed::Literal(literal), grammar_args, location)?
            }
        };
    }
    Ok(())
}
