use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Punct;
use proc_macro2::Span;
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
    OtherPunct(Punct),

    Literal(Literal),

    ParenGroup(Group),
    BraceGroup(Group),
    BracketGroup(Group),
    NoneGroup(Group),
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
    Lalr(Punct, Ident),         // %lalr
    Glr(Punct, Ident),          // %glr
    Prec(Punct, Ident),         // %prec
    Precedence(Punct, Ident),   // %precedence
    NoOptim(Punct, Ident),      // %nooptim
    Dense(Punct, Ident),        // %dense
    Trace(Punct, Ident),        // %trace
    DPrec(Punct, Ident),        // %dprec
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
            Lexed::Slash(punct) => stream.append(punct),
            Lexed::Dot(punct) => stream.append(punct),
            Lexed::OtherPunct(punct) => stream.append(punct),

            Lexed::Literal(lit) => stream.append(lit),

            Lexed::ParenGroup(group) => stream.append(group),
            Lexed::BraceGroup(group) => stream.append(group),
            Lexed::BracketGroup(group) => stream.append(group),
            Lexed::NoneGroup(group) => stream.append(group),

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
            Lexed::Lalr(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Glr(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Prec(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Precedence(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::NoOptim(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Dense(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::Trace(punct, ident) => {
                stream.append(punct);
                stream.append(ident);
            }
            Lexed::DPrec(punct, ident) => {
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
            Lexed::Slash(punct) => punct.span(),
            Lexed::Dot(punct) => punct.span(),
            Lexed::OtherPunct(punct) => punct.span(),

            Lexed::Literal(lit) => lit.span(),

            Lexed::ParenGroup(group) => group.span(),
            Lexed::BraceGroup(group) => group.span(),
            Lexed::BracketGroup(group) => group.span(),
            Lexed::NoneGroup(group) => group.span(),
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
            Lexed::Lalr(punct, ident) => ident.span(),
            Lexed::Glr(punct, ident) => ident.span(),
            Lexed::Prec(punct, ident) => ident.span(),
            Lexed::Precedence(punct, ident) => ident.span(),
            Lexed::NoOptim(punct, ident) => ident.span(),
            Lexed::Dense(punct, ident) => ident.span(),
            Lexed::Trace(punct, ident) => ident.span(),
            Lexed::DPrec(punct, ident) => ident.span(),

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
            Lexed::Slash(_) => write!(f, "'/'"),
            Lexed::Dot(_) => write!(f, "'.'"),
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
            Lexed::Lalr(_, _) => write!(f, "%lalr"),
            Lexed::Glr(_, _) => write!(f, "%glr"),
            Lexed::Prec(_, _) => write!(f, "%prec"),
            Lexed::Precedence(_, _) => write!(f, "%precedence"),
            Lexed::NoOptim(_, _) => write!(f, "%nooptim"),
            Lexed::Dense(_, _) => write!(f, "%dense"),
            Lexed::Trace(_, _) => write!(f, "%trace"),
            Lexed::DPrec(_, _) => write!(f, "%dprec"),

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
        "lalr" => Some(Lexed::Lalr(percent, ident)),
        "glr" => Some(Lexed::Glr(percent, ident)),
        "prec" => Some(Lexed::Prec(percent, ident)),
        "precedence" => Some(Lexed::Precedence(percent, ident)),
        "nooptim" => Some(Lexed::NoOptim(percent, ident)),
        "dense" => Some(Lexed::Dense(percent, ident)),
        "trace" => Some(Lexed::Trace(percent, ident)),
        "dprec" => Some(Lexed::DPrec(percent, ident)),
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
    let mut input = input.into_iter().peekable();

    while let Some(next) = input.next() {
        match next {
            TokenTree::Ident(ident) => {
                context.feed(parser, Lexed::Ident(ident), grammar_args)?;
            }
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' => context.feed(parser, Lexed::Colon(punct), grammar_args)?,
                ';' => context.feed(parser, Lexed::Semicolon(punct), grammar_args)?,
                '|' => context.feed(parser, Lexed::Pipe(punct), grammar_args)?,
                '+' => context.feed(parser, Lexed::Plus(punct), grammar_args)?,
                '*' => context.feed(parser, Lexed::Star(punct), grammar_args)?,
                '?' => context.feed(parser, Lexed::Question(punct), grammar_args)?,
                '^' => context.feed(parser, Lexed::Caret(punct), grammar_args)?,
                '-' => context.feed(parser, Lexed::Minus(punct), grammar_args)?,
                '=' => context.feed(parser, Lexed::Equal(punct), grammar_args)?,
                '!' => context.feed(parser, Lexed::Exclamation(punct), grammar_args)?,
                '/' => context.feed(parser, Lexed::Slash(punct), grammar_args)?,
                '.' => context.feed(parser, Lexed::Dot(punct), grammar_args)?,
                '%' => {
                    // check next is ident, and is a keyword
                    let next_ident = input.peek().cloned();
                    if let Some(TokenTree::Ident(next_ident)) = next_ident {
                        if let Some(keyword_compound) =
                            ident_to_keyword(punct.clone(), next_ident.clone())
                        {
                            input.next();
                            // feed the compound token
                            if context
                                .feed(parser, keyword_compound, grammar_args)
                                .is_err()
                            {
                                // compound token failed
                                // feed the splitted tokens
                                context.feed(parser, Lexed::Percent(punct), grammar_args)?;
                                context.feed(parser, Lexed::Ident(next_ident), grammar_args)?;
                            }
                        } else {
                            context.feed(parser, Lexed::Percent(punct), grammar_args)?;
                        }
                    } else {
                        context.feed(parser, Lexed::Percent(punct), grammar_args)?;
                    }
                }

                _ => context.feed(parser, Lexed::OtherPunct(punct), grammar_args)?,
            },
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {
                    if let Err(err) = context.feed(parser, Lexed::ParenGroup(group), grammar_args) {
                        let term = if let GrammarParseError::InvalidTerminal(err) = err {
                            err.term
                        } else {
                            unreachable!();
                        };
                        let group = if let Lexed::ParenGroup(group) = term {
                            group
                        } else {
                            unreachable!();
                        };
                        // feed the splitted tokens
                        context.feed(parser, Lexed::LParen(group.span_open()), grammar_args)?;
                        feed_recursive(group.stream(), parser, context, grammar_args)?;
                        context.feed(parser, Lexed::RParen(group.span_close()), grammar_args)?;
                    }
                }
                Delimiter::Brace => {
                    // for now, splitted for brace is not in syntax, so ignore it
                    context.feed(parser, Lexed::BraceGroup(group), grammar_args)?;

                    // feed the compound token
                    // if parser
                    //     .feed(context, Lexed::BraceGroup(Some(group.clone())))
                    //     .is_err()
                    // {
                    //     // compound token failed
                    //     // feed the splitted tokens
                    //     parser.feed(context, Lexed::LBrace(group.span_open()))?;
                    //     feed_recursive(group.stream(), parser, context)?;
                    //     parser.feed(context, Lexed::RBrace(group.span_close()))?;
                    // }
                }
                Delimiter::Bracket => {
                    // for now, compound for bracket is not in syntax, so ignore it
                    context.feed(parser, Lexed::LBracket(group.span_open()), grammar_args)?;
                    feed_recursive(group.stream(), parser, context, grammar_args)?;
                    context.feed(parser, Lexed::RBracket(group.span_close()), grammar_args)?;

                    // feed the compound token
                    // if parser
                    //     .feed(context, Lexed::BracketGroup(Some(group.clone())))
                    //     .is_err()
                    // {
                    // compound token failed
                    // feed the splitted tokens
                    // parser.feed(context, Lexed::LBracket(group.span_open()))?;
                    // feed_recursive(group.stream(), parser, context)?;
                    // parser.feed(context, Lexed::RBracket(group.span_close()))?;
                    // }
                }
                _ => {
                    // for now, compound for nonegroup is not in syntax, so ignore it
                    context.feed(parser, Lexed::NoneGroup(group), grammar_args)?;

                    // feed the compound token
                    // if parser
                    //     .feed(context, Lexed::NoneGroup(Some(group.clone())))
                    //     .is_err()
                    // {
                    //     // compound token failed
                    //     // feed the splitted tokens
                    //     feed_recursive(group.stream(), parser, context)?;
                    // }
                }
            },
            TokenTree::Literal(literal) => {
                context.feed(parser, Lexed::Literal(literal), grammar_args)?
            }
        };
    }
    Ok(())
}
