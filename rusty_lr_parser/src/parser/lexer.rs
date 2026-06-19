use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Punct;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use quote::ToTokens;
use quote::TokenStreamExt;

use super::location::Location;
use super::parser_expanded::GrammarContext;
use super::parser_expanded::GrammarParseError;

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

    IntLiteral(syn::LitInt),
    ByteLiteral(syn::LitByte),
    ByteStrLiteral(syn::LitByteStr),
    CharLiteral(syn::LitChar),
    StrLiteral(syn::LitStr),
    OtherLiteral(syn::Lit),

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

    Left(Ident),         // %left
    Right(Ident),        // %right
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
    DPrec(Ident),        // %dprec
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

            Lexed::IntLiteral(i) => stream.extend(i.into_token_stream()),
            Lexed::ByteLiteral(b) => stream.extend(b.into_token_stream()),
            Lexed::ByteStrLiteral(bs) => stream.extend(bs.into_token_stream()),
            Lexed::CharLiteral(c) => stream.extend(c.into_token_stream()),
            Lexed::StrLiteral(s) => stream.extend(s.into_token_stream()),
            Lexed::OtherLiteral(l) => stream.extend(l.into_token_stream()),

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
            Lexed::DPrec(ident) => {
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
            Lexed::IntLiteral(_) => write!(f, "<IntLiteral>"),
            Lexed::ByteLiteral(_) => write!(f, "<ByteLiteral>"),
            Lexed::ByteStrLiteral(_) => write!(f, "<ByteStrLiteral>"),
            Lexed::CharLiteral(_) => write!(f, "<CharLiteral>"),
            Lexed::StrLiteral(_) => write!(f, "<StrLiteral>"),
            Lexed::OtherLiteral(_) => write!(f, "<OtherLiteral>"),
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
            Lexed::DPrec(_) => write!(f, "dprec"),
            Lexed::Location(_) => write!(f, "location"),
        }
    }
}

fn ident_to_keyword(ident: Ident) -> Option<Lexed> {
    match ident.to_string().as_str() {
        "left" => Some(Lexed::Left(ident)),
        "right" => Some(Lexed::Right(ident)),
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
        "dprec" => Some(Lexed::DPrec(ident)),
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
    context: &mut GrammarContext,
) -> Result<(), GrammarParseError> {
    let mut input = input.into_iter().peekable();

    while let Some(next) = input.next() {
        let span = next.span();
        match next {
            TokenTree::Ident(ident) => {
                let location = context.userdata_mut().span_manager.add_span(span);
                let location = Location::Range(location, location + 1);
                if let Some(keyword) = ident_to_keyword(ident.clone()) {
                    if context.can_feed(&keyword) {
                        context.feed_location(keyword, location)?;
                    } else {
                        context.feed_location(Lexed::Ident(ident), location)?;
                    }
                } else {
                    context.feed_location(Lexed::Ident(ident), location)?;
                }
            }
            TokenTree::Punct(punct) => {
                let location = context.userdata_mut().span_manager.add_span(span);
                let location = Location::Range(location, location + 1);

                match punct.as_char() {
                    ':' => context.feed_location(Lexed::Colon(punct), location)?,
                    ';' => context.feed_location(Lexed::Semicolon(punct), location)?,
                    '|' => context.feed_location(Lexed::Pipe(punct), location)?,
                    '+' => context.feed_location(Lexed::Plus(punct), location)?,
                    '*' => context.feed_location(Lexed::Star(punct), location)?,
                    '?' => context.feed_location(Lexed::Question(punct), location)?,
                    '^' => context.feed_location(Lexed::Caret(punct), location)?,
                    '-' => context.feed_location(Lexed::Minus(punct), location)?,
                    '=' => context.feed_location(Lexed::Equal(punct), location)?,
                    '!' => context.feed_location(Lexed::Exclamation(punct), location)?,
                    '/' => context.feed_location(Lexed::Slash(punct), location)?,
                    '.' => context.feed_location(Lexed::Dot(punct), location)?,
                    '%' => context.feed_location(Lexed::Percent(punct), location)?,
                    '$' => context.feed_location(Lexed::Dollar(punct), location)?,
                    ',' => context.feed_location(Lexed::Comma(punct), location)?,
                    _ => context.feed_location(Lexed::OtherPunct(punct), location)?,
                }
            }
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => {
                    let token = Lexed::ParenGroup(group);
                    if context.can_feed(&token) {
                        let location = context.userdata_mut().span_manager.add_span(span);
                        let location = Location::Range(location, location + 1);
                        context.feed_location(token, location)?;
                    } else {
                        let Lexed::ParenGroup(group) = token else {
                            unreachable!();
                        };

                        let open_span = group.span_open();
                        let open_location = context
                            .userdata_mut()
                            .span_manager
                            .add_span(open_span.clone());
                        let open_location = Location::Range(open_location, open_location + 1);

                        // feed the splitted tokens
                        context.feed_location(Lexed::LParen, open_location)?;
                        feed_recursive(group.stream(), context)?;

                        let close_span = group.span_close();
                        let close_location = context
                            .userdata_mut()
                            .span_manager
                            .add_span(close_span.clone());
                        let close_location = Location::Range(close_location, close_location + 1);
                        context.feed_location(Lexed::RParen, close_location)?;
                    }
                }
                Delimiter::Brace => {
                    let location = context.userdata_mut().span_manager.add_span(span);
                    let location = Location::Range(location, location + 1);
                    // for now, splitted for brace is not in syntax, so ignore it
                    context.feed_location(Lexed::BraceGroup(group), location)?;
                }
                Delimiter::Bracket => {
                    let token = Lexed::BracketGroup(group);
                    if context.can_feed(&token) {
                        let location = context.userdata_mut().span_manager.add_span(span);
                        let location = Location::Range(location, location + 1);
                        context.feed_location(token, location)?;
                    } else {
                        let Lexed::BracketGroup(group) = token else {
                            unreachable!();
                        };
                        let open_span = group.span_open();
                        let open_location = context
                            .userdata_mut()
                            .span_manager
                            .add_span(open_span.clone());
                        let open_location = Location::Range(open_location, open_location + 1);
                        // feed the splitted tokens
                        context.feed_location(Lexed::LBracket, open_location)?;
                        feed_recursive(group.stream(), context)?;

                        let close_span = group.span_close();
                        let close_location = context
                            .userdata_mut()
                            .span_manager
                            .add_span(close_span.clone());
                        let close_location = Location::Range(close_location, close_location + 1);
                        context.feed_location(Lexed::RBracket, close_location)?;
                    }
                }
                _ => {
                    let location = context.userdata_mut().span_manager.add_span(span);
                    let location = Location::Range(location, location + 1);
                    // for now, compound for nonegroup is not in syntax, so ignore it
                    context.feed_location(Lexed::NoneGroup(group), location)?;
                }
            },
            TokenTree::Literal(literal) => {
                let lit = match syn::parse2::<syn::Lit>(literal.to_token_stream()) {
                    Ok(lit) => lit,
                    Err(e) => {
                        unreachable!(
                            "Failed to parse literal token: {}, error: {}",
                            literal.to_string(),
                            e
                        )
                    }
                };
                let term = match lit {
                    syn::Lit::Int(i) => Lexed::IntLiteral(i),
                    syn::Lit::Byte(b) => Lexed::ByteLiteral(b),
                    syn::Lit::ByteStr(bs) => Lexed::ByteStrLiteral(bs),
                    syn::Lit::Char(c) => Lexed::CharLiteral(c),
                    syn::Lit::Str(s) => Lexed::StrLiteral(s),
                    _ => Lexed::OtherLiteral(lit),
                };
                let location = context.userdata_mut().span_manager.add_span(span);
                let location = Location::Range(location, location + 1);
                context.feed_location(term, location)?
            }
        };
    }
    Ok(())
}
