use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use quote::format_ident;
use quote::quote;
use quote::ToTokens;

use std::collections::HashMap;

use rusty_lr as rlr;

use super::error::ParseError;
use super::rule::RuleLines;
use super::token::Token;

#[derive(Clone, Debug)]
pub(crate) enum TermType {
    Ident(Option<proc_macro2::Ident>),
    Colon(Option<proc_macro2::Punct>),
    Semicolon(Option<proc_macro2::Punct>),
    Pipe(Option<proc_macro2::Punct>),
    Percent(Option<proc_macro2::Punct>),
    Left(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %left, %l, %reduce
    Right(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %right, %r, %shift
    Error(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %error, %e
    Token(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %token
    Start(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %start
    AugDef(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %aug
    TokenType(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %tokentype
    UserData(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %userdata
    Group(Option<Group>),
    Literal(Option<proc_macro2::Literal>),
    Eof,
}
impl TermType {
    pub fn enum_index(&self) -> usize {
        match self {
            TermType::Ident(_) => 0,
            TermType::Colon(_) => 1,
            TermType::Semicolon(_) => 2,
            TermType::Pipe(_) => 3,
            TermType::Percent(_) => 4,
            TermType::Left(_) => 5,
            TermType::Right(_) => 6,
            TermType::Error(_) => 7,
            TermType::Token(_) => 8,
            TermType::Start(_) => 9,
            TermType::AugDef(_) => 10,
            TermType::TokenType(_) => 11,
            TermType::UserData(_) => 12,
            TermType::Group(_) => 13,
            TermType::Literal(_) => 14,
            TermType::Eof => 15,
        }
    }
    pub fn stream(self) -> TokenStream {
        match self {
            TermType::Ident(ident) => ident.unwrap().to_token_stream(),
            TermType::Colon(punct) => punct.unwrap().to_token_stream(),
            TermType::Semicolon(punct) => punct.unwrap().to_token_stream(),
            TermType::Pipe(punct) => punct.unwrap().to_token_stream(),
            TermType::Percent(punct) => punct.unwrap().to_token_stream(),
            TermType::Left(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Right(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Error(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Token(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Start(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::AugDef(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::TokenType(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::UserData(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Group(group) => group.unwrap().to_token_stream(),
            TermType::Literal(lit) => lit.unwrap().to_token_stream(),
            TermType::Eof => unreachable!("Eof should not be converted to TokenStream"),
        }
    }
}
impl std::fmt::Display for TermType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TermType::Ident(_) => write!(f, "TokenTree::Ident"),
            TermType::Colon(_) => write!(f, ":"),
            TermType::Semicolon(_) => write!(f, ";"),
            TermType::Pipe(_) => write!(f, "|"),
            TermType::Percent(_) => write!(f, "%"),
            TermType::Left(_) => write!(f, "%left"),
            TermType::Right(_) => write!(f, "%right"),
            TermType::Error(_) => write!(f, "%error"),
            TermType::Token(_) => write!(f, "%token"),
            TermType::Start(_) => write!(f, "%start"),
            TermType::AugDef(_) => write!(f, "%augmented"),
            TermType::TokenType(_) => write!(f, "%tokentype"),
            TermType::UserData(_) => write!(f, "%userdata"),
            TermType::Group(_) => write!(f, "TokenTree::Group"),
            TermType::Literal(_) => write!(f, "TokenTree::Literal"),
            TermType::Eof => write!(f, "$"),
        }
    }
}
impl std::hash::Hash for TermType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.enum_index().hash(state);
    }
}
impl PartialEq for TermType {
    fn eq(&self, other: &Self) -> bool {
        self.enum_index() == other.enum_index()
    }
}
impl Eq for TermType {}
impl std::cmp::PartialOrd for TermType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.enum_index().partial_cmp(&other.enum_index())
    }
}
impl std::cmp::Ord for TermType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.enum_index().cmp(&other.enum_index())
    }
}

#[derive(Debug)]
pub struct Grammar {
    pub start_rule_name: Option<Ident>,
    pub augmented: Option<Ident>,
    pub terminals: HashMap<String, (Ident, TokenStream)>,
    pub tokentype: Option<TokenStream>,
    pub userdata_typename: Option<TokenStream>,
    pub rules: Vec<(Ident, TokenStream, RuleLines)>,
    //              name    typename     rules
}

impl Grammar {
    pub fn new() -> Self {
        Self {
            start_rule_name: None,
            augmented: None,
            terminals: HashMap::new(),
            tokentype: None,
            userdata_typename: None,
            rules: Vec::new(),
        }
    }
    pub fn stack_name(name: &Ident) -> Ident {
        let span = name.span();
        let name = name.to_string().to_lowercase();
        let mut ident = format_ident!("rustylr_macro_generated_{}_stack", name);
        ident.set_span(span);
        ident
    }
    pub(crate) fn tokenize(input: TokenStream) -> Result<Vec<TermType>, ParseError> {
        let mut tokens = Vec::new();
        let mut iter = input.into_iter().peekable();
        while let Some(token) = iter.next() {
            match token {
                TokenTree::Ident(ident) => {
                    tokens.push(TermType::Ident(Some(ident)));
                }
                TokenTree::Punct(punct) => match punct.as_char() {
                    ':' => tokens.push(TermType::Colon(Some(punct))),
                    ';' => tokens.push(TermType::Semicolon(Some(punct))),
                    '|' => tokens.push(TermType::Pipe(Some(punct))),
                    '%' => match iter.peek() {
                        Some(TokenTree::Ident(ident)) => match ident.to_string().as_str() {
                            "left" | "l" | "reduce" => {
                                tokens.push(TermType::Left(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            "right" | "r" | "shift" => {
                                tokens.push(TermType::Right(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            "error" | "e" => {
                                tokens.push(TermType::Error(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            "token" => {
                                tokens.push(TermType::Token(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            "start" => {
                                tokens.push(TermType::Start(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            "augmented" | "aug" => {
                                tokens.push(TermType::AugDef(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            "tokentype" => {
                                tokens.push(TermType::TokenType(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            "userdata" => {
                                tokens.push(TermType::UserData(Some((punct, ident.clone()))));
                                iter.next();
                            }
                            _ => {
                                tokens.push(TermType::Percent(Some(punct)));
                            }
                        },
                        _ => {}
                    },

                    other => {
                        return Err(ParseError::InvalidPunct(other));
                    }
                },
                TokenTree::Group(group) => tokens.push(TermType::Group(Some(group))),
                TokenTree::Literal(literal) => {
                    tokens.push(TermType::Literal(Some(literal)));
                }
            }
        }
        Ok(tokens)
    }
    pub(crate) fn build_parser() -> rlr::Parser<TermType, &'static str> {
        use rlr::*;

        let mut grammar = Grammar::new();

        // Rule : Ident RuleType ':' RuleLines ';' ;
        //
        // RuleType: Group | ;
        //
        // RuleLines: RuleLine '|' RuleLines
        //          | RuleLine
        //          ;
        //
        // RuleLine: RuleDef ReduceType Action ;
        //
        // RuleDef: Tokens ;
        //
        // Tokens: TokensOne
        //       |
        //       ;
        // TokensOne: Token TokensOne
        //          | Token
        //          ;
        //
        // Token: Ident
        //
        // ReduceType: Left | Right | Error
        //           |
        //           ;
        //
        // Action: Group
        //       |
        //       ;
        //
        // TokenDef: '%token' Ident RustCode ';'
        //            ;
        // AnyTokenNoSemi: <Any Token Except Semicolon> ;
        // AnyTokens: AnyTokenNoSemi AnyTokens
        //          | AnyTokenNoSemi
        //          ;
        //
        // RustCode: AnyTokens
        //         ;
        //
        // StartDef: '%start' Ident ';'
        //         ;
        // AugDef: '%aug' Ident ';'
        //       ;
        // TokenTypeDef: '%tokentype' RustCode ';'
        //             ;
        // UserDataDef: '%userdata' RustCode ';'
        //            ;
        //
        // Grammar: Rule Grammar
        //        | Rule
        //        | TokenDef Grammar
        //        | TokenDef
        //        | StartDef Grammar
        //        | StartDef
        //        | AugDef Grammar
        //        | AugDef
        //        | TokenTypeDef Grammar
        //        | TokenTypeDef
        //        | UserDataDef Grammar
        //        | UserDataDef
        //        ;

        // Rule : Ident RuleType ':' RuleLines ';' ;
        grammar.add_rule(
            "Rule",
            vec![
                Token::Term(TermType::Ident(None)),
                Token::NonTerm("RuleType"),
                Token::Term(TermType::Colon(None)),
                Token::NonTerm("RuleLines"),
                Token::Term(TermType::Semicolon(None)),
            ],
            ReduceType::Error,
        );
        // RuleType: Group | ;
        grammar.add_rule(
            "RuleType",
            vec![Token::Term(TermType::Group(None))],
            ReduceType::Error,
        );
        grammar.add_rule("RuleType", vec![], ReduceType::Error);

        // RuleLines: RuleLine '|' RuleLines
        //          | RuleLine
        //          ;
        grammar.add_rule(
            "RuleLines",
            vec![
                Token::NonTerm("RuleLine"),
                Token::Term(TermType::Pipe(None)),
                Token::NonTerm("RuleLines"),
            ],
            ReduceType::Error,
        );
        grammar.add_rule(
            "RuleLines",
            vec![Token::NonTerm("RuleLine")],
            ReduceType::Error,
        );

        // RuleLine: RuleDef ReduceType Action ;
        grammar.add_rule(
            "RuleLine",
            vec![
                Token::NonTerm("RuleDef"),
                Token::NonTerm("ReduceType"),
                Token::NonTerm("Action"),
            ],
            ReduceType::Error,
        );

        // RuleDef: Tokens ;
        grammar.add_rule("RuleDef", vec![Token::NonTerm("Tokens")], ReduceType::Error);

        // Tokens: TokensOne
        //       |
        //       ;
        grammar.add_rule(
            "Tokens",
            vec![Token::NonTerm("TokensOne")],
            ReduceType::Error,
        );
        grammar.add_rule("Tokens", vec![], ReduceType::Error);

        // TokensOne: Token TokensOne
        //          | Token
        //          ;
        grammar.add_rule(
            "TokensOne",
            vec![Token::NonTerm("Token"), Token::NonTerm("TokensOne")],
            ReduceType::Error,
        );
        grammar.add_rule(
            "TokensOne",
            vec![Token::NonTerm("Token")],
            ReduceType::Error,
        );

        // Token: Ident
        grammar.add_rule(
            "Token",
            vec![Token::Term(TermType::Ident(None))],
            ReduceType::Error,
        );
        // ReduceType: Left | Right | Error
        //           |
        //           ;
        grammar.add_rule(
            "ReduceType",
            vec![Token::Term(TermType::Left(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "ReduceType",
            vec![Token::Term(TermType::Right(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "ReduceType",
            vec![Token::Term(TermType::Error(None))],
            ReduceType::Error,
        );
        grammar.add_rule("ReduceType", vec![], ReduceType::Error);

        // Action: Group
        //       |
        //       ;
        grammar.add_rule(
            "Action",
            vec![Token::Term(TermType::Group(None))],
            ReduceType::Error,
        );
        grammar.add_rule("Action", vec![], ReduceType::Error);

        // TerminalDef: '%token' Ident RustCode ';'
        //            ;
        grammar.add_rule(
            "TokenDef",
            vec![
                Token::Term(TermType::Token(None)),
                Token::Term(TermType::Ident(None)),
                Token::NonTerm("RustCode"),
                Token::Term(TermType::Semicolon(None)),
            ],
            ReduceType::Error,
        );

        // AnyTokenNoSemi: <Any Token Except Semicolon> ;
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Ident(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Colon(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Pipe(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Percent(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Left(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Right(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Error(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Token(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Start(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::AugDef(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::TokenType(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::UserData(None))],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::Literal(None))],
            ReduceType::Error,
        );

        // AnyTokens: AnyTokens AnyTokenNoSemi
        //          | AnyTokenNoSemi
        //          ;
        grammar.add_rule(
            "AnyTokens",
            vec![
                Token::NonTerm("AnyTokenNoSemi"),
                Token::NonTerm("AnyTokens"),
            ],
            ReduceType::Error,
        );
        grammar.add_rule(
            "AnyTokens",
            vec![Token::NonTerm("AnyTokenNoSemi")],
            ReduceType::Error,
        );

        // RustCode: AnyTokens
        //         ;
        grammar.add_rule(
            "RustCode",
            vec![Token::NonTerm("AnyTokens")],
            ReduceType::Error,
        );

        // StartDef: '%start' Ident ';'
        //         ;
        grammar.add_rule(
            "StartDef",
            vec![
                Token::Term(TermType::Start(None)),
                Token::Term(TermType::Ident(None)),
                Token::Term(TermType::Semicolon(None)),
            ],
            ReduceType::Error,
        );

        // AugDef: '%aug' Ident ';'
        //       ;
        grammar.add_rule(
            "AugDef",
            vec![
                Token::Term(TermType::AugDef(None)),
                Token::Term(TermType::Ident(None)),
                Token::Term(TermType::Semicolon(None)),
            ],
            ReduceType::Error,
        );
        // TokenTypeDef: '%tokentype' RustCode ';'
        //             ;
        grammar.add_rule(
            "TokenTypeDef",
            vec![
                Token::Term(TermType::TokenType(None)),
                Token::NonTerm("RustCode"),
                Token::Term(TermType::Semicolon(None)),
            ],
            ReduceType::Error,
        );
        grammar.add_rule(
            "UserDataDef",
            vec![
                Token::Term(TermType::UserData(None)),
                Token::NonTerm("RustCode"),
                Token::Term(TermType::Semicolon(None)),
            ],
            ReduceType::Error,
        );

        // Grammar: Rule Grammar
        //        | Rule
        //        | TokenDef Grammar
        //        | TokenDef
        //        | StartDef Grammar
        //        | StartDef
        //        | AugDef Grammar
        //        | AugDef
        //        | TokenTypeDef Grammar
        //        | TokenTypeDef
        //        | UserDataDef Grammar
        //        | UserDataDef
        //        ;
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("Rule"), Token::NonTerm("Grammar")],
            ReduceType::Error,
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("Rule")], ReduceType::Error);

        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("TokenDef"), Token::NonTerm("Grammar")],
            ReduceType::Error,
        );
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("TokenDef")],
            ReduceType::Error,
        );

        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("StartDef"), Token::NonTerm("Grammar")],
            ReduceType::Error,
        );
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("StartDef")],
            ReduceType::Error,
        );
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("AugDef"), Token::NonTerm("Grammar")],
            ReduceType::Error,
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("AugDef")], ReduceType::Error);
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("TokenTypeDef"), Token::NonTerm("Grammar")],
            ReduceType::Error,
        );
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("TokenTypeDef")],
            ReduceType::Error,
        );
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("UserDataDef"), Token::NonTerm("Grammar")],
            ReduceType::Error,
        );
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("UserDataDef")],
            ReduceType::Error,
        );

        grammar.add_rule(
            "Augmented",
            vec![Token::NonTerm("Grammar"), Token::Term(TermType::Eof)],
            ReduceType::Error,
        );

        match grammar.build("Augmented") {
            Ok(parser) => parser,
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

    // AugDef: '%aug' Ident ';'
    //       ;
    pub(crate) fn parse_augdef(
        tree: &rlr::Tree,
        terms: &[TermType],
        _parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<Ident, ParseError> {
        match tree {
            rlr::Tree::NonTerminal(_, children, _) => {
                let augname = children[1].slice(terms);
                let augname = if let TermType::Ident(augname) = &augname[0] {
                    augname.clone().unwrap()
                } else {
                    unreachable!();
                };

                Ok(augname)
            }
            _ => {
                unreachable!();
            }
        }
    }

    // StartDef: '%start' Ident ';'
    //         ;
    pub(crate) fn parse_startdef(
        tree: &rlr::Tree,
        terms: &[TermType],
        _parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<Ident, ParseError> {
        match tree {
            rlr::Tree::NonTerminal(_, children, _) => {
                let startname = children[1].slice(terms);
                let startname = if let TermType::Ident(startname) = &startname[0] {
                    startname.clone().unwrap()
                } else {
                    unreachable!();
                };

                Ok(startname)
            }
            _ => {
                unreachable!();
            }
        }
    }
    // TokenTypeDef: '%tokentype' RustCode ';'
    //             ;
    pub(crate) fn parse_tokentypedef(
        tree: &rlr::Tree,
        terms: &[TermType],
        _parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<TokenStream, ParseError> {
        match tree {
            rlr::Tree::NonTerminal(_, children, _) => {
                let mut rustcode = TokenStream::new();
                for token in children[1].slice(terms).iter() {
                    rustcode.extend(token.clone().stream());
                }

                Ok(rustcode)
            }
            _ => {
                unreachable!();
            }
        }
    }

    // TokenDef: '%token' Ident RustCode ';'
    //            ;
    // AnyTokenNoSemi: <Any Token Except Semicolon> ;
    // AnyTokens: AnyTokens AnyTokenNoSemi
    //          | AnyTokenNoSemi
    //          ;
    //
    // RustCode: AnyTokens
    //         ;
    pub(crate) fn parse_tokendef(
        tree: &rlr::Tree,
        terms: &[TermType],
        _parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<(Ident, TokenStream), ParseError> {
        match tree {
            rlr::Tree::NonTerminal(_, children, _) => {
                let tokenname = children[1].slice(terms);
                let tokenname = if let TermType::Ident(tokenname) = &tokenname[0] {
                    tokenname.as_ref().unwrap()
                } else {
                    unreachable!();
                };
                let mut rustcode = TokenStream::new();
                for token in children[2].slice(terms).iter() {
                    rustcode.extend(token.clone().stream());
                }

                Ok((tokenname.clone(), rustcode))
            }
            _ => {
                unreachable!();
            }
        }
    }
    // RuleType: Group | ;
    pub(crate) fn parse_ruletype(
        tree: &rlr::Tree,
        terms: &[TermType],
        _parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<TokenStream, ParseError> {
        match tree {
            rlr::Tree::NonTerminal(_, children, _) => {
                if children.len() == 0 {
                    Ok(quote! {()})
                } else if children.len() == 1 {
                    if let Some(token) = children[0].slice(terms).get(0) {
                        if let TermType::Group(group) = token {
                            Ok(group.as_ref().unwrap().stream())
                        } else {
                            unreachable!();
                        }
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            }
            _ => {
                unreachable!();
            }
        }
    }

    // Rule : Ident RuleType ':' RuleLines ';' ;
    pub(crate) fn parse_rule(
        tree: &rlr::Tree,
        terms: &[TermType],
        parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<(Ident, TokenStream, RuleLines), ParseError> {
        match tree {
            rlr::Tree::NonTerminal(_, children, _) => {
                let rulename = children[0].slice(terms);
                let rulename = if let TermType::Ident(rulename) = &rulename[0] {
                    rulename.as_ref().unwrap()
                } else {
                    unreachable!();
                };

                let typename = Self::parse_ruletype(&children[1], terms, parser)?;

                // it is reversed
                let mut rulelines = RuleLines::parse_tree(&children[3], terms, parser)?;
                rulelines.rule_lines.reverse();

                Ok((rulename.clone(), typename, rulelines))
            }
            _ => {
                unreachable!();
            }
        }
    }
    // UserDataDef: '%userdata' RustCode ';'
    pub(crate) fn parse_userdatadef(
        tree: &rlr::Tree,
        terms: &[TermType],
        _parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<TokenStream, ParseError> {
        match tree {
            rlr::Tree::NonTerminal(_, children, _) => {
                let mut rustcode = TokenStream::new();
                for token in children[1].slice(terms).iter() {
                    rustcode.extend(token.clone().stream());
                }

                Ok(rustcode)
            }
            _ => {
                unreachable!();
            }
        }
    }

    // Grammar: Rule Grammar
    //        | Rule
    //        | TokenDef Grammar
    //        | TokenDef
    //        | StartDef Grammar
    //        | StartDef
    //        | AugDef Grammar
    //        | AugDef
    //        | TokenTypeDef Grammar
    //        | TokenTypeDef
    //        | UserDataDef Grammar
    //        | UserDataDef
    //        ;
    // returned Vec of Rule is reversed
    pub(crate) fn parse_tree_impl(
        tree: &rlr::Tree,
        terms: &[TermType],
        parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<Self, ParseError> {
        match tree {
            rlr::Tree::NonTerminal(ruleid, children, _) => {
                let rule = parser.rules.get(*ruleid).unwrap();
                match (rule.rule.get(0).as_ref(), rule.rule.get(1).as_ref()) {
                    (Some(rlr::Token::NonTerm("Rule")), Some(rlr::Token::NonTerm("Grammar"))) => {
                        let rule = Self::parse_rule(&children[0], terms, parser)?;
                        let mut grammar = Self::parse_tree_impl(&children[1], terms, parser)?;
                        grammar.rules.push(rule);

                        Ok(grammar)
                    }
                    (Some(rlr::Token::NonTerm("Rule")), None) => {
                        let rule = Self::parse_rule(&children[0], terms, parser)?;
                        let mut grammar = Grammar::new();
                        grammar.rules.push(rule);

                        Ok(grammar)
                    }
                    (
                        Some(rlr::Token::NonTerm("TokenDef")),
                        Some(rlr::Token::NonTerm("Grammar")),
                    ) => {
                        let (tokenname, tokenreplace) =
                            Self::parse_tokendef(&children[0], terms, parser)?;
                        let mut grammar = Self::parse_tree_impl(&children[1], terms, parser)?;
                        let old = grammar
                            .terminals
                            .insert(tokenname.to_string(), (tokenname.clone(), tokenreplace));
                        if let Some(rule) = old {
                            return Err(ParseError::MultipleTokenDefinition(tokenname, rule.1));
                        }

                        Ok(grammar)
                    }
                    (Some(rlr::Token::NonTerm("TokenDef")), None) => {
                        let (tokenname, tokenreplace) =
                            Self::parse_tokendef(&children[0], terms, parser)?;
                        let mut grammar = Grammar::new();
                        grammar
                            .terminals
                            .insert(tokenname.to_string(), (tokenname.clone(), tokenreplace));

                        Ok(grammar)
                    }
                    (
                        Some(rlr::Token::NonTerm("StartDef")),
                        Some(rlr::Token::NonTerm("Grammar")),
                    ) => {
                        let startname = Self::parse_startdef(&children[0], terms, parser)?;
                        let mut grammar = Self::parse_tree_impl(&children[1], terms, parser)?;

                        if let Some(old_start) = grammar.start_rule_name {
                            return Err(ParseError::MultipleStartDefinition(old_start, startname));
                        }
                        grammar.start_rule_name = Some(startname);

                        Ok(grammar)
                    }
                    (Some(rlr::Token::NonTerm("StartDef")), None) => {
                        let startname = Self::parse_startdef(&children[0], terms, parser)?;
                        let mut grammar = Grammar::new();

                        grammar.start_rule_name = Some(startname);

                        Ok(grammar)
                    }
                    (Some(rlr::Token::NonTerm("AugDef")), Some(rlr::Token::NonTerm("Grammar"))) => {
                        let augname = Self::parse_augdef(&children[0], terms, parser)?;
                        let mut grammar = Self::parse_tree_impl(&children[1], terms, parser)?;

                        if let Some(old_aug) = grammar.augmented {
                            return Err(ParseError::MultipleAugmentedDefinition(old_aug, augname));
                        }
                        grammar.augmented = Some(augname);

                        Ok(grammar)
                    }
                    (Some(rlr::Token::NonTerm("AugDef")), None) => {
                        let augname = Self::parse_augdef(&children[1], terms, parser)?;
                        let mut grammar = Grammar::new();

                        grammar.augmented = Some(augname);

                        Ok(grammar)
                    }
                    (
                        Some(rlr::Token::NonTerm("TokenTypeDef")),
                        Some(rlr::Token::NonTerm("Grammar")),
                    ) => {
                        let tokentype_stream =
                            Self::parse_tokentypedef(&children[0], terms, parser)?;
                        let mut grammar = Self::parse_tree_impl(&children[1], terms, parser)?;

                        if let Some(old_tokentype) = grammar.tokentype {
                            return Err(ParseError::MultipleTokenTypeDefinition(
                                old_tokentype,
                                tokentype_stream,
                            ));
                        }
                        grammar.tokentype = Some(tokentype_stream);

                        Ok(grammar)
                    }
                    (Some(rlr::Token::NonTerm("TokenTypeDef")), None) => {
                        let tokentype_stream =
                            Self::parse_tokentypedef(&children[0], terms, parser)?;
                        let mut grammar = Grammar::new();

                        grammar.tokentype = Some(tokentype_stream);

                        Ok(grammar)
                    }
                    (
                        Some(rlr::Token::NonTerm("UserDataDef")),
                        Some(rlr::Token::NonTerm("Grammar")),
                    ) => {
                        let userdata_stream = Self::parse_userdatadef(&children[0], terms, parser)?;
                        let mut grammar = Self::parse_tree_impl(&children[1], terms, parser)?;

                        if let Some(old_userdata) = grammar.userdata_typename {
                            return Err(ParseError::MultipleUserDataDefinition(
                                old_userdata,
                                userdata_stream,
                            ));
                        }
                        grammar.userdata_typename = Some(userdata_stream);

                        Ok(grammar)
                    }
                    (Some(rlr::Token::NonTerm("UserDataDef")), None) => {
                        let userdata_stream = Self::parse_userdatadef(&children[0], terms, parser)?;
                        let mut grammar = Grammar::new();

                        grammar.userdata_typename = Some(userdata_stream);

                        Ok(grammar)
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }
            _ => {
                unreachable!();
            }
        }
    }

    pub fn parse_tree(
        tree: &rlr::Tree,
        terms: &[TermType],
        parser: &rlr::Parser<TermType, &'static str>,
    ) -> Result<Self, ParseError> {
        let mut grammar = Self::parse_tree_impl(tree, terms, parser)?;
        // reverse the rules
        grammar.rules.reverse();

        // check start defined
        if grammar.start_rule_name.is_none() {
            return Err(ParseError::StartNotDefined);
        }
        // check augmented production rule defined
        if grammar.augmented.is_none() {
            return Err(ParseError::AugmentedNotDefined);
        }

        // filter token in production rules
        let terminals = grammar.terminals.clone();
        for (_, _, rule) in grammar.rules.iter_mut() {
            for rule_line in rule.rule_lines.iter_mut() {
                for token in rule_line.tokens.iter_mut() {
                    let name = match token {
                        Token::Term(term) => term.clone(),
                        Token::NonTerm(nonterm) => nonterm.clone(),
                    };

                    if terminals.contains_key(&name.to_string()) {
                        *token = Token::Term(name);
                    } else {
                        *token = Token::NonTerm(name);
                    }
                }
            }
        }

        Ok(grammar)
    }
}
