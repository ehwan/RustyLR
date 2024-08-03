use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;

use std::collections::HashMap;

use rusty_lr_core as rlr;

use crate::tokenizer::Tokenizer;

use super::callback::Callback;
use super::error::ParseError;
use super::rule::RuleLines;
use super::term::TermType;
use super::token::Token;

#[derive(Debug)]
pub struct Grammar {
    pub token_typename: Option<TokenStream>,
    pub userdata_typename: Option<TokenStream>,

    pub start_rule_name: Option<Ident>,
    pub eof: Option<TokenStream>,

    pub terminals: HashMap<String, (Ident, TokenStream)>,
    pub reduce_types: HashMap<String, (Ident, rlr::ReduceType)>,

    pub rules: HashMap<String, (Ident, Option<TokenStream>, RuleLines)>,
    //                          name       typename           rules
}

impl Grammar {
    pub fn new() -> Self {
        Self {
            token_typename: None,
            userdata_typename: None,

            start_rule_name: None,
            eof: None,

            terminals: HashMap::new(),
            reduce_types: HashMap::new(),

            rules: HashMap::new(),
        }
    }
    pub fn stack_name(name: &Ident) -> Ident {
        let span = name.span();
        let name = name.to_string().to_lowercase();
        let mut ident = format_ident!("rustylr_macro_generated_{}_stack", name);
        ident.set_span(span);
        ident
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
        // RuleLine: RuleDef Action ;
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
        // Token: Ident ;
        //
        // Action: Group
        //       |
        //       ;
        //
        // TokenDef: '%token' Ident RustCode ';'
        //         ;
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
        // EofDef: '%eof' RustCode ';'
        //       ;
        // TokenTypeDef: '%tokentype' RustCode ';'
        //             ;
        // UserDataDef: '%userdata' RustCode ';'
        //            ;
        // ReduceDef: '%left' Ident ';'
        //          | '%right' Ident';'
        //          ;
        //
        // Grammar: Rule Grammar
        //        | Rule
        //        | TokenDef Grammar
        //        | TokenDef
        //        | StartDef Grammar
        //        | StartDef
        //        | EofDef Grammar
        //        | EofDef
        //        | TokenTypeDef Grammar
        //        | TokenTypeDef
        //        | UserDataDef Grammar
        //        | UserDataDef
        //        | ReduceDef Grammar
        //        | ReduceDef
        //        ;

        grammar.add_rule(
            "Rule",
            vec![
                Token::Term(TermType::Ident(None)),
                Token::NonTerm("RuleType"),
                Token::Term(TermType::Colon(None)),
                Token::NonTerm("RuleLines"),
                Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule("RuleType", vec![Token::Term(TermType::Group(None))]);
        grammar.add_rule("RuleType", vec![]);

        grammar.add_rule(
            "RuleLines",
            vec![
                Token::NonTerm("RuleLine"),
                Token::Term(TermType::Pipe(None)),
                Token::NonTerm("RuleLines"),
            ],
        );
        grammar.add_rule("RuleLines", vec![Token::NonTerm("RuleLine")]);

        grammar.add_rule(
            "RuleLine",
            vec![Token::NonTerm("RuleDef"), Token::NonTerm("Action")],
        );

        grammar.add_rule("RuleDef", vec![Token::NonTerm("Tokens")]);

        grammar.add_rule("Tokens", vec![Token::NonTerm("TokensOne")]);
        grammar.add_rule("Tokens", vec![]);

        grammar.add_rule(
            "TokensOne",
            vec![Token::NonTerm("Token"), Token::NonTerm("TokensOne")],
        );
        grammar.add_rule("TokensOne", vec![Token::NonTerm("Token")]);

        grammar.add_rule("Token", vec![Token::Term(TermType::Ident(None))]);

        grammar.add_rule("Action", vec![Token::Term(TermType::Group(None))]);
        grammar.add_rule("Action", vec![]);

        grammar.add_rule(
            "TokenDef",
            vec![
                Token::Term(TermType::Token(None)),
                Token::Term(TermType::Ident(None)),
                Token::NonTerm("RustCode"),
                Token::Term(TermType::Semicolon(None)),
            ],
        );

        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Ident(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Colon(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Pipe(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Percent(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Left(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Right(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Token(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Start(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::EofDef(None))]);
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::TokenType(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::UserData(None))],
        );
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Group(None))]);
        grammar.add_rule("AnyTokenNoSemi", vec![Token::Term(TermType::Literal(None))]);
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![Token::Term(TermType::OtherPunct(None))],
        );

        grammar.add_rule(
            "AnyTokens",
            vec![
                Token::NonTerm("AnyTokenNoSemi"),
                Token::NonTerm("AnyTokens"),
            ],
        );
        grammar.add_rule("AnyTokens", vec![Token::NonTerm("AnyTokenNoSemi")]);

        grammar.add_rule("RustCode", vec![Token::NonTerm("AnyTokens")]);

        grammar.add_rule(
            "StartDef",
            vec![
                Token::Term(TermType::Start(None)),
                Token::Term(TermType::Ident(None)),
                Token::Term(TermType::Semicolon(None)),
            ],
        );

        grammar.add_rule(
            "EofDef",
            vec![
                Token::Term(TermType::EofDef(None)),
                Token::NonTerm("RustCode"),
                Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "TokenTypeDef",
            vec![
                Token::Term(TermType::TokenType(None)),
                Token::NonTerm("RustCode"),
                Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "UserDataDef",
            vec![
                Token::Term(TermType::UserData(None)),
                Token::NonTerm("RustCode"),
                Token::Term(TermType::Semicolon(None)),
            ],
        );

        grammar.add_rule(
            "ReduceDef",
            vec![
                Token::Term(TermType::Left(None)),
                Token::Term(TermType::Ident(None)),
                Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "ReduceDef",
            vec![
                Token::Term(TermType::Right(None)),
                Token::Term(TermType::Ident(None)),
                Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("Rule"), Token::NonTerm("Grammar")],
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("Rule")]);

        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("TokenDef"), Token::NonTerm("Grammar")],
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("TokenDef")]);

        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("StartDef"), Token::NonTerm("Grammar")],
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("StartDef")]);
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("EofDef"), Token::NonTerm("Grammar")],
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("EofDef")]);
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("TokenTypeDef"), Token::NonTerm("Grammar")],
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("TokenTypeDef")]);
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("UserDataDef"), Token::NonTerm("Grammar")],
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("UserDataDef")]);
        grammar.add_rule(
            "Grammar",
            vec![Token::NonTerm("ReduceDef"), Token::NonTerm("Grammar")],
        );
        grammar.add_rule("Grammar", vec![Token::NonTerm("ReduceDef")]);

        grammar.add_rule(
            "Augmented",
            vec![Token::NonTerm("Grammar"), Token::Term(TermType::Eof)],
        );

        match grammar.build("Augmented") {
            Ok(parser) => parser,
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

    /// parse the input TokenStream and return a parsed Grammar
    pub fn parse(input: TokenStream) -> Result<Self, ParseError> {
        let mut tokenizer = Tokenizer::new(input);
        let mut callback = Callback::new();

        let parser = Self::build_parser();
        let mut context = parser.begin();

        while let Some(token) = tokenizer.next_token() {
            let span = token.span().unwrap();
            match parser.feed_callback(&mut context, &mut callback, token) {
                Ok(_) => {}
                Err(err) => match err {
                    rlr::ParseError::Callback(err) => {
                        return Err(err);
                    }
                    _ => {
                        return Err(ParseError::InternalGrammar(span, format!("{}", err)));
                    }
                },
            }
        }
        match parser.feed_callback(&mut context, &mut callback, TermType::Eof) {
            Ok(_) => {}
            Err(err) => match err {
                rlr::ParseError::Callback(err) => {
                    return Err(err);
                }
                _ => {
                    let span = Span::call_site();
                    return Err(ParseError::InternalGrammar(span, format!("{}", err)));
                }
            },
        }

        let mut grammar = callback.grammar;

        // check eof is defined
        if let Some(eof) = &grammar.eof {
            if let Some((ident, _)) = grammar
                .terminals
                .insert("eof".to_string(), (format_ident!("eof"), eof.clone()))
            {
                return Err(ParseError::EofDefined(ident));
            }
        } else {
            return Err(ParseError::EofNotDefined);
        }

        // replace all terminal Ident with Term
        for (_, (_name, _ruletype, rules)) in grammar.rules.iter_mut() {
            for rule in rules.rule_lines.iter_mut() {
                for token in rule.tokens.iter_mut() {
                    if let Token::NonTerm(ident) = token.clone() {
                        if grammar.terminals.contains_key(&ident.to_string()) {
                            // set the token to Term
                            *token = Token::Term(ident);
                        }
                    }
                }
            }
        }

        // check token_typename is defined
        if grammar.token_typename.is_none() {
            return Err(ParseError::TokenTypeNotDefined);
        }

        // check all NonTerminals are defined
        for (_, (_name, _ruletype, rules)) in grammar.rules.iter() {
            for rule in rules.rule_lines.iter() {
                for token in rule.tokens.iter() {
                    if let Token::NonTerm(ident) = token {
                        if !grammar.rules.contains_key(&ident.to_string()) {
                            return Err(ParseError::NonTerminalNotDefined(ident.clone()));
                        }
                    }
                }
            }
        }

        // check start rule is defined
        if let Some(start_rule) = &grammar.start_rule_name {
            if !grammar.rules.contains_key(&start_rule.to_string()) {
                return Err(ParseError::NonTerminalNotDefined(start_rule.clone()));
            }
        } else {
            return Err(ParseError::StartNotDefined);
        }

        Ok(grammar)
    }
}
