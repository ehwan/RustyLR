use rusty_lr_core as rlr;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote;

use std::collections::BTreeMap;
use std::collections::HashMap;

use crate::rule::RuleLine;
use crate::token::TokenMapped;
use crate::tokenizer::Tokenizer;

use super::error::ParseError;
// use super::parser::GrammarParser;
use super::parser_expanded::GrammarParser;
use super::rule::RuleLines;
use super::term::TermType;
use super::token::Token;
use super::utils;

#[derive(Debug)]
pub struct Grammar {
    /// for bootstrapping the generated code; the prefix for the '::rusty_lr' module
    pub module_prefix: Option<TokenStream>,

    pub token_typename: Option<TokenStream>,
    pub userdata_typename: Option<TokenStream>,

    pub start_rule_name: Option<Ident>,
    pub eof: Option<TokenStream>,

    pub terminals: BTreeMap<String, (Ident, TokenStream)>,
    pub reduce_types: BTreeMap<String, (Ident, rlr::ReduceType)>,

    pub error_typename: Option<TokenStream>,

    pub rules: BTreeMap<String, (Ident, Option<TokenStream>, RuleLines)>,
    //                           name       typename           rules
}

impl Grammar {
    pub fn new() -> Self {
        Self {
            module_prefix: None,

            token_typename: None,
            userdata_typename: None,

            start_rule_name: None,
            eof: None,

            terminals: BTreeMap::new(),
            reduce_types: BTreeMap::new(),

            error_typename: None,

            rules: BTreeMap::new(),
        }
    }

    // make A+ -> A A+ | A
    fn new_plus_rule(
        ident: &Ident,
        ruletype: Option<&TokenStream>, // if A is term, then TokenStream for termtype
    ) -> (Ident, Option<TokenStream>, RuleLines) {
        let new_ident = utils::generate_plus_rule_name(ident);
        if let Some(ruletype) = ruletype {
            // ruletype exists,
            // A+ -> A+ A { Ap.push(A); Ap }
            //     | A    { vec![A] }
            let line1 = RuleLine {
                tokens: vec![TokenMapped {
                    token: Token::NonTerm(ident.clone()),
                    mapto: Ident::new("A", ident.span()),
                }],
                reduce_action: Some(quote! {
                    { vec![A] }
                }),
            };
            let line2 = RuleLine {
                tokens: vec![
                    TokenMapped {
                        token: Token::NonTerm(new_ident.clone()),
                        mapto: Ident::new("Ap", ident.span()),
                    },
                    TokenMapped {
                        token: Token::NonTerm(ident.clone()),
                        mapto: Ident::new("A", ident.span()),
                    },
                ],
                reduce_action: Some(quote! {
                    { Ap.push(A); Ap }
                }),
            };
            let rule_lines = RuleLines {
                rule_lines: vec![line1, line2],
            };

            (new_ident, Some(quote! { (Vec<#ruletype>) }), rule_lines)
        } else {
            // ruletype not exist,
            // A+ -> A A+
            //     | A
            let line1 = RuleLine {
                tokens: vec![TokenMapped {
                    token: Token::NonTerm(ident.clone()),
                    mapto: Ident::new("A", ident.span()),
                }],
                reduce_action: None,
            };
            let line2 = RuleLine {
                tokens: vec![
                    TokenMapped {
                        token: Token::NonTerm(ident.clone()),
                        mapto: Ident::new("A", ident.span()),
                    },
                    TokenMapped {
                        token: Token::NonTerm(new_ident.clone()),
                        mapto: Ident::new("Ap", ident.span()),
                    },
                ],
                reduce_action: None,
            };
            let rule_lines = RuleLines {
                rule_lines: vec![line1, line2],
            };
            (new_ident, None, rule_lines)
        }
    }

    // make A* -> A+ | ε
    fn new_star_rule(
        ident: &Ident,
        ruletype: Option<&TokenStream>, // if A is term, then TokenStream for tokentype
    ) -> (Ident, Option<TokenStream>, RuleLines) {
        let plus_ident = utils::generate_plus_rule_name(ident);
        let new_ident = utils::generate_star_rule_name(ident);
        if let Some(ruletype) = ruletype {
            // ruletype exists,
            // A* -> A+ { Ap }
            //     |    { vec![] }
            let line1 = RuleLine {
                tokens: vec![],
                reduce_action: Some(quote! {
                    { vec![] }
                }),
            };
            let line2 = RuleLine {
                tokens: vec![TokenMapped {
                    token: Token::NonTerm(plus_ident),
                    mapto: Ident::new("Ap", ident.span()),
                }],
                reduce_action: Some(quote! {
                    { Ap }
                }),
            };
            let rule_lines = RuleLines {
                rule_lines: vec![line1, line2],
            };

            (new_ident, Some(quote! { (Vec<#ruletype>) }), rule_lines)
        } else {
            // ruletype not exist,
            // A* -> A+
            //     |
            let line1 = RuleLine {
                tokens: vec![],
                reduce_action: None,
            };
            let line2 = RuleLine {
                tokens: vec![TokenMapped {
                    token: Token::NonTerm(plus_ident),
                    mapto: Ident::new("Ap", ident.span()),
                }],
                reduce_action: None,
            };
            let rule_lines = RuleLines {
                rule_lines: vec![line1, line2],
            };

            (new_ident, None, rule_lines)
        }
    }

    // make A? -> A | ε
    fn new_question_rule(
        ident: &Ident,
        ruletype: Option<&TokenStream>, // if A is term, then TokenStream for tokentype
    ) -> (Ident, Option<TokenStream>, RuleLines) {
        let new_ident = utils::generate_question_rule_name(ident);
        if let Some(ruletype) = ruletype {
            // ruletype exists,
            // A? -> A { Some(Ap) }
            //     |   { None }
            let line1 = RuleLine {
                tokens: vec![],
                reduce_action: Some(quote! {
                    { None }
                }),
            };
            let line2 = RuleLine {
                tokens: vec![TokenMapped {
                    token: Token::NonTerm(ident.clone()),
                    mapto: Ident::new("A", ident.span()),
                }],
                reduce_action: Some(quote! {
                    { Some(A) }
                }),
            };
            let rule_lines = RuleLines {
                rule_lines: vec![line1, line2],
            };

            (new_ident, Some(quote! { (Option<#ruletype>) }), rule_lines)
        } else {
            // ruletype not exist,
            // A? -> A
            //     |
            let line1 = RuleLine {
                tokens: vec![],
                reduce_action: None,
            };
            let line2 = RuleLine {
                tokens: vec![TokenMapped {
                    token: Token::NonTerm(ident.clone()),
                    mapto: Ident::new("A", ident.span()),
                }],
                reduce_action: None,
            };
            let rule_lines = RuleLines {
                rule_lines: vec![line1, line2],
            };

            (new_ident, None, rule_lines)
        }
    }

    /// parse the input TokenStream and return a parsed Grammar
    pub fn parse(input: TokenStream) -> Result<Self, ParseError> {
        let mut tokenizer = Tokenizer::new(input);

        let parser = GrammarParser::new();
        let mut context = parser.begin();

        while let Some(token) = tokenizer.next_token() {
            let span = token.span().unwrap();
            match parser.feed(&mut context, token) {
                Ok(_) => {}
                Err(err) => match err {
                    rlr::ParseError::ReduceAction(err) => {
                        return Err(err);
                    }
                    _ => {
                        return Err(ParseError::InternalGrammar(span, format!("{}", err)));
                    }
                },
            }
        }
        match parser.feed(&mut context, TermType::Eof) {
            Ok(_) => {}
            Err(err) => match err {
                rlr::ParseError::ReduceAction(err) => {
                    return Err(err);
                }
                _ => {
                    let span = Span::call_site();
                    return Err(ParseError::InternalGrammar(span, format!("{}", err)));
                }
            },
        }

        let mut grammar = context.accept();

        // check eof is defined
        if let Some(eof) = &grammar.eof {
            // check if 'eof' is used as terminal symbol
            // else insert
            if let Some((ident, _)) = grammar.terminals.insert(
                utils::EOF_NAME.to_string(),
                (Ident::new(utils::EOF_NAME, Span::call_site()), eof.clone()),
            ) {
                return Err(ParseError::EofDefined(ident));
            }

            // check if 'eof' is used as non-terminal symbol
            if let Some((ident, _, _)) = grammar.rules.get(utils::EOF_NAME) {
                return Err(ParseError::EofDefined(ident.clone()));
            }
        } else {
            return Err(ParseError::EofNotDefined);
        }

        // check name 'Augmented' is being used
        if let Some((ident, _)) = grammar.terminals.get(utils::AUGMENTED_NAME) {
            return Err(ParseError::AugmentedDefined(ident.clone()));
        }
        if let Some((ident, _, _)) = grammar.rules.get(utils::AUGMENTED_NAME) {
            return Err(ParseError::AugmentedDefined(ident.clone()));
        }

        // check if there are same names for terminals and non-terminals
        for (name, (ident, _, _)) in grammar.rules.iter() {
            if grammar.terminals.contains_key(name) {
                return Err(ParseError::TermNonTermConflict(ident.span(), name.clone()));
            }
        }

        // check all terminal symbols in '%left', '%right' are defined
        for (name, (ident, _)) in grammar.reduce_types.iter() {
            if !grammar.terminals.contains_key(name) {
                return Err(ParseError::TerminalNotDefined(ident.clone()));
            }
        }

        // check token_typename is defined
        if grammar.token_typename.is_none() {
            return Err(ParseError::TokenTypeNotDefined);
        }

        // check start rule is defined
        if let Some(start_rule) = &grammar.start_rule_name {
            if !grammar.rules.contains_key(&start_rule.to_string()) {
                return Err(ParseError::NonTerminalNotDefined(start_rule.clone()));
            }
        } else {
            return Err(ParseError::StartNotDefined);
        }

        // check Token::Plus, Token::Star, Token::Question tokens
        // and define new rules for them
        // A+ -> A+ A | A
        // A* -> A+ | ε
        // A? -> A | ε
        //
        // <RuleType>
        // if A is nonterm and A has ruletype T,
        // then A+ -> Vec<T>, A* -> Vec<T>, A? -> Option<T>
        // if A does not have ruletype, then ()
        //
        // if A is term, then () since we can always trace terminals by slice

        {
            // new non-terminal rules will be generated for Plus, Star, Question
            // for tracking if new rule 'name' is already defined
            let mut new_rules = HashMap::new();
            for (_, (_name, _ruletype, rules)) in grammar.rules.iter() {
                for rule in rules.rule_lines.iter() {
                    for token in rule.tokens.iter() {
                        match &token.token {
                            Token::Plus(ident) => {
                                let ident_str = ident.to_string();
                                // get typename for current token
                                let ruletype = if grammar.terminals.contains_key(&ident_str) {
                                    grammar.token_typename.as_ref()
                                } else {
                                    match grammar.rules.get(&ident_str) {
                                        Some((_name, ruletype, _rules)) => ruletype.as_ref(),
                                        None => {
                                            return Err(ParseError::NonTerminalNotDefined(
                                                ident.clone(),
                                            ));
                                        }
                                    }
                                };

                                // add plus rule
                                let new_rule = Self::new_plus_rule(&ident, ruletype);
                                new_rules.insert(new_rule.0.to_string(), new_rule);
                            }
                            Token::Star(ident) => {
                                let ident_str = ident.to_string();

                                // get typename for current token
                                let ruletype = if grammar.terminals.contains_key(&ident_str) {
                                    grammar.token_typename.as_ref()
                                } else {
                                    match grammar.rules.get(&ident_str) {
                                        Some((_name, ruletype, _rules)) => ruletype.as_ref(),
                                        None => {
                                            return Err(ParseError::NonTerminalNotDefined(
                                                ident.clone(),
                                            ));
                                        }
                                    }
                                };

                                // add plus rule
                                let new_rule = Self::new_plus_rule(&ident, ruletype);
                                new_rules.insert(new_rule.0.to_string(), new_rule);

                                // add star rule
                                let new_rule = Self::new_star_rule(&ident, ruletype);
                                new_rules.insert(new_rule.0.to_string(), new_rule);
                            }
                            Token::Question(ident) => {
                                let ident_str = ident.to_string();

                                // get typename for current token
                                let ruletype = if grammar.terminals.contains_key(&ident_str) {
                                    grammar.token_typename.as_ref()
                                } else {
                                    match grammar.rules.get(&ident_str) {
                                        Some((_name, ruletype, _rules)) => ruletype.as_ref(),
                                        None => {
                                            return Err(ParseError::NonTerminalNotDefined(
                                                ident.clone(),
                                            ));
                                        }
                                    }
                                };

                                // add question rule
                                let new_rule = Self::new_question_rule(&ident, ruletype);
                                new_rules.insert(new_rule.0.to_string(), new_rule);
                            }
                            _ => {}
                        }
                    }
                }
            }
            for (name, rule_lines) in new_rules {
                if let Some((ident, _, _)) = grammar.rules.insert(name, rule_lines) {
                    return Err(ParseError::ReservedNonTerminal(ident));
                }
            }
        }

        // replace all Plus, Star, Question to NonTerm
        // check if new rule 'name' is already defined
        for (_, (_name, _ruletype, rules)) in grammar.rules.iter_mut() {
            for rule in rules.rule_lines.iter_mut() {
                for token in rule.tokens.iter_mut() {
                    match token.token.clone() {
                        Token::Plus(ident) => {
                            token.token = Token::NonTerm(utils::generate_plus_rule_name(&ident));
                        }
                        Token::Star(ident) => {
                            token.token = Token::NonTerm(utils::generate_star_rule_name(&ident));
                        }
                        Token::Question(ident) => {
                            token.token =
                                Token::NonTerm(utils::generate_question_rule_name(&ident));
                        }
                        _ => {}
                    }
                }
            }
        }

        // replace all NonTerm to Term if it is in terminals
        for (_, (_name, _ruletype, rules)) in grammar.rules.iter_mut() {
            for rule in rules.rule_lines.iter_mut() {
                for token in rule.tokens.iter_mut() {
                    if let Token::NonTerm(ident) = token.token.clone() {
                        if grammar.terminals.contains_key(&ident.to_string()) {
                            // set the token to Term
                            token.token = Token::Term(ident);
                        }
                    }
                }
            }
        }

        // check all NonTerminals are defined
        for (_, (_name, _ruletype, rules)) in grammar.rules.iter() {
            for rule in rules.rule_lines.iter() {
                for token in rule.tokens.iter() {
                    if let Token::NonTerm(ident) = &token.token {
                        if !grammar.rules.contains_key(&ident.to_string()) {
                            return Err(ParseError::NonTerminalNotDefined(ident.clone()));
                        }
                    }
                }
            }
        }

        Ok(grammar)
    }
}
