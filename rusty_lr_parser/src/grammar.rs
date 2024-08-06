use rusty_lr_core as rlr;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;

use std::collections::HashMap;
use std::collections::HashSet;

use crate::tokenizer::Tokenizer;

use super::error::ParseError;
// use super::parser::GrammarParser;
use super::parser_expanded::GrammarParser;
use super::rule::RuleLines;
use super::term::TermType;
use super::token::Token;

#[derive(Debug)]
pub struct Grammar {
    /// for bootstrapping the generated code; the prefix for the '::rusty_lr' module
    pub module_prefix: Option<TokenStream>,

    pub token_typename: Option<TokenStream>,
    pub userdata_typename: Option<TokenStream>,

    pub start_rule_name: Option<Ident>,
    pub eof: Option<TokenStream>,

    pub terminals: HashMap<String, (Ident, TokenStream)>,
    pub reduce_types: HashMap<String, (Ident, rlr::ReduceType)>,

    pub error_typename: Option<TokenStream>,

    pub rules: HashMap<String, (Ident, Option<TokenStream>, RuleLines)>,
    //                          name       typename           rules
}

impl Grammar {
    pub fn new() -> Self {
        Self {
            module_prefix: None,

            token_typename: None,
            userdata_typename: None,

            start_rule_name: None,
            eof: None,

            terminals: HashMap::new(),
            reduce_types: HashMap::new(),

            error_typename: None,

            rules: HashMap::new(),
        }
    }
    pub fn stack_name(name: &Ident) -> Ident {
        let span = name.span();
        let mut ident = format_ident!("rustylr_macro_generated_{}_stack", name);
        ident.set_span(span);
        ident
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
            if let Some((ident, _)) = grammar
                .terminals
                .insert("eof".to_string(), (format_ident!("eof"), eof.clone()))
            {
                return Err(ParseError::EofDefined(ident));
            }
        } else {
            return Err(ParseError::EofNotDefined);
        }

        // check if there are same names for terminals and non-terminals
        for (name, (ident, _, _)) in grammar.rules.iter() {
            if grammar.terminals.contains_key(name) {
                return Err(ParseError::TermNonTermConflict(ident.span(), name.clone()));
            }
        }

        // replace all terminal Ident with Term
        let terminals: HashSet<String> = grammar.terminals.keys().cloned().collect();
        for (_, (_name, _ruletype, rules)) in grammar.rules.iter_mut() {
            for rule in rules.rule_lines.iter_mut() {
                for token in rule.tokens.iter_mut() {
                    if let Token::NonTerm(ident) = token.token.clone() {
                        let name = ident.to_string();
                        if terminals.contains(&name) {
                            // set the token to Term
                            token.token = Token::Term(ident);
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
                    if let Token::NonTerm(ident) = &token.token {
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
