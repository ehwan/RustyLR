use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;

use std::collections::BTreeMap;

use crate::error::ParseError;
use crate::parser::args::ReduceTypeArgs;
use crate::parser::lexer::Lexed;
use crate::parser::lexer::Lexer;
use crate::parser::parser_expanded::GrammarParser;
use crate::parser::terminalset_expanded::TerminalSetParser;
use crate::pattern::Pattern;
use crate::rule::RuleLine;
use crate::rule::RuleLines;
use crate::terminalset::TerminalSet;
use crate::token::TokenMapped;
use crate::utils;

#[derive(Debug)]
pub struct Grammar {
    /// for bootstrapping the generated code; the prefix for the '::rusty_lr' module
    pub(crate) module_prefix: TokenStream,
    pub(crate) token_typename: TokenStream,
    pub(crate) userdata_typename: Option<TokenStream>,
    pub(crate) start_rule_name: Ident,
    pub(crate) eof: TokenStream,
    // index, typename
    pub(crate) terminals: BTreeMap<Ident, (usize, TokenStream)>,
    // terminals sorted by index, insertion order
    pub(crate) terminals_index: Vec<Ident>,

    pub(crate) reduce_types: BTreeMap<Ident, rusty_lr_core::ReduceType>,
    pub(crate) error_typename: TokenStream,

    //                  name       typename           rules
    pub(crate) rules: BTreeMap<Ident, RuleLines>,
    pub(crate) nonterm_typenames: BTreeMap<Ident, Option<TokenStream>>,

    /// pattern map for auto-generated rules
    pub(crate) pattern_map: BTreeMap<Pattern, Ident>,
}

impl Grammar {
    /// get typename(TokenStream) for given name of terminal or non-terminal symbol
    pub(crate) fn get_typename(&self, ident: &Ident) -> Option<&TokenStream> {
        if self.terminals.contains_key(ident) {
            Some(&self.token_typename)
        } else {
            self.nonterm_typenames
                .get(ident)
                .expect(&format!("get_typename: {}", ident))
                .as_ref()
        }
    }

    /// parse the input TokenStream and return a parsed Grammar
    pub fn parse(input: TokenStream) -> Result<Self, ParseError> {
        let mut lexer = Lexer::new(input);

        let parser = GrammarParser::new();
        let mut context = parser.begin();

        while let Some(lexed) = lexer.next_token() {
            let span = lexed.span().unwrap();
            match parser.feed(&mut context, lexed) {
                Ok(_) => {}
                Err(err) => match err {
                    rusty_lr_core::ParseError::ReduceAction(err) => {
                        return Err(err);
                    }
                    _ => {
                        return Err(ParseError::InternalGrammar(span, format!("{}", err)));
                    }
                },
            }
        }
        match parser.feed(&mut context, Lexed::Eof) {
            Ok(_) => {}
            Err(err) => match err {
                rusty_lr_core::ParseError::ReduceAction(err) => {
                    return Err(err);
                }
                _ => {
                    let span = Span::call_site();
                    return Err(ParseError::InternalGrammar(span, format!("{}", err)));
                }
            },
        }

        let grammar_args = context.accept();

        // token_typename
        if grammar_args.token_typename.is_none() {
            return Err(ParseError::TokenTypeNotDefined);
        }

        // eof
        if grammar_args.eof.is_none() {
            return Err(ParseError::EofNotDefined);
        }

        if grammar_args.start_rule_name.is_none() {
            return Err(ParseError::StartNotDefined);
        }

        let mut grammar = Grammar {
            module_prefix: grammar_args.module_prefix.unwrap_or(quote! { ::rusty_lr}),
            token_typename: grammar_args.token_typename.unwrap(),
            userdata_typename: grammar_args.userdata_typename,
            start_rule_name: grammar_args.start_rule_name.unwrap(),
            eof: grammar_args.eof.unwrap(),
            terminals: BTreeMap::new(),
            terminals_index: Vec::new(),
            reduce_types: BTreeMap::new(),
            error_typename: grammar_args.error_typename.unwrap_or(quote! {String}),
            rules: BTreeMap::new(),
            nonterm_typenames: BTreeMap::new(),
            pattern_map: BTreeMap::new(),
        };

        for (index, (ident, typename)) in grammar_args.terminals.into_iter().enumerate() {
            // check reserved name
            utils::check_reserved_name(&ident)?;

            // check duplicate
            if grammar
                .terminals
                .insert(ident.clone(), (index, typename))
                .is_some()
            {
                return Err(ParseError::MultipleTokenDefinition(ident));
            }
            grammar.terminals_index.push(ident);
        }
        // add eof as terminal

        // add eof
        grammar.terminals.insert(
            Ident::new(utils::EOF_NAME, Span::call_site()),
            (grammar.terminals_index.len(), grammar.eof.clone()),
        );
        grammar
            .terminals_index
            .push(Ident::new(utils::EOF_NAME, Span::call_site()));

        let terminal_set_parser = TerminalSetParser::new();

        // reduce types
        for (terminals, reduce_type) in grammar_args.reduce_types.into_iter() {
            match terminals {
                ReduceTypeArgs::Ident(terminal) => {
                    if !grammar.terminals.contains_key(&terminal) {
                        return Err(ParseError::TerminalNotDefined(terminal));
                    }
                    if let Some(old) = grammar.reduce_types.insert(terminal.clone(), reduce_type) {
                        if old != reduce_type {
                            return Err(ParseError::MultipleReduceDefinition(terminal));
                        }
                    }
                }
                ReduceTypeArgs::TerminalSet(group) => {
                    let terminal_set = TerminalSet::parse(group.stream(), &terminal_set_parser)?;
                    for terminal in terminal_set.to_terminal_set(&grammar)?.into_iter() {
                        if let Some(old) =
                            grammar.reduce_types.insert(terminal.clone(), reduce_type)
                        {
                            if old != reduce_type {
                                return Err(ParseError::MultipleReduceDefinition(terminal));
                            }
                        }
                    }
                }
            }
        }

        // insert rule typenames
        for rules in grammar_args.rules.iter() {
            grammar
                .nonterm_typenames
                .insert(rules.name.clone(), rules.typename.clone());
        }

        // insert rules
        for rules in grammar_args.rules.into_iter() {
            // check reserved name
            utils::check_reserved_name(&rules.name)?;

            let mut rule_lines = Vec::new();
            for rule in rules.rule_lines.into_iter() {
                let mut tokens = Vec::new();
                for (mapto, pattern) in rule.tokens.into_iter() {
                    let pattern = pattern.to_pattern(&terminal_set_parser, &grammar)?;
                    let token_rule = pattern.get_rule(&mut grammar)?;
                    let mapto = mapto.unwrap_or_else(|| pattern.base_ident());

                    tokens.push(TokenMapped {
                        token: token_rule,
                        mapto,
                    });
                }

                rule_lines.push(RuleLine {
                    tokens,
                    reduce_action: rule.reduce_action,
                });
            }

            if grammar
                .rules
                .insert(rules.name.clone(), RuleLines { rule_lines })
                .is_some()
            {
                return Err(ParseError::MultipleRuleDefinition(rules.name));
            }
        }

        // check all token is defined as either terminal or non-terminal
        for (_name, rule_lines) in grammar.rules.iter() {
            for rule in rule_lines.rule_lines.iter() {
                for token in rule.tokens.iter() {
                    let is_terminal = grammar.terminals.contains_key(&token.token);
                    let is_non_terminal = grammar.rules.contains_key(&token.token);
                    if is_terminal && is_non_terminal {
                        return Err(ParseError::TermNonTermConflict(token.token.clone()));
                    }
                    if !is_terminal && !is_non_terminal {
                        return Err(ParseError::TerminalNotDefined(token.token.clone()));
                    }
                }
            }
        }

        // check start rule is valid
        if !grammar.rules.contains_key(&grammar.start_rule_name) {
            return Err(ParseError::NonTerminalNotDefined(
                grammar.start_rule_name.clone(),
            ));
        }

        Ok(grammar)
    }

    /// create the rusty_lr_core::Grammar from the parsed CFGs
    pub fn create_grammar(&self) -> Result<rusty_lr_core::Grammar<String, String>, ParseError> {
        let mut grammar: rusty_lr_core::Grammar<String, String> = rusty_lr_core::Grammar::new();

        // reduce types
        for (term, reduce_type) in self.reduce_types.iter() {
            match grammar.set_reduce_type(term.to_string(), *reduce_type) {
                Ok(_) => {}
                Err(_) => {
                    unreachable!("set_reduce_type error");
                }
            }
        }

        // rules
        for (name, rules) in self.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let mut tokens = Vec::with_capacity(rule.tokens.len());
                for token in rule.tokens.iter() {
                    if self.terminals.contains_key(&token.token) {
                        tokens.push(rusty_lr_core::Token::Term(token.token.to_string()));
                    } else if self.rules.contains_key(&token.token) {
                        tokens.push(rusty_lr_core::Token::NonTerm(token.token.to_string()));
                    }
                }

                grammar.add_rule(name.to_string(), tokens);
            }
        }

        // augmented rule
        grammar.add_rule(
            utils::AUGMENTED_NAME.to_string(),
            vec![
                rusty_lr_core::Token::NonTerm(self.start_rule_name.to_string()),
                rusty_lr_core::Token::Term("eof".to_string()),
            ],
        );

        Ok(grammar)
    }
}
