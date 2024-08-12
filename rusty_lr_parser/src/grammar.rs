use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;

use std::collections::BTreeMap;

use crate::error::ParseError;
use crate::parser::args::ReduceTypeArgs;
use crate::parser::lexer::Lexed;
use crate::parser::parser_expanded::GrammarParser;
use crate::pattern::Pattern;
use crate::rule::RuleLine;
use crate::rule::RuleLines;
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
    pub rules: BTreeMap<Ident, RuleLines>,
    pub(crate) nonterm_typenames: BTreeMap<Ident, Option<TokenStream>>,

    /// pattern map for auto-generated rules
    pub(crate) pattern_map: BTreeMap<Pattern, Ident>,
    /// span range that generated rule was originated from
    pub generated_root_span: BTreeMap<Ident, (Span, Span)>,
}

impl Grammar {
    /// get typename(TokenStream) for given name of terminal or non-terminal symbol
    pub(crate) fn get_typename(&self, ident: &Ident) -> Option<&TokenStream> {
        if self.terminals.contains_key(ident) {
            Some(&self.token_typename)
        } else {
            self.nonterm_typenames
                .get(ident)
                .unwrap_or_else(|| panic!("get_typename: {}", ident))
                .as_ref()
        }
    }

    /// get rule by ruleid
    pub fn get_rule_by_id(&self, mut ruleid: usize) -> Option<(&Ident, &RuleLines, usize)> {
        for (name, rules) in self.rules.iter() {
            if ruleid < rules.rule_lines.len() {
                return Some((name, rules, ruleid));
            }
            ruleid -= rules.rule_lines.len();
        }
        None
    }

    /// parse the input TokenStream and return a parsed Grammar
    pub fn parse(input: TokenStream) -> Result<Self, ParseError> {
        let parser = GrammarParser::new();
        let mut context = parser.begin();

        match crate::parser::lexer::feed_recursive(input, &parser, &mut context) {
            Ok(_) => {}
            Err((span, err)) => match err {
                _ => {
                    let message = err.short_message();
                    return Err(ParseError::MacroLineParse { span, message });
                }
            },
        }
        match parser.feed(&mut context, Lexed::Eof) {
            Ok(_) => {}
            Err(err) => match err {
                _ => {
                    let message = err.short_message();
                    return Err(ParseError::MacroLineParseEnd { message });
                }
            },
        }

        let grammar_args = context.accept();

        // %error
        let error_typename = if grammar_args.error_typename.len() > 1 {
            return Err(ParseError::MultipleErrorDefinition(
                grammar_args.error_typename[0].clone(),
                grammar_args.error_typename[1].clone(),
            ));
        } else if grammar_args.error_typename.is_empty() {
            quote! { String }
        } else {
            grammar_args.error_typename[0].1.clone()
        };

        // %moduleprefix
        let module_prefix = if grammar_args.module_prefix.len() > 1 {
            return Err(ParseError::MultipleUserDataDefinition(
                grammar_args.module_prefix[0].clone(),
                grammar_args.module_prefix[1].clone(),
            ));
        } else if grammar_args.module_prefix.is_empty() {
            quote! { ::rusty_lr }
        } else {
            grammar_args.module_prefix[0].1.clone()
        };

        // %userdata
        let userdata = if grammar_args.userdata_typename.len() > 1 {
            return Err(ParseError::MultipleUserDataDefinition(
                grammar_args.userdata_typename[0].clone(),
                grammar_args.userdata_typename[1].clone(),
            ));
        } else if grammar_args.userdata_typename.is_empty() {
            None
        } else {
            Some(grammar_args.userdata_typename[0].1.clone())
        };

        // %tokentype
        let tokentype = if grammar_args.token_typename.is_empty() {
            return Err(ParseError::TokenTypeNotDefined);
        } else if grammar_args.token_typename.len() > 1 {
            return Err(ParseError::MultipleTokenTypeDefinition(
                grammar_args.token_typename[0].clone(),
                grammar_args.token_typename[1].clone(),
            ));
        } else {
            grammar_args.token_typename[0].1.clone()
        };

        // %eof
        let eof = if grammar_args.eof.is_empty() {
            return Err(ParseError::EofNotDefined);
        } else if grammar_args.eof.len() > 1 {
            return Err(ParseError::MultipleEofDefinition(
                grammar_args.eof[0].clone(),
                grammar_args.eof[1].clone(),
            ));
        } else {
            grammar_args.eof[0].1.clone()
        };

        // %start
        let start = if grammar_args.start_rule_name.is_empty() {
            return Err(ParseError::StartNotDefined);
        } else if grammar_args.start_rule_name.len() > 1 {
            return Err(ParseError::MultipleStartDefinition(
                grammar_args.start_rule_name[0].clone(),
                grammar_args.start_rule_name[1].clone(),
            ));
        } else {
            grammar_args.start_rule_name[0].clone()
        };

        let mut grammar = Grammar {
            module_prefix: module_prefix,
            token_typename: tokentype,
            userdata_typename: userdata,
            start_rule_name: start,
            eof,
            error_typename,
            terminals: BTreeMap::new(),
            terminals_index: Vec::new(),
            reduce_types: BTreeMap::new(),
            rules: BTreeMap::new(),
            nonterm_typenames: BTreeMap::new(),
            pattern_map: BTreeMap::new(),
            generated_root_span: BTreeMap::new(),
        };

        for (index, (ident, typename)) in grammar_args.terminals.into_iter().enumerate() {
            // check reserved name
            utils::check_reserved_name(&ident)?;

            // check duplicate
            if let Some((k, _)) = grammar.terminals.get_key_value(&ident) {
                return Err(ParseError::MultipleTokenDefinition(k.clone(), ident));
            }
            grammar.terminals.insert(ident.clone(), (index, typename));
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
                ReduceTypeArgs::TerminalSet(terminal_set) => {
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

            if let Some((old, _)) = grammar.rules.get_key_value(&rules.name) {
                return Err(ParseError::MultipleRuleDefinition(
                    old.clone(),
                    rules.name.clone(),
                ));
            }

            let mut rule_lines = Vec::new();
            for rule in rules.rule_lines.into_iter() {
                let mut tokens = Vec::new();
                for (mapto, pattern) in rule.tokens.into_iter() {
                    let (begin_span, end_span) = pattern.span_pair();
                    let pattern = pattern.to_pattern(&grammar)?;
                    let token_rule = pattern.get_rule(&mut grammar, (begin_span, end_span))?;
                    let mapto = mapto.unwrap_or_else(|| pattern.base_ident());

                    tokens.push(TokenMapped {
                        token: token_rule,
                        mapto,
                        begin_span,
                        end_span,
                    });
                }

                rule_lines.push(RuleLine {
                    tokens,
                    reduce_action: rule.reduce_action,
                    separator_span: rule.separator_span,
                });
            }

            grammar
                .rules
                .insert(rules.name.clone(), RuleLines { rule_lines });
        }

        // check all token is defined as one of terminal or non-terminal
        for (_name, rule_lines) in grammar.rules.iter() {
            for rule in rule_lines.rule_lines.iter() {
                for token in rule.tokens.iter() {
                    let is_terminal = grammar.terminals.get_key_value(&token.token);
                    let is_non_terminal = grammar.rules.get_key_value(&token.token);
                    if is_terminal.is_some() && is_non_terminal.is_some() {
                        return Err(ParseError::TermNonTermConflict {
                            name: token.token.clone(),
                            terminal: is_terminal.unwrap().0.clone(),
                            non_terminal: is_non_terminal.unwrap().0.clone(),
                        });
                    }
                    if is_terminal.is_none() && is_non_terminal.is_none() {
                        return Err(ParseError::TerminalNotDefined(token.token.clone()));
                    }
                }
            }
        }

        // check start rule is valid
        if !grammar.rules.contains_key(&grammar.start_rule_name) {
            return Err(ParseError::StartNonTerminalNotDefined(
                grammar.start_rule_name.clone(),
            ));
        }

        Ok(grammar)
    }

    /// create the rusty_lr_core::Grammar from the parsed CFGs
    pub fn create_grammar(&self) -> Result<rusty_lr_core::Grammar<Ident, Ident>, ParseError> {
        let mut grammar: rusty_lr_core::Grammar<Ident, Ident> = rusty_lr_core::Grammar::new();

        // reduce types
        for (term, reduce_type) in self.reduce_types.iter() {
            match grammar.set_reduce_type(term.clone(), *reduce_type) {
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
                        tokens.push(rusty_lr_core::Token::Term(token.token.clone()));
                    } else if self.rules.contains_key(&token.token) {
                        tokens.push(rusty_lr_core::Token::NonTerm(token.token.clone()));
                    } else {
                        unreachable!("create_grammar: token not found");
                    }
                }

                grammar.add_rule(name.clone(), tokens);
            }
        }

        // augmented rule
        grammar.add_rule(
            Ident::new(utils::AUGMENTED_NAME, Span::call_site()),
            vec![
                rusty_lr_core::Token::NonTerm(self.start_rule_name.clone()),
                rusty_lr_core::Token::Term(Ident::new(utils::EOF_NAME, Span::call_site())),
            ],
        );

        Ok(grammar)
    }
}
