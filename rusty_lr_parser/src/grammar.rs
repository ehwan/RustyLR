use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;

use crate::error::ArgError;
use crate::error::ParseArgError;
use crate::error::ParseError;
use crate::parser::args::GrammarArgs;
use crate::parser::lexer::Lexed;
use crate::parser::parser_expanded::GrammarParseError;
use crate::parser::parser_expanded::GrammarParser;
use crate::pattern::Pattern;
use crate::rule::RuleLine;
use crate::rule::RuleLines;
use crate::token::TokenMapped;
use crate::utils;

use rusty_lr_core::HashMap;

#[derive(Debug)]
pub struct Grammar {
    /// for bootstrapping the generated code; the prefix for the '::rusty_lr' module
    pub(crate) module_prefix: TokenStream,
    pub(crate) token_typename: TokenStream,
    pub(crate) userdata_typename: Option<TokenStream>,
    pub(crate) start_rule_name: Ident,
    pub(crate) eof: TokenStream,
    // index, typename
    pub(crate) terminals: HashMap<Ident, (usize, TokenStream)>,
    /// terminals sorted by index, insertion order
    pub(crate) terminals_index: Vec<Ident>,

    /// %left or %right for terminals
    pub reduce_types: HashMap<Ident, rusty_lr_core::ReduceType>,

    /// setted reduce types originated from
    pub reduce_types_origin: HashMap<Ident, (Span, Span)>,
    /// %err
    pub(crate) error_typename: TokenStream,

    /// rule definitions
    pub rules: HashMap<Ident, RuleLines>,
    /// rule typenames
    pub(crate) nonterm_typenames: HashMap<Ident, TokenStream>,
    /// rules sorted by index, insertion order
    pub(crate) rules_index: Vec<Ident>,

    /// pattern map for auto-generated rules
    pub(crate) pattern_map: HashMap<Pattern, Ident>,
    /// span range that generated rule was originated from
    pub generated_root_span: HashMap<Ident, (Span, Span)>,

    /// whether to generate GLR parser
    pub(crate) glr: bool,
}

impl Grammar {
    /// get typename(TokenStream) for given name of terminal or non-terminal symbol
    pub(crate) fn get_typename(&self, ident: &Ident) -> Option<&TokenStream> {
        if self.terminals.contains_key(ident) {
            Some(&self.token_typename)
        } else {
            self.nonterm_typenames.get(ident)
        }
    }

    /// get rule by ruleid
    pub fn get_rule_by_id(&self, mut ruleid: usize) -> Option<(&Ident, &RuleLines, usize)> {
        for name in self.rules_index.iter() {
            // rule_name is same as name, but can have different Span
            let (rule_name, rules) = self.rules.get_key_value(name).unwrap();
            if ruleid < rules.rule_lines.len() {
                return Some((rule_name, rules, ruleid));
            }
            ruleid -= rules.rule_lines.len();
        }
        None
    }

    pub fn parse_args(input: TokenStream) -> Result<GrammarArgs, ParseArgError> {
        let parser = GrammarParser::new();
        let mut context = parser.begin();

        let mut grammar_args = GrammarArgs::default();

        match crate::parser::lexer::feed_recursive(input, &parser, &mut context, &mut grammar_args)
        {
            Ok(_) => {}
            Err(err) => {
                let message = err.to_string();
                let span = match err {
                    GrammarParseError::InvalidTerminal(term) => term.term.span(),
                    _ => unreachable!("feed error"),
                };
                return Err(ParseArgError::MacroLineParse { span, message });
            }
        }
        match parser.feed(&mut context, Lexed::Eof, &mut grammar_args) {
            Ok(_) => {}
            Err(err) => {
                let message = err.to_string();
                return Err(ParseArgError::MacroLineParseEnd { message });
            }
        }

        Ok(grammar_args)
    }
    pub fn arg_check_error(grammar_args: &GrammarArgs) -> Result<(), ArgError> {
        // %error
        if grammar_args.error_typename.len() > 1 {
            return Err(ArgError::MultipleErrorDefinition(
                grammar_args.error_typename[0].clone(),
                grammar_args.error_typename[1].clone(),
            ));
        }

        // %moduleprefix
        if grammar_args.module_prefix.len() > 1 {
            return Err(ArgError::MultipleUserDataDefinition(
                grammar_args.module_prefix[0].clone(),
                grammar_args.module_prefix[1].clone(),
            ));
        }

        // %userdata
        if grammar_args.userdata_typename.len() > 1 {
            return Err(ArgError::MultipleUserDataDefinition(
                grammar_args.userdata_typename[0].clone(),
                grammar_args.userdata_typename[1].clone(),
            ));
        }

        // %tokentype
        if grammar_args.token_typename.is_empty() {
            return Err(ArgError::TokenTypeNotDefined);
        } else if grammar_args.token_typename.len() > 1 {
            return Err(ArgError::MultipleTokenTypeDefinition(
                grammar_args.token_typename[0].clone(),
                grammar_args.token_typename[1].clone(),
            ));
        }

        // %eof
        if grammar_args.eof.is_empty() {
            return Err(ArgError::EofNotDefined);
        } else if grammar_args.eof.len() > 1 {
            return Err(ArgError::MultipleEofDefinition(
                grammar_args.eof[0].clone(),
                grammar_args.eof[1].clone(),
            ));
        }

        // %start
        if grammar_args.start_rule_name.is_empty() {
            return Err(ArgError::StartNotDefined);
        } else if grammar_args.start_rule_name.len() > 1 {
            return Err(ArgError::MultipleStartDefinition(
                grammar_args.start_rule_name[0].clone(),
                grammar_args.start_rule_name[1].clone(),
            ));
        }
        Ok(())
    }

    /// parse the input TokenStream and return a parsed Grammar
    pub fn from_grammar_args(grammar_args: GrammarArgs) -> Result<Self, ParseError> {
        let module_prefix =
            if let Some(module_prefix) = grammar_args.module_prefix.into_iter().next() {
                module_prefix.1
            } else {
                quote! { ::rusty_lr }
            };
        let error_typename =
            if let Some(error_typename) = grammar_args.error_typename.into_iter().next() {
                error_typename.1
            } else {
                quote! { #module_prefix::DefaultReduceActionError }
            };
        let mut grammar = Grammar {
            module_prefix,
            token_typename: grammar_args.token_typename.into_iter().next().unwrap().1,
            userdata_typename: grammar_args
                .userdata_typename
                .into_iter()
                .next()
                .map(|(_, stream)| stream),
            start_rule_name: grammar_args.start_rule_name.into_iter().next().unwrap(),
            eof: grammar_args.eof.into_iter().next().unwrap().1,
            error_typename,
            terminals: Default::default(),
            terminals_index: Default::default(),
            reduce_types: Default::default(),
            reduce_types_origin: Default::default(),
            rules: Default::default(),
            nonterm_typenames: Default::default(),
            rules_index: Default::default(),
            pattern_map: Default::default(),
            generated_root_span: Default::default(),
            glr: grammar_args.glr,
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
            let new_span = terminals.span_pair();
            for terminal in terminals.to_terminal_set(&grammar)?.into_iter() {
                if let Some(old) = grammar.reduce_types.insert(terminal.clone(), reduce_type) {
                    let old_span = grammar.reduce_types_origin.get(&terminal).unwrap();
                    return Err(ParseError::MultipleReduceDefinition {
                        terminal,
                        old: (old_span.0, old_span.1, old),
                        new: (new_span.0, new_span.1, reduce_type),
                    });
                }
                grammar.reduce_types_origin.insert(terminal, new_span);
            }
        }

        // insert rule typenames first, since it will be used when inserting rule definitions below
        for rules in grammar_args.rules.iter() {
            // check reserved name
            utils::check_reserved_name(&rules.name)?;

            if let Some(typename) = rules.typename.as_ref() {
                grammar
                    .nonterm_typenames
                    .insert(rules.name.clone(), typename.clone());
            }
        }

        // insert rules
        for rules in grammar_args.rules.into_iter() {
            if let Some((old, _)) = grammar.rules.get_key_value(&rules.name) {
                return Err(ParseError::MultipleRuleDefinition(
                    old.clone(),
                    rules.name.clone(),
                ));
            }
            grammar.rules_index.push(rules.name.clone());

            let mut rule_lines = Vec::new();
            for rule in rules.rule_lines.into_iter() {
                let mut tokens = Vec::new();
                for (mapto, pattern) in rule.tokens.into_iter() {
                    let (begin_span, end_span) = pattern.span_pair();
                    let pattern = pattern.into_pattern(&grammar, false)?;
                    let token_rule = pattern.get_rule(&mut grammar, (begin_span, end_span))?;
                    let mapto = match pattern.typename(&grammar) {
                        Some((_, mapto_)) => mapto.or(Some(mapto_)),
                        None => None,
                    };

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
                    lookaheads: None,
                    id: rule.id,
                });
            }

            grammar
                .rules
                .insert(rules.name.clone(), RuleLines { rule_lines });
        }
        // insert augmented rule
        grammar
            .rules_index
            .push(Ident::new(utils::AUGMENTED_NAME, Span::call_site()));
        grammar.rules.insert(
            Ident::new(utils::AUGMENTED_NAME, Span::call_site()),
            RuleLines {
                rule_lines: vec![RuleLine {
                    tokens: vec![
                        TokenMapped {
                            token: grammar.start_rule_name.clone(),
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        },
                        TokenMapped {
                            token: Ident::new(utils::EOF_NAME, Span::call_site()),
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        },
                    ],
                    reduce_action: None,
                    separator_span: Span::call_site(),
                    lookaheads: None,
                    id: 0,
                }],
            },
        );

        // check all token is defined as one of terminal or non-terminal
        for (_name, rule_lines) in grammar.rules.iter() {
            for rule in rule_lines.rule_lines.iter() {
                for token in rule.tokens.iter() {
                    let is_terminal = grammar.terminals.get_key_value(&token.token);
                    let is_non_terminal = grammar.rules.get_key_value(&token.token);
                    match (is_terminal, is_non_terminal) {
                        (Some(term), Some(nonterm)) => {
                            return Err(ParseError::TermNonTermConflict {
                                name: token.token.clone(),
                                terminal: term.0.clone(),
                                non_terminal: nonterm.0.clone(),
                            });
                        }
                        (None, None) => {
                            return Err(ParseError::TerminalNotDefined(token.token.clone()));
                        }
                        _ => {}
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

    /// for backward compatibility
    pub fn parse(input: TokenStream) -> Result<Self, TokenStream> {
        let grammar_args = match Grammar::parse_args(input) {
            Ok(grammar_args) => grammar_args,
            Err(e) => return Err(e.to_compile_error()),
        };
        match Grammar::arg_check_error(&grammar_args) {
            Ok(_) => {}
            Err(e) => return Err(e.to_compile_error()),
        }
        match Grammar::from_grammar_args(grammar_args) {
            Ok(grammar) => Ok(grammar),
            Err(e) => Err(e.to_compile_error()),
        }
    }

    /// create the rusty_lr_core::Grammar from the parsed CFGs
    pub fn create_grammar(&self) -> rusty_lr_core::builder::Grammar<Ident, Ident> {
        let mut grammar: rusty_lr_core::builder::Grammar<Ident, Ident> =
            rusty_lr_core::builder::Grammar::new();
        if self.glr {
            grammar.allow_conflict();
        }

        // reduce types
        for (term, reduce_type) in self.reduce_types.iter() {
            if !grammar.set_reduce_type(term.clone(), *reduce_type) {
                unreachable!("set_reduce_type error");
            }
        }

        // add rules
        for name in self.rules_index.iter() {
            let rule_lines = self.rules.get(name).unwrap();

            for rule in rule_lines.rule_lines.iter() {
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

                if let Some(lookaheads) = rule.lookaheads.as_ref() {
                    grammar.add_rule_with_lookaheads(
                        name.clone(),
                        tokens,
                        rule.id,
                        lookaheads.clone(),
                    );
                } else {
                    grammar.add_rule(name.clone(), tokens, rule.id);
                }
            }
        }

        grammar
    }
}
