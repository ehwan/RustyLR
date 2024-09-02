use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use rusty_lr_core::Token;

use crate::error::ArgError;
use crate::error::ParseArgError;
use crate::error::ParseError;
use crate::nonterminal_info::NonTerminalInfo;
use crate::nonterminal_info::Rule;
use crate::parser::args::GrammarArgs;
use crate::parser::lexer::Lexed;
use crate::parser::parser_expanded::GrammarParseError;
use crate::parser::parser_expanded::GrammarParser;
use crate::pattern::Pattern;
use crate::pattern::PatternResult;
use crate::terminal_info::ReduceTypeInfo;
use crate::terminal_info::TerminalInfo;
use crate::token::TokenMapped;
use crate::utils;

use rusty_lr_core::HashMap;

use std::collections::BTreeSet;

pub struct Grammar {
    /// %moduleprefix
    pub(crate) module_prefix: TokenStream,

    /// %tokentype
    pub(crate) token_typename: TokenStream,

    /// %userdata
    pub(crate) userdata_typename: Option<TokenStream>,

    /// %err
    pub(crate) error_typename: TokenStream,

    /// %start
    pub(crate) start_rule_name: Ident,

    pub terminals: Vec<TerminalInfo>,
    /// ident -> index map for terminals
    pub terminals_index: HashMap<Ident, usize>,

    /// rule definitions
    pub nonterminals: Vec<NonTerminalInfo>,
    /// ident - index map for non-terminals
    pub nonterminals_index: HashMap<Ident, usize>,

    /// whether to generate GLR parser
    pub(crate) glr: bool,
}

impl Grammar {
    /// get rule by ruleid
    pub fn get_rule_by_id(&self, mut ruleid: usize) -> Option<(&NonTerminalInfo, usize)> {
        for nonterm in self.nonterminals.iter() {
            // rule_name is same as name, but can have different Span
            if ruleid < nonterm.rules.len() {
                return Some((nonterm, ruleid));
            }
            ruleid -= nonterm.rules.len();
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
                let expected: BTreeSet<String> = context
                    .expected(&parser)
                    .map(|term| term.to_string())
                    .collect();

                let mut message = err.to_string();
                for (idx, expected) in expected.into_iter().enumerate() {
                    if idx == 0 {
                        message.push_str("\nExpected: ");
                    } else {
                        message.push_str(", ");
                    }
                    message.push_str(&expected);
                }
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
                let expected: BTreeSet<String> = context
                    .expected(&parser)
                    .map(|term| term.to_string())
                    .collect();

                let mut message = err.to_string();
                for (idx, expected) in expected.into_iter().enumerate() {
                    if idx == 0 {
                        message.push_str("\nExpected: ");
                    } else {
                        message.push_str(", ");
                    }
                    message.push_str(&expected);
                }
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
            error_typename,
            start_rule_name: grammar_args.start_rule_name.into_iter().next().unwrap(),

            terminals: Default::default(),
            terminals_index: Default::default(),

            nonterminals: Default::default(),
            nonterminals_index: Default::default(),

            glr: grammar_args.glr,
        };

        // add terminals
        for (index, (ident, token_expr)) in grammar_args.terminals.into_iter().enumerate() {
            // check reserved name
            utils::check_reserved_name(&ident)?;

            // check duplicate
            if let Some((k, _)) = grammar.terminals_index.get_key_value(&ident) {
                return Err(ParseError::MultipleTokenDefinition(k.clone(), ident));
            }

            let terminal_info = TerminalInfo {
                name: ident.clone(),
                reduce_type: None,
                body: token_expr,
            };
            grammar.terminals.push(terminal_info);
            grammar.terminals_index.insert(ident, index);
        }
        // add eof
        {
            let eof_ident = Ident::new(utils::EOF_NAME, Span::call_site());

            let terminal_info = TerminalInfo {
                name: eof_ident.clone(),
                reduce_type: None,
                body: grammar_args.eof.into_iter().next().unwrap().1,
            };
            let idx = grammar.terminals.len();
            grammar.terminals.push(terminal_info);
            grammar.terminals_index.insert(eof_ident, idx);
        }

        // reduce types
        for (terminals, reduce_type) in grammar_args.reduce_types.into_iter() {
            let new_span = terminals.span_pair();
            for term_idx in terminals.to_terminal_set(&grammar)?.into_iter() {
                let terminal_name = grammar.terminals[term_idx].name.clone();
                if let Some(old) = &mut grammar.terminals[term_idx].reduce_type {
                    if old.reduce_type != reduce_type {
                        return Err(ParseError::MultipleReduceDefinition {
                            terminal: terminal_name,
                            old: (old.sources[0].0, old.sources[0].1, old.reduce_type),
                            new: (new_span.0, new_span.1, reduce_type),
                        });
                    } else {
                        old.sources.push(new_span);
                    }
                } else {
                    grammar.terminals[term_idx].reduce_type = Some(ReduceTypeInfo {
                        reduce_type,
                        sources: vec![new_span],
                    });
                }
            }
        }

        // insert rule typenames first, since it will be used when inserting rule definitions below
        for (rule_idx, rules_arg) in grammar_args.rules.iter().enumerate() {
            // check reserved name
            utils::check_reserved_name(&rules_arg.name)?;

            let nonterminal = NonTerminalInfo {
                name: rules_arg.name.clone(),
                pretty_name: rules_arg.name.to_string(),
                ruletype: rules_arg.typename.clone(),
                rules: Vec::new(), // production rules will be added later
                regex_span: None,
            };

            grammar.nonterminals.push(nonterminal);

            // check duplicate
            if let Some(old) = grammar
                .nonterminals_index
                .insert(rules_arg.name.clone(), rule_idx)
            {
                return Err(ParseError::MultipleRuleDefinition(
                    grammar.nonterminals[old].name.clone(),
                    rules_arg.name.clone(),
                ));
            }
        }

        // pattern map for auto-generated rules
        let mut pattern_map: HashMap<Pattern, PatternResult> = HashMap::default();

        // insert production rules & auto-generated rules from regex pattern
        for (rule_idx, rules) in grammar_args.rules.into_iter().enumerate() {
            let mut rule_lines = Vec::new();
            for rule in rules.rule_lines.into_iter() {
                let mut tokens = Vec::with_capacity(rule.tokens.len());
                for (mapto, pattern) in rule.tokens.into_iter() {
                    let (begin_span, end_span) = pattern.span_pair();
                    let pattern = pattern.into_pattern(&grammar, false)?;
                    let pattern_rule =
                        pattern.to_rule(&mut grammar, &mut pattern_map, (begin_span, end_span))?;

                    let mapto = match &pattern_rule.ruletype_map {
                        Some((_, mapto_)) => mapto.or(Some(mapto_.clone())),
                        None => None,
                    };

                    tokens.push(TokenMapped {
                        token: pattern_rule.token,
                        mapto,
                        begin_span,
                        end_span,
                    });
                }

                rule_lines.push(Rule {
                    tokens,
                    reduce_action: rule.reduce_action,
                    separator_span: rule.separator_span,
                    lookaheads: None,
                    id: rule.id,
                });
            }

            // production rules set here
            grammar.nonterminals[rule_idx].rules = rule_lines;
        }
        drop(pattern_map);

        // check start rule is valid
        if !grammar
            .nonterminals_index
            .contains_key(&grammar.start_rule_name)
        {
            return Err(ParseError::StartNonTerminalNotDefined(
                grammar.start_rule_name.clone(),
            ));
        }

        // insert augmented rule
        {
            let augmented_ident = Ident::new(utils::AUGMENTED_NAME, Span::call_site());
            let start_idx = grammar
                .nonterminals_index
                .get(&grammar.start_rule_name)
                .unwrap();
            let eof_idx = grammar
                .terminals_index
                .get(&Ident::new(utils::EOF_NAME, Span::call_site()))
                .unwrap();
            let augmented_rule = Rule {
                tokens: vec![
                    TokenMapped {
                        token: Token::NonTerm(*start_idx),
                        mapto: None,
                        begin_span: Span::call_site(),
                        end_span: Span::call_site(),
                    },
                    TokenMapped {
                        token: Token::Term(*eof_idx),
                        mapto: None,
                        begin_span: Span::call_site(),
                        end_span: Span::call_site(),
                    },
                ],
                reduce_action: None,
                separator_span: Span::call_site(),
                lookaheads: None,
                id: 0,
            };
            let nonterminal_info = NonTerminalInfo {
                name: augmented_ident.clone(),
                pretty_name: utils::AUGMENTED_NAME.to_string(),
                ruletype: None,
                regex_span: None,
                rules: vec![augmented_rule],
            };

            let augmented_idx = grammar.nonterminals.len();
            grammar.nonterminals.push(nonterminal_info);
            grammar
                .nonterminals_index
                .insert(augmented_ident, augmented_idx);
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
    pub fn create_grammar(&self) -> rusty_lr_core::builder::Grammar<usize, usize> {
        let mut grammar: rusty_lr_core::builder::Grammar<usize, usize> =
            rusty_lr_core::builder::Grammar::new();
        if self.glr {
            grammar.allow_conflict();
        }

        // reduce types
        for (idx, term_info) in self.terminals.iter().enumerate() {
            if let Some(reduce_type) = &term_info.reduce_type {
                if !grammar.set_reduce_type(idx, reduce_type.reduce_type) {
                    unreachable!("set_reduce_type error");
                }
            }
        }

        // add rules
        for (idx, nonterminal) in self.nonterminals.iter().enumerate() {
            for rule in nonterminal.rules.iter() {
                let tokens = rule
                    .tokens
                    .iter()
                    .map(|token_mapped| token_mapped.token)
                    .collect();
                if let Some(lookaheads) = rule.lookaheads.as_ref() {
                    grammar.add_rule_with_lookaheads(idx, tokens, rule.id, lookaheads.clone());
                } else {
                    grammar.add_rule(idx, tokens, rule.id);
                }
            }
        }

        grammar
    }
}
