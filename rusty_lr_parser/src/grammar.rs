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
use crate::parser::args::IdentOrLiteral;
use crate::parser::lexer::Lexed;
use crate::parser::parser_expanded::GrammarContext;
use crate::parser::parser_expanded::GrammarParseError;
use crate::parser::parser_expanded::GrammarParser;
use crate::pattern::Pattern;
use crate::pattern::PatternResult;
use crate::terminal_info::ReduceTypeInfo;
use crate::terminal_info::TerminalInfo;
use crate::token::TokenMapped;
use crate::utils;

use rusty_lr_core::HashMap;

pub struct PrecDefinition {
    pub ident: Ident,
    pub reduce_type: Option<ReduceTypeInfo>,
}

pub struct Grammar {
    /// %moduleprefix
    pub(crate) module_prefix: TokenStream,

    /// %tokentype
    pub(crate) token_typename: TokenStream,

    /// %userdata
    pub(crate) userdata_typename: TokenStream,

    /// %err
    pub(crate) error_typename: TokenStream,

    /// %start
    pub(crate) start_rule_name: Ident,

    pub terminals: Vec<TerminalInfo>,
    /// ident -> index map for terminals
    pub terminals_index: HashMap<Ident, usize>,

    /// single literal character -> terminal index map
    pub literal_index: HashMap<char, usize>,

    /// precedence orders
    pub precedences: HashMap<rusty_lr_core::builder::Operator<usize>, (Span, usize)>,

    /// %prec definitions
    pub prec_defeinitions: Vec<PrecDefinition>,

    /// rule definitions
    pub nonterminals: Vec<NonTerminalInfo>,
    /// ident - index map for non-terminals
    pub nonterminals_index: HashMap<Ident, usize>,

    /// whether to generate LALR parser
    pub lalr: bool,

    /// whether to generate GLR parser
    pub glr: bool,

    /// if %tokentype is `char` or `u8`
    pub is_char: bool,
    pub is_u8: bool,
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

    pub(crate) fn find_prec_definition(&self, ident: &Ident) -> Option<usize> {
        for (idx, prec) in self.prec_defeinitions.iter().enumerate() {
            if ident == &prec.ident {
                return Some(idx);
            }
        }
        None
    }

    /// returns None if literal is not supported
    pub(crate) fn add_or_get_literal_character(
        &mut self,
        literal: syn::Lit,
        reduce_type: Option<ReduceTypeInfo>,
    ) -> Option<usize> {
        let value = match &literal {
            syn::Lit::Char(lit) => lit.value(),
            syn::Lit::Byte(lit) => lit.value() as char,
            _ => return None,
        };
        if let Some(idx) = self.literal_index.get(&value).copied() {
            return Some(idx);
        } else {
            let new_idx = self.terminals.len();
            let name = Ident::new(&format!("_Literal{}", new_idx), Span::call_site());
            let info = TerminalInfo {
                name: name.clone(),
                reduce_type,
                body: quote! { #literal },
            };
            self.terminals.push(info);
            self.terminals_index.insert(name, new_idx);
            self.literal_index.insert(value, new_idx);

            Some(new_idx)
        }
    }

    pub fn parse_args(input: TokenStream) -> Result<GrammarArgs, ParseArgError> {
        let parser = GrammarParser::new();
        let mut context = GrammarContext::new();

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
        match context.feed(&parser, Lexed::Eof, &mut grammar_args) {
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
                .map(|(_, stream)| stream)
                .unwrap_or(quote! { () }),

            error_typename,
            start_rule_name: grammar_args.start_rule_name.into_iter().next().unwrap(),

            terminals: Default::default(),
            terminals_index: Default::default(),
            literal_index: Default::default(),
            precedences: Default::default(),
            prec_defeinitions: Default::default(),

            nonterminals: Default::default(),
            nonterminals_index: Default::default(),

            lalr: grammar_args.lalr,
            glr: grammar_args.glr,

            is_char: false,
            is_u8: false,
        };
        grammar.is_char = grammar.token_typename.to_string() == "char";
        grammar.is_u8 = grammar.token_typename.to_string() == "u8";

        // add terminals
        for (index, (ident, token_expr)) in grammar_args.terminals.into_iter().enumerate() {
            // check if %tokentype is `char` or `u8`
            if grammar.is_char || grammar.is_u8 {
                return Err(ParseError::TokenInLiteralMode(ident.span()));
            }

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
        // add terminals from %prec definition in each rule
        for rules_arg in grammar_args.rules.iter() {
            for rule in rules_arg.rule_lines.iter() {
                if let Some(ref prec_ident) = rule.precedence {
                    if prec_ident.to_terminal(&mut grammar).is_err() {
                        // no terminal is defined;
                        // define new terminal just for operator

                        // is_err() is true if Literal is invalid
                        if let IdentOrLiteral::Ident(prec_ident) = prec_ident {
                            // check reserved name
                            utils::check_reserved_name(prec_ident)?;

                            // add to prec definition
                            grammar.prec_defeinitions.push(PrecDefinition {
                                ident: prec_ident.clone(),
                                reduce_type: None,
                            });
                        }
                    }
                }
            }
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
        for (reduce_type, terminals_arg) in grammar_args.reduce_types.into_iter() {
            for terminal_arg in terminals_arg {
                let span = terminal_arg.span();
                let terminal = terminal_arg.to_terminal(&mut grammar)?;

                match terminal {
                    rusty_lr_core::builder::Operator::Term(term_idx) => {
                        let terminal_name = grammar.terminals[term_idx].name.clone();
                        if let Some(old) = &mut grammar.terminals[term_idx].reduce_type {
                            if old.reduce_type != reduce_type {
                                return Err(ParseError::MultipleReduceDefinition {
                                    terminal: terminal_name,
                                    old: (old.source, old.reduce_type),
                                    new: (span, reduce_type),
                                });
                            } else {
                                old.source = span;
                            }
                        } else {
                            grammar.terminals[term_idx].reduce_type = Some(ReduceTypeInfo {
                                reduce_type,
                                source: span,
                            });
                        }
                    }
                    rusty_lr_core::builder::Operator::Prec(prec) => {
                        let prec_ident = grammar.prec_defeinitions[prec].ident.clone();
                        if let Some(old) = &mut grammar.prec_defeinitions[prec].reduce_type {
                            if old.reduce_type != reduce_type {
                                return Err(ParseError::MultipleReduceDefinition {
                                    terminal: prec_ident,
                                    old: (old.source, old.reduce_type),
                                    new: (span, reduce_type),
                                });
                            } else {
                                old.source = span;
                            }
                        } else {
                            grammar.prec_defeinitions[prec].reduce_type = Some(ReduceTypeInfo {
                                reduce_type,
                                source: span,
                            });
                        }
                    }
                }
            }
        }

        // precedence orders
        for (idx, orders) in grammar_args.precedences.into_iter().enumerate() {
            for term_arg in orders {
                let span = term_arg.span();
                let terminal = term_arg.to_terminal(&mut grammar)?;
                if let Some((old_span, old_idx)) = grammar.precedences.insert(terminal, (span, idx))
                {
                    if old_idx != idx {
                        return Err(ParseError::MultiplePrecedenceOrderDefinition {
                            cur: term_arg,
                            old: old_span,
                        });
                    }
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
                    let pattern = pattern.into_pattern(&mut grammar, false)?;
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

                let prec = if let Some(prec) = rule.precedence {
                    let prec_op = prec.to_terminal(&mut grammar)?;
                    Some((prec_op, prec))
                } else {
                    None
                };

                rule_lines.push(Rule {
                    tokens,
                    reduce_action: rule.reduce_action,
                    separator_span: rule.separator_span,
                    lookaheads: None,
                    id: rule.id,
                    prec,
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
                prec: None,
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

        // reduce types
        use rusty_lr_core::builder::Operator;
        for (idx, term_info) in self.terminals.iter().enumerate() {
            if let Some(reduce_type) = &term_info.reduce_type {
                if !grammar.add_reduce_type(Operator::Term(idx), reduce_type.reduce_type) {
                    unreachable!("set_reduce_type error");
                }
            }
        }
        for (idx, prec_def) in self.prec_defeinitions.iter().enumerate() {
            if let Some(reduce_type) = &prec_def.reduce_type {
                if !grammar.add_reduce_type(Operator::Prec(idx), reduce_type.reduce_type) {
                    unreachable!("set_reduce_type error");
                }
            }
        }

        // precedence orders
        for (&op, &level) in self.precedences.iter() {
            if !grammar.add_precedence(op, level.1) {
                unreachable!("add_precedence error");
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

                let mut op = rule.prec.as_ref().map(|(op, _)| *op);
                if op.is_none() {
                    for token in rule.tokens.iter().rev() {
                        if let Token::Term(term_idx) = token.token {
                            op = Some(rusty_lr_core::builder::Operator::Term(term_idx));
                            break;
                        }
                    }
                }
                grammar.add_rule(idx, tokens, rule.lookaheads.clone(), op);
            }
        }

        grammar
    }
}
