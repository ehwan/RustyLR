use std::collections::BTreeMap;
use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use rusty_lr_core::Token;

use crate::error::ArgError;
use crate::error::ConflictError;
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

pub struct TerminalClassDefinintion {
    pub terminals: Vec<usize>,
    /// counter for only terminal clasas (have more than 1 terminal)
    /// dummy if it is single-terminal class
    pub multiterm_counter: usize,
}

pub enum OptimizeRemove {
    TerminalClassRuleMerge(Rule),
    SingleNonTerminalRule(Rule, Span),
}
pub struct OptimizeDiag {
    /// if `__rustylr_other_terminals` is used
    pub other_used: bool,
    /// deleted rules
    pub removed: Vec<OptimizeRemove>,
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

    /// do terminal classificate optimization
    pub optimize: bool,
    pub builder: rusty_lr_core::builder::Grammar<usize, usize>,
    pub states: Vec<rusty_lr_core::builder::State<usize, usize>>,

    /// set of terminals for each terminal class
    pub terminal_classes: Vec<TerminalClassDefinintion>,
    /// id of teminal class for each terminal
    pub terminal_class_id: Vec<usize>,
    /// class id for terminal that does not belong to any class
    pub other_terminal_class_id: usize,
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

    pub(crate) fn calculate_terminal_set(
        &self,
        negate: bool,
        terminalset: &BTreeSet<usize>,
    ) -> Vec<usize> {
        if negate {
            let mut ret = Vec::new();
            for i in 0..self.terminals.len() {
                if terminalset.contains(&i) {
                    continue;
                }
                ret.push(i);
            }
            ret
        } else {
            terminalset.iter().copied().collect()
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
            optimize: !grammar_args.no_optim,

            builder: rusty_lr_core::builder::Grammar::new(),
            states: Vec::new(),

            terminal_class_id: Vec::new(),
            terminal_classes: Vec::new(),
            other_terminal_class_id: 0,
        };
        grammar.is_char = grammar.token_typename.to_string() == "char";
        grammar.is_u8 = grammar.token_typename.to_string() == "u8";

        // add %token terminals
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
        // add other_terminals
        {
            let ident = Ident::new(utils::OTHERS_TERMINAL_NAME, Span::call_site());

            let terminal_info = TerminalInfo {
                name: ident.clone(),
                reduce_type: None,
                body: quote! { #ident },
            };
            let idx = grammar.terminals.len();
            grammar.terminals.push(terminal_info);
            grammar.terminals_index.insert(ident, idx);
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
                    Some((prec_op, prec.span()))
                } else {
                    None
                };

                rule_lines.push(Rule {
                    tokens,
                    reduce_action: rule.reduce_action,
                    reduce_action_generated: false,
                    separator_span: rule.separator_span,
                    lookaheads: None,
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
                reduce_action_generated: false,
                separator_span: Span::call_site(),
                lookaheads: None,
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

        // check reduce action
        for nonterm in &mut grammar.nonterminals {
            if nonterm.ruletype.is_some() {
                // typename is defined, reduce action must be defined
                for rule in nonterm.rules.iter_mut() {
                    if rule.reduce_action.is_none() {
                        // action is not defined,

                        // check for special case:
                        // only one token in this rule have <RuleType> defined (include terminal)
                        // the unique value will be pushed to stack
                        let mut unique_mapto = None;
                        for token in rule.tokens.iter() {
                            if token.mapto.is_some() {
                                if unique_mapto.is_some() {
                                    unique_mapto = None;
                                    break;
                                } else {
                                    unique_mapto = token.mapto.as_ref();
                                }
                            }
                        }
                        if let Some(unique_mapto) = unique_mapto {
                            let action = quote! { #unique_mapto };
                            rule.reduce_action = Some(action);
                            rule.reduce_action_generated = true;
                        } else {
                            let span = if rule.tokens.is_empty() {
                                (rule.separator_span, rule.separator_span)
                            } else {
                                let first = rule.separator_span;
                                let last = rule.tokens.last().unwrap().end_span;
                                (first, last)
                            };

                            return Err(ParseError::RuleTypeDefinedButActionNotDefined {
                                name: nonterm.name.clone(),
                                span,
                            });
                        }
                    }
                }
            }
        }
        // set operator for each rule
        for nonterm in &mut grammar.nonterminals {
            for rule in &mut nonterm.rules {
                if rule.prec.is_none() {
                    let mut op = None;
                    for token in &rule.tokens {
                        if let Token::Term(term_idx) = token.token {
                            op = Some((
                                rusty_lr_core::builder::Operator::Term(term_idx),
                                token.end_span,
                            ));
                            break;
                        }
                    }
                    rule.prec = op;
                }
            }
        }
        // initialize terminal classes with one-terminal-one-class
        grammar.terminal_class_id.reserve(grammar.terminals.len());
        grammar.terminal_classes.reserve(grammar.terminals.len());
        for i in 0..grammar.terminals.len() {
            grammar.terminal_class_id.push(i);
            grammar.terminal_classes.push(TerminalClassDefinintion {
                terminals: vec![i],
                multiterm_counter: 0,
            });
        }
        grammar.other_terminal_class_id = grammar.terminal_class_id[*grammar
            .terminals_index
            .get(&Ident::new(utils::OTHERS_TERMINAL_NAME, Span::call_site()))
            .unwrap()];

        Ok(grammar)
    }

    /// optimize grammar
    /// 'optimize' could be multiple iterations, this function do only one iteration
    fn optimize_iterate(&mut self) -> Option<OptimizeDiag> {
        let mut builder = self.create_builder();
        let augmented_idx = *self
            .nonterminals_index
            .get(&Ident::new(utils::AUGMENTED_NAME, Span::call_site()))
            .unwrap();
        let tables = if self.lalr {
            builder.build_lalr(augmented_idx)
        } else {
            builder.build(augmented_idx)
        };
        let states = match tables {
            Ok(tables) => tables.states,
            Err(_) => {
                unreachable!("optimize_iterate");
            }
        };

        let mut term_sets = BTreeSet::new();
        term_sets.insert((0..self.terminal_classes.len()).collect());
        // collect precedence orders
        // all terminals in one class must have same precedence order
        let mut precedence_sets: HashMap<_, Vec<usize>> = Default::default();
        for (&op, &level) in &builder.precedence_map {
            if let rusty_lr_core::builder::Operator::Term(term_idx) = op {
                precedence_sets
                    .entry(level)
                    .or_insert_with(Vec::new)
                    .push(term_idx);
            }
        }
        for (_, mut terms) in precedence_sets {
            terms.sort();
            term_sets.insert(terms);
        }

        for state in &states {
            // collect {set of terminals} that have same reduce action
            let mut same_reduce = BTreeMap::new();
            for (&term, reduces) in &state.reduce_map {
                same_reduce
                    .entry(reduces)
                    .or_insert_with(Vec::new)
                    .push(term);
            }
            term_sets.extend(same_reduce.into_values());

            // collect {set of terminals} that have same prefix-suffix-reduce_action in the production rules
            // so we can merge those terminals into one class
            let mut same_ruleset = BTreeMap::new();
            for (&term, &next_state) in &state.shift_goto_map_term {
                let ruleset = states[next_state].unshifted_ruleset().collect::<Vec<_>>();

                let mut no_reduceaction_sets = Vec::new();
                for rule in ruleset {
                    let (nonterm, local_id) = self.get_rule_by_id(rule.rule).unwrap();
                    let r = &nonterm.rules[local_id];

                    // if this rule has reduce action, and it is not auto-generated,
                    // this terminal should be completely distinct from others (for user-defined inspection action)
                    // so put this terminal into separate class
                    if r.reduce_action.is_some() && !r.reduce_action_generated {
                        term_sets.insert(vec![term]);
                        continue;
                    }

                    let lookahead = &r.lookaheads;
                    let mut presuffix: Vec<Token<_, _>> =
                        r.tokens.iter().map(|token| token.token).collect();
                    presuffix.remove(rule.shifted);
                    // add rule name to the end of the presuffix
                    let nonterm_id = *self.nonterminals_index.get(&nonterm.name).unwrap();
                    presuffix.push(Token::NonTerm(nonterm_id));

                    no_reduceaction_sets.push((presuffix, lookahead));
                }

                same_ruleset
                    .entry(no_reduceaction_sets)
                    .or_insert_with(Vec::new)
                    .push(term);
            }
            term_sets.extend(same_ruleset.into_values());
        }

        let term_partition = crate::partition::minimal_partition(
            term_sets.into_iter().map(|terms| terms.into_iter()),
        );

        // check if two or more terminals can be merged into one class
        if term_partition.len() == self.terminal_classes.len() {
            return None;
        }

        // convert all terminals using terminal class
        // Keep only the rules related with first terminal
        // and remove the rest
        let mut is_first_oldclass_in_newclass = Vec::new();
        is_first_oldclass_in_newclass.resize(self.terminal_classes.len(), false);

        let mut old_class_to_new_class = Vec::new();
        old_class_to_new_class.resize(self.terminal_classes.len(), 0);

        let mut new_class_defs = Vec::with_capacity(term_partition.len());
        let mut new_term_class_id = Vec::with_capacity(self.terminals.len());
        new_term_class_id.resize(self.terminals.len(), 0);
        let mut multiterm_counter = 0;
        for (new_class_id, (_setids, old_classes)) in term_partition.into_iter().enumerate() {
            let mut terms = Vec::new();
            for &old_class in old_classes.iter() {
                for &term in &self.terminal_classes[old_class].terminals {
                    new_term_class_id[term] = new_class_id;
                    terms.push(term);
                }
                old_class_to_new_class[old_class] = new_class_id;
            }
            is_first_oldclass_in_newclass[old_classes[0]] = true;
            if terms.len() > 1 {
                multiterm_counter += 1;
            }
            let class_def = TerminalClassDefinintion {
                terminals: terms,
                multiterm_counter,
            };
            new_class_defs.push(class_def);
        }

        self.terminal_class_id = new_term_class_id;
        self.terminal_classes = new_class_defs;
        self.other_terminal_class_id = self.terminal_class_id[*self
            .terminals_index
            .get(&Ident::new(utils::OTHERS_TERMINAL_NAME, Span::call_site()))
            .unwrap()];

        let mut removed_rules_diag = Vec::new();

        use rusty_lr_core::Token;

        let mut other_was_used = false;
        for nonterm in &mut self.nonterminals {
            let rules = std::mem::take(&mut nonterm.rules);
            let mut new_rules = Vec::new();
            for mut rule in rules {
                // check if this rule contains any terminal that is not the first terminal in the class
                let mut remove_this_rule = false;
                for token in &rule.tokens {
                    if let Token::Term(old_class) = token.token {
                        if !is_first_oldclass_in_newclass[old_class] {
                            remove_this_rule = true;
                            break;
                        }
                    }
                }

                if remove_this_rule {
                    // this rule contains terminal that is not the first terminal in the class
                    // so remove this rule
                    // add to diags only if it was not auto-generated
                    if nonterm.regex_span.is_none() {
                        let diag = OptimizeRemove::TerminalClassRuleMerge(rule);
                        removed_rules_diag.push(diag);
                    }
                    continue;
                }

                // change any terminal to its class id
                for token in &mut rule.tokens {
                    if let Token::Term(old_class) = token.token {
                        let new_class = old_class_to_new_class[old_class];
                        if new_class == self.other_terminal_class_id {
                            other_was_used = true;
                        }
                        token.token = Token::Term(new_class);
                    }
                }
                new_rules.push(rule);
            }
            nonterm.rules = new_rules;
        }

        // remove rules which is consisted of only one terminal
        loop {
            let mut changed = false;
            let mut nonterm_replace: HashMap<Token<usize, usize>, (usize, bool)> =
                Default::default();
            for (nonterm_id, nonterm) in self.nonterminals.iter_mut().enumerate() {
                if nonterm.rules.len() != 1 {
                    continue;
                }
                let rule = &nonterm.rules[0];
                if rule.tokens.len() != 1 {
                    continue;
                }
                let token = rule.tokens[0].token;
                let Token::Term(newclass) = token else {
                    continue;
                };

                // check if this rule's ruletype is %tokentype and reduce action is auto-generated
                if nonterm.ruletype.is_none()
                    || (nonterm.ruletype.as_ref().unwrap().to_string()
                        == self.token_typename.to_string()
                        && rule.reduce_action_generated)
                {
                    changed = true;

                    // this nonterm can be reduced to a single terminal class
                    let rules = std::mem::take(&mut nonterm.rules);
                    let rule = rules.into_iter().next().unwrap();

                    // add to diags only if it was not auto-generated
                    if nonterm.regex_span.is_none() {
                        let diag = OptimizeRemove::SingleNonTerminalRule(rule, nonterm.name.span());
                        removed_rules_diag.push(diag);
                    }

                    nonterm_replace.insert(
                        Token::NonTerm(nonterm_id),
                        (newclass, nonterm.ruletype.is_none()),
                    );
                }
            }
            // replace all Token::NonTerm that can be replaced into Token::Term calculated above
            for nonterm in self.nonterminals.iter_mut() {
                for rule in &mut nonterm.rules {
                    for token in &mut rule.tokens {
                        if let Some(&(newclass, is_ruletype_none)) =
                            nonterm_replace.get(&token.token)
                        {
                            token.token = Token::Term(newclass);
                            if is_ruletype_none {
                                token.mapto = None;
                            }
                            changed = true;
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }

        return Some(OptimizeDiag {
            other_used: other_was_used,
            removed: removed_rules_diag,
        });
    }

    pub fn optimize(&mut self, max_iter: usize) -> OptimizeDiag {
        let mut diag = OptimizeDiag {
            other_used: false,
            removed: Vec::new(),
        };
        for _ in 0..max_iter {
            let ret = self.optimize_iterate();
            match ret {
                Some(new_diag) => {
                    diag.other_used |= new_diag.other_used;
                    diag.removed.extend(new_diag.removed.into_iter());
                }
                None => break,
            }
        }

        diag
    }

    pub fn term_pretty_name(&self, term_idx: usize) -> String {
        if self.is_char || self.is_u8 {
            self.terminals[term_idx].body.to_string()
        } else {
            let name = &self.terminals[term_idx].name;
            if name == utils::OTHERS_TERMINAL_NAME {
                "<Others>".to_string()
            } else {
                name.to_string()
            }
        }
    }
    /// returns either 'term' or 'TerminalClassX'
    pub fn class_pretty_name_abbr(&self, class_idx: usize) -> String {
        let class = &self.terminal_classes[class_idx];
        if class.terminals.len() == 1 {
            self.term_pretty_name(class.terminals[0])
        } else {
            format!("TerminalClass{}", class.multiterm_counter)
        }
    }
    /// returns either 'term' or '[term1, term2, ...]'
    pub fn class_pretty_name_list(&self, class_idx: usize, max_len: usize) -> String {
        let class = &self.terminal_classes[class_idx];
        if class.terminals.len() == 1 {
            self.term_pretty_name(class.terminals[0])
        } else if class.terminals.len() < max_len {
            let f = self.terminal_classes[class_idx]
                .terminals
                .iter()
                .map(|&term| self.term_pretty_name(term))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{f}]")
        } else {
            let class = &self.terminal_classes[class_idx];
            let len = class.terminals.len();
            let first = class.terminals[0];
            let second = class.terminals[1];
            let last = *class.terminals.last().unwrap();

            let first = self.term_pretty_name(first);
            let second = self.term_pretty_name(second);
            let last = self.term_pretty_name(last);
            format!("[{first}, {second}, ..., {last}] ({len} terms)")
        }
    }
    pub fn nonterm_pretty_name(&self, nonterm_idx: usize) -> String {
        self.nonterminals[nonterm_idx].pretty_name.clone()
    }

    pub fn conflict(&self) -> Result<(), Box<ConflictError>> {
        // check for SR/RR conflicts if it's not GLR
        if !self.glr {
            let term_mapper = |class| self.class_pretty_name_list(class, 5);
            let nonterm_mapper = |nonterm| self.nonterm_pretty_name(nonterm);

            // rr conflicts
            for state in self.states.iter() {
                if let Some((rules, lookaheads)) = state.conflict_rr().next() {
                    let lookahead = *lookaheads.into_iter().next().unwrap();
                    let (rule1, rule2) = {
                        let mut iter = rules.iter();
                        let r1 = *iter.next().unwrap();
                        (r1, *iter.next().unwrap())
                    };
                    return Err(Box::new(ConflictError::ReduceReduceConflict {
                        lookahead: term_mapper(lookahead),
                        rule1: (
                            rule1,
                            self.builder.rules[rule1]
                                .rule
                                .clone()
                                .map(&term_mapper, &nonterm_mapper),
                        ),
                        rule2: (
                            rule2,
                            self.builder.rules[rule2]
                                .rule
                                .clone()
                                .map(&term_mapper, &nonterm_mapper),
                        ),
                    }));
                }
            }

            // sr conflicts
            for state in self.states.iter() {
                if let Some((&term, reduces, shift_rules)) =
                    state.conflict_sr(|idx| &self.states[idx]).next()
                {
                    let reduce = *reduces.iter().next().unwrap();
                    let shift_rules = shift_rules
                        .into_iter()
                        .map(|rule| {
                            let prod_rule = self.builder.rules[rule.rule]
                                .rule
                                .clone()
                                .map(&term_mapper, &nonterm_mapper);
                            (
                                rule.rule,
                                rusty_lr_core::ShiftedRule {
                                    rule: prod_rule,
                                    shifted: rule.shifted,
                                },
                            )
                        })
                        .collect();
                    return Err(Box::new(ConflictError::ShiftReduceConflict {
                        term: term_mapper(term),
                        reduce_rule: (
                            reduce,
                            self.builder.rules[reduce]
                                .rule
                                .clone()
                                .map(term_mapper, nonterm_mapper),
                        ),
                        shift_rules,
                    }));
                }
            }
        }
        Ok(())
    }

    /// create the rusty_lr_core::Grammar from the parsed CFGs
    pub fn create_builder(&self) -> rusty_lr_core::builder::Grammar<usize, usize> {
        let mut grammar: rusty_lr_core::builder::Grammar<usize, usize> =
            rusty_lr_core::builder::Grammar::new();

        // reduce types
        use rusty_lr_core::builder::Operator;
        for (idx, term_info) in self.terminals.iter().enumerate() {
            if let Some(reduce_type) = &term_info.reduce_type {
                let class = self.terminal_class_id[idx];
                if !grammar.add_reduce_type(Operator::Term(class), reduce_type.reduce_type) {
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
            let level = level.1;
            match op {
                Operator::Prec(_) => {
                    if !grammar.add_precedence(op, level) {
                        unreachable!("add_precedence error");
                    }
                }
                Operator::Term(term) => {
                    let class = self.terminal_class_id[term];
                    if !grammar.add_precedence(Operator::Term(class), level) {
                        unreachable!("add_precedence error");
                    }
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

                grammar.add_rule(
                    idx,
                    tokens,
                    rule.lookaheads.clone(),
                    rule.prec.map(|(op, _)| op),
                );
            }
        }

        grammar
    }

    pub fn build_grammar_without_resolve(&mut self) {
        let augmented_idx = *self
            .nonterminals_index
            .get(&Ident::new(utils::AUGMENTED_NAME, Span::call_site()))
            .unwrap();
        let states = if self.lalr {
            self.builder.build_lalr_without_resolving(augmented_idx)
        } else {
            self.builder.build_without_resolving(augmented_idx)
        };
        let Ok(states) = states else {
            unreachable!("grammar build error");
        };
        self.states = states.states;
    }
    pub fn resolve_precedence(&mut self) {
        let mut dfa = rusty_lr_core::builder::DFA {
            states: std::mem::take(&mut self.states),
        };
        self.builder.resolve_precedence(&mut dfa);
        self.states = dfa.states;
    }
}
