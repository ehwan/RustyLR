use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use rusty_lr_core::builder::error::BuildError;
use rusty_lr_core::ShiftedRule;

use crate::error::EmitError;
use crate::grammar::Grammar;
use crate::utils;

use rusty_lr_core as rlr;

use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// emit Rust code for the parser
impl Grammar {
    /// write type alias
    fn emit_type_alises(&self) -> TokenStream {
        let module_prefix = &self.module_prefix;
        let start_rule_name = &self.start_rule_name;
        let rule_typename = format_ident!("{}Rule", start_rule_name);
        let state_typename = format_ident!("{}State", start_rule_name);
        let token_typename = &self.token_typename;
        let enum_name = utils::generate_enum_name(start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let parse_error_typename = format_ident!("{}ParseError", start_rule_name);
        let invalid_terminal_error = format_ident!("{}InvalidTerminalError", start_rule_name);

        if self.glr {
            let multiple_path_error = format_ident!("{}MultiplePathError", start_rule_name);
            let tree_typename = format_ident!("{}Tree", start_rule_name);
            let tree_nonterminal_typename = format_ident!("{}TreeNonTerminal", start_rule_name);
            quote! {
                /// type alias for CFG production rule
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::ProductionRule<#token_typename, #enum_name>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::glr::State<#token_typename, #enum_name>;
                /// type alias for `InvalidTerminalError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #invalid_terminal_error = #module_prefix::glr::InvalidTerminalError<#token_typename, #reduce_error_typename>;
                /// type alias for `MultiplePathError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #multiple_path_error = #module_prefix::glr::MultiplePathError<#token_typename, #enum_name>;
                /// type alias for `Tree`
                #[allow(non_camel_case_types,dead_code)]
                pub type #tree_typename = #module_prefix::glr::Tree0;
                /// type alias for `TreeNonTerminal`
                #[allow(non_camel_case_types,dead_code)]
                pub type #tree_nonterminal_typename = #module_prefix::glr::TreeNonTerminal0;
            }
        } else {
            quote! {
                /// type alias for CFG production rule
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::ProductionRule<#token_typename, #enum_name>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::lr::State<#token_typename, #enum_name>;
                /// type alias for `ParseError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::lr::ParseError<#token_typename, #reduce_error_typename>;
                /// type alias for `InvalidTerminalError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #invalid_terminal_error = #module_prefix::lr::InvalidTerminalError<#token_typename>;
            }
        }
    }

    /// write enum that represents non-terminal symbols, including Augmented
    fn emit_nonterm_enum(&self) -> TokenStream {
        // =====================================================================
        // =====================Writing NonTerminal Enum========================
        // =====================================================================

        let start_rule_name = &self.start_rule_name;
        let enum_typename = utils::generate_enum_name(start_rule_name);

        let mut comma_separated_variants = TokenStream::new();
        let mut case_display = TokenStream::new();
        for name in self.rules_index.iter() {
            comma_separated_variants.extend(quote! {
                #name,
            });

            let name_str = name.to_string();
            case_display.extend(quote! {
                #enum_typename::#name=> write!(f, #name_str),
            });
        }

        quote! {
            /// An enum that represents non-terminal symbols
            #[allow(non_camel_case_types)]
            #[derive(Debug, Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
            pub enum #enum_typename {
                #comma_separated_variants
            }

            impl std::fmt::Display for #enum_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        #case_display
                    }
                }
            }
        }
    }

    // build grammar at compile time
    fn emit_grammar_compiletime(&self, lalr: bool) -> Result<TokenStream, Box<EmitError>> {
        let mut grammar = self.create_grammar();

        // build
        let dfa = if lalr {
            grammar.build_lalr(Ident::new(utils::AUGMENTED_NAME, Span::call_site()))
        } else {
            grammar.build(Ident::new(utils::AUGMENTED_NAME, Span::call_site()))
        };
        let dfa = match dfa {
            Ok(dfa) => dfa,
            Err(e) => match e {
                BuildError::NoAugmented | BuildError::RuleNotFound(_) => {
                    unreachable!("Unreachable grammar build error")
                }
                BuildError::ReduceReduceConflict {
                    lookahead,
                    rule1,
                    rule2,
                } => {
                    return Err(Box::new(EmitError::ReduceReduceConflict {
                        lookahead,
                        rule1: (rule1, grammar.rules[rule1].0.clone()),
                        rule2: (rule2, grammar.rules[rule2].0.clone()),
                    }))
                }
                BuildError::ShiftReduceConflict {
                    reduce,
                    shift,
                    term,
                } => {
                    let mut shift_rules = Vec::new();
                    for (r, _) in shift.rules.into_iter() {
                        let shifted_rule = ShiftedRule {
                            rule: grammar.rules[r.rule].0.clone(),
                            shifted: r.shifted,
                        };
                        shift_rules.push((r.rule, shifted_rule));
                    }

                    return Err(Box::new(EmitError::ShiftReduceConflict {
                        term,
                        reduce_rule: (reduce, grammar.rules[reduce].0.clone()),
                        shift_rules,
                    }));
                }
            },
        };

        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = utils::generate_enum_name(&self.start_rule_name);
        let rule_typename = format_ident!("{}Rule", &self.start_rule_name);
        let state_typename = format_ident!("{}State", &self.start_rule_name);

        // write vector of terminals
        let comma_separated_terminals = {
            let mut comma_separated_terminals = TokenStream::new();
            for term in self.terminals_index.iter() {
                let stream = &self.terminals.get(term).unwrap().1;
                comma_separated_terminals.extend(quote! {
                    #stream,
                });
            }
            comma_separated_terminals
        };

        let (rules, states) = {
            let term_mapper = |term: Ident| self.terminals.get(&term).unwrap().0;
            let nonterm_mapper = |nonterm: Ident| nonterm;
            let rules: Vec<_> = grammar
                .rules
                .into_iter()
                .map(|(r, _)| r.map(term_mapper, nonterm_mapper))
                .collect();
            let states: Vec<_> = dfa
                .states
                .into_iter()
                .map(|s| s.to_export_glr().map(term_mapper, nonterm_mapper))
                .collect();
            (rules, states)
        };

        // =====================================================================
        // ==================Writing Production Rules===========================
        // =====================================================================
        let mut tokens_initializer = TokenStream::new();
        let mut rule_names_initializer = TokenStream::new();
        let mut rule_id_initializer = TokenStream::new();

        {
            for rule in rules.into_iter() {
                let mut comma_separated_tokens = TokenStream::new();
                for token in rule.rule.into_iter() {
                    match token {
                        rlr::Token::Term(term) => {
                            let term = term as u16;
                            comma_separated_tokens
                                .extend(quote! {#module_prefix::Token::Term(#term),});
                        }
                        rlr::Token::NonTerm(nonterm) => {
                            comma_separated_tokens
                                .extend(quote! {#module_prefix::Token::NonTerm(#nonterminals_enum_name::#nonterm),});
                        }
                    }
                }

                tokens_initializer.extend(quote! {
                    &[#comma_separated_tokens],
                });
                let name = rule.name;
                rule_names_initializer.extend(quote! {
                    #nonterminals_enum_name::#name,
                });
                let id = rule.id;
                rule_id_initializer.extend(quote! {
                    #id,
                });
            }
        };

        // =====================================================================
        // =========================Writing States==============================
        // =====================================================================
        let mut reduce_terminals_cache_initializer = TokenStream::new();
        let mut ruleset_shifted0_cache_initializer = TokenStream::new();
        let mut shift_term_initializer = TokenStream::new();
        let mut shift_nonterm_initializer = TokenStream::new();
        let mut reduce_initializer = TokenStream::new();
        let mut ruleset_initializer = TokenStream::new();
        let mut ruleset_shifted0_initialiezr = TokenStream::new();

        {
            // when generating code for 'inserting tokens to reduce_map',
            // there could be multiple lookahead tokens for one rule
            // inserting all of them one by one is inefficient
            let mut reduce_terminals_map = BTreeMap::new();
            let mut ruleset0_map = BTreeMap::new();

            for state in states.into_iter() {
                // use BTreeMap to sort keys, for consistent output
                {
                    let mut term_state_comma_separated = TokenStream::new();
                    let shift_goto_map_term: BTreeMap<_, _> =
                        state.shift_goto_map_term.into_iter().collect();
                    for (term, goto) in shift_goto_map_term.into_iter() {
                        let term = term as u16;
                        let goto = goto as u16;
                        term_state_comma_separated.extend(quote! {
                            (#term, #goto),
                        });
                    }
                    shift_term_initializer.extend(quote! {
                        &[#term_state_comma_separated],
                    });
                }

                {
                    let mut nonterm_state_comma_separated = TokenStream::new();
                    let shift_goto_map_nonterm: BTreeMap<_, _> =
                        state.shift_goto_map_nonterm.into_iter().collect();
                    for (nonterm, goto) in shift_goto_map_nonterm.into_iter() {
                        let goto = goto as u16;
                        nonterm_state_comma_separated.extend(quote! {
                            (#nonterminals_enum_name::#nonterm, #goto),
                        });
                    }
                    shift_nonterm_initializer.extend(quote! {
                        &[#nonterm_state_comma_separated],
                    });
                }

                let mut reduce_map_by_rule_id = BTreeMap::new();
                for (term, ruleids) in state.reduce_map.into_iter() {
                    for ruleid in ruleids.into_iter() {
                        reduce_map_by_rule_id
                            .entry(ruleid)
                            .or_insert_with(BTreeSet::new)
                            .insert(term);
                    }
                }
                {
                    let mut terminalsetid_rule_comma_separated = TokenStream::new();
                    for (ruleid, tokens) in reduce_map_by_rule_id.into_iter() {
                        let terminal_set_num = if let Some(id) = reduce_terminals_map.get(&tokens) {
                            *id
                        } else {
                            let len = reduce_terminals_map.len();
                            reduce_terminals_map.insert(tokens.clone(), len);

                            let mut init_terminals_comma_separated = TokenStream::new();
                            for term in tokens.into_iter() {
                                let term = term as u16;
                                init_terminals_comma_separated.extend(quote! {
                                    #term,
                                });
                            }

                            reduce_terminals_cache_initializer.extend(quote! {
                                &[#init_terminals_comma_separated],
                            });

                            len
                        };

                        let ruleid = ruleid as u16;
                        let terminal_set_num = terminal_set_num as u16;
                        terminalsetid_rule_comma_separated.extend(quote! {
                            (#terminal_set_num, #ruleid),
                        });
                    }
                    reduce_initializer.extend(quote! {
                        &[#terminalsetid_rule_comma_separated],
                    });
                }
                let ruleset0_id = {
                    let mut ruleset0 = BTreeSet::new();
                    {
                        let mut shifted_rules_comma_separated = TokenStream::new();
                        for rule in state.ruleset.into_iter() {
                            if rule.shifted == 0 {
                                let ruleid = rule.rule;
                                ruleset0.insert(ruleid);
                            } else {
                                let ruleid = rule.rule as u16;
                                let shifted = rule.shifted as u16;
                                shifted_rules_comma_separated.extend(quote! {
                                    (#ruleid,#shifted),
                                });
                            }
                        }
                        ruleset_initializer.extend(quote! {
                            &[#shifted_rules_comma_separated],
                        });
                    }

                    let ruleset0_id = if let Some(ruleset0) = ruleset0_map.get(&ruleset0) {
                        *ruleset0
                    } else {
                        let len = ruleset0_map.len();
                        ruleset0_map.insert(ruleset0.clone(), len);

                        let mut comma_separated_ruleset0 = TokenStream::new();
                        for ruleid in ruleset0.into_iter() {
                            let ruleid = ruleid as u16;
                            comma_separated_ruleset0.extend(quote! {
                                #ruleid,
                            });
                        }

                        ruleset_shifted0_cache_initializer.extend(quote! {
                            &[#comma_separated_ruleset0],
                        });

                        len
                    };

                    ruleset0_id
                };

                let ruleset0_id = ruleset0_id as u16;
                ruleset_shifted0_initialiezr.extend(quote! {
                    #ruleset0_id,
                });
            }
        }
        let init_reduce_map = if self.glr {
            quote! {
                let mut reduce_map = #module_prefix::HashMap::default();
                for (terminal_set_id, ruleid) in reduce_map_.iter() {
                    for term in RUSTYLR_REDUCE_TERMINALS_CACHE[*terminal_set_id as usize].iter() {
                        reduce_map.entry( __rustylr_terminals[*term as usize].clone() )
                            .or_insert_with( Vec::new )
                            .push( *ruleid as usize );
                    }
                }
            }
        } else {
            quote! {
                let mut reduce_map = #module_prefix::HashMap::default();
                for (terminal_set_id, ruleid) in reduce_map_.iter() {
                    reduce_map.extend(
                        RUSTYLR_REDUCE_TERMINALS_CACHE[*terminal_set_id as usize].iter().map(
                            | term_idx | {
                                (__rustylr_terminals[*term_idx as usize].clone(), *ruleid as usize)
                            }
                        )
                    );
                }
            }
        };

        Ok(quote! {
            let __rustylr_terminals = vec![#comma_separated_terminals];
            const RUSTYLR_RULES_TOKENS: &[&[#module_prefix::Token<u16, #nonterminals_enum_name>]] = &[#tokens_initializer];
            const RUSTYLR_RULES_NAME: &[#nonterminals_enum_name] = &[#rule_names_initializer];
            const RUSTYLR_RULES_ID: &[usize] = &[#rule_id_initializer];

            let rules: Vec<#rule_typename> = RUSTYLR_RULES_NAME.iter().zip(
                RUSTYLR_RULES_TOKENS.iter().zip(
                    RUSTYLR_RULES_ID.iter()
                )
            ).map(
                | (name, (tokens, id)) | {
                    #rule_typename {
                        name: *name,
                        rule: tokens.iter().map(
                            | token | {
                                match token {
                                    #module_prefix::Token::Term(term) => #module_prefix::Token::Term(__rustylr_terminals[*term as usize].clone()),
                                    #module_prefix::Token::NonTerm(nonterm) => #module_prefix::Token::NonTerm(*nonterm),
                                }
                            }
                        ).collect(),
                        id: *id,
                    }
                }
            ).collect();

            const RUSTYLR_REDUCE_TERMINALS_CACHE: &[&[u16]] = &[#reduce_terminals_cache_initializer];
            const RUSTYLR_RULESET_SHIFTED0_CACHE: &[&[u16]] = &[#ruleset_shifted0_cache_initializer];
            const RUSTYLR_SHIFT_TERM_MAP: &[&[(u16, u16)]] = &[#shift_term_initializer];
            const RUSTYLR_SHIFT_NONTERM_MAP: &[&[(#nonterminals_enum_name, u16)]] = &[#shift_nonterm_initializer];
            const RUSTYLR_REDUCE_MAP: &[&[(u16, u16)]] = &[#reduce_initializer];
            const RUSTYLR_RULESET_MAP: &[&[(u16,u16)]] = &[#ruleset_initializer];
            const RUSTYLR_RULESET_SHIFTED0_MAP: &[u16] = &[#ruleset_shifted0_initialiezr];

            let states:Vec<#state_typename> = RUSTYLR_SHIFT_TERM_MAP.iter().zip(
                RUSTYLR_SHIFT_NONTERM_MAP.iter().zip(
                    RUSTYLR_REDUCE_MAP.iter().zip(
                        RUSTYLR_RULESET_MAP.iter().zip(
                            RUSTYLR_RULESET_SHIFTED0_MAP.iter()
                        )
                    )
                )
            ).map(
                | (shift_goto_map_term, (shift_goto_map_nonterm, (reduce_map_, (ruleset,ruleset0_id)))) | {
                    #init_reduce_map


                    let mut ruleset: std::collections::BTreeSet<#module_prefix::ShiftedRuleRef> = ruleset.iter().map(
                        | (ruleid, shifted) | {
                            #module_prefix::ShiftedRuleRef {
                                rule: *ruleid as usize,
                                shifted: *shifted as usize,
                            }
                        }
                    ).collect();
                    ruleset.extend(RUSTYLR_RULESET_SHIFTED0_CACHE[*ruleset0_id as usize].iter().map(
                        | ruleid |
                        {
                            #module_prefix::ShiftedRuleRef {
                                rule: *ruleid as usize,
                                shifted: 0,
                            }
                        }
                    ));


                    #state_typename {
                        shift_goto_map_term: shift_goto_map_term.iter().map(
                            | (term_idx, goto) | {
                                (__rustylr_terminals[*term_idx as usize].clone(), *goto as usize)
                            }
                        ).collect(),
                        shift_goto_map_nonterm: shift_goto_map_nonterm
                            .iter()
                            .map(|(nonterm, goto)| (*nonterm, *goto as usize))
                            .collect(),
                        reduce_map,
                        ruleset,
                    }
                }
            ).collect();
        })
    }

    fn emit_parser(&self, grammar_emit: TokenStream) -> Result<TokenStream, Box<EmitError>> {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = utils::generate_enum_name(&self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let parseerror_typename = format_ident!("{}ParseError", self.start_rule_name);
        let rule_typename = format_ident!("{}Rule", self.start_rule_name);
        let state_typename = format_ident!("{}State", self.start_rule_name);
        let parser_struct_name = format_ident!("{}Parser", self.start_rule_name);
        let context_struct_name = format_ident!("{}Context", self.start_rule_name);

        // stack_name for each non-terminal
        let mut stack_names_by_nonterm = rusty_lr_core::HashMap::default();
        // (stack_name, TokenStream for typename) sorted in rule insertion order
        // for consistent output
        let mut stack_names_in_order = Vec::new();
        {
            // <RuleType as ToString> - <Stackname> map
            let mut stack_name_by_typename = rusty_lr_core::HashMap::default();

            // insert terminal token type
            let term_stack_name = Ident::new(utils::TERMINAL_STACK_NAME, Span::call_site());
            stack_name_by_typename.insert(self.token_typename.to_string(), term_stack_name.clone());
            stack_names_in_order.push((term_stack_name, self.token_typename.clone()));

            // for non-terminal have <RuleType> defined
            for name in self.rules_index.iter() {
                if let Some(typename) = self.nonterm_typenames.get(name) {
                    let len = stack_names_in_order.len();
                    let stack_name = stack_name_by_typename
                        .entry(typename.to_string())
                        .or_insert_with(|| {
                            let new_stack_name = Ident::new(
                                &format!("__rustylr_generated_stack_{}", len),
                                Span::call_site(),
                            );
                            stack_names_in_order.push((new_stack_name.clone(), typename.clone()));

                            new_stack_name
                        });
                    stack_names_by_nonterm.insert(name.clone(), stack_name.clone());
                }
            }
        }

        // =====================================================================
        // =========================Writing Parser==============================
        // =====================================================================

        // TokenStream for userdata parameter definition, if defined
        let user_data_parameter_name =
            Ident::new(utils::USER_DATA_PARAMETER_NAME, Span::call_site());
        let (user_data_typename, user_data_parameter_def, user_data_var, user_data_dummy) =
            if let Some(user_data) = &self.userdata_typename {
                (
                    user_data.clone(),
                    quote! { #user_data_parameter_name: &mut #user_data, },
                    quote! { #user_data_parameter_name, },
                    quote! { #user_data_parameter_name },
                )
            } else {
                (quote! {()}, quote! {}, quote! {}, quote! {&mut ()})
            };

        let mut ruleid: usize = 0;
        let mut case_streams = quote! {};
        // stack for end index of each rule in term stack
        let terms_stack_name = Ident::new(utils::TERMINAL_STACK_NAME, Span::call_site());
        let token_typename = &self.token_typename;

        // TokenStream to define reduce function for each production rule
        let mut fn_reduce_for_each_rule_stream = TokenStream::new();

        for name in self.rules_index.iter() {
            let rules = self.rules.get(name).unwrap();
            for (rule_local_id, rule) in rules.rule_lines.iter().enumerate() {
                let mut token_pop_stream = TokenStream::new();
                for token in rule.tokens.iter().rev() {
                    if self.terminals.contains_key(&token.token) {
                        match &token.mapto {
                            Some(mapto) => {
                                token_pop_stream.extend(quote! {
                                    let mut #mapto = self.#terms_stack_name.pop().unwrap();
                                });
                            }
                            None => {
                                token_pop_stream.extend(quote! {
                                    self.#terms_stack_name.pop();
                                });
                            }
                        }
                    } else if self.nonterm_typenames.contains_key(&token.token) {
                        // if <RuleType> is defined for this nonterm,
                        // pop value from the stack to 'mapto'
                        let stack_name = stack_names_by_nonterm.get(&token.token).unwrap();

                        match &token.mapto {
                            Some(mapto) => {
                                token_pop_stream.extend(quote! {
                                    let mut #mapto = self.#stack_name.pop().unwrap();
                                });
                            }
                            None => {
                                token_pop_stream.extend(quote! {
                                    self.#stack_name.pop();
                                });
                            }
                        }
                    }
                }

                let reduce_fn_ident = format_ident!("reduce_{}_{}", name, rule_local_id);

                case_streams.extend(quote! {
                    #ruleid => {
                        self.#reduce_fn_ident( lookahead, #user_data_var )
                    }
                });

                // if typename is defined for this rule, push result of action to stack
                // else, just execute action
                let typename = self.nonterm_typenames.get(name);
                if typename.is_some() {
                    // push result to this stack
                    let stack_name = stack_names_by_nonterm.get(name).unwrap();

                    // typename is defined, reduce action must be defined
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, lookahead: &#token_typename, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
                                #token_pop_stream
                                self.#stack_name.push(#action);
                                Ok(())
                            }
                        });
                    } else {
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
                            fn_reduce_for_each_rule_stream.extend(quote! {
                                fn #reduce_fn_ident(&mut self, lookahead: &#token_typename, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
                                    #token_pop_stream
                                    self.#stack_name.push(#unique_mapto);
                                    Ok(())
                                }
                            });
                        } else {
                            return Err(Box::new(EmitError::RuleTypeDefinedButActionNotDefined {
                                name: name.clone(),
                                rule_local_id,
                            }));
                        }
                    }
                } else {
                    // <RuleType> is not defined,
                    // just execute action
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, lookahead: &#token_typename, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
                                #token_pop_stream
                                #action
                                Ok(())
                            }
                        });
                    } else {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, lookahead: &#token_typename, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
                                #token_pop_stream
                                Ok(())
                            }
                        });
                    }
                }

                ruleid += 1;
            }
        }

        // TokenStream for <RuleType> of start rule
        // and pop from start rule stack
        let (return_start_rule_typename, pop_from_start_rule_stack) = {
            if let Some(start_typename) = self.nonterm_typenames.get(&self.start_rule_name) {
                let start_rule_stack_name =
                    stack_names_by_nonterm.get(&self.start_rule_name).unwrap();
                (
                    quote! { -> #start_typename },
                    quote! {
                        self.#start_rule_stack_name.pop().unwrap()
                    },
                )
            } else {
                (TokenStream::new(), TokenStream::new())
            }
        };

        // TokenStream for member variables declaration
        let mut stack_def_streams = quote! {};
        let mut stack_init_streams = quote! {};

        for (stack_name, typename) in stack_names_in_order.into_iter() {
            stack_def_streams.extend(quote! {
                #stack_name : Vec<#typename>,
            });
            stack_init_streams.extend(quote! {
                #stack_name : Vec::new(),
            });
        }

        let mut derives_stream = TokenStream::new();
        for derive in &self.derives {
            derives_stream.extend(quote! {
                #derive,
            });
        }
        derives_stream = if self.derives.is_empty() {
            TokenStream::new()
        } else {
            quote! {
                #[derive(#derives_stream)]
            }
        };

        Ok(quote! {
        /// struct that holds internal parser data,
        /// including data stack for each non-terminal,
        /// and state stack for DFA
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        #derives_stream
        pub struct #context_struct_name {
            /// state stack, user must not modify this
            pub state_stack: Vec<usize>,
            #stack_def_streams
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #context_struct_name {
            pub fn new() -> Self {
                Self {
                    state_stack: vec![0],
                    #stack_init_streams
                }
            }

            #fn_reduce_for_each_rule_stream

            /// pop value from start rule
            #[inline]
            pub fn accept(&mut self) #return_start_rule_typename {
                #pop_from_start_rule_stack
            }
        }

        impl #module_prefix::lr::Context for #context_struct_name {
            type Term = #token_typename;
            type ReduceActionError = #reduce_error_typename;
            type UserData = #user_data_typename;

            fn reduce(&mut self,
                rustylr_macro_generated_ruleid__: usize,
                data: &mut #user_data_typename,
                lookahead: &#token_typename,
            ) -> Result<(), #reduce_error_typename> {
                match rustylr_macro_generated_ruleid__ {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                    }
                }
            }
            fn push( &mut self, term: #token_typename ) {
                self.#terms_stack_name.push(term);
            }

            fn get_state_stack(&self) -> &[usize] {
                &self.state_stack
            }
            fn get_state_stack_mut(&mut self) -> &mut Vec<usize> {
                &mut self.state_stack
            }
        }


        /// struct that holds parser data, DFA tables
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        pub struct #parser_struct_name {
            /// production rules
            pub rules: Vec<#rule_typename>,
            /// states
            pub states: Vec<#state_typename>,
        }
        impl #module_prefix::lr::Parser for #parser_struct_name {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;

            fn get_rules(&self) -> &[#rule_typename] {
                &self.rules
            }
            fn get_states(&self) -> &[#state_typename] {
                &self.states
            }
        }

        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        impl #parser_struct_name {
            /// feed one terminal to parser, and update state stack
            pub fn feed(
                &self,
                context: &mut #context_struct_name,
                term: #token_typename,
                #user_data_parameter_def
            ) -> Result<(), #parseerror_typename> {
                #module_prefix::lr::feed(
                    self,
                    context,
                    term,
                    #user_data_dummy,
                )
            }
            /// Create new context for parsing
            #[inline]
            pub fn begin(&self) -> #context_struct_name {
                #context_struct_name::new()
            }

            /// Create new parser instance.
            /// Parser can be reused with different context, for multiple parsing.
            pub fn new() -> Self {
                #grammar_emit
                Self {
                    rules,
                    states,
                }
            }
        }

        })
    }

    fn emit_parser_glr(&self, grammar_emit: TokenStream) -> Result<TokenStream, Box<EmitError>> {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = utils::generate_enum_name(&self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let rule_typename = format_ident!("{}Rule", self.start_rule_name);
        let state_typename = format_ident!("{}State", self.start_rule_name);
        let parser_struct_name = format_ident!("{}Parser", self.start_rule_name);
        let node_struct_name = format_ident!("{}Node", self.start_rule_name);
        let node_enum_name = format_ident!("{}NodeEnum", self.start_rule_name);
        let context_struct_name = format_ident!("{}Context", self.start_rule_name);
        let token_typename = &self.token_typename;
        let tree0_typename = format_ident!("{}Tree", self.start_rule_name);
        let multiple_path_error_typename =
            format_ident!("{}MultiplePathError", self.start_rule_name);
        let invalid_terminal_error_typename =
            format_ident!("{}InvalidTerminalError", self.start_rule_name);
        // let tree0_nonterminal_typename = format_ident!("{}TreeNonTerminal", self.start_rule_name);

        // =====================================================================
        // =========================Writing Parser==============================
        // =====================================================================

        let terms_enum_name = Ident::new("__Terminals", Span::call_site());

        // TokenStream for userdata parameter definition, if defined
        let user_data_parameter_name =
            Ident::new(utils::USER_DATA_PARAMETER_NAME, Span::call_site());
        let (user_data_typename, user_data_parameter_def, user_data_var, user_data_dummy) =
            if let Some(user_data) = &self.userdata_typename {
                (
                    user_data.clone(),
                    quote! { #user_data_parameter_name: &mut #user_data, },
                    quote! { #user_data_parameter_name, },
                    quote! { #user_data_parameter_name },
                )
            } else {
                (quote! {()}, quote! {}, quote! {}, quote! {&mut ()})
            };

        let mut rule_index: usize = 0;
        let mut case_streams = quote! {};

        // TokenStream to define reduce function for each production rule
        let mut fn_reduce_for_each_rule_stream = TokenStream::new();

        for name in self.rules_index.iter() {
            let rules = self.rules.get(name).unwrap();
            for (rule_local_id, rule) in rules.rule_lines.iter().enumerate() {
                let mut extract_data_from_node_enum = TokenStream::new();
                for (token_idx, token) in rule.tokens.iter().enumerate().rev() {
                    let tree_token_ident = format_ident!("__rustylr_tree_token_{}", token_idx);
                    if self.terminals.contains_key(&token.token) {
                        match &token.mapto {
                            Some(mapto) => {
                                extract_data_from_node_enum.extend(quote! {
                                    let (__rustylr_data,#tree_token_ident) = match std::rc::Rc::try_unwrap(__rustylr_node) {
                                        Ok(node) => {
                                            __rustylr_node = node.parent.unwrap();
                                            (node.data.unwrap(), node.tree.unwrap().to_tree0())
                                        }
                                        Err(rc_node) => {
                                            __rustylr_node = std::rc::Rc::clone(rc_node.parent.as_ref().unwrap());
                                            (rc_node.data.as_ref().unwrap().clone(), rc_node.tree.as_ref().unwrap().to_tree0())
                                        }
                                    };
                                    let mut #mapto = if let #node_enum_name::#terms_enum_name(#mapto) = __rustylr_data {
                                        #mapto
                                    } else {
                                        unreachable!()
                                    };
                                });
                            }
                            None => {
                                extract_data_from_node_enum.extend(quote! {
                                    let #tree_token_ident = match std::rc::Rc::try_unwrap(__rustylr_node) {
                                        Ok(node) => {
                                            __rustylr_node = node.parent.unwrap();
                                            node.tree.unwrap().to_tree0()
                                        }
                                        Err(rc_node) => {
                                            __rustylr_node = std::rc::Rc::clone(rc_node.parent.as_ref().unwrap());
                                            rc_node.tree.as_ref().unwrap().to_tree0()
                                        }
                                    };
                                });
                            }
                        }
                    } else if self.nonterm_typenames.contains_key(&token.token) {
                        // if <RuleType> is defined for this nonterm,
                        // extract data from enum

                        let variant_name = &token.token;

                        match &token.mapto {
                            Some(mapto) => {
                                let ruleid_mapto = format_ident!("{}_rule", mapto);
                                extract_data_from_node_enum.extend(quote! {
                                    let (__rustylr_data,#tree_token_ident) = match std::rc::Rc::try_unwrap(__rustylr_node) {
                                        Ok(node) => {
                                            __rustylr_node = node.parent.unwrap();
                                            (node.data.unwrap(), node.tree.unwrap().to_tree0())
                                        }
                                        Err(rc_node) => {
                                            __rustylr_node = std::rc::Rc::clone(rc_node.parent.as_ref().unwrap());
                                            (rc_node.data.as_ref().unwrap().clone(), rc_node.tree.as_ref().unwrap().to_tree0())
                                        }
                                    };
                                    let mut #mapto = if let #node_enum_name::#variant_name(#mapto) = __rustylr_data {
                                        #mapto
                                    } else {
                                        unreachable!()
                                    };
                                    let #ruleid_mapto = #tree_token_ident.rule_id();
                                });
                            }
                            None => {
                                extract_data_from_node_enum.extend(quote! {
                                    let #tree_token_ident = match std::rc::Rc::try_unwrap(__rustylr_node) {
                                        Ok(node) => {
                                            __rustylr_node = node.parent.unwrap();
                                            node.tree.unwrap().to_tree0()
                                        }
                                        Err(rc_node) => {
                                            __rustylr_node = std::rc::Rc::clone(rc_node.parent.as_ref().unwrap());
                                            rc_node.tree.as_ref().unwrap().to_tree0()
                                        }
                                    };
                                });
                            }
                        }
                    } else {
                        extract_data_from_node_enum.extend(quote! {
                            let #tree_token_ident = match std::rc::Rc::try_unwrap(__rustylr_node) {
                                Ok(node) => {
                                    __rustylr_node = node.parent.unwrap();
                                    node.tree.unwrap().to_tree0()
                                }
                                Err(rc_node) => {
                                    __rustylr_node = std::rc::Rc::clone(rc_node.parent.as_ref().unwrap());
                                    rc_node.tree.as_ref().unwrap().to_tree0()
                                }
                            };
                        });
                    }
                }
                let mut tree_initializer = TokenStream::new();
                for (token_idx, _) in rule.tokens.iter().enumerate() {
                    let tree_token_ident = format_ident!("__rustylr_tree_token_{}", token_idx);
                    tree_initializer.extend(quote! {
                        #tree_token_ident,
                    });
                }
                extract_data_from_node_enum.extend(quote! {
                    let __rustylr_tree = vec![#tree_initializer];
                    let tree:&[#module_prefix::glr::Tree0] = &__rustylr_tree;
                });

                let reduce_fn_ident = format_ident!("reduce_{}_{}", name, rule_local_id);

                case_streams.extend(quote! {
                    #rule_index => {
                        Self::#reduce_fn_ident( node, lookahead, #user_data_var )?
                    }
                });

                // if typename is defined for this rule, push result of action to stack
                // else, just execute action
                let typename = self.nonterm_typenames.get(name);
                if typename.is_some() {
                    // typename is defined, reduce action must be defined
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(
                                mut __rustylr_node: std::rc::Rc<Self>,
                                lookahead: &#token_typename,
                                #user_data_parameter_def
                            ) -> Result<(#node_enum_name,std::rc::Rc<Self>,Vec<#tree0_typename>), #reduce_error_typename> {
                                #extract_data_from_node_enum

                                Ok(( #node_enum_name::#name(#action), __rustylr_node, __rustylr_tree ))
                            }
                        });
                    } else {
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
                            fn_reduce_for_each_rule_stream.extend(quote! {
                                fn #reduce_fn_ident(
                                    mut __rustylr_node: std::rc::Rc<Self>,
                                    lookahead: &#token_typename,
                                    #user_data_parameter_def
                                ) -> Result<(#node_enum_name,std::rc::Rc<Self>, Vec<#tree0_typename>), #reduce_error_typename> {
                                    #extract_data_from_node_enum

                                    Ok(( #node_enum_name::#name(#unique_mapto), __rustylr_node, __rustylr_tree ))
                                }
                            });
                        } else {
                            return Err(Box::new(EmitError::RuleTypeDefinedButActionNotDefined {
                                name: name.clone(),
                                rule_local_id,
                            }));
                        }
                    }
                } else {
                    // <RuleType> is not defined,
                    // just execute action
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {

                            fn #reduce_fn_ident(
                                mut __rustylr_node: std::rc::Rc<Self>,
                                lookahead: &#token_typename,
                                #user_data_parameter_def
                            ) -> Result<(#node_enum_name,std::rc::Rc<Self>,Vec<#tree0_typename>), #reduce_error_typename> {
                                #extract_data_from_node_enum
                                #action

                                Ok(( #node_enum_name::#name, __rustylr_node, __rustylr_tree ))
                            }
                        });
                    } else {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(
                                mut __rustylr_node: std::rc::Rc<Self>,
                                lookahead: &#token_typename,
                                #user_data_parameter_def
                            ) -> Result<(#node_enum_name,std::rc::Rc<Self>,Vec<#tree0_typename>), #reduce_error_typename> {
                                #extract_data_from_node_enum

                                Ok(( #node_enum_name::#name, __rustylr_node, __rustylr_tree ))
                            }
                        });
                    }
                }

                rule_index += 1;
            }
        }

        let mut derives_stream = TokenStream::new();
        for derive in &self.derives {
            derives_stream.extend(quote! {
                #derive,
            });
        }
        derives_stream = if self.derives.is_empty() {
            TokenStream::new()
        } else {
            quote! {
                #[derive(#derives_stream)]
            }
        };

        let (start_typename, start_extract) =
            if let Some(start_typename) = self.nonterm_typenames.get(&self.start_rule_name) {
                let startname = &self.start_rule_name;
                (
                    start_typename.clone(),
                    quote! {
                        {
                            let data_enum = std::rc::Rc::into_inner(node).unwrap().data.unwrap();
                            if let #node_enum_name::#startname(start) = data_enum {
                                start
                            } else {
                                unreachable!()
                            }
                        }
                    },
                )
            } else {
                (quote! {()}, quote! {()})
            };

        // enum data type
        let mut enum_variants_stream = quote! {
            #terms_enum_name(#token_typename),
        };
        for name in self.rules_index.iter() {
            if let Some(typename) = self.nonterm_typenames.get(name) {
                enum_variants_stream.extend(quote! {
                    #name(#typename),
                });
            } else {
                enum_variants_stream.extend(quote! {
                    #name,
                });
            }
        }

        Ok(quote! {
        /// enum for each non-terminal and terminal symbol, that actually hold data
        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
        #[derive(Clone)]
        pub enum #node_enum_name {
            #enum_variants_stream
        }


        /// Each node represents single state transition.
        /// Either shifting one terminal or non-temrinal symbol.
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        #derives_stream
        pub struct #node_struct_name {
            data: Option<#node_enum_name>,
            state: usize,
            parent: Option<std::rc::Rc<Self>>,
            tree: Option<#module_prefix::glr::Tree1>,
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #node_struct_name {
            #fn_reduce_for_each_rule_stream
        }

        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        impl #module_prefix::glr::node::Node for #node_struct_name {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;
            type ReduceActionError = #reduce_error_typename;
            type UserData = #user_data_typename;

            fn state(&self) -> usize {
                self.state
            }
            fn set_state(&mut self, state: usize) {
                self.state = state;
            }
            fn parent(&self) -> Option<&std::rc::Rc<Self>> {
                self.parent.as_ref()
            }

            fn make_term_children(parent: std::rc::Rc<Self>, state: usize, term: #token_typename) -> Self {
                Self {
                    data: Some(#node_enum_name::#terms_enum_name(term)),
                    state,
                    parent: Some(parent),
                    tree: Some(#module_prefix::glr::Tree1::Terminal),
                }
            }
            fn reduce(
                node: std::rc::Rc<Self>,
                rule_index: usize,
                rule_id: usize,
                lookahead: &Self::Term,
                data: &mut #user_data_typename,
            ) -> Result<Self, #reduce_error_typename> {
                let (data, parent, tree) = match rule_index {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rule_index );
                    }
                };


                Ok(
                    Self {
                        data: Some(data),
                        state: 0,
                        parent: Some(parent),
                        tree: Some(
                            #module_prefix::glr::Tree1::NonTerminal(
                                #module_prefix::glr::TreeNonTerminal1 {
                                    rule: rule_index,
                                    ruleid: rule_id,
                                    tokens: tree,
                                }
                            )
                        ),
                    }
                )
            }

            fn tree(&self) -> Option<&#module_prefix::glr::Tree1> {
                self.tree.as_ref()
            }
        }



        /// Context is holding multiple nodes, that represents current state of parser.
        /// If there are multiple nodes, it means parser is in ambiguous state.
        /// It must be resolved(merged) into single node by feeding more terminal symbols.
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        #derives_stream
        pub struct #context_struct_name {
            pub current_nodes: #module_prefix::glr::node::NodeSet<#node_struct_name>,
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #context_struct_name {
            pub fn new() -> Self {
                let mut nodeset = #module_prefix::glr::node::NodeSet::new();
                nodeset.nodes.push(
                    std::rc::Rc::new(
                        #node_struct_name {
                            data: None,
                            state: 0,
                            parent: None,
                            tree: None,
                        }
                    )
                );
                Self {
                    current_nodes: nodeset,
                }
            }

            /// pop value from start rule
            #[inline]
            pub fn accept(self, parser: &#parser_struct_name) -> Result<#start_typename,#multiple_path_error_typename> {
                let node = self.current_nodes.accept(
                    parser
                )?;
                let data = #start_extract;
                Ok(data)
            }
        }
        impl #module_prefix::glr::Context for #context_struct_name {
            type Node = #node_struct_name;

            fn take_current_nodes(&mut self) -> #module_prefix::glr::node::NodeSet<#node_struct_name> {
                std::mem::take( &mut self.current_nodes )
            }
            fn is_empty(&self) -> bool {
                self.current_nodes.is_empty()
            }

            fn get_current_nodes_mut(&mut self) -> &mut #module_prefix::glr::node::NodeSet<#node_struct_name> {
                &mut self.current_nodes
            }
        }


        /// struct that holds parser data, DFA tables
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        pub struct #parser_struct_name {
            /// production rules
            pub rules: Vec<#rule_typename>,
            /// states
            pub states: Vec<#state_typename>,
        }
        impl #module_prefix::glr::Parser for #parser_struct_name {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;

            fn get_rules(&self) -> &[#rule_typename] {
                &self.rules
            }
            fn get_states(&self) -> &[#state_typename] {
                &self.states
            }
        }

        /// Parser is holding DFA state tables and production rules
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        impl #parser_struct_name {
            /// feed one terminal to parser, and update state stack
            pub fn feed(
                &self,
                context: &mut #context_struct_name,
                term: #token_typename,
                #user_data_parameter_def
            ) -> Result<(), #invalid_terminal_error_typename> {
                #module_prefix::glr::feed(
                    self,
                    context,
                    term,
                    #user_data_dummy,
                )
            }
            /// Create new context for parsing
            #[inline]
            pub fn begin(&self) -> #context_struct_name {
                #context_struct_name::new()
            }

            /// Create new parser instance.
            /// Parser can be reused with different context, for multiple parsing.
            pub fn new() -> Self {
                #grammar_emit
                Self {
                    rules,
                    states,
                }
            }
        }

        })
    }

    pub fn emit_compiletime(&self, lalr: bool) -> Result<TokenStream, Box<EmitError>> {
        let type_alias_emit = self.emit_type_alises();
        let enum_emit = self.emit_nonterm_enum();
        let grammar_emit = self.emit_grammar_compiletime(lalr)?;
        let parser_emit = if self.glr {
            self.emit_parser_glr(grammar_emit)?
        } else {
            self.emit_parser(grammar_emit)?
        };

        Ok(quote! {
            #type_alias_emit
            #enum_emit
            #parser_emit
        })
    }
}
