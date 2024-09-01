use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use rusty_lr_core::builder::error::BuildError;
use rusty_lr_core::HashMap;
use rusty_lr_core::ShiftedRule;
use rusty_lr_core::Token;

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
        let enum_name = format_ident!("{}NonTerminals", start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let parse_error_typename = format_ident!("{}ParseError", start_rule_name);
        let invalid_terminal_error = format_ident!("{}InvalidTerminalError", start_rule_name);
        let context_struct_name = format_ident!("{}Context", start_rule_name);

        #[cfg(feature = "tree")]
        let tree_typename = format_ident!("{}Tree", start_rule_name);
        #[cfg(feature = "tree")]
        let treelist_typename = format_ident!("{}TreeList", start_rule_name);
        #[cfg(feature = "tree")]
        let treenonterm_typename = format_ident!("{}TreeNonTerminal", start_rule_name);
        #[cfg(feature = "tree")]
        let tree_alias = quote! {
            /// type alias for `Tree`
            pub type #tree_typename = #module_prefix::Tree<#token_typename, #enum_name>;

            /// type alias for `TreeNonTerminal`
            pub type #treenonterm_typename = #module_prefix::TreeNonTerminal<#token_typename, #enum_name>;

            /// type alias for `TreeList`
            pub type #treelist_typename = #module_prefix::TreeList<#token_typename, #enum_name>;
        };

        #[cfg(not(feature = "tree"))]
        let tree_alias = quote! {};

        if self.glr {
            let multiple_path_error = format_ident!("{}MultiplePathError", start_rule_name);
            let node_enum_name = format_ident!("{}NodeEnum", start_rule_name);

            quote! {
                /// type alias for `Context`
                #[allow(non_camel_case_types,dead_code)]
                pub type #context_struct_name = #module_prefix::glr::Context<#node_enum_name>;
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
                pub type #multiple_path_error = #module_prefix::glr::MultiplePathError;

                #tree_alias
            }
        } else {
            let stack_struct_name = format_ident!("{}Stack", start_rule_name);
            quote! {
                /// type alias for `Context`
                #[allow(non_camel_case_types,dead_code)]
                pub type #context_struct_name = #module_prefix::lr::Context<#stack_struct_name>;
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

                #tree_alias
            }
        }
    }

    /// write enum that represents non-terminal symbols, including Augmented
    fn emit_nonterm_enum(&self) -> TokenStream {
        // =====================================================================
        // =====================Writing NonTerminal Enum========================
        // =====================================================================

        let start_rule_name = &self.start_rule_name;
        let enum_typename = format_ident!("{}NonTerminals", start_rule_name);

        // for impl `Display` and `Debug`
        let mut comma_separated_variants = TokenStream::new();
        let mut case_display = TokenStream::new();
        for nonterm in self.nonterminals.iter() {
            let name = &nonterm.name;
            comma_separated_variants.extend(quote! {
                #name,
            });

            let display_str = &nonterm.pretty_name;
            case_display.extend(quote! {
                #enum_typename::#name=> write!(f, #display_str),
            });
        }

        quote! {
            /// An enum that represents non-terminal symbols
            #[allow(non_camel_case_types)]
            #[derive(Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
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
            impl std::fmt::Debug for #enum_typename {
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
        let augmented_idx = *self
            .nonterminals_index
            .get(&Ident::new(utils::AUGMENTED_NAME, Span::call_site()))
            .unwrap();
        let dfa = if lalr {
            grammar.build_lalr(augmented_idx)
        } else {
            grammar.build(augmented_idx)
        };
        let dfa = match dfa {
            Ok(dfa) => dfa,
            Err(e) => {
                // to map production rule to its pretty name abbreviation
                let term_mapper = |term_idx: usize| self.terminals[term_idx].name.to_string();
                let nonterm_mapper =
                    |nonterm_idx: usize| self.nonterminals[nonterm_idx].pretty_name.clone();
                match e {
                    BuildError::NoAugmented | BuildError::RuleNotFound(_) => {
                        unreachable!("Unreachable grammar build error")
                    }
                    BuildError::ReduceReduceConflict {
                        lookahead,
                        rule1,
                        rule2,
                    } => {
                        return Err(Box::new(EmitError::ReduceReduceConflict {
                            lookahead: self.terminals[lookahead].name.clone(),
                            rule1: (
                                rule1,
                                grammar.rules[rule1]
                                    .0
                                    .clone()
                                    .map(term_mapper, nonterm_mapper),
                            ),
                            rule2: (
                                rule2,
                                grammar.rules[rule2]
                                    .0
                                    .clone()
                                    .map(term_mapper, nonterm_mapper),
                            ),
                        }));
                    }
                    BuildError::ShiftReduceConflict {
                        reduce,
                        shift,
                        term,
                    } => {
                        let mut shift_rules = Vec::new();
                        for (r, _) in shift.rules.into_iter() {
                            let shifted_rule = ShiftedRule {
                                rule: grammar.rules[r.rule]
                                    .0
                                    .clone()
                                    .map(term_mapper, nonterm_mapper),
                                shifted: r.shifted,
                            };
                            shift_rules.push((r.rule, shifted_rule));
                        }

                        return Err(Box::new(EmitError::ShiftReduceConflict {
                            term: self.terminals[term].name.clone(),
                            reduce_rule: (
                                reduce,
                                grammar.rules[reduce]
                                    .0
                                    .clone()
                                    .map(term_mapper, nonterm_mapper),
                            ),
                            shift_rules,
                        }));
                    }
                }
            }
        };

        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let rule_typename = format_ident!("{}Rule", &self.start_rule_name);
        let state_typename = format_ident!("{}State", &self.start_rule_name);

        // write vector of terminals
        let comma_separated_terminals = {
            let mut comma_separated_terminals = TokenStream::new();
            for term in self.terminals.iter() {
                let stream = &term.body;
                comma_separated_terminals.extend(quote! {
                    #stream,
                });
            }
            comma_separated_terminals
        };

        let (rules, states) = {
            let term_mapper = |term_idx: usize| term_idx;
            let nonterm_mapper = |nonterm_idx: usize| self.nonterminals[nonterm_idx].name.clone();
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
        // get TokenStream of typename that can represent 'count'
        fn integer_typename(count: usize) -> TokenStream {
            if count < 256 {
                quote! {u8}
            } else if count < 65536 {
                quote! {u16}
            } else {
                quote! {usize}
            }
        }

        let rule_index_typename = integer_typename(rules.len());
        let state_index_typename = integer_typename(states.len());
        let terminal_index_typename = integer_typename(self.terminals_index.len());

        let shift_typename =
            integer_typename(rules.iter().map(|rule| rule.rule.len()).max().unwrap());

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
                            let term = Literal::usize_unsuffixed(term);
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
                let id = Literal::usize_unsuffixed(rule.id);
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

        let (reduce_terminals_cache_count, ruleset0_cache_count) = {
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
                        let term = Literal::usize_unsuffixed(term);
                        let goto = Literal::usize_unsuffixed(goto);
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
                        let goto = Literal::usize_unsuffixed(goto);
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
                                let term = Literal::usize_unsuffixed(term);
                                init_terminals_comma_separated.extend(quote! {
                                    #term,
                                });
                            }

                            reduce_terminals_cache_initializer.extend(quote! {
                                &[#init_terminals_comma_separated],
                            });

                            len
                        };

                        let ruleid = Literal::usize_unsuffixed(ruleid);
                        let terminal_set_num = Literal::usize_unsuffixed(terminal_set_num);
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
                                let ruleid = Literal::usize_unsuffixed(rule.rule);
                                let shifted = Literal::usize_unsuffixed(rule.shifted);
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
                            let ruleid = Literal::usize_unsuffixed(ruleid);
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

                let ruleset0_id = Literal::usize_unsuffixed(ruleset0_id);
                ruleset_shifted0_initialiezr.extend(quote! {
                    #ruleset0_id,
                });
            }

            (reduce_terminals_map.len(), ruleset0_map.len())
        };
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

        let rule_id_typename = integer_typename(
            self.nonterminals
                .iter()
                .map(|nonterm| nonterm.rules.iter().map(|rule| rule.id).max().unwrap())
                .max()
                .unwrap()
                + 1,
        );
        let reduce_terminals_cache_typename = integer_typename(reduce_terminals_cache_count);
        let ruleset0_cache_typename = integer_typename(ruleset0_cache_count);

        Ok(quote! {
            let __rustylr_terminals = vec![#comma_separated_terminals];
            const RUSTYLR_RULES_TOKENS: &[&[#module_prefix::Token<#terminal_index_typename, #nonterminals_enum_name>]] = &[#tokens_initializer];
            const RUSTYLR_RULES_NAME: &[#nonterminals_enum_name] = &[#rule_names_initializer];
            const RUSTYLR_RULES_ID: &[#rule_id_typename] = &[#rule_id_initializer];

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
                        id: *id as usize,
                    }
                }
            ).collect();

            const RUSTYLR_REDUCE_TERMINALS_CACHE: &[&[#terminal_index_typename]] = &[#reduce_terminals_cache_initializer];
            const RUSTYLR_RULESET_SHIFTED0_CACHE: &[&[#rule_index_typename]] = &[#ruleset_shifted0_cache_initializer];
            const RUSTYLR_SHIFT_TERM_MAP: &[&[(#terminal_index_typename, #state_index_typename)]] = &[#shift_term_initializer];
            const RUSTYLR_SHIFT_NONTERM_MAP: &[&[(#nonterminals_enum_name, #state_index_typename)]] = &[#shift_nonterm_initializer];
            const RUSTYLR_REDUCE_MAP: &[&[(#terminal_index_typename, #reduce_terminals_cache_typename)]] = &[#reduce_initializer];
            const RUSTYLR_RULESET_MAP: &[&[(#rule_index_typename,#shift_typename)]] = &[#ruleset_initializer];
            const RUSTYLR_RULESET_SHIFTED0_MAP: &[#ruleset0_cache_typename] = &[#ruleset_shifted0_initialiezr];

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
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let parseerror_typename = format_ident!("{}ParseError", self.start_rule_name);
        let rule_typename = format_ident!("{}Rule", self.start_rule_name);
        let state_typename = format_ident!("{}State", self.start_rule_name);
        let parser_struct_name = format_ident!("{}Parser", self.start_rule_name);
        let context_struct_name = format_ident!("{}Context", self.start_rule_name);
        let stack_struct_name = format_ident!("{}Stack", self.start_rule_name);
        let token_typename = &self.token_typename;
        let terms_stack_name = Ident::new(utils::TERMINAL_STACK_NAME, Span::call_site());

        // stack name for each non-terminal
        let mut stack_names_for_nonterm = Vec::with_capacity(self.nonterminals.len());

        // (<RuleType as ToString>, stack_name) map
        let mut ruletype_stackname_map: HashMap<String, Ident> = HashMap::default();

        // (stack_name, TokenStream for typename) sorted in insertion order
        // for consistent output
        let mut stack_names_in_order = Vec::new();

        // insert terminal token type
        ruletype_stackname_map.insert(self.token_typename.to_string(), terms_stack_name.clone());
        stack_names_in_order.push((terms_stack_name.clone(), self.token_typename.clone()));

        for nonterm in self.nonterminals.iter() {
            if let Some(ruletype) = &nonterm.ruletype {
                let ruletype_str = ruletype.to_string();

                let cur_len = ruletype_stackname_map.len();
                let stack_name = ruletype_stackname_map
                    .entry(ruletype_str)
                    .or_insert_with(|| {
                        let new_stack_name = Ident::new(
                            &format!("__rustylr_generated_stack_{}", cur_len),
                            Span::call_site(),
                        );
                        stack_names_in_order.push((new_stack_name.clone(), ruletype.clone()));
                        new_stack_name
                    })
                    .clone();

                stack_names_for_nonterm.push(Some(stack_name));
            } else {
                stack_names_for_nonterm.push(None);
            }
        }
        drop(ruletype_stackname_map);

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

        // TokenStream to define reduce function for each production rule
        let mut fn_reduce_for_each_rule_stream = TokenStream::new();

        // define reduce function for each production rule
        for (nonterm_idx, nonterm) in self.nonterminals.iter().enumerate() {
            for (rule_local_id, rule) in nonterm.rules.iter().enumerate() {
                let mut token_pop_stream = TokenStream::new();
                for token in rule.tokens.iter().rev() {
                    match &token.token {
                        Token::Term(_) => {
                            if let Some(mapto) = &token.mapto {
                                token_pop_stream.extend(quote! {
                                    let mut #mapto = self.#terms_stack_name.pop().unwrap();
                                });
                            } else {
                                token_pop_stream.extend(quote! {
                                    self.#terms_stack_name.pop();
                                });
                            }
                        }
                        Token::NonTerm(nonterm_idx) => {
                            if let Some(stack_name) = &stack_names_for_nonterm[*nonterm_idx] {
                                if let Some(mapto) = &token.mapto {
                                    token_pop_stream.extend(quote! {
                                        let mut #mapto = self.#stack_name.pop().unwrap();
                                    });
                                } else {
                                    token_pop_stream.extend(quote! {
                                        self.#stack_name.pop();
                                    });
                                }
                            }
                        }
                    }
                }

                let reduce_fn_ident = format_ident!("reduce_{}_{}", nonterm.name, rule_local_id);

                case_streams.extend(quote! {
                    #ruleid => {
                        self.#reduce_fn_ident( lookahead, #user_data_var )
                    }
                });

                // if typename is defined for this rule, push result of action to stack
                // else, just execute action
                if nonterm.ruletype.is_some() {
                    // push result to this stack
                    let stack_name = stack_names_for_nonterm[nonterm_idx]
                        .as_ref()
                        .unwrap_or_else(|| {
                            unreachable!("Non-terminal stack name must be defined for <RuleType>")
                        });

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
                                name: nonterm.name.clone(),
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
        let start_nonterm_idx = self.nonterminals_index.get(&self.start_rule_name).unwrap();
        let (start_typename, pop_from_start_rule_stack) = {
            if let Some(start_stack_name) = &stack_names_for_nonterm[*start_nonterm_idx] {
                (
                    self.nonterminals[*start_nonterm_idx]
                        .ruletype
                        .as_ref()
                        .unwrap()
                        .clone(),
                    quote! {
                        self.#start_stack_name.pop().unwrap()
                    },
                )
            } else {
                (quote! {()}, quote! {()})
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

        Ok(quote! {
        /// struct that holds internal parser data,
        /// including data stack for each non-terminal,
        /// and state stack for DFA
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        pub struct #stack_struct_name {
            #stack_def_streams
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #stack_struct_name {
            #fn_reduce_for_each_rule_stream
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #module_prefix::lr::Stack for #stack_struct_name {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;
            type ReduceActionError = #reduce_error_typename;
            type UserData = #user_data_typename;

            type StartType = #start_typename;

            fn new() -> Self {
                Self {
                    #stack_init_streams
                }
            }
            fn push( &mut self, term: Self::Term ) {
                self.#terms_stack_name.push(term);
            }
            fn reduce(&mut self,
                rustylr_macro_generated_ruleid__: usize,
                data: &mut Self::UserData,
                lookahead: &Self::Term,
            ) -> Result<(), Self::ReduceActionError> {
                match rustylr_macro_generated_ruleid__ {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                    }
                }
            }

            fn pop_start(&mut self) -> Self::StartType {
                #pop_from_start_rule_stack
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
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let rule_typename = format_ident!("{}Rule", self.start_rule_name);
        let state_typename = format_ident!("{}State", self.start_rule_name);
        let parser_struct_name = format_ident!("{}Parser", self.start_rule_name);
        let context_struct_name = format_ident!("{}Context", self.start_rule_name);
        let node_enum_name = format_ident!("{}NodeEnum", self.start_rule_name);
        let token_typename = &self.token_typename;
        let invalid_terminal_error_typename =
            format_ident!("{}InvalidTerminalError", self.start_rule_name);
        let terms_variant_name = Ident::new("Terminals", Span::call_site());

        // enum variant name for each non-terminal
        let mut variant_names_for_nonterm = Vec::with_capacity(self.nonterminals.len());

        // (<RuleType as ToString>, variant_name) map
        let mut ruletype_variant_map: HashMap<String, Ident> = HashMap::default();

        // (variant_name, TokenStream for typename) sorted in insertion order
        // for consistent output
        let mut variant_names_in_order = Vec::new();

        // insert variant for empty-ruletype
        ruletype_variant_map.insert(
            "".to_string(),
            Ident::new("NonTerminals", Span::call_site()),
        );
        variant_names_in_order.push((Ident::new("NonTerminals", Span::call_site()), quote! {}));

        // insert variant for terminal token type
        ruletype_variant_map.insert(self.token_typename.to_string(), terms_variant_name.clone());
        variant_names_in_order.push((terms_variant_name.clone(), self.token_typename.clone()));

        for nonterm in self.nonterminals.iter() {
            let (ruletype_stream, ruletype_string) = if let Some(ruletype) = &nonterm.ruletype {
                (ruletype.clone(), ruletype.to_string())
            } else {
                (quote! {}, "".to_string())
            };

            let cur_len = ruletype_variant_map.len();
            let variant_name = ruletype_variant_map
                .entry(ruletype_string)
                .or_insert_with(|| {
                    let new_variant_name =
                        Ident::new(&format!("Variant{}", cur_len), Span::call_site());
                    variant_names_in_order
                        .push((new_variant_name.clone(), ruletype_stream.clone()));
                    new_variant_name
                })
                .clone();
            variant_names_for_nonterm.push(variant_name);
        }
        drop(ruletype_variant_map);

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

        for (nonterm_idx, nonterm) in self.nonterminals.iter().enumerate() {
            for (rule_local_id, rule) in nonterm.rules.iter().enumerate() {
                let mut extract_data_from_node_enum = TokenStream::new();
                for token in rule.tokens.iter() {
                    match &token.token {
                        Token::Term(_) => match &token.mapto {
                            Some(mapto) => {
                                extract_data_from_node_enum.extend(quote! {
                                    let mut #mapto = if let #node_enum_name::#terms_variant_name(#mapto) = __rustylr_args.pop().unwrap() {
                                        #mapto
                                    } else {
                                        unreachable!()
                                    };
                                });
                            }
                            None => {
                                extract_data_from_node_enum.extend(quote! {
                                    __rustylr_args.pop();
                                });
                            }
                        },
                        Token::NonTerm(nonterm_idx) => {
                            match &self.nonterminals[*nonterm_idx].ruletype {
                                Some(_) => {
                                    let variant_name = &variant_names_for_nonterm[*nonterm_idx];
                                    match &token.mapto {
                                        Some(mapto) => {
                                            extract_data_from_node_enum.extend(quote! {
                                            let mut #mapto = if let #node_enum_name::#variant_name(#mapto) = __rustylr_args.pop().unwrap() {
                                                #mapto
                                            } else {
                                                unreachable!()
                                            };
                                            });
                                        }
                                        None => {
                                            extract_data_from_node_enum.extend(quote! {
                                                __rustylr_args.pop();
                                            });
                                        }
                                    }
                                }
                                None => {
                                    extract_data_from_node_enum.extend(quote! {
                                        __rustylr_args.pop();
                                    });
                                }
                            }
                        }
                    }
                }
                let reduce_fn_ident = format_ident!("reduce_{}_{}", nonterm.name, rule_local_id);

                case_streams.extend(quote! {
                    #rule_index => {
                        Self::#reduce_fn_ident( &mut context.reduce_args, shift, lookahead, #user_data_var )
                    }
                });

                // if typename is defined for this rule, push result of action to stack
                // else, just execute action
                if nonterm.ruletype.is_some() {
                    // typename is defined, reduce action must be defined
                    let variant_name = &variant_names_for_nonterm[nonterm_idx];
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(
                                __rustylr_args: &mut Vec<#node_enum_name>,
                                shift: &mut bool,
                                lookahead: &#token_typename,
                                #user_data_parameter_def
                            ) -> Result<#node_enum_name, #reduce_error_typename> {
                                #extract_data_from_node_enum

                                Ok( #node_enum_name::#variant_name(#action) )
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
                                    __rustylr_args: &mut Vec<#node_enum_name>,
                                    shift: &mut bool,
                                    lookahead: &#token_typename,
                                    #user_data_parameter_def
                                ) -> Result<#node_enum_name, #reduce_error_typename> {
                                    #extract_data_from_node_enum

                                    Ok( #node_enum_name::#variant_name(#unique_mapto) )
                                }
                            });
                        } else {
                            return Err(Box::new(EmitError::RuleTypeDefinedButActionNotDefined {
                                name: nonterm.name.clone(),
                                rule_local_id,
                            }));
                        }
                    }
                } else {
                    // <RuleType> is not defined,
                    // just execute action

                    let variant_name = &variant_names_for_nonterm[nonterm_idx];
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {

                            fn #reduce_fn_ident(
                                __rustylr_args: &mut Vec<#node_enum_name>,
                                shift: &mut bool,
                                lookahead: &#token_typename,
                                #user_data_parameter_def
                            ) -> Result<#node_enum_name, #reduce_error_typename> {
                                #extract_data_from_node_enum
                                #action

                                Ok( #node_enum_name::#variant_name )
                            }
                        });
                    } else {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(
                                __rustylr_args: &mut Vec<#node_enum_name>,
                                shift: &mut bool,
                                lookahead: &#token_typename,
                                #user_data_parameter_def
                            ) -> Result<#node_enum_name, #reduce_error_typename> {
                                __rustylr_args.clear();
                                Ok( #node_enum_name::#variant_name )
                            }
                        });
                    }
                }

                rule_index += 1;
            }
        }

        let start_idx = self.nonterminals_index.get(&self.start_rule_name).unwrap();
        let (start_typename, start_extract) =
            if let Some(start_typename) = self.nonterminals[*start_idx].ruletype.as_ref() {
                let start_variant_name = &variant_names_for_nonterm[*start_idx];
                (
                    start_typename.clone(),
                    quote! {
                        match self {
                            #node_enum_name::#start_variant_name(start) => start,
                            _ => unreachable!(),
                        }
                    },
                )
            } else {
                (quote! {()}, quote! {()})
            };

        // enum data type
        let mut enum_variants_stream = TokenStream::new();
        for (variant_name, typename) in variant_names_in_order.into_iter() {
            if typename.is_empty() {
                enum_variants_stream.extend(quote! {
                    #variant_name,
                });
            } else {
                enum_variants_stream.extend(quote! {
                    #variant_name(#typename),
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

        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #node_enum_name {
            #fn_reduce_for_each_rule_stream
        }


        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types, unused_variables)]
        impl #module_prefix::glr::NodeData for #node_enum_name {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;
            type ReduceActionError = #reduce_error_typename;
            type UserData = #user_data_typename;

            type StartType = #start_typename;

            fn new_term(term: #token_typename) -> Self {
                #node_enum_name::#terms_variant_name(term)
            }
            fn new_nonterm(
                rule_index: usize,
                context: &mut #module_prefix::glr::Context<Self>,
                shift: &mut bool,
                lookahead: &Self::Term,
                data: &mut Self::UserData,
            ) -> Result<Self, Self::ReduceActionError> {
                match rule_index {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rule_index );
                    }
                }
            }

            fn into_start(self) -> Self::StartType {
                #start_extract
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
