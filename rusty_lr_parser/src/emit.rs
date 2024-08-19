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

        quote! {
            /// type alias for CFG production rule
            #[allow(non_camel_case_types,dead_code)]
            pub type #rule_typename = #module_prefix::ProductionRule<#token_typename, #enum_name>;
            /// type alias for DFA state
            #[allow(non_camel_case_types,dead_code)]
            pub type #state_typename = #module_prefix::State<#token_typename, #enum_name>;
            /// type alias for `ParseError`
            #[allow(non_camel_case_types,dead_code)]
            pub type #parse_error_typename = #module_prefix::ParseError<#token_typename, #reduce_error_typename>;
            /// type alias for `InvalidTerminalError`
            #[allow(non_camel_case_types,dead_code)]
            pub type #invalid_terminal_error = #module_prefix::InvalidTerminalError<#token_typename>;
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
                        rule1: (rule1, grammar.rules[rule1].clone()),
                        rule2: (rule2, grammar.rules[rule2].clone()),
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
                            rule: grammar.rules[r.rule].clone(),
                            shifted: r.shifted,
                        };
                        shift_rules.push((r.rule, shifted_rule));
                    }

                    return Err(Box::new(EmitError::ShiftReduceConflict {
                        term,
                        reduce_rule: (reduce, grammar.rules[reduce].clone()),
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
                .map(|r| r.map(term_mapper, nonterm_mapper))
                .collect();
            let states: Vec<_> = dfa
                .states
                .into_iter()
                .map(|s| s.to_export().map(term_mapper, nonterm_mapper))
                .collect();
            (rules, states)
        };

        // =====================================================================
        // ==================Writing Production Rules===========================
        // =====================================================================
        let (comma_separated_rule_names, comma_separated_rule_tokens) = {
            let mut comma_separated_rule_names = TokenStream::new();
            let mut comma_separated_rule_tokens = TokenStream::new();
            for rule in rules.into_iter() {
                let mut comma_separated_tokens = TokenStream::new();
                for token in rule.rule.into_iter() {
                    match token {
                        rlr::Token::Term(term) => {
                            comma_separated_tokens
                                .extend(quote! {#module_prefix::Token::Term(#term),});
                        }
                        rlr::Token::NonTerm(nonterm) => {
                            comma_separated_tokens
                                .extend(quote! {#module_prefix::Token::NonTerm(#nonterminals_enum_name::#nonterm),});
                        }
                    }
                }

                let nonterm_name = &rule.name;
                comma_separated_rule_names.extend(quote! {
                    #nonterminals_enum_name::#nonterm_name,
                });
                comma_separated_rule_tokens.extend(quote! {
                    vec![ #comma_separated_tokens ],
                });
            }
            (comma_separated_rule_names, comma_separated_rule_tokens)
        };

        // =====================================================================
        // =========================Writing States==============================
        // =====================================================================
        let mut comma_separated_reduce_terminal_sets = TokenStream::new();

        let (
            shift_term_initializer,
            shift_nonterm_initializer,
            reduce_map_initializer,
            ruleset_initializer,
        ) = {
            // when generating code for 'inserting tokens to reduce_map',
            // there could be multiple lookahead tokens for one rule
            // inserting all of them one by one is inefficient
            let mut reduce_terminals_map = BTreeMap::new();

            let mut shift_term_initializer = TokenStream::new();
            let mut shift_nonterm_initializer = TokenStream::new();
            let mut reduce_map_initializer = TokenStream::new();
            let mut ruleset_initializer = TokenStream::new();

            for state in states.into_iter() {
                // use BTreeMap to sort keys, for consistent output
                let shift_term = {
                    let mut term_state_comma_separated = TokenStream::new();
                    let shift_goto_map_term: BTreeMap<_, _> =
                        state.shift_goto_map_term.into_iter().collect();
                    for (term, goto) in shift_goto_map_term.into_iter() {
                        term_state_comma_separated.extend(quote! {
                            (#term, #goto),
                        });
                    }
                    quote! {
                        vec![#term_state_comma_separated],
                    }
                };

                let shift_nonterm = {
                    let mut nonterm_state_comma_separated = TokenStream::new();
                    let shift_goto_map_nonterm: BTreeMap<_, _> =
                        state.shift_goto_map_nonterm.into_iter().collect();
                    for (nonterm, goto) in shift_goto_map_nonterm.into_iter() {
                        nonterm_state_comma_separated.extend(quote! {
                            (#nonterminals_enum_name::#nonterm, #goto),
                        });
                    }
                    quote! {
                        vec![#nonterm_state_comma_separated],
                    }
                };

                let mut reduce_map_by_rule_id = BTreeMap::new();
                for (term, ruleid) in state.reduce_map.into_iter() {
                    reduce_map_by_rule_id
                        .entry(ruleid)
                        .or_insert_with(BTreeSet::new)
                        .insert(term);
                }
                let reduce_map = {
                    let mut init_reduce_comma_separated_terminals_rules = TokenStream::new();
                    for (ruleid, tokens) in reduce_map_by_rule_id.into_iter() {
                        let terminal_set_num = if let Some(id) = reduce_terminals_map.get(&tokens) {
                            *id
                        } else {
                            let len = reduce_terminals_map.len();
                            reduce_terminals_map.insert(tokens.clone(), len);

                            let mut init_terminals_comma_separated = TokenStream::new();
                            for term in tokens.into_iter() {
                                init_terminals_comma_separated.extend(quote! {
                                    #term,
                                });
                            }

                            comma_separated_reduce_terminal_sets.extend(quote! {
                                vec![#init_terminals_comma_separated],
                            });

                            len
                        };

                        init_reduce_comma_separated_terminals_rules.extend(quote! {
                            (#terminal_set_num, #ruleid),
                        });
                    }
                    quote! {
                        vec![#init_reduce_comma_separated_terminals_rules],
                    }
                };
                let ruleset = {
                    let mut comma_separated_rules = TokenStream::new();
                    for rule in state.ruleset.into_iter() {
                        let ruleid = rule.rule;
                        let shifted = rule.shifted;
                        comma_separated_rules.extend(quote! {
                            (#ruleid,#shifted),
                        });
                    }
                    quote! {
                        vec![#comma_separated_rules],
                    }
                };

                shift_term_initializer.extend(shift_term);
                shift_nonterm_initializer.extend(shift_nonterm);
                reduce_map_initializer.extend(reduce_map);
                ruleset_initializer.extend(ruleset);
            }

            (
                shift_term_initializer,
                shift_nonterm_initializer,
                reduce_map_initializer,
                ruleset_initializer,
            )
        };

        Ok(quote! {
            let __rustylr_terminals = vec![#comma_separated_terminals];

            let rules:Vec<#rule_typename> = [#comma_separated_rule_names].into_iter().zip(
                [#comma_separated_rule_tokens]
            ).map(
                | (name, tokens) | {
                    #rule_typename {
                        name,
                        rule: tokens.into_iter().map(
                            | token | {
                                match token {
                                    #module_prefix::Token::Term(term) => #module_prefix::Token::Term(__rustylr_terminals[term].clone()),
                                    #module_prefix::Token::NonTerm(nonterm) => #module_prefix::Token::NonTerm(nonterm),
                                }
                            }
                        ).collect(),
                    }
                }
            ).collect();

            let __rustylr_reduce_terminals = vec![#comma_separated_reduce_terminal_sets];

            let states:Vec<#state_typename> = [#shift_term_initializer].into_iter().zip(
                [#shift_nonterm_initializer].into_iter().zip(
                    [#reduce_map_initializer].into_iter().zip(
                        [#ruleset_initializer]
                    )
                )
            ).map(
                | (shift_goto_map_term, (shift_goto_map_nonterm, (reduce_map_init, ruleset))) | {
                    let mut reduce_map = #module_prefix::HashMap::default();
                    for (terminal_set_id, ruleid) in reduce_map_init.into_iter() {
                        reduce_map.extend(
                            __rustylr_reduce_terminals[terminal_set_id].iter().map(
                                | term_idx | {
                                    (__rustylr_terminals[*term_idx].clone(), ruleid)
                                }
                            )
                        );
                    }
                    #state_typename {
                        shift_goto_map_term: shift_goto_map_term.into_iter().map(
                            | (term_idx, goto) | {
                                (__rustylr_terminals[term_idx].clone(), goto)
                            }
                        ).collect(),
                        shift_goto_map_nonterm: shift_goto_map_nonterm.into_iter().collect(),
                        reduce_map,
                        ruleset: ruleset.into_iter().map(
                            | (ruleid, shifted) | {
                                #module_prefix::ShiftedRuleRef {
                                    rule: ruleid,
                                    shifted,
                                }
                            }
                        ).collect(),
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
        let invalid_terminal_error = format_ident!("{}InvalidTerminalError", self.start_rule_name);
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
        let (user_data_parameter_def, user_data_var) =
            if let Some(user_data) = &self.userdata_typename {
                (
                    quote! { #user_data_parameter_name: &mut #user_data, },
                    quote! { #user_data_parameter_name, },
                )
            } else {
                (quote! {}, quote! {})
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
                        self.#reduce_fn_ident( #user_data_var )?;
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
                            fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
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
                                fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
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
                            fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
                                #token_pop_stream
                                #action
                                Ok(())
                            }
                        });
                    } else {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #reduce_error_typename> {
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

            /// reduce items in stack, this function is called automatically by parser
            pub fn reduce(&mut self,
                rulelen: usize,
                rustylr_macro_generated_ruleid__: usize,
                #user_data_parameter_def
            ) -> Result<(), #reduce_error_typename> {
                match rustylr_macro_generated_ruleid__ {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                    }
                }
                Ok(())
            }

            /// pop value from start rule
            #[inline]
            pub fn accept(&mut self) #return_start_rule_typename {
                #pop_from_start_rule_stack
            }

            /// push terminal symbol to stack, this function is called automatically by parser
            #[inline]
            pub fn push( &mut self, term: #token_typename ) {
                self.#terms_stack_name.push(term);
            }
        }
        impl #module_prefix::GetContext<#token_typename, #nonterminals_enum_name> for #context_struct_name {
            fn get_state_stack(&self) -> &[usize] {
                &self.state_stack
            }
        }

        /// struct that holds parser data, DFA tables
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        #[derive(Clone)]
        pub struct #parser_struct_name {
            /// production rules
            pub rules: Vec<#rule_typename>,
            /// states
            pub states: Vec<#state_typename>,
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
                // reduce if possible
                self.lookahead(context, &term, #user_data_var)?;

                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().unwrap()];

                // feed token to current state and get action
                // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
                // since it is resolved in state generation ( Grammar::build() )
                if let Some(next_state_id) = state.shift_goto_term(&term) {
                    context.state_stack.push(next_state_id);
                    context.push( term );

                    Ok(())
                }else {
                    let error = #invalid_terminal_error {
                        term,
                        expected: state.expected().into_iter().cloned().collect(),
                    };
                    Err(#parseerror_typename::InvalidTerminal(error))
                }
            }
            /// Create new context for parsing
            #[inline]
            pub fn begin(&self) -> #context_struct_name {
                #context_struct_name::new()
            }


            /// give lookahead token to parser, and check if there is any reduce action
            fn lookahead(
                &self,
                context: &mut #context_struct_name,
                term: &#token_typename,
                #user_data_parameter_def
            ) -> Result<(), #parseerror_typename> {
                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().unwrap()];

                // feed token to current state and get action
                // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
                // since it is resolved in state generation ( Grammar::build() )
                if let Some(reduce_rule) = state.reduce(term) {
                    // reduce items in stack
                    let rule = &self.rules[reduce_rule];
                    context
                        .state_stack
                        .truncate(context.state_stack.len() - rule.rule.len());
                    context.reduce(
                        self.rules[reduce_rule].rule.len(),
                        reduce_rule,
                        #user_data_var
                    ).map_err(#parseerror_typename::ReduceAction)?;


                    // feed reduced token
                    self.feed_nonterm(context, &rule.name)?;

                    // original lookahead token is not shifted, so feed it again
                    self.lookahead(context, term, #user_data_var)?;
                }
                Ok(())
            }

            /// feed one non-terminal to parser, and update state stack
            fn feed_nonterm(
                &self,
                context: &mut #context_struct_name,
                nonterm: &#nonterminals_enum_name,
            ) -> Result<(), #parseerror_typename> {
                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().unwrap()];

                // feed token to current state and get action
                // for shift/reduce confict, shift has higher priority
                if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
                    context.state_stack.push(next_state_id);
                }else {
                    unreachable!( "Invalid NonTerminal: {}", nonterm );
                }
                Ok(())
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

        impl #module_prefix::GetParser<#token_typename, #nonterminals_enum_name> for #parser_struct_name {
            fn get_rules(&self) -> &[#rule_typename] {
                &self.rules
            }
            fn get_states(&self) -> &[#state_typename] {
                &self.states
            }
        }
        })
    }

    pub fn emit_compiletime(&self, lalr: bool) -> Result<TokenStream, Box<EmitError>> {
        let type_alias_emit = self.emit_type_alises();
        let enum_emit = self.emit_nonterm_enum();
        let grammar_emit = self.emit_grammar_compiletime(lalr)?;
        let parser_emit = self.emit_parser(grammar_emit)?;

        Ok(quote! {
            #type_alias_emit
            #enum_emit
            #parser_emit
        })
    }
}
