use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::utils;

use rusty_lr_core as rlr;

use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// emit Rust code for the parser
impl Grammar {
    /// write enum that represents non-terminal symbols, including Augmented
    fn emit_nonterm_enum(&self) -> Result<TokenStream, ParseError> {
        // =====================================================================
        // =====================Writing NonTerminal Enum========================
        // =====================================================================

        let start_rule_name = &self.start_rule_name;
        let enum_name = utils::generate_enum_name(start_rule_name);

        let mut comma_separated_variants = TokenStream::new();
        let mut case_display = TokenStream::new();
        for (name, _) in self.rules.iter() {
            comma_separated_variants.extend(quote! {
                #name,
            });

            let name_str = name.to_string();
            case_display.extend(quote! {
                #enum_name::#name=> write!(f, #name_str),
            });
        }

        let aug_ident = Ident::new(utils::AUGMENTED_NAME, Span::call_site());
        let aug_ident_str = utils::AUGMENTED_NAME;

        Ok(quote! {
            /// An enum that represents non-terminal symbols
            #[allow(non_camel_case_types)]
            #[derive(Debug, Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
            pub enum #enum_name {
                #comma_separated_variants
                #aug_ident,
            }

            impl std::fmt::Display for #enum_name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        #case_display
                        #enum_name::#aug_ident => write!(f, #aug_ident_str),
                    }
                }
            }
        })
    }
    // build grammar at compile time
    fn emit_grammar_compiletime(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let module_prefix = &self.module_prefix;

        let mut grammar = self.create_grammar()?;

        // build
        let parser = if lalr {
            match grammar.build_lalr(utils::AUGMENTED_NAME.to_string()) {
                Ok(parser) => parser,
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        } else {
            match grammar.build(utils::AUGMENTED_NAME.to_string()) {
                Ok(parser) => parser,
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        };

        let nonterminals_enum_name = utils::generate_enum_name(&self.start_rule_name);

        // generate code that copy rules and states to parser
        // =====================================================================
        // ==================Writing Production Rules===========================
        // =====================================================================
        let mut comma_separated_rules = TokenStream::new();
        for rule in parser.rules.iter() {
            let mut comma_separated_tokens = quote! {};
            for token in rule.rule.iter() {
                match token {
                    rlr::Token::Term(term) => {
                        let (_, term_stream) = self
                            .terminals
                            .get(&Ident::new(term, Span::call_site()))
                            .unwrap();
                        comma_separated_tokens
                            .extend(quote! {#module_prefix::Token::Term(#term_stream),});
                    }
                    rlr::Token::NonTerm(nonterm) => {
                        let ident = Ident::new(nonterm, Span::call_site());
                        comma_separated_tokens
                            .extend(quote! {#module_prefix::Token::NonTerm(#nonterminals_enum_name::#ident),});
                    }
                }
            }

            let nonterm = Ident::new(&rule.name, Span::call_site());
            comma_separated_rules.extend(quote! {
                #module_prefix::ProductionRule {
                    name: #nonterminals_enum_name::#nonterm,
                    rule: vec![#comma_separated_tokens],
                },
            });
        }
        let write_rules = quote! {
            let rules = vec![
                #comma_separated_rules
            ];
        };

        // =====================================================================
        // =========================Writing States==============================
        // =====================================================================

        let mut write_states = TokenStream::new();

        // writing lookaheads are removed
        // since it is not used in runtime
        // and it takes too much space

        {
            // this write closure that convert (rule_id, shifted) pair to ShiftedRuleRef
            // to shorten the emitted code
            let term_typename = &self.token_typename;
            write_states.extend(
            quote!{
                let pair_to_rule = |(rule, shifted):(usize,usize)| -> (#module_prefix::ShiftedRuleRef, std::collections::BTreeSet<#term_typename>) {
                    (
                        #module_prefix::ShiftedRuleRef {
                            rule,
                            shifted,
                        },
                        Default::default()
                    )
                };
                }
            );
        }
        {
            let state_len = parser.states.len();
            write_states.extend(quote! {
                let mut states = Vec::with_capacity(#state_len);
            });
        }

        // when generating code for 'inserting tokens to reduce_map',
        // there could be multiple lookahead tokens for one rule
        // inserting all of them one by one is inefficient
        let mut reduce_terminals_map = BTreeMap::new();
        let mut init_reduce_terminals_stream = TokenStream::new();

        for state in parser.states.into_iter() {
            let mut init_shift_term_stream = TokenStream::new();
            let mut init_shift_nonterm_stream = TokenStream::new();
            let mut init_reduce_stream = TokenStream::new();
            {
                let shift_term_len = state.shift_goto_map_term.len();
                let shift_nonterm_len = state.shift_goto_map_nonterm.len();
                let reduce_len = state.reduce_map.len();

                if shift_term_len > 0 {
                    init_shift_term_stream.extend(quote! {
                        let mut shift_goto_map_term = #module_prefix::HashMap::default();
                        shift_goto_map_term.reserve(#shift_term_len);
                    });
                } else {
                    init_shift_term_stream.extend(quote! {
                        let shift_goto_map_term = #module_prefix::HashMap::default();
                    });
                }
                if shift_nonterm_len > 0 {
                    init_shift_nonterm_stream.extend(quote! {
                        let mut shift_goto_map_nonterm = #module_prefix::HashMap::default();
                        shift_goto_map_nonterm.reserve(#shift_nonterm_len);
                    });
                } else {
                    init_shift_nonterm_stream.extend(quote! {
                        let shift_goto_map_nonterm = #module_prefix::HashMap::default();
                    });
                }
                if reduce_len > 0 {
                    init_reduce_stream.extend(quote! {
                        let mut reduce_map = #module_prefix::HashMap::default();
                        reduce_map.reserve(#reduce_len);
                    });
                } else {
                    init_reduce_stream.extend(quote! {
                        let reduce_map = #module_prefix::HashMap::default();
                    });
                }
            }

            // use BTreeMap to sort keys, for consistent output
            let shift_goto_map_term: BTreeMap<_, _> =
                state.shift_goto_map_term.into_iter().collect();
            for (term, goto) in shift_goto_map_term.into_iter() {
                let (_, term_stream) = self
                    .terminals
                    .get(&Ident::new(&term, Span::call_site()))
                    .unwrap();
                init_shift_term_stream.extend(quote! {
                    shift_goto_map_term.insert( #term_stream, #goto );
                });
            }

            // use BTreeMap to sort keys, for consistent output
            let shift_goto_map_nonterm: BTreeMap<_, _> =
                state.shift_goto_map_nonterm.into_iter().collect();
            for (nonterm, goto) in shift_goto_map_nonterm.into_iter() {
                let nonterm = Ident::new(&nonterm, Span::call_site());
                init_shift_nonterm_stream.extend(quote! {
                    shift_goto_map_nonterm.insert(#nonterminals_enum_name::#nonterm, #goto);
                });
            }

            // use BTreeMap to sort keys, for consistent output
            let mut reduce_map_by_rule_id = BTreeMap::new();
            for (term, ruleid) in state.reduce_map.into_iter() {
                reduce_map_by_rule_id
                    .entry(ruleid)
                    .or_insert_with(BTreeSet::new)
                    .insert(term);
            }
            for (ruleid, tokens) in reduce_map_by_rule_id.into_iter() {
                let terminal_set_ident_num = if let Some(id) = reduce_terminals_map.get(&tokens) {
                    *id
                } else {
                    let len = reduce_terminals_map.len();
                    reduce_terminals_map.insert(tokens.clone(), len);

                    let mut init_terminals_comma_separated = TokenStream::new();
                    for term_name in tokens.into_iter() {
                        let term_ident = Ident::new(&term_name, Span::call_site());
                        let (_, term_stream) = self.terminals.get(&term_ident).unwrap();
                        init_terminals_comma_separated.extend(quote! {
                            #term_stream,
                        });
                    }

                    let terminals_ident = format_ident!("_rustylr_generated_terminals_{}", len);
                    init_reduce_terminals_stream.extend(quote! {
                        let #terminals_ident = vec![
                            #init_terminals_comma_separated
                        ];
                    });

                    len
                };
                let terminals_ident =
                    format_ident!("_rustylr_generated_terminals_{}", terminal_set_ident_num);

                init_reduce_stream.extend(quote! {
                    for term in #terminals_ident.iter() {
                        reduce_map.insert( term.clone(), #ruleid );
                    }
                });
            }

            let mut comma_separated_rule_shifted = TokenStream::new();
            for (rule, _lookaheads) in state.ruleset.rules.into_iter() {
                let rule_id = rule.rule;
                let shifted = rule.shifted;
                comma_separated_rule_shifted.extend(quote! {
                    ( #rule_id, #shifted ),
                });
            }

            write_states.extend(quote! {
                {
                    #init_shift_term_stream
                    #init_shift_nonterm_stream
                    #init_reduce_stream
                    let rule_shifted_pairs = vec![ #comma_separated_rule_shifted ];
                    let ruleset = #module_prefix::LookaheadRuleRefSet {
                        rules: std::collections::BTreeMap::from_iter(
                            rule_shifted_pairs.into_iter().map(
                                pair_to_rule
                            )
                        ),
                    };
                    let state = #module_prefix::State {
                        shift_goto_map_term,
                        shift_goto_map_nonterm,
                        reduce_map,
                        ruleset,
                    };
                    states.push(state);
                }
            });
        }

        Ok(quote! {
            #write_rules
            #init_reduce_terminals_stream
            #write_states
        })
    }

    /// emit code that build grammar at runtime
    fn emit_grammar_runtime(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = utils::generate_enum_name(&self.start_rule_name);

        let mut build_grammar_stream = quote! {
            let mut grammar = #module_prefix::Grammar::new();
        };

        // reduce types
        for (term, reduce_type) in self.reduce_types.iter() {
            let stream = &self.terminals.get(term).unwrap().1;
            match reduce_type {
                rlr::ReduceType::Left => {
                    build_grammar_stream.extend(quote! {
                        match grammar.set_reduce_type(#stream, #module_prefix::ReduceType::Left) {
                            Ok(_) => {},
                            Err(err) => {
                                panic!( "Error setting reduce type:\n{:?}", err );
                            }
                        }
                    });
                }
                rlr::ReduceType::Right => {
                    build_grammar_stream.extend(quote! {
                        match grammar.set_reduce_type(#stream, #module_prefix::ReduceType::Right) {
                            Ok(_) => {},
                            Err(err) => {
                                panic!( "Error setting reduce type:\n{:?}", err );
                            }
                        }
                    });
                }
            }
        }

        // rules
        for (name, rules) in self.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let mut comma_separated_tokens = quote! {};
                for token in rule.tokens.iter() {
                    if let Some((_term_idx, term_stream)) = self.terminals.get(&token.token) {
                        comma_separated_tokens
                            .extend(quote! {#module_prefix::Token::Term(#term_stream),});
                    } else if self.rules.contains_key(&token.token) {
                        let nonterm = &token.token;
                        comma_separated_tokens
                            .extend(quote! {#module_prefix::Token::NonTerm(#nonterminals_enum_name::#nonterm),});
                    } else {
                        unreachable!("Token should be either terminal or non-terminal");
                    }
                }

                build_grammar_stream.extend(quote! {
                    grammar.add_rule(#nonterminals_enum_name::#name, vec![#comma_separated_tokens]);
                });
            }
        }

        // augmented rule
        let aug_ident = Ident::new(utils::AUGMENTED_NAME, Span::call_site());
        {
            let start_name = &self.start_rule_name;
            let eof_stream = &self.eof;
            build_grammar_stream.extend(quote! {
                grammar.add_rule(
                    #nonterminals_enum_name::#aug_ident,
                    vec![
                        #module_prefix::Token::NonTerm(#nonterminals_enum_name::#start_name),
                        #module_prefix::Token::Term(#eof_stream),
                    ],
                );
            });
        }

        // build grammar
        if lalr {
            build_grammar_stream.extend(quote! {
                let parser = match grammar.build_lalr(#nonterminals_enum_name::#aug_ident) {

                    Ok(parser) => parser,
                    Err(err) => {
                        panic!( "Error building LALR parser:\n{:?}", err );
                    }
                };
            });
        } else {
            build_grammar_stream.extend(quote! {
                let parser = match grammar.build_lalr(#nonterminals_enum_name::#aug_ident) {
                    Ok(parser) => parser,
                    Err(err) => {
                        panic!( "Error building LR parser:\n{:?}", err );
                    }
                };
            });
        }

        build_grammar_stream.extend(quote! {
            let rules = parser.rules;
            let states = parser.states;
        });
        Ok(build_grammar_stream)
    }

    fn emit_parser(&self, grammar_emit: TokenStream) -> Result<TokenStream, ParseError> {
        let module_prefix = &self.module_prefix;

        let nonterminals_enum_name = utils::generate_enum_name(&self.start_rule_name);

        // =====================================================================
        // =========================Writing Parser==============================
        // =====================================================================

        // error typename from '%error'
        let error_typename = &self.error_typename;

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
        let term_typename = &self.token_typename;

        // TokenStream to define reduce function for each production rule
        let mut fn_reduce_for_each_rule_stream = TokenStream::new();

        for (name, rules) in self.rules.iter() {
            for (rule_local_id, rule) in rules.rule_lines.iter().enumerate() {
                let mut token_pop_stream = TokenStream::new();
                for token in rule.tokens.iter().rev() {
                    if self.terminals.contains_key(&token.token) {
                        let mapto = &token.mapto;
                        token_pop_stream.extend(quote! {
                            let mut #mapto = self.#terms_stack_name.pop().expect("Something wrong! term_stack is empty");
                        });
                    } else if let Some(typename) = self.nonterm_typenames.get(&token.token) {
                        if typename.is_some() {
                            // if <RuleType> is defined for this nonterm,
                            // pop value from the stack to 'mapto'
                            let stack_name = utils::generate_stack_name(&token.token);
                            let error_message =
                                format!("Something wrong! {} stack is empty", &token.token);

                            let mapto = &token.mapto;
                            token_pop_stream.extend(quote! {
                                let mut #mapto = self.#stack_name.pop().expect(#error_message);
                            });
                        }
                    } else {
                        unreachable!("Token should be either terminal or non-terminal");
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
                let typename = self.nonterm_typenames.get(name).unwrap();
                if typename.is_some() {
                    // push result to this stack
                    let stack_name = utils::generate_stack_name(name);

                    // typename is defined, reduce action must be defined
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #error_typename> {
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
                            let typename = self.get_typename(&token.token);
                            if typename.is_some() {
                                if unique_mapto.is_some() {
                                    unique_mapto = None;
                                    break;
                                } else {
                                    unique_mapto = Some(&token.mapto);
                                }
                            }
                        }
                        if let Some(unique_mapto) = unique_mapto {
                            fn_reduce_for_each_rule_stream.extend(quote! {
                                fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #error_typename> {
                                    #token_pop_stream
                                    self.#stack_name.push(#unique_mapto);
                                    Ok(())
                                }
                            });
                        } else {
                            return Err(ParseError::RuleTypeDefinedButActionNotDefined(
                                name.clone(),
                            ));
                        }
                    }
                } else {
                    // <RuleType> is not defined,
                    // just execute action
                    if let Some(action) = &rule.reduce_action {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #error_typename> {
                                #token_pop_stream
                                #action
                                Ok(())
                            }
                        });
                    } else {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, #user_data_parameter_def) -> Result<(), #error_typename> {
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
        let start_rule_name = &self.start_rule_name;
        let (start_rule_typename, pop_from_start_rule_stack) = {
            if let Some(start_typename) = &self.nonterm_typenames.get(start_rule_name).unwrap() {
                let start_rule_stack_name = utils::generate_stack_name(start_rule_name);
                (
                    start_typename.clone(),
                    quote! {
                        self.#start_rule_stack_name.pop().expect("Something wrong! start_rule_stack is empty")
                    },
                )
            } else {
                (quote! {()}, quote! {})
            }
        };

        // TokenStream for member variables declaration
        let mut stack_def_streams = quote! {};
        let mut stack_init_streams = quote! {};
        for (name, typename) in self.nonterm_typenames.iter() {
            // push result to this stack
            let stack_name = utils::generate_stack_name(name);

            if let Some(typename) = typename {
                stack_def_streams.extend(quote! {
                    #stack_name : Vec<#typename>,
                });
                stack_init_streams.extend(quote! {
                    #stack_name : Vec::new(),
                });
            }
        }

        let mut struct_name = format_ident!("{}Parser", start_rule_name);
        struct_name.set_span(start_rule_name.span());
        let mut stack_struct_name = format_ident!("{}Context", start_rule_name);
        stack_struct_name.set_span(start_rule_name.span());

        Ok(quote! {
        /// struct that holds internal parser data, for reduce action and state transition
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        pub struct #stack_struct_name {
            /// state stack, user must not modify this
            pub state_stack: Vec<usize>,
            #terms_stack_name: Vec<#term_typename>,
            #stack_def_streams
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        impl #stack_struct_name {
            pub fn new() -> Self {
                Self {
                    state_stack: vec![0],
                    #terms_stack_name: Vec::new(),
                    #stack_init_streams
                }
            }

            #fn_reduce_for_each_rule_stream

            /// reduce items in stack, this function is called automatically by parser
            pub fn reduce(&mut self,
                rulelen: usize,
                rustylr_macro_generated_ruleid__: usize,
                #user_data_parameter_def
            ) -> Result<(), #error_typename> {
                match rustylr_macro_generated_ruleid__ {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                    }
                }
                Ok(())
            }

            /// pop value from start rule
            pub fn accept(&mut self) -> #start_rule_typename {
                #pop_from_start_rule_stack
            }

            /// push terminal symbol to stack, this function is called automatically by parser
            pub fn push( &mut self, term: #term_typename ) {
                self.#terms_stack_name.push(term);
            }
        }
        /// struct that holds parser data, DFA tables
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        pub struct #struct_name {
            /// production rules
            pub rules: Vec<#module_prefix::ProductionRule<#term_typename, #nonterminals_enum_name>>,
            /// states
            pub states: Vec<#module_prefix::State<#term_typename, #nonterminals_enum_name>>,
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        impl #struct_name {
            pub fn new() -> Self {
                #grammar_emit
                Self {
                    rules,
                    states,
                }
            }

            /// give lookahead token to parser, and check if there is any reduce action
            fn lookahead<'a, C: #module_prefix::Callback<#term_typename, #nonterminals_enum_name>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                term: &#term_typename,
                #user_data_parameter_def
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, #nonterminals_enum_name, C::Error, #error_typename>> {
                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().expect("Something wrong! state_stack is empty")];

                // feed token to current state and get action
                // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
                // since it is resolved in state generation ( Grammar::build() )
                if let Some(reduce_rule) = state.reduce(term) {
                    // reduce items in stack
                    let rule = &self.rules[reduce_rule];
                    if context.state_stack.len() < rule.rule.len() {
                        // this should not be happened if DFA is generated correctly
                        panic!(
                            "State stack not enough for reduce: {:?}",
                            context.state_stack
                        );
                    }
                    context
                        .state_stack
                        .truncate(context.state_stack.len() - rule.rule.len());
                    context.reduce(
                        self.rules[reduce_rule].rule.len(),
                        reduce_rule,
                        #user_data_var
                    ).map_err(#module_prefix::ParseError::ReduceAction)?;
                    callback
                        .reduce(&self.rules, &self.states, &context.state_stack, reduce_rule)
                        .map_err(#module_prefix::ParseError::Callback)?;


                    // feed reduced token
                    self.feed_nonterm(context, callback, &rule.name)?;

                    // original lookahead token is not shifted, so feed it again
                    self.lookahead(context, callback, term, #user_data_var)?;
                }
                Ok(())
            }
            /// feed one terminal to parser, and update state stack
            pub fn feed<'a>(
                &'a self,
                context: &mut #stack_struct_name,
                term: #term_typename,
                #user_data_parameter_def
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, #nonterminals_enum_name, u8, #error_typename>> {
                self.feed_callback( context, &mut #module_prefix::DefaultCallback {}, term, #user_data_var )
            }
            /// feed one terminal to parser, and update state stack
            pub fn feed_callback<'a, C: #module_prefix::Callback<#term_typename, #nonterminals_enum_name>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                term: #term_typename,
                #user_data_parameter_def
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, #nonterminals_enum_name, C::Error, #error_typename>> {
                // reduce if possible
                self.lookahead(context, callback, &term, #user_data_var)?;

                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().expect("Something wrong! state_stack is empty")];

                // feed token to current state and get action
                // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
                // since it is resolved in state generation ( Grammar::build() )
                if let Some(next_state_id) = state.shift_goto_term(&term) {
                    context.state_stack.push(next_state_id);
                    callback
                        .shift_and_goto(&self.rules, &self.states, &context.state_stack, &term)
                        .map_err(#module_prefix::ParseError::Callback)?;

                    context.push( term );

                    Ok(())
                }else {
                    Err(#module_prefix::ParseError::InvalidTerminal(
                        term,
                        &self.rules,
                        &self.states,
                        context.state_stack.clone(),
                    ))
                }
            }

            /// feed one non-terminal to parser, and update state stack
            fn feed_nonterm<'a, C: #module_prefix::Callback<#term_typename, #nonterminals_enum_name>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                nonterm: &'a #nonterminals_enum_name,
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, #nonterminals_enum_name, C::Error, #error_typename>> {
                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().expect("Something wrong! state_stack is empty")];

                // feed token to current state and get action
                // for shift/reduce confict, shift has higher priority
                if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
                    context.state_stack.push(next_state_id);
                    callback
                        .shift_and_goto_nonterm(&self.rules, &self.states, &context.state_stack, nonterm)
                        .map_err(#module_prefix::ParseError::Callback)?;
                    Ok(())
                }else {
                    Err(#module_prefix::ParseError::InvalidNonTerminal(
                        nonterm,
                        &self.rules,
                        &self.states,
                        context.state_stack.clone(),
                    ))
                }
            }

            pub fn begin(&self) -> #stack_struct_name {
                #stack_struct_name::new()
            }
        }
        })
    }

    pub fn emit_compiletime(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let enum_emit = self.emit_nonterm_enum()?;
        let grammar_emit = self.emit_grammar_compiletime(lalr)?;
        let parser_emit = self.emit_parser(grammar_emit)?;

        Ok(quote! {
            #enum_emit
            #parser_emit
        })
    }
    pub fn emit_runtime(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let enum_emit = self.emit_nonterm_enum()?;
        let grammar_emit = self.emit_grammar_runtime(lalr)?;
        let parser_emit = self.emit_parser(grammar_emit)?;

        Ok(quote! {
            #enum_emit
            #parser_emit
        })
    }
}
