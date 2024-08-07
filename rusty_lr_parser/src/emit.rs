use proc_macro2::Ident;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::token::Token;

use rusty_lr_core as rlr;

use std::collections::BTreeMap;

/// emit Rust code for the parser
impl Grammar {
    // build grammar at compile time
    fn emit_grammar_compiletime(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let mut grammar: rlr::Grammar<String, String> = rlr::Grammar::new();
        let module_prefix = self
            .module_prefix
            .as_ref()
            .cloned()
            .unwrap_or_else(|| quote! {::rusty_lr});

        // reduce types
        for (term, (_, reduce_type)) in self.reduce_types.iter() {
            match grammar.set_reduce_type(term.clone(), *reduce_type) {
                Ok(_) => {}
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        }

        // rules
        for (name, (_name_ident, _typename, rules)) in self.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let mut tokens = Vec::with_capacity(rule.tokens.len());
                for token in rule.tokens.iter() {
                    match &token.token {
                        Token::Term(term) => {
                            tokens.push(rlr::Token::Term(term.to_string()));
                        }
                        Token::NonTerm(nonterm) => {
                            tokens.push(rlr::Token::NonTerm(nonterm.to_string()));
                        }

                        _ => {
                            unreachable!("Only Term and NonTerm should be in rule");
                        }
                    }
                }

                grammar.add_rule(name.clone(), tokens);
            }
        }

        // augmented rule
        grammar.add_rule(
            "<Augmented>".to_string(),
            vec![
                rlr::Token::NonTerm(self.start_rule_name.as_ref().unwrap().to_string()),
                rlr::Token::Term("eof".to_string()),
            ],
        );

        // build
        let parser = if lalr {
            match grammar.build_lalr("<Augmented>".to_string()) {
                Ok(parser) => parser,
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        } else {
            match grammar.build("<Augmented>".to_string()) {
                Ok(parser) => parser,
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        };

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
                        let (_, term_stream) = self.terminals.get(term).unwrap();
                        comma_separated_tokens
                            .extend(quote! {#module_prefix::Token::Term(#term_stream),});
                    }
                    rlr::Token::NonTerm(nonterm) => {
                        comma_separated_tokens
                            .extend(quote! {#module_prefix::Token::NonTerm(#nonterm),});
                    }
                }
            }

            let nonterm = &rule.name;
            comma_separated_rules.extend(quote! {
                #module_prefix::ProductionRule {
                    name: #nonterm,
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

        let mut lookaheads_definition_stream = TokenStream::new();
        let mut lookaheads_map: BTreeMap<_, Ident> = BTreeMap::new();
        let mut write_states = TokenStream::new();

        {
            let state_len = parser.states.len();
            write_states.extend(quote! {
                let mut states = Vec::with_capacity(#state_len);
            });
        }
        for state in parser.states.iter() {
            // use BTreeMap to sort keys, for consistent output
            let shift_goto_map_term: BTreeMap<_, _> = state.shift_goto_map_term.iter().collect();
            let mut comma_separated_shift_goto_map_term = quote! {};
            for (term, goto) in shift_goto_map_term.into_iter() {
                let (_, term_stream) = self.terminals.get(&term.to_string()).unwrap();
                comma_separated_shift_goto_map_term.extend(quote! {
                    (#term_stream, #goto),
                });
            }

            // use BTreeMap to sort keys, for consistent output
            let shift_goto_map_nonterm: BTreeMap<_, _> =
                state.shift_goto_map_nonterm.iter().collect();
            let mut comma_separated_shift_goto_map_nonterm = quote! {};
            for (nonterm, goto) in shift_goto_map_nonterm.into_iter() {
                comma_separated_shift_goto_map_nonterm.extend(quote! {
                    (#nonterm, #goto),
                });
            }

            // use BTreeMap to sort keys, for consistent output
            let reduce_map: BTreeMap<_, _> = state.reduce_map.iter().collect();
            let mut comma_separated_reduce_map = quote! {};
            for (term, ruleid) in reduce_map.into_iter() {
                let (_, term_stream) = self.terminals.get(&term.to_string()).unwrap();
                comma_separated_reduce_map.extend(quote! {
                    (#term_stream, #ruleid),
                });
            }

            let mut comma_separated_ruleset = quote! {};
            for (rule, lookaheads) in state.ruleset.rules.iter() {
                let lookaheads_ident = if let Some(lookaheads_ident) =
                    lookaheads_map.get(lookaheads)
                {
                    lookaheads_ident.clone()
                } else {
                    let new_ident =
                        format_ident!("rustylr_macrogenerated_lookaheads_{}", lookaheads_map.len());
                    lookaheads_map.insert(lookaheads.clone(), new_ident.clone());

                    let mut comma_separated_lookaheads = TokenStream::new();
                    for lookahead_name in lookaheads.iter() {
                        let (_, term_stream) = self.terminals.get(lookahead_name).unwrap();
                        comma_separated_lookaheads.extend(quote! {#term_stream,});
                    }

                    lookaheads_definition_stream.extend(quote! {
                        let #new_ident = std::collections::BTreeSet::from(
                            [ #comma_separated_lookaheads ]
                        );
                    });

                    new_ident
                };

                let rule_id = rule.rule;
                let shifted = rule.shifted;
                comma_separated_ruleset.extend(quote! {
                    (
                        #module_prefix::ShiftedRuleRef {
                            rule: #rule_id,
                            shifted: #shifted,
                        },
                        #lookaheads_ident.clone(),
                    ),
                });
            }

            write_states.extend(quote! {
                {
                    let shift_goto_map_term = std::collections::HashMap::from(
                        [ #comma_separated_shift_goto_map_term ]
                    );
                    let shift_goto_map_nonterm = std::collections::HashMap::from(
                        [ #comma_separated_shift_goto_map_nonterm ]
                    );
                    let reduce_map = std::collections::HashMap::from(
                        [ #comma_separated_reduce_map ]
                    );
                    let ruleset = #module_prefix::LookaheadRuleRefSet {
                        rules: std::collections::BTreeMap::from(
                            [ #comma_separated_ruleset ]
                        )
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
            #lookaheads_definition_stream
            #write_states
        })
    }

    /// emit code that build grammar at runtime
    fn emit_grammar_runtime(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let module_prefix = self
            .module_prefix
            .as_ref()
            .cloned()
            .unwrap_or_else(|| quote! {::rusty_lr});

        let mut build_grammar_stream = quote! {
            let mut grammar = #module_prefix::Grammar::new();
        };

        // reduce types
        for (term, (_, reduce_type)) in self.reduce_types.iter() {
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
        for (name, (_name_ident, _typename, rules)) in self.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let mut comma_separated_tokens = quote! {};
                for token in rule.tokens.iter() {
                    match &token.token {
                        Token::Term(term) => {
                            let term_stream = &self.terminals.get(&term.to_string()).unwrap().1;
                            comma_separated_tokens
                                .extend(quote! {#module_prefix::Token::Term(#term_stream),});
                        }
                        Token::NonTerm(nonterm) => {
                            let nonterm_stream = nonterm.to_string();
                            comma_separated_tokens
                                .extend(quote! {#module_prefix::Token::NonTerm(#nonterm_stream),});
                        }

                        _ => {
                            unreachable!("Only Term and NonTerm should be in rule");
                        }
                    }
                }

                build_grammar_stream.extend(quote! {
                    grammar.add_rule(#name, vec![#comma_separated_tokens]);
                });
            }
        }

        // augmented rule
        {
            let start_name = self.start_rule_name.as_ref().unwrap().to_string();
            let eof_stream = self.eof.as_ref().unwrap();
            build_grammar_stream.extend(quote! {
                grammar.add_rule(
                    "<Augmented>",
                    vec![
                        #module_prefix::Token::NonTerm(#start_name),
                        #module_prefix::Token::Term(#eof_stream),
                    ],
                );
            });
        }

        // build grammar
        if lalr {
            build_grammar_stream.extend(quote! {
                let parser = match grammar.build_lalr("<Augmented>") {
                    Ok(parser) => parser,
                    Err(err) => {
                        panic!( "Error building LALR parser:\n{:?}", err );
                    }
                };
            });
        } else {
            build_grammar_stream.extend(quote! {
                let parser = match grammar.build_lalr("<Augmented>") {
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
        let module_prefix = self
            .module_prefix
            .as_ref()
            .cloned()
            .unwrap_or_else(|| quote! {::rusty_lr});
        // =====================================================================
        // =========================Writing Parser==============================
        // =====================================================================

        let mut ruleid: usize = 0;
        let mut case_streams = quote! {};
        // stack for end index of each rule in term stack
        let end_stack_name = Self::stack_name(&format_ident! {"rl_end"});
        let terms_stack_name = Self::stack_name(&format_ident! {"rl_terms"});
        let term_typename = self.token_typename.as_ref().unwrap();

        for (_name, (name_ident, typename, rules)) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name_ident);

            for rule in rules.rule_lines.iter() {
                let action = if let Some(action) = &rule.reduce_action {
                    action.clone()
                } else {
                    quote! {}
                };
                let mut token_pop_stream = TokenStream::new();
                for token in rule.tokens.iter().rev() {
                    match &token.token {
                        Token::Term(_) => {
                            let mapto = &token.mapto;
                            token_pop_stream.extend(quote! {
                                let index = self.#end_stack_name.pop().unwrap()-1;
                                let mut #mapto = #module_prefix::TermData::new(
                                    &self.#terms_stack_name[index],
                                    index
                                );
                            });
                        }
                        Token::NonTerm(nonterm) => {
                            let mapto = &token.mapto;

                            // pop value from stack of 'nonterm'
                            let stack_pop_stream =
                                // if <RuleType> is defined for this nonterm
                                if self.rules.get(&nonterm.to_string()).unwrap().1.is_some() {
                                    // stack name of this token
                                    let stack_name = Self::stack_name(nonterm);
                                    quote! {
                                        self.#stack_name.pop().unwrap()
                                    }
                                } else {
                                    quote! {
                                        ()
                                    }
                                };

                            token_pop_stream.extend(quote! {
                                // TODO unreachable! if stack is empty
                                let end = self.#end_stack_name.pop().unwrap();
                                let begin = *self.#end_stack_name.last().unwrap();
                                let mut #mapto = #module_prefix::NonTermData::new(
                                    &self.#terms_stack_name[begin..end],
                                    #stack_pop_stream,
                                    begin..end,
                                );
                            });
                        }
                        _ => {
                            unreachable!("Only Term and NonTerm should be in rule");
                        }
                    }
                }
                if typename.is_some() {
                    case_streams.extend(quote! {
                        #ruleid => {
                            #token_pop_stream
                            self.#stack_name.push(#action);
                        }
                    });
                } else {
                    case_streams.extend(quote! {
                        #ruleid => {
                            #token_pop_stream
                            #action
                        }
                    });
                }

                ruleid += 1;
            }
        }

        let start_rule_name = self.start_rule_name.as_ref().unwrap();
        let (start_rule_typename, pop_from_start_rule_stack) = {
            if let Some(start_typename) = &self.rules.get(&start_rule_name.to_string()).unwrap().1 {
                let start_rule_stack_name = Self::stack_name(start_rule_name);
                (
                    start_typename.clone(),
                    quote! {
                        self.#start_rule_stack_name.pop().unwrap()
                    },
                )
            } else {
                (quote! {()}, quote! {})
            }
        };

        let mut stack_def_streams = quote! {};
        let mut stack_init_streams = quote! {};
        for (_name, (name, typename, _rules)) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name);

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

        // error typename from '%error'
        let error_typename = if let Some(error_typename) = &self.error_typename {
            error_typename.clone()
        } else {
            quote! { String }
        };

        let (user_data_parameter_def, user_data_var) =
            if let Some(user_data) = &self.userdata_typename {
                (quote! { data: &mut #user_data, }, quote! { data, })
            } else {
                (quote! {}, quote! {})
            };

        Ok(quote! {
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        pub struct #stack_struct_name {
            #terms_stack_name: Vec<#term_typename>,
            #end_stack_name: Vec<usize>,
            pub state_stack: Vec<usize>,
            #stack_def_streams
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        impl #stack_struct_name {
            pub fn new() -> Self {
                Self {
                    #terms_stack_name: Vec::new(),
                    #end_stack_name: vec![0],
                    state_stack: vec![0],
                    #stack_init_streams
                }
            }

            pub fn reduce(&mut self,
                rulelen: usize,
                rustylr_macro_generated_ruleid__: usize,
                #user_data_parameter_def
            ) -> Result<(), #error_typename> {
                // get range of current rule
                // TODO unreachable! if stack is empty
                let rusty_lr_macro_generated_new_begin = *self.#end_stack_name.get( self.#end_stack_name.len() - rulelen-1 ).unwrap();
                let rusty_lr_macro_generated_new_end = *self.#end_stack_name.last().unwrap();
                let s = &self.#terms_stack_name[rusty_lr_macro_generated_new_begin..rusty_lr_macro_generated_new_end];
                match rustylr_macro_generated_ruleid__ {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                    }
                }
                self.#end_stack_name.push(rusty_lr_macro_generated_new_end);
                Ok(())
            }
            pub fn accept(&mut self) -> #start_rule_typename {
                #pop_from_start_rule_stack
            }

            pub fn push( &mut self, term: #term_typename ) {
                self.#terms_stack_name.push(term);
                self.#end_stack_name.push(self.#terms_stack_name.len());
            }
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
        pub struct #struct_name {
            pub rules: Vec<#module_prefix::ProductionRule<#term_typename, &'static str>>,
            pub states: Vec<#module_prefix::State<#term_typename, &'static str>>,
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
            fn lookahead<'a, C: #module_prefix::Callback<#term_typename, &'static str>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                term: &#term_typename,
                #user_data_parameter_def
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, &'static str, C::Error, #error_typename>> {
                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().unwrap()];

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
                    ).map_err(|e| #module_prefix::ParseError::ReduceAction(e))?;
                    callback
                        .reduce(&self.rules, &self.states, &context.state_stack, reduce_rule)
                        .map_err(|e| #module_prefix::ParseError::Callback(e))?;


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
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, &'static str, u8, #error_typename>> {
                self.feed_callback( context, &mut #module_prefix::DefaultCallback {}, term, #user_data_var )
            }
            /// feed one terminal to parser, and update state stack
            pub fn feed_callback<'a, C: #module_prefix::Callback<#term_typename, &'static str>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                term: #term_typename,
                #user_data_parameter_def
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, &'static str, C::Error, #error_typename>> {
                // reduce if possible
                self.lookahead(context, callback, &term, #user_data_var)?;

                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().unwrap()];

                // feed token to current state and get action
                // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
                // since it is resolved in state generation ( Grammar::build() )
                if let Some(next_state_id) = state.shift_goto_term(&term) {
                    context.state_stack.push(next_state_id);
                    callback
                        .shift_and_goto(&self.rules, &self.states, &context.state_stack, &term)
                        .map_err(|e| #module_prefix::ParseError::Callback(e))?;

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
            fn feed_nonterm<'a, C: #module_prefix::Callback<#term_typename, &'static str>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                nonterm: &'a &'static str,
            ) -> Result<(), #module_prefix::ParseError<'a, #term_typename, &'static str, C::Error, #error_typename>> {
                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().unwrap()];

                // feed token to current state and get action
                // for shift/reduce confict, shift has higher priority
                if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
                    context.state_stack.push(next_state_id);
                    callback
                        .shift_and_goto_nonterm(&self.rules, &self.states, &context.state_stack, nonterm)
                        .map_err(|e| #module_prefix::ParseError::Callback(e))?;
                    Ok(())
                }else {
                    Err(#module_prefix::ParseError::InvalidNonTerminal(
                        &nonterm,
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
        let grammar_emit = self.emit_grammar_compiletime(lalr)?;
        let parser_emit = self.emit_parser(grammar_emit)?;
        Ok(parser_emit)
    }
    pub fn emit_runtime(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let grammar_emit = self.emit_grammar_runtime(lalr)?;
        let parser_emit = self.emit_parser(grammar_emit)?;
        Ok(parser_emit)
    }
}
