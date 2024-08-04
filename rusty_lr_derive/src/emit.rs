use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::token::Token;

use rusty_lr_core as rlr;

/// emit Rust code for the parser
impl Grammar {
    pub fn emit(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let mut grammar: rlr::Grammar<String, String> = rlr::Grammar::new();

        for (term, (ident, reduce_type)) in self.reduce_types.iter() {
            if !self.terminals.contains_key(term) {
                return Err(ParseError::TerminalNotDefined(ident.clone()));
            }
            match grammar.set_reduce_type(term.clone(), *reduce_type) {
                Ok(_) => {}
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        }

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
                    }
                }

                grammar.add_rule(name.clone(), tokens);
            }
        }

        grammar.add_rule(
            "<Augmented>".to_string(),
            vec![
                rlr::Token::NonTerm(self.start_rule_name.as_ref().unwrap().to_string()),
                rlr::Token::Term("eof".to_string()),
            ],
        );

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

        let mut write_parser = quote! {};

        // =====================================================================
        // ==================Writing Production Rules===========================
        // =====================================================================
        write_parser.extend(quote! {
            let mut rules = Vec::new();
        });
        for rule in parser.rules.iter() {
            let mut comma_separated_tokens = quote! {};
            for token in rule.rule.iter() {
                match token {
                    rlr::Token::Term(term) => {
                        let (_, term_stream) = self.terminals.get(term).unwrap();
                        comma_separated_tokens
                            .extend(quote! {::rusty_lr::Token::Term(#term_stream),});
                    }
                    rlr::Token::NonTerm(nonterm) => {
                        comma_separated_tokens
                            .extend(quote! {::rusty_lr::Token::NonTerm(#nonterm),});
                    }
                }
            }

            let nonterm = rule.name.clone();

            write_parser.extend(quote! {
                {
                let production_rule = ::rusty_lr::ProductionRule {
                    name: #nonterm,
                    rule: vec![#comma_separated_tokens],
                };
                rules.push(production_rule);
            }
            });
        }

        // =====================================================================
        // =========================Writing States==============================
        // =====================================================================
        write_parser.extend(quote! {
            let mut states = Vec::new();
        });
        for state in parser.states.iter() {
            let mut init_shift_goto_map_term = quote! {};
            init_shift_goto_map_term.extend(quote! {
                let mut shift_goto_map_term = std::collections::HashMap::new();
            });
            for (term, goto) in state.shift_goto_map_term.iter() {
                let (_, term_stream) = self.terminals.get(&term.to_string()).unwrap();
                init_shift_goto_map_term.extend(quote! {
                    shift_goto_map_term.insert( #term_stream, #goto);
                });
            }

            let mut init_shift_goto_map_nonterm = quote! {};
            init_shift_goto_map_nonterm.extend(quote! {
                let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            });
            for (nonterm, goto) in state.shift_goto_map_nonterm.iter() {
                init_shift_goto_map_nonterm.extend(quote! {
                    shift_goto_map_nonterm.insert( #nonterm, #goto);
                });
            }

            let mut init_reduce_map = quote! {};
            init_reduce_map.extend(quote! {
                let mut reduce_map = std::collections::HashMap::new();
            });
            for (term, ruleid) in state.reduce_map.iter() {
                let (_, term_stream) = self.terminals.get(&term.to_string()).unwrap();
                init_reduce_map.extend(quote! {
                    reduce_map.insert( #term_stream, #ruleid);
                });
            }

            let mut init_ruleset = quote! {};
            init_ruleset.extend(quote! {
                let mut ruleset = ::rusty_lr::LookaheadRuleRefSet::new();
            });
            for (rule, lookaheads) in state.ruleset.rules.iter() {
                let mut init_lookaheads = quote! {
                    let mut lookaheads = std::collections::BTreeSet::new();
                };
                for lookahead_name in lookaheads.iter() {
                    let (_, term_stream) = self.terminals.get(&lookahead_name.to_string()).unwrap();
                    init_lookaheads.extend(quote! {
                        lookaheads.insert(#term_stream);
                    });
                }

                let rule_id = rule.rule;
                let shifted = rule.shifted;
                init_ruleset.extend(quote! {
                    {
                    let shifted_rule = ::rusty_lr::ShiftedRuleRef {
                        rule: #rule_id,
                        shifted: #shifted,
                    };
                    #init_lookaheads

                    ruleset.add( shifted_rule, lookaheads );
                }

                });
            }

            write_parser.extend(quote! {
                {
                    #init_shift_goto_map_term
                    #init_shift_goto_map_nonterm
                    #init_reduce_map
                    #init_ruleset
                    let state = ::rusty_lr::State {
                        shift_goto_map_term,
                        shift_goto_map_nonterm,
                        reduce_map,
                        ruleset,
                    };
                    states.push(state);
                }
            });
        }

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
                let rulelen = rule.tokens.len();
                token_pop_stream.extend(quote! {
                    rusty_lr_macro_generated_new_begin = if self.#end_stack_name.len() == #rulelen {
                        0usize
                    } else {
                        *self.#end_stack_name.get( self.#end_stack_name.len() - #rulelen - 1 ).unwrap()
                    };
                    rusty_lr_macro_generated_new_end = self.#end_stack_name.last().copied().unwrap_or(0usize);
                    let s = &self.#terms_stack_name[rusty_lr_macro_generated_new_begin..rusty_lr_macro_generated_new_end];
                });
                for token in rule.tokens.iter().rev() {
                    match &token.token {
                        Token::Term(term) => {
                            let mapped = token.mapped.as_ref().unwrap_or(term);
                            token_pop_stream.extend(quote! {
                                let index = self.#end_stack_name.pop().unwrap()-1;
                                let #mapped = ::rusty_lr::TermData::new(
                                    &self.#terms_stack_name[index],
                                    index
                                );
                            });
                        }
                        Token::NonTerm(nonterm) => {
                            let mapped = token.mapped.as_ref().unwrap_or(nonterm);
                            // if typename is defined for this nonterm, pop from stack and assign to v{i}
                            if self.rules.get(&nonterm.to_string()).unwrap().1.is_some() {
                                let stack_name = Self::stack_name(nonterm);
                                token_pop_stream.extend(quote! {
                                    let end = self.#end_stack_name.pop().unwrap_or(0);
                                    let begin = self.#end_stack_name.last().copied().unwrap_or(0);
                                    let #mapped = ::rusty_lr::NonTermData::new(
                                        &self.#terms_stack_name[begin..end],
                                        self.#stack_name.pop().unwrap(),
                                        begin..end,
                                    );
                                });
                            } else {
                                token_pop_stream.extend(quote! {
                                    let end = self.#end_stack_name.pop().unwrap_or(0);
                                    let begin = self.#end_stack_name.last().copied().unwrap_or(0);
                                    let #mapped = ::rusty_lr::NonTermData::new(
                                        &self.#terms_stack_name[begin..end],
                                        (),
                                        begin..end,
                                    );
                                });
                            }
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
        case_streams.extend(quote! {});
        let match_streams = quote! {
            let mut rusty_lr_macro_generated_new_begin: usize = self.#terms_stack_name.len();
            let mut rusty_lr_macro_generated_new_end: usize = rusty_lr_macro_generated_new_begin;
            match rustylr_macro_generated_ruleid__ {
                #case_streams
                _ => {
                    unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                }
            }
            self.#end_stack_name.push(rusty_lr_macro_generated_new_end);
        };

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
                    pub #stack_name : Vec<#typename>,
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
        let (user_data_parameter_def, user_data_var) =
            if let Some(user_data) = &self.userdata_typename {
                (quote! { data: &mut #user_data, }, quote! { data, })
            } else {
                (quote! {}, quote! {})
            };

        Ok(quote! {
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case)]
        pub struct #stack_struct_name {
            pub #terms_stack_name: Vec<#term_typename>,
            pub #end_stack_name: Vec<usize>,
            pub state_stack: Vec<usize>,
            #stack_def_streams
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case)]
        impl #stack_struct_name {
            pub fn new() -> Self {
                Self {
                    #terms_stack_name: Vec::new(),
                    #end_stack_name: Vec::new(),
                    state_stack: vec![0],
                    #stack_init_streams
                }
            }

            pub fn reduce(&mut self,
                rustylr_macro_generated_ruleid__: usize,
                #user_data_parameter_def
            ) -> Result<(), String> {
                #match_streams
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
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case)]
        pub struct #struct_name {
            pub rules: Vec<::rusty_lr::ProductionRule<#term_typename, &'static str>>,
            pub states: Vec<::rusty_lr::State<#term_typename, &'static str>>,
        }
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case)]
        impl #struct_name {
            pub fn new() -> Self {
                #write_parser
                Self {
                    rules,
                    states,
                }
            }

            /// give lookahead token to parser, and check if there is any reduce action
            fn lookahead<'a, C: ::rusty_lr::Callback<#term_typename, &'static str>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                term: &#term_typename,
                #user_data_parameter_def
            ) -> Result<(), ::rusty_lr::ParseError<'a, #term_typename, &'static str, C::Error>> {
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
                    context.reduce(reduce_rule, #user_data_var).map_err(|e| ::rusty_lr::ParseError::CustomMessage(e))?;
                    callback
                        .reduce(&self.rules, &self.states, &context.state_stack, reduce_rule)
                        .map_err(|e| ::rusty_lr::ParseError::Callback(e))?;


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
            ) -> Result<(), ::rusty_lr::ParseError<'a, #term_typename, &'static str, ()>> {
                self.feed_callback( context, &mut ::rusty_lr::DefaultCallback {}, term, #user_data_var )
            }
            /// feed one terminal to parser, and update state stack
            pub fn feed_callback<'a, C: ::rusty_lr::Callback<#term_typename, &'static str>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                term: #term_typename,
                #user_data_parameter_def
            ) -> Result<(), ::rusty_lr::ParseError<'a, #term_typename, &'static str, C::Error>> {
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
                        .map_err(|e| ::rusty_lr::ParseError::Callback(e))?;

                    context.push( term );

                    Ok(())
                }else {
                    Err(::rusty_lr::ParseError::InvalidTerminal(
                        term,
                        &self.rules,
                        &self.states,
                        context.state_stack.clone(),
                    ))
                }
            }

            /// feed one non-terminal to parser, and update state stack
            fn feed_nonterm<'a, C: ::rusty_lr::Callback<#term_typename, &'static str>>(
                &'a self,
                context: &mut #stack_struct_name,
                callback: &mut C,
                nonterm: &'a &'static str,
            ) -> Result<(), ::rusty_lr::ParseError<'a, #term_typename, &'static str, C::Error>> {
                // fetch state from state stack
                let state = &self.states[*context.state_stack.last().unwrap()];

                // feed token to current state and get action
                // for shift/reduce confict, shift has higher priority
                if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
                    context.state_stack.push(next_state_id);
                    callback
                        .shift_and_goto_nonterm(&self.rules, &self.states, &context.state_stack, nonterm)
                        .map_err(|e| ::rusty_lr::ParseError::Callback(e))?;
                    Ok(())
                }else {
                    Err(::rusty_lr::ParseError::InvalidNonTerminal(
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
}
