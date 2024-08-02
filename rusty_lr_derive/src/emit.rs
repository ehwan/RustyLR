use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::token::Token;

use rusty_lr as rlr;

/// emit Rust code for the parser
impl Grammar {
    pub fn emit(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        if self.eof.is_none() {
            return Err(ParseError::EofNotDefined);
        }
        if self.start_rule_name.is_none() {
            return Err(ParseError::StartNotDefined);
        }
        if self.token_typename.is_none() {
            return Err(ParseError::TokenTypeNotDefined);
        }

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

        for (name, _typename, rules) in self.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let mut tokens = Vec::with_capacity(rule.tokens.len());
                for token in rule.tokens.iter() {
                    match token {
                        Token::Term(term) => {
                            tokens.push(rlr::Token::Term(term.to_string()));
                        }
                        Token::NonTerm(nonterm) => {
                            tokens.push(rlr::Token::NonTerm(nonterm.to_string()));
                        }
                    }
                }

                grammar.add_rule(name.to_string(), tokens);
            }
        }

        grammar.add_rule(
            "<Augmented>".to_string(),
            vec![
                rlr::Token::NonTerm(self.start_rule_name.as_ref().unwrap().to_string()),
                rlr::Token::Term("<eof>".to_string()),
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
        let range_stack_name = Self::stack_name(&format_ident! {"rl_range"});
        let terms_stack_name = Self::stack_name(&format_ident! {"rl_terms"});
        let term_typename = self.token_typename.as_ref().unwrap();

        for (name, typename, rules) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name);

            for rule in rules.rule_lines.iter() {
                let action = if let Some(action) = &rule.reduce_action {
                    action.clone()
                } else {
                    quote! {}
                };
                let mut token_pop_stream = TokenStream::new();
                let rulelen = rule.tokens.len();
                if rulelen == 0 {
                    token_pop_stream.extend(quote! {
                        let s = &terms[0..0];
                    });
                } else {
                    token_pop_stream.extend(quote! {
                        rusty_lr_macro_generated_new_begin = self.#range_stack_name[self.#range_stack_name.len() - #rulelen].start;
                        rusty_lr_macro_generated_new_end = self.#range_stack_name[self.#range_stack_name.len() - 1].end;
                        let s = &self.#terms_stack_name[rusty_lr_macro_generated_new_begin..rusty_lr_macro_generated_new_end];
                    });
                }
                for (idx, token) in rule.tokens.iter().enumerate().rev() {
                    let var_name = format_ident!("v{}", idx);
                    let slice_name = format_ident!("s{}", idx);
                    match token {
                        Token::Term(_) => {
                            token_pop_stream.extend(quote! {
                                let #slice_name = &self.#terms_stack_name[self.#range_stack_name.pop().unwrap()];
                                let #var_name = &#slice_name[0];
                            });
                        }
                        Token::NonTerm(nonterm) => {
                            token_pop_stream.extend(quote! {
                                let #slice_name = &self.#terms_stack_name[self.#range_stack_name.pop().unwrap()];
                            });

                            // if typename is defined, pop from stack and assign to v{i}
                            let mut typename_search = None;
                            for (name, typename, _) in self.rules.iter() {
                                if name.to_string() == nonterm.to_string() {
                                    typename_search = typename.clone();
                                    break;
                                }
                            }
                            if typename_search.is_some() {
                                let stack_name = Self::stack_name(nonterm);
                                token_pop_stream.extend(quote! {
                                    let mut #var_name = self.#stack_name.pop().unwrap();
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
            let mut rusty_lr_macro_generated_new_begin: usize = 0;
            let mut rusty_lr_macro_generated_new_end: usize = 0;
            match rustylr_macro_generated_ruleid__ {
                #case_streams
                _ => {
                    unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                }
            }
            self.#range_stack_name.push(rusty_lr_macro_generated_new_begin..rusty_lr_macro_generated_new_end);
        };

        let start_rule_name = self.start_rule_name.as_ref().unwrap();
        let (start_rule_typename, pop_from_start_rule_stack) = {
            let start_rule_stack_name = Self::stack_name(start_rule_name);
            let mut start_rule_typename = None;
            for (name, typename, _) in self.rules.iter() {
                if name == self.start_rule_name.as_ref().unwrap() {
                    start_rule_typename = typename.clone();
                    break;
                }
            }
            if let Some(start_rule_typename) = start_rule_typename {
                (
                    start_rule_typename,
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
        for (name, typename, _rules) in self.rules.iter() {
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
        #[allow(unused_braces, unused_parens)]
        pub struct #stack_struct_name {
            pub #terms_stack_name: Vec<#term_typename>,
            pub #range_stack_name: Vec<std::ops::Range<usize>>,
            pub state_stack: Vec<usize>,
            #stack_def_streams
        }
        #[allow(unused_braces, unused_parens)]
        impl #stack_struct_name {
            pub fn new() -> Self {
                Self {
                    #terms_stack_name: Vec::new(),
                    #range_stack_name: Vec::new(),
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
                let l = self.#terms_stack_name.len();
                self.#terms_stack_name.push(term);
                self.#range_stack_name.push(l..l+1);
            }
        }
        #[allow(unused_braces, unused_parens)]
        pub struct #struct_name {
            pub rules: Vec<::rusty_lr::ProductionRule<#term_typename, &'static str>>,
            pub states: Vec<::rusty_lr::State<#term_typename, &'static str>>,
        }
        #[allow(unused_braces, unused_parens)]
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
