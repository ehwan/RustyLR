use proc_macro2::Literal;
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
        let terminals = &self.terminals;

        let mut grammar: rlr::Grammar<String, String> = rlr::Grammar::new();

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
                        Token::Literal(literal) => {
                            return Err(ParseError::LiteralInCustomType(literal.clone()));
                        }
                    }
                }

                grammar.add_rule(name.to_string(), tokens, rule.reduce_type);
            }
        }
        let augmented = self.augmented.as_ref().unwrap().to_string();
        let mut parser = match grammar.build(augmented) {
            Ok(parser) => parser,
            Err(err) => {
                return Err(ParseError::GrammarBuildError(format!("{}", err)));
            }
        };
        if lalr {
            match parser.optimize_lalr() {
                Ok(_) => {}
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        }

        let mut write_parser = quote! {};

        // =====================================================================
        // ==================Writing Production Rules===========================
        // =====================================================================
        write_parser.extend(
            quote! {
                let mut rules = Vec::new();
            }
            .into_iter(),
        );
        for rule in parser.rules.iter() {
            let mut comma_separated_tokens = quote! {};
            for token in rule.rule.iter() {
                match token {
                    rlr::Token::Term(term) => {
                        let (_, term_stream) = terminals.get(term).unwrap();
                        comma_separated_tokens
                            .extend(quote! {::rusty_lr::Token::Term(#term_stream),}.into_iter());
                    }
                    rlr::Token::NonTerm(nonterm) => {
                        comma_separated_tokens
                            .extend(quote! {::rusty_lr::Token::NonTerm(#nonterm),}.into_iter());
                    }
                }
            }

            let reduce_type = match rule.reduce_type {
                rlr::ReduceType::Left => quote! {::rusty_lr::ReduceType::Left},
                rlr::ReduceType::Right => quote! {::rusty_lr::ReduceType::Right},
                rlr::ReduceType::Error => quote! {::rusty_lr::ReduceType::Error},
            };

            let nonterm = rule.name.clone();

            write_parser.extend(
                quote! {
                    {
                    let production_rule = ::rusty_lr::ProductionRule {
                        name: #nonterm,
                        rule: vec![#comma_separated_tokens],
                        reduce_type: #reduce_type,
                    };
                    rules.push(production_rule);
                }
                }
                .into_iter(),
            );
        }

        // =====================================================================
        // =========================Writing States==============================
        // =====================================================================
        write_parser.extend(
            quote! {
                let mut states = Vec::new();
            }
            .into_iter(),
        );
        for state in parser.states.iter() {
            let mut init_shift_goto_map_term = quote! {};
            init_shift_goto_map_term.extend(
                quote! {
                    let mut shift_goto_map_term = std::collections::HashMap::new();
                }
                .into_iter(),
            );
            for (term, goto) in state.shift_goto_map_term.iter() {
                let (_, term_stream) = terminals.get(&term.to_string()).unwrap();
                init_shift_goto_map_term.extend(quote! {
                    shift_goto_map_term.insert( #term_stream, #goto);
                });
            }

            let mut init_shift_goto_map_nonterm = quote! {};
            init_shift_goto_map_nonterm.extend(
                quote! {
                    let mut shift_goto_map_nonterm = std::collections::HashMap::new();
                }
                .into_iter(),
            );
            for (nonterm, goto) in state.shift_goto_map_nonterm.iter() {
                init_shift_goto_map_nonterm.extend(quote! {
                    shift_goto_map_nonterm.insert( #nonterm, #goto);
                });
            }

            let mut init_reduce_map = quote! {};
            init_reduce_map.extend(
                quote! {
                    let mut reduce_map = std::collections::HashMap::new();
                }
                .into_iter(),
            );
            for (term, ruleid) in state.reduce_map.iter() {
                let (_, term_stream) = terminals.get(&term.to_string()).unwrap();
                init_reduce_map.extend(quote! {
                    reduce_map.insert( #term_stream, #ruleid);
                });
            }

            let mut init_ruleset = quote! {};
            init_ruleset.extend(
                quote! {
                    let mut ruleset = ::rusty_lr::LookaheadRuleRefSet::new();
                }
                .into_iter(),
            );
            for (rule, lookaheads) in state.ruleset.rules.iter() {
                let mut init_lookaheads = quote! {
                    let mut lookaheads = std::collections::BTreeSet::new();
                };
                for lookahead_name in lookaheads.iter() {
                    let (_, term_stream) = terminals.get(&lookahead_name.to_string()).unwrap();
                    init_lookaheads.extend(quote! {
                        lookaheads.insert(#term_stream);
                    });
                }

                let rule_id = rule.rule;
                let shifted = rule.shifted;
                init_ruleset.extend(
                    quote! {
                        {
                        let shifted_rule = ::rusty_lr::ShiftedRuleRef {
                            rule: #rule_id,
                            shifted: #shifted,
                        };
                        #init_lookaheads

                        ruleset.add( shifted_rule, lookaheads );
                    }

                    }
                    .into_iter(),
                );
            }

            write_parser.extend(
                quote! {
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
                }
                .into_iter(),
            );
        }

        // =====================================================================
        // =========================Writing Parser==============================
        // =====================================================================
        let main_state = parser.main_state;
        let parser_emit = quote! {
            {
                #write_parser
                ::rusty_lr::Parser {
                    rules,
                    states,
                    main_state: #main_state,
                }
            }
        };

        // =====================================================================
        // ========================Writing Reducer==============================
        // =====================================================================
        // let mut stack_defs = quote! {};
        let mut ruleid: usize = 0;
        let mut case_streams = quote! {};
        let term_stack_name = Self::stack_name(&format_ident! {"rlterminal"});
        if self.tokentype.is_none() {
            return Err(ParseError::TokenTypeNotDefined);
        }
        let term_typename = self.tokentype.as_ref().unwrap();

        for (name, typename, rules) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name);

            for rule in rules.rule_lines.iter() {
                let action = if let Some(action) = &rule.reduce_action {
                    action
                } else {
                    unreachable!();
                };
                let mut token_pop_stream = TokenStream::new();
                for (idx, token) in rule.tokens.iter().enumerate().rev() {
                    let var_name = format_ident!("v{}", idx);
                    let slice_name = format_ident!("s{}", idx);
                    token_pop_stream.extend(
                        quote! {
                            let #slice_name = &terms[children[#idx].range()];
                        }
                        .into_iter(),
                    );
                    match token {
                        Token::Term(_) => {
                            token_pop_stream.extend(quote! {
                                let #var_name = self.#term_stack_name.pop().unwrap();
                            });
                        }
                        Token::NonTerm(nonterm) => {
                            let stack_name = Self::stack_name(nonterm);

                            let mut typename_search = None;
                            for (name, typename, _) in self.rules.iter() {
                                if name == nonterm {
                                    typename_search = typename.clone();
                                    break;
                                }
                            }

                            if typename_search.is_some() {
                                token_pop_stream.extend(quote! {
                                    let #var_name = self.#stack_name.pop().unwrap();
                                });
                            }
                        }
                        Token::Literal(literal) => {
                            return Err(ParseError::LiteralInCustomType(literal.clone()));
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
        case_streams.extend(
            quote! {
                _ => {
                    unreachable!();
                }
            }
            .into_iter(),
        );
        let match_streams = quote! {
            match rustylr_macro_generated_ruleid__ {
                #case_streams
            }
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
                stack_def_streams.extend(
                    quote! {
                        pub #stack_name : Vec<#typename>,
                    }
                    .into_iter(),
                );
                stack_init_streams.extend(
                    quote! {
                        #stack_name : Vec::new(),
                    }
                    .into_iter(),
                );
            }
        }

        let mut reducer_name = format_ident!("{}Reducer", start_rule_name);
        reducer_name.set_span(start_rule_name.span());
        let mut struct_name = format_ident!("{}Parser", start_rule_name);
        struct_name.set_span(start_rule_name.span());
        let (user_data_parameter_def, user_data_var) =
            if let Some(user_data) = &self.userdata_typename {
                (quote! { data: &mut #user_data, }, quote! { data, })
            } else {
                (quote! {}, quote! {})
            };

        Ok(quote! {
            struct #reducer_name<'a> {
                pub #term_stack_name : Vec<&'a #term_typename>,
                #stack_def_streams
            }
            impl<'a> #reducer_name<'a> {
                pub fn new() -> Self {
                    Self {
                        #term_stack_name : Vec::new(),
                        #stack_init_streams
                    }
                }
            fn reduce_impl(
                &mut self,
                terms: &'a [#term_typename],
                tree: &::rusty_lr::Tree,
                #user_data_parameter_def
            )
            {
                // slice of this tree
                let s = &terms[tree.range()];
                match tree {
                    ::rusty_lr::Tree::Terminal(term_idx) => {
                        let term = &terms[*term_idx];
                        self.#term_stack_name.push(term);
                    }
                    ::rusty_lr::Tree::NonTerminal(rustylr_macro_generated_ruleid__, children, idx) => {
                        for child in children.iter() {
                            self.reduce_impl(terms, child, #user_data_var);
                        }
                        #match_streams
                    }
                }
            }

            pub fn reduce(&mut self, terms: &'a [#term_typename], tree: &::rusty_lr::Tree, #user_data_parameter_def) -> #start_rule_typename
            {
                self.reduce_impl(terms, tree, #user_data_var);
                #pop_from_start_rule_stack
            }
            }

            struct #struct_name {
                pub parser: ::rusty_lr::Parser<#term_typename, &'static str>,
            }
            impl #struct_name {
                pub fn new() -> Self {
                    let parser = #parser_emit;
                    Self {
                        parser
                    }
                }
                pub fn parse_with_callback<'a, C: ::rusty_lr::Callback<#term_typename, &'static str>>(
                    &'a self,
                    terminals: &'a [#term_typename],
                    callback: &mut C,
                    eof: #term_typename,
                    #user_data_parameter_def
                ) -> Result<#start_rule_typename, ::rusty_lr::ParseError<'a, #term_typename, &'static str>>
                {
                    let tree = self.parser.parse_with_callback( terminals, callback, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree, #user_data_var))
                }
                pub fn parse<'a>(&'a self, terminals: &'a [#term_typename], eof: #term_typename, #user_data_parameter_def) -> Result<#start_rule_typename, ::rusty_lr::ParseError<'a, #term_typename, &'static str>>
                {
                    let tree = self.parser.parse( terminals, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree, #user_data_var))
                }
            }
        })
    }
    pub fn emit_str(&self, lalr: bool) -> Result<TokenStream, ParseError> {
        let mut grammar: rlr::Grammar<char, String> = rlr::Grammar::new();

        for (name, _typename, rules) in self.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let mut tokens = Vec::with_capacity(rule.tokens.len());
                for token in rule.tokens.iter() {
                    match token {
                        Token::Term(_) => {
                            unreachable!("Term type in str_parser is not supported");
                        }
                        Token::NonTerm(nonterm) => {
                            tokens.push(rlr::Token::NonTerm(nonterm.to_string()));
                        }
                        Token::Literal(literal) => {
                            let s = Self::literal_to_string(literal)?;
                            for ch in s.chars() {
                                tokens.push(rlr::Token::Term(ch));
                            }
                        }
                    }
                }

                grammar.add_rule(name.to_string(), tokens, rule.reduce_type);
            }
        }
        let augmented = self.augmented.as_ref().unwrap().to_string();
        let mut parser = match grammar.build(augmented) {
            Ok(parser) => parser,
            Err(err) => {
                return Err(ParseError::GrammarBuildError(format!("{}", err)));
            }
        };
        if lalr {
            match parser.optimize_lalr() {
                Ok(_) => {}
                Err(err) => {
                    return Err(ParseError::GrammarBuildError(format!("{}", err)));
                }
            }
        }

        let mut parser_write = quote! {};

        // =====================================================================
        // ==================Writing Production Rules===========================
        // =====================================================================
        parser_write.extend(quote! {
            let mut rules = Vec::new();
        });
        for rule in parser.rules.iter() {
            let mut comma_separated_tokens = quote! {};
            for token in rule.rule.iter() {
                match token {
                    rlr::Token::Term(term) => {
                        let ch = Literal::character(*term);
                        comma_separated_tokens
                            .extend(quote! {::rusty_lr::Token::Term(#ch),}.into_iter());
                    }
                    rlr::Token::NonTerm(nonterm) => {
                        comma_separated_tokens
                            .extend(quote! {::rusty_lr::Token::NonTerm(#nonterm),}.into_iter());
                    }
                }
            }

            let reduce_type = match rule.reduce_type {
                rlr::ReduceType::Left => quote! {::rusty_lr::ReduceType::Left},
                rlr::ReduceType::Right => quote! {::rusty_lr::ReduceType::Right},
                rlr::ReduceType::Error => quote! {::rusty_lr::ReduceType::Error},
            };

            let nonterm = rule.name.clone();

            parser_write.extend(quote! {
                {
                let production_rule = ::rusty_lr::ProductionRule {
                    name: #nonterm,
                    rule: vec![#comma_separated_tokens],
                    reduce_type: #reduce_type,
                };
                rules.push(production_rule);
                }
            });
        }

        // =====================================================================
        // =========================Writing States==============================
        // =====================================================================
        parser_write.extend(
            quote! {
                let mut states = Vec::new();
            }
            .into_iter(),
        );
        for state in parser.states.iter() {
            let mut init_shift_goto_map_term = quote! {};
            init_shift_goto_map_term.extend(
                quote! {
                    let mut shift_goto_map_term = std::collections::HashMap::new();
                }
                .into_iter(),
            );
            for (term, goto) in state.shift_goto_map_term.iter() {
                let ch = Literal::character(*term);
                init_shift_goto_map_term.extend(quote! {
                    shift_goto_map_term.insert( #ch, #goto);
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
                let ch = Literal::character(*term);
                init_reduce_map.extend(quote! {
                    reduce_map.insert( #ch, #ruleid);
                });
            }

            let mut init_ruleset = quote! {};
            init_ruleset.extend(
                quote! {
                    let mut ruleset = ::rusty_lr::LookaheadRuleRefSet::new();
                }
                .into_iter(),
            );
            for (rule, lookaheads) in state.ruleset.rules.iter() {
                let mut init_lookaheads = quote! {
                    let mut lookaheads = std::collections::BTreeSet::new();
                };
                for lookahead_name in lookaheads.iter() {
                    let ch = Literal::character(*lookahead_name);
                    init_lookaheads.extend(quote! {
                        lookaheads.insert(#ch);
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

            parser_write.extend(quote! {
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
        let main_state = parser.main_state;
        parser_write.extend(quote! {
            let parser = ::rusty_lr::Parser {
                rules,
                states,
                main_state: #main_state,
            };
        });

        let parser_emit = quote! {
            {
            #parser_write
            parser
            }
        };

        // =====================================================================
        // ========================Writing Reducer==============================
        // =====================================================================
        // let mut stack_defs = quote! {};
        let mut ruleid: usize = 0;
        let mut case_streams = quote! {};

        for (name, typename, rules) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name);

            for rule in rules.rule_lines.iter() {
                let action = if let Some(action) = &rule.reduce_action {
                    quote! { {#action} }
                } else {
                    unreachable!();
                };
                let mut token_pop_stream = TokenStream::new();
                let mut ch_idx: usize = 0;
                for (token_idx, token) in rule.tokens.iter().enumerate() {
                    let var_name = format_ident!("v{}", token_idx);
                    let slice_name = format_ident!("s{}", token_idx);
                    match token {
                        Token::Term(_) => {
                            unreachable!();
                        }
                        Token::NonTerm(nonterm) => {
                            let stack_name = Self::stack_name(nonterm);

                            let mut typename_search = None;
                            for (name, typename, _) in self.rules.iter() {
                                if name == nonterm {
                                    typename_search = typename.clone();
                                    break;
                                }
                            }

                            if typename_search.is_some() {
                                token_pop_stream.extend(quote! {
                                    let #var_name = self.#stack_name.pop().unwrap();
                                });
                            }
                            token_pop_stream.extend(quote! {
                                let #slice_name = &terms[children[#ch_idx].range()];
                            });
                            ch_idx += 1;
                        }
                        Token::Literal(literal) => {
                            let s = Self::literal_to_string(literal)?;

                            if s.is_empty() {
                                token_pop_stream.extend(quote! {
                                    let #var_name = #literal;
                                    let #slice_name = #var_name;
                                });
                            } else {
                                let count = s.chars().count();
                                let first_idx = ch_idx;
                                let last_idx = first_idx + count - 1;

                                token_pop_stream.extend(quote! {
                                    let #slice_name = &terms[children[#first_idx].begin()..children[#last_idx].end()];
                                    let #var_name = #literal;
                                });

                                ch_idx += count;
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
        case_streams.extend(quote! {
            _ => {
                unreachable!();
            }
        });
        let match_streams = quote! {
            match rustylr_macro_generated_ruleid__ {
                #case_streams
            }
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

        let mut reducer_name = format_ident!("{}Reducer", start_rule_name);
        reducer_name.set_span(start_rule_name.span());
        let mut struct_name = format_ident!("{}Parser", start_rule_name);
        struct_name.set_span(start_rule_name.span());
        let (user_data_parameter_def, user_data_var) =
            if let Some(user_data) = &self.userdata_typename {
                (quote! { data: &mut #user_data, }, quote! { data, })
            } else {
                (quote! {}, quote! {})
            };

        Ok(quote! {
            pub struct #reducer_name {
                #stack_def_streams
            }
            impl #reducer_name {
                pub fn new() -> Self {
                    Self {
                        #stack_init_streams
                    }
                }
                fn reduce_impl(
                    &mut self,
                    terms: &str,
                    tree: &::rusty_lr::TreeStr,
                    #user_data_parameter_def
                )
                {
                    match tree {
                        ::rusty_lr::TreeStr::Terminal(begin, end) => {
                        }
                        ::rusty_lr::TreeStr::NonTerminal(rustylr_macro_generated_ruleid__, children, beg, end) => {
                            for child in children.iter().rev() {
                                self.reduce_impl(terms, child, #user_data_var);
                            }
                            // slice of this tree
                            let s = &terms[*beg..*end];
                            #match_streams
                        }
                    }
                }

                pub fn reduce(&mut self, terms: &str, tree: &::rusty_lr::TreeStr, #user_data_parameter_def)-> #start_rule_typename
                {
                    self.reduce_impl(terms, tree, #user_data_var);
                    #pop_from_start_rule_stack
                }
            }

            pub struct #struct_name {
                pub parser: ::rusty_lr::Parser<char, &'static str>,
            }
            impl #struct_name {
                pub fn new() -> Self {
                    let parser = #parser_emit;
                    Self {
                        parser
                    }
                }
                pub fn parse_str_with_callback<'a, C: ::rusty_lr::CallbackStr<char, &'static str>>(
                    &'a self,
                    terminals: &'a str,
                    callback: &mut C,
                    eof: char,
                    #user_data_parameter_def
                ) -> Result<#start_rule_typename, ::rusty_lr::ParseErrorStr<'a, char, &'static str>>
                {
                    let tree = self.parser.parse_str_with_callback( terminals, callback, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree, #user_data_var))
                }
                pub fn parse_str<'a>(&'a self, terminals: &'a str, eof: char, #user_data_parameter_def)-> Result<#start_rule_typename, ::rusty_lr::ParseErrorStr<'a, char, &'static str>>
                {
                    let tree = self.parser.parse_str( terminals, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree, #user_data_var))
                }
            }
        })
    }
}
