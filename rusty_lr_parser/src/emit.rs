use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use crate::grammar::Grammar;
use crate::terminal_info::TerminalName;
use crate::utils;

/// emit Rust code for the parser
impl Grammar {
    /// write type alias Context, Rule, State, Error...
    fn emit_type_alises(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let start_rule_name = &self.start_rule_name;
        let rule_typename = format_ident!("{}Rule", start_rule_name);
        let state_typename = format_ident!("{}State", start_rule_name);
        let enum_name = format_ident!("{}NonTerminals", start_rule_name);
        let parse_error_typename = format_ident!("{}ParseError", start_rule_name);
        let context_struct_name = format_ident!("{}Context", start_rule_name);
        let data_stack_typename = format_ident!("{}DataStack", start_rule_name);

        let state_structname = if self.emit_dense {
            format_ident!("DenseState")
        } else {
            format_ident!("SparseState")
        };

        let state_index_typename = if self.states.len() <= u8::MAX as usize {
            quote! { u8 }
        } else if self.states.len() <= u16::MAX as usize {
            quote! { u16 }
        } else if self.states.len() <= u32::MAX as usize {
            quote! { u32 }
        } else {
            quote! { usize }
        };
        let class_index_typename = if self.terminal_classes.len() <= u8::MAX as usize {
            quote! { u8 }
        } else if self.terminal_classes.len() <= u16::MAX as usize {
            quote! { u16 }
        } else if self.terminal_classes.len() <= u32::MAX as usize {
            quote! { u32 }
        } else {
            quote! { usize }
        };

        if self.glr {
            // count the number of rules
            // and calculate the integral type for rule index -> u8, u16, u32, usize ...
            let rule_container_type = if self.builder.rules.len() <= u8::MAX as usize {
                quote! { #module_prefix::parser::state::SmallVecU8 }
            } else if self.builder.rules.len() <= u16::MAX as usize {
                quote! { #module_prefix::parser::state::SmallVecU16 }
            } else if self.builder.rules.len() <= u32::MAX as usize {
                quote! { #module_prefix::parser::state::SmallVecU32 }
            } else {
                quote! { #module_prefix::parser::state::SmallVecUsize }
            };
            stream.extend(
            quote! {
                    /// type alias for `Context`
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #context_struct_name = #module_prefix::parser::nondeterministic::Context<#data_stack_typename, #state_index_typename>;
                    /// type alias for CFG production rule
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #rule_typename = #module_prefix::rule::ProductionRule<&'static str, #enum_name>;
                    /// type alias for DFA state
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #state_typename = #module_prefix::parser::state::#state_structname<#class_index_typename, #enum_name, #rule_container_type, #state_index_typename>;
                    /// type alias for `InvalidTerminalError`
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #parse_error_typename = #module_prefix::parser::nondeterministic::ParseError<#data_stack_typename>;
                }
            );
        } else {
            stream.extend(
        quote! {
                /// type alias for `Context`
                #[allow(non_camel_case_types,dead_code)]
                pub type #context_struct_name = #module_prefix::parser::deterministic::Context<#data_stack_typename, #state_index_typename>;
                /// type alias for CFG production rule
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::rule::ProductionRule<&'static str, #enum_name>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::parser::state::#state_structname<#class_index_typename, #enum_name, usize, #state_index_typename>;
                /// type alias for `ParseError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::parser::deterministic::ParseError<#data_stack_typename>;
            }
            );
        }
    }

    /// write `NonTerminal` enum
    fn emit_nonterm_enum(&self, stream: &mut TokenStream) {
        // =====================================================================
        // =====================Writing NonTerminal Enum========================
        // =====================================================================

        let start_rule_name = &self.start_rule_name;
        let enum_typename = format_ident!("{}NonTerminals", start_rule_name);
        let module_prefix = &self.module_prefix;

        let mut comma_separated_variants = TokenStream::new();
        let mut case_as_str = TokenStream::new();
        let mut nonterm_trait_is_trace_case = TokenStream::new();
        let mut nonterm_type_case = TokenStream::new();
        for nonterm in self.nonterminals.iter() {
            let name = &nonterm.name;
            // enum variants definition
            comma_separated_variants.extend(quote! {
                #name,
            });

            // impl `Display` and `Debug` for NonTerminal
            let display_str = nonterm.pretty_name.as_str();
            case_as_str.extend(quote! {
                #enum_typename::#name => #display_str,
            });

            let is_trace = if name == utils::AUGMENTED_NAME {
                false
            } else {
                // non-term is auto-generated if nonterm.regex_span.is_some()
                nonterm.trace
            };

            nonterm_trait_is_trace_case.extend(quote! {
                #enum_typename::#name => #is_trace,
            });
            if let Some(enum_name) = &nonterm.nonterm_type {
                let enum_name = format!("{:?}", enum_name);
                let enum_name = Ident::new(&enum_name, Span::call_site());
                nonterm_type_case.extend(quote! {
                    #enum_typename::#name => Some(#module_prefix::nonterminal::NonTerminalType::#enum_name),
                });
            } else {
                nonterm_type_case.extend(quote! {
                    #enum_typename::#name => None,
                });
            }
        }

        stream.extend(
    quote! {
            /// An enum that represents non-terminal symbols
            #[allow(non_camel_case_types, dead_code)]
            #[derive(Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
            pub enum #enum_typename {
                #comma_separated_variants
            }
            impl std::fmt::Display for #enum_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    use #module_prefix::nonterminal::NonTerminal;
                    write!(f, "{}", self.as_str())
                }
            }
            impl std::fmt::Debug for #enum_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    use #module_prefix::nonterminal::NonTerminal;
                    write!(f, "{}", self.as_str())
                }
            }

            impl #module_prefix::nonterminal::NonTerminal for #enum_typename{
                fn as_str(&self) -> &'static str {
                    match self {
                        #case_as_str
                    }
                }
                fn is_trace(&self) -> bool {
                    match self {
                        #nonterm_trait_is_trace_case
                    }
                }
                fn nonterm_type(&self) -> Option<#module_prefix::nonterminal::NonTerminalType> {
                    match self {
                        #nonterm_type_case
                    }
                }
                fn to_usize(&self) -> usize {
                    *self as usize
                }
            }
        }
        );
    }

    fn emit_parser(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let rule_typename = format_ident!("{}Rule", self.start_rule_name);
        let state_typename = format_ident!("{}State", self.start_rule_name);
        let parser_struct_name = format_ident!("{}Parser", self.start_rule_name);
        let token_typename = &self.token_typename;

        // ======================
        // building grammar
        // ======================
        use rusty_lr_core::builder::ReduceType;
        use rusty_lr_core::TerminalSymbol;
        use rusty_lr_core::Token;

        let reduce_type_to_stream = |reduce_type: ReduceType| -> TokenStream {
            match reduce_type {
                ReduceType::Left => quote! { #module_prefix::builder::ReduceType::Left },
                ReduceType::Right => quote! { #module_prefix::builder::ReduceType::Right },
            }
        };
        let precedence_to_stream = |op: rusty_lr_core::rule::Precedence| -> TokenStream {
            match op {
                rusty_lr_core::rule::Precedence::Fixed(level) => {
                    quote! { #module_prefix::rule::Precedence::Fixed(#level) }
                }
                rusty_lr_core::rule::Precedence::Dynamic(idx) => {
                    quote! { #module_prefix::rule::Precedence::Dynamic(#idx) }
                }
            }
        };
        let nonterminals_token: Vec<_> = self
            .nonterminals
            .iter()
            .map(|nonterm| {
                let name = &nonterm.name;
                quote! {
                    #nonterminals_enum_name::#name
                }
            })
            .collect();
        let terminal_symbol_to_stream = |term: TerminalSymbol<usize>| -> TokenStream {
            match term {
                TerminalSymbol::Term(term) => {
                    let term = proc_macro2::Literal::usize_unsuffixed(term);
                    quote! { #module_prefix::TerminalSymbol::Term(#term) }
                }
                TerminalSymbol::Error => {
                    quote! { #module_prefix::TerminalSymbol::Error }
                }
                TerminalSymbol::Eof => {
                    quote! { #module_prefix::TerminalSymbol::Eof }
                }
            }
        };
        let token_to_stream = |token: Token<TerminalSymbol<usize>, usize>| -> TokenStream {
            match token {
                Token::Term(term) => {
                    let term = terminal_symbol_to_stream(term);
                    quote! { #module_prefix::Token::Term(#term) }
                }
                Token::NonTerm(nonterm) => {
                    let nonterm = &nonterminals_token[nonterm];
                    quote! { #module_prefix::Token::NonTerm(#nonterm) }
                }
            }
        };

        let mut terminal_class_names_stream = TokenStream::new();
        for class in 0..self.terminal_classes.len() {
            let name = self.class_pretty_name_list(TerminalSymbol::Term(class), 4);
            terminal_class_names_stream.extend(quote! {
                #name,
            });
        }

        // helper function to generate match-case stream.
        // convert integer list to `a | b..=c | d` syntax
        fn list_to_case_stream(list: impl Iterator<Item = usize>) -> TokenStream {
            let mut stream = TokenStream::new();
            let mut prev = None;
            for val in list {
                if let Some((prev_start, prev_last)) = prev {
                    if prev_last + 1 == val {
                        // extend the range
                        prev = Some((prev_start, val));
                    } else {
                        // push the previous range
                        if !stream.is_empty() {
                            stream.extend(quote! { | });
                        }
                        if prev_start == prev_last {
                            let prev_start = proc_macro2::Literal::usize_unsuffixed(prev_start);
                            stream.extend(quote! { #prev_start });
                        } else {
                            let prev_start = proc_macro2::Literal::usize_unsuffixed(prev_start);
                            let prev_last = proc_macro2::Literal::usize_unsuffixed(prev_last);
                            stream.extend(quote! { #prev_start..=#prev_last });
                        }

                        prev = Some((val, val));
                    }
                } else {
                    prev = Some((val, val));
                }
            }
            // push the previous range
            if let Some((prev_start, prev_last)) = prev {
                if !stream.is_empty() {
                    stream.extend(quote! { | });
                }
                if prev_start == prev_last {
                    let prev_start = proc_macro2::Literal::usize_unsuffixed(prev_start);
                    stream.extend(quote! { #prev_start });
                } else {
                    let prev_start = proc_macro2::Literal::usize_unsuffixed(prev_start);
                    let prev_last = proc_macro2::Literal::usize_unsuffixed(prev_last);
                    stream.extend(quote! { #prev_start..=#prev_last });
                }
            }

            stream
        }

        // generate precedence level -> reduce type match stream
        // match level {
        //     0 => Left,
        //     1 => Right,
        //     ...
        // }
        let precedence_types_match_body_stream = {
            let lefts = list_to_case_stream(
                self.builder
                    .precedence_types
                    .iter()
                    .copied()
                    .enumerate()
                    .filter_map(|(level, reduce_type)| {
                        debug_assert!(level < u8::MAX as usize);
                        if reduce_type == Some(ReduceType::Left) {
                            Some(level)
                        } else {
                            None
                        }
                    }),
            );
            let rights = list_to_case_stream(
                self.builder
                    .precedence_types
                    .iter()
                    .copied()
                    .enumerate()
                    .filter_map(|(level, reduce_type)| {
                        debug_assert!(level < u8::MAX as usize);
                        if reduce_type == Some(ReduceType::Right) {
                            Some(level)
                        } else {
                            None
                        }
                    }),
            );

            let mut stream = TokenStream::new();
            if !lefts.is_empty() {
                stream.extend(quote! {
                    #lefts => Some(#module_prefix::builder::ReduceType::Left),
                });
            }
            if !rights.is_empty() {
                stream.extend(quote! {
                    #rights => Some(#module_prefix::builder::ReduceType::Right),
                });
            }
            stream.extend(quote! {
                _ => None,
            });
            stream
        };

        let class_index_typename = if self.terminal_classes.len() <= u8::MAX as usize {
            quote! { u8 }
        } else if self.terminal_classes.len() <= u16::MAX as usize {
            quote! { u16 }
        } else if self.terminal_classes.len() <= u32::MAX as usize {
            quote! { u32 }
        } else {
            quote! { usize }
        };

        let grammar_build_stream = if self.compiled {
            // do not build at runtime
            // write all parser tables and production rules directly here

            let state_index_typename = if self.states.len() <= u8::MAX as usize {
                quote! { u8 }
            } else if self.states.len() <= u16::MAX as usize {
                quote! { u16 }
            } else if self.states.len() <= u32::MAX as usize {
                quote! { u32 }
            } else {
                quote! { usize }
            };

            let rule_index_typename = if self.builder.rules.len() <= u8::MAX as usize {
                quote! { u8 }
            } else if self.builder.rules.len() <= u16::MAX as usize {
                quote! { u16 }
            } else if self.builder.rules.len() <= u32::MAX as usize {
                quote! { u32 }
            } else {
                quote! { usize }
            };

            let mut production_rules_body_stream = TokenStream::new();
            for rule in &self.builder.rules {
                let mut tokens_vec_body_stream = TokenStream::new();
                for &token in &rule.rule.rule {
                    let token_stream = token_to_stream(token);
                    tokens_vec_body_stream.extend(quote! {
                        #token_stream,
                    });
                }
                let name = &nonterminals_token[rule.rule.name];
                let precedence_stream = if let Some(precedence) = rule.rule.precedence {
                    let s = precedence_to_stream(precedence);
                    quote! { Some(#s) }
                } else {
                    quote! { None }
                };

                // lookaheads
                production_rules_body_stream.extend(quote! {
                    #module_prefix::rule::ProductionRule{
                        name: #name,
                        rule: vec![ #tokens_vec_body_stream ],
                        precedence: #precedence_stream,
                    },
                });
            }
            let mut states_body_stream = TokenStream::new();
            let mut terminal_sets_name_map = std::collections::BTreeMap::new();
            let mut get_or_insert_terminal_set = |set: std::collections::BTreeSet<usize>| -> Ident {
                let new_index = terminal_sets_name_map.len();
                terminal_sets_name_map
                    .entry(set)
                    .or_insert_with(|| format_ident!("__rustylr_tset{new_index}"))
                    .clone()
            };
            for state in &self.states {
                let mut shift_term_body_stream = TokenStream::new();
                let mut error_shift_stream = quote! {None};
                let mut eof_shift_stream = quote! {None};
                for (&term, &next_state) in &state.shift_goto_map_term {
                    let next_state = proc_macro2::Literal::usize_unsuffixed(next_state);
                    match term {
                        TerminalSymbol::Error => {
                            error_shift_stream = quote! {Some(#next_state)};
                            continue;
                        }
                        TerminalSymbol::Eof => {
                            eof_shift_stream = quote! {Some(#next_state)};
                            continue;
                        }
                        TerminalSymbol::Term(term) => {
                            let term = proc_macro2::Literal::usize_unsuffixed(term);
                            shift_term_body_stream.extend(quote! {
                                (#term, #next_state),
                            });
                        }
                    }
                }

                let mut shift_nonterm_body_stream = TokenStream::new();
                for (&nonterm, &next_state) in &state.shift_goto_map_nonterm {
                    let nonterm_stream = &nonterminals_token[nonterm];
                    let next_state = proc_macro2::Literal::usize_unsuffixed(next_state);
                    shift_nonterm_body_stream.extend(quote! {
                        (#nonterm_stream, #next_state),
                    });
                }

                let mut reduce_body_stream = TokenStream::new();
                let mut reduce_rules_terms_map = std::collections::BTreeMap::new();
                let mut error_reduce_stream = quote! {None};
                let mut eof_reduce_stream = quote! {None};
                for (&term, rules) in &state.reduce_map {
                    match term {
                        TerminalSymbol::Error => {
                            let rules_it = rules
                                .iter()
                                .map(|&rule| proc_macro2::Literal::usize_unsuffixed(rule));
                            error_reduce_stream = quote! {Some(vec![#(#rules_it),*])};
                            continue;
                        }
                        TerminalSymbol::Eof => {
                            let rules_it = rules
                                .iter()
                                .map(|&rule| proc_macro2::Literal::usize_unsuffixed(rule));
                            eof_reduce_stream = quote! {Some(vec![#(#rules_it),*])};
                            continue;
                        }
                        TerminalSymbol::Term(term) => {
                            reduce_rules_terms_map
                                .entry(rules)
                                .or_insert_with(std::collections::BTreeSet::new)
                                .insert(term);
                        }
                    }
                }
                for (rules, terms) in reduce_rules_terms_map {
                    let terms_set_name = get_or_insert_terminal_set(terms);

                    let rules_it = rules
                        .iter()
                        .map(|&rule| proc_macro2::Literal::usize_unsuffixed(rule));

                    reduce_body_stream.extend(quote! {
                        let reduce_rules = vec![#(#rules_it),*];
                        __reduce_map.extend(
                            #terms_set_name.iter().map(
                                |term| (*term, reduce_rules.clone())
                            )
                        );
                    });
                }

                let mut ruleset_rules_body_stream = TokenStream::new();
                let mut ruleset_shifted_body_stream = TokenStream::new();
                let mut max_shifted = 0;
                for &rule in &state.ruleset {
                    max_shifted = max_shifted.max(rule.shifted);
                    let shifted = proc_macro2::Literal::usize_unsuffixed(rule.shifted);
                    let rule = proc_macro2::Literal::usize_unsuffixed(rule.rule);
                    ruleset_rules_body_stream.extend(quote! {
                        #rule,
                    });
                    ruleset_shifted_body_stream.extend(quote! {
                        #shifted,
                    });
                }
                let shifted_typename = if max_shifted <= u8::MAX as usize {
                    quote! { u8 }
                } else if max_shifted <= u16::MAX as usize {
                    quote! { u16 }
                } else if max_shifted <= u32::MAX as usize {
                    quote! { u32 }
                } else {
                    quote! { usize }
                };

                states_body_stream.extend(quote! {
                    #module_prefix::parser::state::IntermediateState {
                        shift_goto_map_term: vec![#shift_term_body_stream],
                        error_shift: #error_shift_stream,
                        eof_shift: #eof_shift_stream,
                        shift_goto_map_nonterm: vec![#shift_nonterm_body_stream],
                        reduce_map: {
                            let mut __reduce_map = std::collections::BTreeMap::new();
                            #reduce_body_stream
                            __reduce_map.into_iter().collect()
                        },
                        error_reduce: #error_reduce_stream,
                        eof_reduce: #eof_reduce_stream,
                        ruleset: {
                            let rules: &'static [#rule_index_typename] = &[
                                #ruleset_rules_body_stream
                            ];
                            let shifted: &'static [#shifted_typename] = &[
                                #ruleset_shifted_body_stream
                            ];
                            rules.iter().zip(shifted.iter()).map(
                                |(&rule, &shifted)| {
                                    #module_prefix::rule::ShiftedRuleRef {
                                        rule: rule as usize,
                                        shifted: shifted as usize,
                                    }
                                }
                            ).collect()
                        }
                    },
                });
            }

            let mut terminal_set_initialize_stream = TokenStream::new();
            for (set, name) in terminal_sets_name_map {
                let set_it = set
                    .into_iter()
                    .map(|val| proc_macro2::Literal::usize_unsuffixed(val));
                terminal_set_initialize_stream.extend(quote! {
                    let #name: Vec<#class_index_typename> = vec![#(#set_it),*];
                });
            }

            quote! {
                let rules: Vec<#module_prefix::rule::ProductionRule<
                    #module_prefix::TerminalSymbol<#class_index_typename>, _
                >> = vec![
                    #production_rules_body_stream
                ];
                let terminal_class_names = vec![
                    #terminal_class_names_stream
                ];
                let rules = rules.into_iter().map(
                    move |rule| {
                        rule.map(
                            |term| match term {
                                #module_prefix::TerminalSymbol::Term(term) => {
                                    terminal_class_names[term as usize]
                                }
                                #module_prefix::TerminalSymbol::Error => {
                                    "error"
                                }
                                #module_prefix::TerminalSymbol::Eof => {
                                    "eof"
                                }
                            },
                            |nonterm| nonterm,
                        )
                    }
                ).collect();

                #terminal_set_initialize_stream
                let states: Vec<#module_prefix::parser::state::IntermediateState<
                    #class_index_typename, _, #state_index_typename, #rule_index_typename
                >> = vec![
                    #states_body_stream
                ];
                let states:Vec<#state_typename> = states.into_iter().map(
                    |state| state.into(),
                ).collect();
            }
        } else {
            // build runtime

            // adding precedence levels
            let mut add_precedence_levels_stream = TokenStream::new();
            for (&term, &level) in self.builder.precedence_levels.iter() {
                let term = terminal_symbol_to_stream(term);
                add_precedence_levels_stream.extend(quote! {
                    builder.add_precedence(#term, #level);
                });
            }

            // adding production rules
            let mut add_rules_stream = TokenStream::new();
            for rule in &self.builder.rules {
                let mut tokens_vec_body_stream = TokenStream::new();
                for &token in &rule.rule.rule {
                    let token_stream = token_to_stream(token);
                    tokens_vec_body_stream.extend(quote! {
                        #token_stream,
                    });
                }

                // lookaheads
                let lookaheads_stream = if let Some(lookaheads) = rule.lookaheads.as_ref() {
                    let mut lookaheads_body_stream = TokenStream::new();
                    for &lookahead in lookaheads.iter() {
                        let lookahead = terminal_symbol_to_stream(lookahead);
                        lookaheads_body_stream.extend(quote! {
                            #lookahead,
                        });
                    }
                    quote! {
                        Some(std::collections::BTreeSet::from([#lookaheads_body_stream]))
                    }
                } else {
                    quote! { None }
                };

                // calculate operator
                let prec_stream = match rule.rule.precedence {
                    None => quote! { None },
                    Some(op) => {
                        let op_stream = precedence_to_stream(op);
                        quote! {Some(#op_stream)}
                    }
                };

                let priority = rule.priority;
                let dprec_stream = quote! { #priority };

                let nonterm_name = &nonterminals_token[rule.rule.name];
                add_rules_stream.extend(quote! {
                    builder.add_rule(
                        #nonterm_name,
                        vec![ #tokens_vec_body_stream ],
                        #lookaheads_stream,
                        #prec_stream,
                        #dprec_stream
                    );
                });
            }

            let prec_cap = self.builder.precedence_types.len();
            let mut precedence_types_stream = quote! {
                let mut precedence_types = Vec::with_capacity(#prec_cap);
            };
            for &reduce_type in self.builder.precedence_types.iter() {
                if let Some(reduce_type) = reduce_type {
                    let reduce_type = reduce_type_to_stream(reduce_type);
                    precedence_types_stream.extend(quote! {
                        precedence_types.push(Some(#reduce_type));
                    });
                } else {
                    precedence_types_stream.extend(quote! {
                        precedence_types.push(None);
                    });
                }
            }
            precedence_types_stream.extend(quote! {
                builder.set_precedence_types(precedence_types);
            });

            // building grammar
            let augmented_name = Ident::new(utils::AUGMENTED_NAME, Span::call_site());
            let build_stream = if self.lalr {
                quote! {
                    let Ok(states) = builder.build_lalr(
                        #nonterminals_enum_name::#augmented_name,
                        &mut #module_prefix::builder::DiagnosticCollector::new(false),
                    ) else {
                        unreachable!( "Failed to build LALR parser" )
                    };
                    let states = states.states;
                }
            } else {
                quote! {
                    let Ok(states) = builder.build(
                        #nonterminals_enum_name::#augmented_name,
                        &mut #module_prefix::builder::DiagnosticCollector::new(false),
                    ) else {
                        unreachable!( "Failed to build LR parser" )
                    };
                    let states = states.states;
                }
            };

            quote! {
                // create grammar builder
                let mut builder: #module_prefix::builder::Grammar<
                    #module_prefix::TerminalSymbol<#class_index_typename>, _
                > = #module_prefix::builder::Grammar::new();

                // add reduce types
                #add_precedence_levels_stream

                // add precedences
                #precedence_types_stream

                // production rules
                #add_rules_stream

                #build_stream

                let terminal_class_names = vec![
                    #terminal_class_names_stream
                ];

                let rules = builder.rules.into_iter().map(
                    move |rule| {
                        rule.rule.map(
                            |term| match term {
                                #module_prefix::TerminalSymbol::Term(term) => {
                                    terminal_class_names[term as usize]
                                }
                                #module_prefix::TerminalSymbol::Error => {
                                    "error"
                                }
                                #module_prefix::TerminalSymbol::Eof => {
                                    "eof"
                                }
                            },
                            |nonterm| nonterm,
                        )
                    }
                ).collect();

                let states:Vec<#state_typename> = states.into_iter().map(
                    |state| {
                        let state: #module_prefix::parser::state::IntermediateState<_,_,_,_> = state.into();
                        state.into()
                    }
                ).collect();
            }
        };

        let use_range_based_optimization = if self.is_char || self.is_u8 {
            self.calculate_range_terminal_class_map()
        } else {
            false
        };

        let other_class_id = self.other_terminal_class_id;

        // generate `Parser::class_precedence()` terminal class -> precedence level match body
        let class_level_match_body_stream = {
            let mut stream = TokenStream::new();

            let mut level_classes = Vec::new();
            level_classes.resize(
                self.builder.precedence_types.len(),
                std::collections::BTreeSet::new(),
            );
            for (&class, &level) in self.builder.precedence_levels.iter() {
                level_classes[level].insert(class.into_term().unwrap());
            }

            for (level, classes) in level_classes.into_iter().enumerate() {
                if classes.is_empty() {
                    continue;
                } else {
                    let case_stream = list_to_case_stream(classes.into_iter());
                    debug_assert!(level < u8::MAX as usize);
                    let level = proc_macro2::Literal::usize_unsuffixed(level);
                    stream.extend(quote! {
                        #case_stream => #module_prefix::parser::Precedence::new(#level),
                    });
                }
            }
            stream
        };

        let error_used = self.error_used;
        let error_prec_stream = if let Some(error_prec) = self.error_precedence {
            debug_assert!(error_prec < u8::MAX as usize);
            let error_prec = proc_macro2::Literal::usize_unsuffixed(error_prec);
            quote! { #module_prefix::parser::Precedence::new(#error_prec) }
        } else {
            quote! { #module_prefix::parser::Precedence::none() }
        };

        // building terminal-class_id map
        if use_range_based_optimization {
            // range-compressed Vec based terminal-class_id map

            // for terminal_class -> [terminals] map get_terminals()
            let mut classes_body = TokenStream::new();
            for (class_id, class_def) in self.terminal_classes.iter().enumerate() {
                if class_id == self.other_terminal_class_id {
                    classes_body.extend(quote! {
                        vec![],
                    });
                }
                let mut terminals_body = TokenStream::new();
                for &(s, e) in class_def.ranges.iter() {
                    let stream = if self.is_char {
                        let s = unsafe { char::from_u32_unchecked(s) };
                        let e = unsafe { char::from_u32_unchecked(e) };

                        quote! { #s..=#e }
                    } else if self.is_u8 {
                        let s = s as u8;
                        let e = e as u8;

                        quote! { #s..=#e }
                    } else {
                        unreachable!("unexpected char type")
                    };

                    terminals_body.extend(quote! {
                        #stream,
                    });
                }
                classes_body.extend(quote! {
                    vec![ #terminals_body ],
                });
            }

            // for terminal -> terminal_class_id map to_terminal_class()
            let mut terminal_class_match_body_stream = TokenStream::new();
            for (class_id, class_def) in self.terminal_classes.iter().enumerate() {
                if class_id == self.other_terminal_class_id {
                    continue;
                }
                let mut match_case_stream = TokenStream::new();
                for (i, &(s, e)) in class_def.ranges.iter().enumerate() {
                    let stream = if self.is_char {
                        let s = unsafe { char::from_u32_unchecked(s) };
                        let e = unsafe { char::from_u32_unchecked(e) };

                        if s == e {
                            quote! { #s }
                        } else {
                            quote! { #s..=#e }
                        }
                    } else if self.is_u8 {
                        let s = s as u8;
                        let e = e as u8;

                        if s == e {
                            quote! { #s }
                        } else {
                            quote! { #s..=#e }
                        }
                    } else {
                        unreachable!("unexpected char type")
                    };

                    if i > 0 {
                        match_case_stream.extend(quote! {|});
                    }
                    match_case_stream.extend(quote! { #stream });
                }
                let class_id = proc_macro2::Literal::usize_unsuffixed(class_id);
                terminal_class_match_body_stream.extend(quote! {
                    #match_case_stream => #class_id,
                });
            }
            {
                let other_class_id = proc_macro2::Literal::usize_unsuffixed(other_class_id);
                terminal_class_match_body_stream.extend(quote! {
                    _ => #other_class_id,
                });
            }

            stream.extend(quote! {
            /// A struct that holds the entire parser table and production rules.
            #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
            pub struct #parser_struct_name {
                /// production rules
                pub rules: Vec<#rule_typename>,
                /// states
                pub states: Vec<#state_typename>,
                /// terminal classes
                pub classes: Vec<Vec<::std::ops::RangeInclusive<#token_typename>>>,
            }
            impl #module_prefix::parser::Parser for #parser_struct_name {
                type Term = #token_typename;
                type NonTerm = #nonterminals_enum_name;
                type State = #state_typename;
                type TerminalClassElement = ::std::ops::RangeInclusive<#token_typename>;

                const ERROR_USED:bool = #error_used;

                fn class_precedence(&self, class: #module_prefix::TerminalSymbol<usize>) -> #module_prefix::parser::Precedence {
                    match class {
                        #module_prefix::TerminalSymbol::Term(class) => {
                            #[allow(unreachable_patterns)]
                            match class {
                                #class_level_match_body_stream
                                _ => #module_prefix::parser::Precedence::none(),
                            }
                        }
                        #module_prefix::TerminalSymbol::Error => #error_prec_stream,
                        #module_prefix::TerminalSymbol::Eof => {
                            unreachable!("eof token cannot be used in precedence levels")
                        }
                    }
                }
                fn precedence_types(&self, level: u8) -> Option<#module_prefix::builder::ReduceType> {
                    #[allow(unreachable_patterns)]
                    match level {
                        #precedence_types_match_body_stream
                    }
                }
                fn get_rules(&self) -> &[#rule_typename] {
                    &self.rules
                }
                fn get_states(&self) -> &[#state_typename] {
                    &self.states
                }
                fn get_terminals(&self, i: usize) -> Option<impl IntoIterator<Item = Self::TerminalClassElement> + '_> {
                    self.classes.get(i).map(
                        |class| class.iter().cloned()
                    )
                }
                fn to_terminal_class(&self, terminal: &Self::Term) -> usize {
                    // Self::Term is char or u8 here
                    #[allow(unreachable_patterns)]
                    match *terminal {
                        #terminal_class_match_body_stream
                    }
                }
            }

            /// A struct that holds the whole parser table.
            #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
            impl #parser_struct_name {
                /// Calculates the states and parser tables from the grammar.
                #[allow(clippy::clone_on_copy)]
                pub fn new() -> Self {
                    #grammar_build_stream

                    Self {
                        rules,
                        states,
                        classes: vec![ #classes_body ],
                    }
                }
            }

            });
        } else {
            // match based terminal-class_id map

            // for terminal_class -> [terminals] map get_terminals()
            let mut classes_body = TokenStream::new();
            for (class_id, class_def) in self.terminal_classes.iter().enumerate() {
                // no need to store all characters for 'other' class, if it was not used in the grammar
                if class_id == self.other_terminal_class_id {
                    continue;
                }
                let mut terminals_body = TokenStream::new();
                for &term in &class_def.terminals {
                    // check if this term is range-based character
                    match &self.terminals[term].name {
                        TerminalName::CharRange(s, l) => {
                            if self.is_char {
                                let range_stream = if s == l {
                                    quote! { #s }.to_string()
                                } else {
                                    quote! {
                                        #s-#l
                                    }
                                    .to_string()
                                };
                                terminals_body.extend(quote! {
                                    #range_stream,
                                });
                            } else if self.is_u8 {
                                let s = *s as u8;
                                let l = *l as u8;
                                let range_stream = if s == l {
                                    quote! { #s }.to_string()
                                } else {
                                    quote! {
                                        #s-#l
                                    }
                                    .to_string()
                                };
                                terminals_body.extend(quote! {
                                    #range_stream,
                                });
                            } else {
                                unreachable!("unexpected char type")
                            }
                        }
                        TerminalName::Ident(ident) => {
                            let name = ident.to_string();
                            terminals_body.extend(quote! {
                                #name,
                            });
                        }
                    }
                }

                classes_body.extend(quote! {
                    vec![ #terminals_body ],
                });
            }

            // for terminal -> terminal_class_id map to_terminal_class()
            let mut terminal_class_match_body_stream = TokenStream::new();
            for (class_id, class_def) in self.terminal_classes.iter().enumerate() {
                if class_id == self.other_terminal_class_id {
                    continue;
                }
                let mut match_case_stream = TokenStream::new();
                for (i, &term) in class_def.terminals.iter().enumerate() {
                    // check if this term is range-based character
                    let case_stream = match &self.terminals[term].name {
                        TerminalName::CharRange(s, l) => {
                            if self.is_char {
                                if s == l {
                                    quote! {#s}
                                } else {
                                    quote! {#s..=#l}
                                }
                            } else if self.is_u8 {
                                let s = *s as u8;
                                let l = *l as u8;
                                if s == l {
                                    quote! {#s}
                                } else {
                                    quote! {#s..=#l}
                                }
                            } else {
                                unreachable!("unexpected char type")
                            }
                        }
                        TerminalName::Ident(_) => {
                            let term_stream = &self.terminals[term].body;
                            quote! {#term_stream}
                        }
                    };

                    if i > 0 {
                        match_case_stream.extend(quote! { | });
                    }
                    match_case_stream.extend(case_stream);
                }
                let class_id = proc_macro2::Literal::usize_unsuffixed(class_id);
                terminal_class_match_body_stream.extend(quote! {
                    #match_case_stream => #class_id,
                });
            }
            {
                let other_class_id = proc_macro2::Literal::usize_unsuffixed(other_class_id);
                terminal_class_match_body_stream.extend(quote! {
                    _ => #other_class_id,
                });
            }

            let match_terminal_filter_expression = if let Some(filter) = &self.filter {
                quote! { #filter(terminal) }
            } else {
                quote! {terminal}
            };

            stream.extend(quote! {
            /// A struct that holds the entire parser table and production rules.
            #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
            pub struct #parser_struct_name {
                /// production rules
                pub rules: Vec<#rule_typename>,
                /// states
                pub states: Vec<#state_typename>,
                /// terminal classes
                pub classes: Vec<Vec<&'static str>>,
            }
            impl #module_prefix::parser::Parser for #parser_struct_name {
                type Term = #token_typename;
                type NonTerm = #nonterminals_enum_name;
                type State = #state_typename;
                type TerminalClassElement = &'static str;

                const ERROR_USED:bool = #error_used;

                fn class_precedence(&self, class: #module_prefix::TerminalSymbol<usize>) -> #module_prefix::parser::Precedence {
                    match class {
                        #module_prefix::TerminalSymbol::Term(class) => {
                            #[allow(unreachable_patterns)]
                            match class {
                                #class_level_match_body_stream
                                _ => #module_prefix::parser::Precedence::none(),
                            }
                        }
                        #module_prefix::TerminalSymbol::Error => #error_prec_stream,
                        #module_prefix::TerminalSymbol::Eof => {
                            unreachable!("eof token cannot be used in precedence levels")
                        }
                    }
                }
                fn precedence_types(&self, level: u8) -> Option<#module_prefix::builder::ReduceType> {
                    #[allow(unreachable_patterns)]
                    match level {
                        #precedence_types_match_body_stream
                    }
                }
                fn get_rules(&self) -> &[#rule_typename] {
                    &self.rules
                }
                fn get_states(&self) -> &[#state_typename] {
                    &self.states
                }
                fn get_terminals(&self, i: usize) -> Option<impl IntoIterator<Item = Self::TerminalClassElement> + '_> {
                    self.classes.get(i).map(
                        |class| class.iter().copied()
                    )
                }
                fn to_terminal_class(&self, terminal: &Self::Term) -> usize {
                    #[allow(unreachable_patterns)]
                    match #match_terminal_filter_expression {
                        #terminal_class_match_body_stream
                    }
                }
            }

            /// A struct that holds the whole parser table.
            #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
            impl #parser_struct_name {
                /// Calculates the states and parser tables from the grammar.
                #[allow(clippy::clone_on_copy)]
                pub fn new() -> Self {
                    #grammar_build_stream

                    Self {
                        rules,
                        states,
                        classes: vec![#classes_body],
                    }
                }
            }

            });
        }
    }

    fn emit_data_stack(&self, stream: &mut TokenStream) {
        use rusty_lr_core::TerminalSymbol;
        use rusty_lr_core::Token;

        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let data_stack_typename = format_ident!("{}DataStack", self.start_rule_name);
        let token_typename = &self.token_typename;
        let user_data_parameter_name =
            Ident::new(utils::USER_DATA_PARAMETER_NAME, Span::call_site());
        let user_data_typename = &self.userdata_typename;
        let location_typename = self
            .location_typename
            .as_ref()
            .cloned()
            .unwrap_or_else(|| quote! { #module_prefix::DefaultLocation });

        // stack name for tags
        let tag_stack_name = format_ident!("__tags");
        let tag_enum_name = format_ident!("{}Tags", &self.start_rule_name);

        // empty tag
        let empty_tag_name = format_ident!("Empty");

        // stack name for terminal symbol
        let terminal_stack_name = format_ident!("__terminals");
        // stack name for each non-terminal
        let mut stack_names_for_nonterm = Vec::with_capacity(self.nonterminals.len());

        // (<RuleType as ToString>, stack_name) map
        let mut ruletype_stack_map: rusty_lr_core::hash::HashMap<String, Option<Ident>> =
            Default::default();

        // (stack_name, TokenStream for typename) sorted in insertion order
        // for consistent output
        let mut stack_names_in_order = Vec::new();

        // insert stack for empty
        ruletype_stack_map.insert("".to_string(), None);

        // insert stack for terminal token type
        ruletype_stack_map.insert(
            self.token_typename.to_string(),
            Some(terminal_stack_name.clone()),
        );
        stack_names_in_order.push((terminal_stack_name.clone(), self.token_typename.clone()));

        fn remove_whitespaces(s: String) -> String {
            s.chars().filter(|c| !c.is_whitespace()).collect()
        }

        for nonterm in self.nonterminals.iter() {
            let ruletype_stream = nonterm.ruletype.as_ref().cloned().unwrap_or_default();

            let cur_len = ruletype_stack_map.len();
            let stack_name = ruletype_stack_map
                .entry(remove_whitespaces(ruletype_stream.to_string()))
                .or_insert_with(|| {
                    let new_stack_name = format_ident!("__stack{}", cur_len);
                    stack_names_in_order.push((new_stack_name.clone(), ruletype_stream.clone()));
                    Some(new_stack_name)
                })
                .clone();
            stack_names_for_nonterm.push(stack_name);
        }

        // Token -> Option<stack_name> map, `None` for empty
        let token_to_stack_name = |token: Token<TerminalSymbol<usize>, usize>| match token {
            Token::Term(term) => match term {
                TerminalSymbol::Term(_) => Some(&terminal_stack_name),
                TerminalSymbol::Error | TerminalSymbol::Eof => None,
            },
            Token::NonTerm(nonterm_idx) => stack_names_for_nonterm[nonterm_idx].as_ref(),
        };

        let mut reduce_action_case_streams = quote! {};

        // TokenStream to define reduce function for each production rule
        let mut fn_reduce_for_each_rule_stream = TokenStream::new();

        let mut rule_index: usize = 0;
        for (nonterm_idx, nonterm) in self.nonterminals.iter().enumerate() {
            for (rule_local_id, rule) in nonterm.rules.iter().enumerate() {
                let reduce_fn_ident = format_ident!("reduce_{}_{}", nonterm.name, rule_local_id);

                let rule_debug_str = format!(
                    "{} -> {}",
                    self.nonterm_pretty_name(nonterm_idx),
                    rule.tokens
                        .iter()
                        .map(|t| {
                            match t.token {
                                Token::Term(term) => self.class_pretty_name_list(term, 5),
                                Token::NonTerm(nonterm) => self.nonterm_pretty_name(nonterm),
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                );

                use super::nonterminal_info::ReduceAction;

                match &rule.reduce_action {
                    Some(ReduceAction::Custom(reduce_action)) => {
                        let mut debug_tag_check_stream = TokenStream::new();
                        let mut stack_mapto_map = std::collections::BTreeMap::new();
                        for (token_index_from_end, token) in rule.tokens.iter().rev().enumerate() {
                            let stack_name = token_to_stack_name(token.token);
                            let tag_name = stack_name.unwrap_or(&empty_tag_name);

                            #[derive(PartialEq, Eq, PartialOrd, Ord)]
                            enum StackName {
                                DataStack(Ident),
                                LocationStack,
                            }
                            impl StackName {
                                pub fn to_token_stream(&self) -> TokenStream {
                                    match self {
                                        StackName::DataStack(name) => quote! {__data_stack.#name},
                                        StackName::LocationStack => quote! {__location_stack},
                                    }
                                }
                            }
                            fn tokenstream_contains_ident(
                                stream: TokenStream,
                                ident: &Ident,
                            ) -> bool {
                                for t in stream {
                                    match t {
                                        proc_macro2::TokenTree::Ident(i) if &i == ident => {
                                            return true
                                        }
                                        proc_macro2::TokenTree::Group(g) => {
                                            if tokenstream_contains_ident(g.stream(), ident) {
                                                return true;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                                false
                            }

                            if let Some(stack_name) = stack_name {
                                // if variable was not used at this reduce action,
                                // we can use `truncate` instead of `pop` for optimization
                                // so check it here
                                let mapto = if let Some(mapto) = &token.mapto {
                                    if tokenstream_contains_ident(reduce_action.clone(), mapto) {
                                        Some(mapto.clone())
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };
                                stack_mapto_map
                                    .entry(StackName::DataStack(stack_name.clone()))
                                    .or_insert_with(Vec::new)
                                    .push(mapto);
                            }
                            let location_mapto = if let Some(mapto) = &token.mapto {
                                let location_varname =
                                    format_ident!("__rustylr_location_{}", mapto);

                                // if variable was not used at this reduce action,
                                // we can use `truncate` instead of `pop` for optimization
                                // so check it here
                                if tokenstream_contains_ident(
                                    reduce_action.clone(),
                                    &location_varname,
                                ) {
                                    Some(location_varname)
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            stack_mapto_map
                                .entry(StackName::LocationStack)
                                .or_insert_with(Vec::new)
                                .push(location_mapto);

                            debug_tag_check_stream.extend(quote! {
                                debug_assert!(
                                    __data_stack.#tag_stack_name.get(
                                        __data_stack.#tag_stack_name.len()-1-#token_index_from_end
                                    ) == Some( &#tag_enum_name::#tag_name )
                                );
                            });
                        }

                        // if there are variable with same name, the last one will be used (by shadowing)
                        // so set the front one to `None`
                        // this will also help optimizing performance by using `truncate` instead of `pop`
                        for maptos in &mut stack_mapto_map.values_mut() {
                            for var_right in (0..maptos.len()).rev() {
                                if let Some(var_name) = maptos[var_right].as_ref().cloned() {
                                    for var_left in 0..var_right {
                                        if maptos[var_left].as_ref() == Some(&var_name) {
                                            maptos[var_left] = None;
                                        }
                                    }
                                }
                            }
                        }

                        // new tag that will be inserted by this reduce action
                        let new_tag_name = stack_names_for_nonterm[nonterm_idx]
                            .as_ref()
                            .unwrap_or(&empty_tag_name);
                        // pop n tokens from tag_stack and push new reduced tag
                        let modify_tag_stream = if rule.tokens.len() > 0 {
                            // if first token's tag is equal to new_tag, no need to (pop n tokens -> push new token).
                            // just pop n-1 tokens
                            let first_tag_name = token_to_stack_name(rule.tokens[0].token)
                                .unwrap_or(&empty_tag_name);

                            if first_tag_name == new_tag_name {
                                // pop n-1 tokens, no new insertion
                                let len = rule.tokens.len() - 1;
                                let truncate_stream = if len > 0 {
                                    quote! {__data_stack.#tag_stack_name.truncate(__data_stack.#tag_stack_name.len() - #len);}
                                } else {
                                    TokenStream::new()
                                };
                                truncate_stream
                            } else {
                                let len = rule.tokens.len();
                                // len > 0 here
                                quote! {
                                    __data_stack.#tag_stack_name.truncate(__data_stack.#tag_stack_name.len() - #len);
                                    __data_stack.#tag_stack_name.push(#tag_enum_name::#new_tag_name);
                                }
                            }
                        } else {
                            quote! {
                                __data_stack.#tag_stack_name.push(#tag_enum_name::#new_tag_name);
                            }
                        };

                        let mut extract_data_stream = TokenStream::new();
                        for (stack_name, maptos) in stack_mapto_map.iter() {
                            let stack_stream = stack_name.to_token_stream();

                            // if there are consecutive `None` mapto, truncate instead of pop
                            let mut last_none_count: usize = 0;
                            for mapto in maptos {
                                match mapto {
                                    Some(mapto) => {
                                        if last_none_count > 0 {
                                            extract_data_stream.extend(quote! {
                                                #stack_stream.truncate(#stack_stream.len() - #last_none_count);
                                            });
                                            last_none_count = 0;
                                        }
                                        extract_data_stream.extend(quote! {
                                            let mut #mapto = #stack_stream.pop().unwrap();
                                        });
                                    }
                                    None => {
                                        last_none_count += 1;
                                    }
                                }
                            }
                            if last_none_count > 0 {
                                extract_data_stream.extend(quote! {
                                    #stack_stream.truncate(#stack_stream.len() - #last_none_count);
                                });
                            }
                        }

                        reduce_action_case_streams.extend(quote! {
                            #rule_index => Self::#reduce_fn_ident( data_stack, location_stack, shift, lookahead, user_data, location0 ),
                        });

                        // typename is defined, reduce action must be defined
                        if let Some(stack_name) = &stack_names_for_nonterm[nonterm_idx] {
                            fn_reduce_for_each_rule_stream.extend(quote! {
                                #[doc = #rule_debug_str]
                                #[inline]
                                fn #reduce_fn_ident(
                                    __data_stack: &mut Self,
                                    __location_stack: &mut Vec<#location_typename>,
                                    shift: &mut bool,
                                    lookahead: &#module_prefix::TerminalSymbol<#token_typename>,
                                    #user_data_parameter_name: &mut #user_data_typename,
                                    __rustylr_location0: &mut #location_typename,
                                ) -> Result<(), #reduce_error_typename> {
                                    #[cfg(debug_assertions)]
                                    {
                                        #debug_tag_check_stream
                                    }
                                    #modify_tag_stream

                                    #extract_data_stream

                                    let __res = #reduce_action ;
                                    __data_stack.#stack_name.push(__res);

                                    Ok(())
                                }
                            });
                        } else {
                            fn_reduce_for_each_rule_stream.extend(quote! {
                                #[doc = #rule_debug_str]
                                #[inline]
                                fn #reduce_fn_ident(
                                    __data_stack: &mut Self,
                                    __location_stack: &mut Vec<#location_typename>,
                                    shift: &mut bool,
                                    lookahead: &#module_prefix::TerminalSymbol<#token_typename>,
                                    #user_data_parameter_name: &mut #user_data_typename,
                                    __rustylr_location0: &mut #location_typename,
                                ) -> Result<(), #reduce_error_typename> {
                                    #[cfg(debug_assertions)]
                                    {
                                        #debug_tag_check_stream
                                    }
                                    #modify_tag_stream

                                    #extract_data_stream

                                    #reduce_action

                                    Ok(())
                                }
                            });
                        }
                    }

                    // E(i32): a b c(i32) d e ;
                    // 'c' will be automatically chosen, even if there is no reduce action
                    &Some(ReduceAction::Identity(identity_token_idx)) => {
                        reduce_action_case_streams.extend(quote! {
                            #rule_index => { Self::#reduce_fn_ident( data_stack, location_stack ); Ok(()) }
                        });

                        let mut stack_count_map = std::collections::BTreeMap::new();
                        let mut pop_stack_idx_pair = None;
                        for (token_idx, token) in rule.tokens.iter().enumerate().rev() {
                            let stack_name = token_to_stack_name(token.token);

                            if token_idx == identity_token_idx {
                                if let Some(stack_name) = stack_name {
                                    let idx_from_back =
                                        stack_count_map.get(stack_name).copied().unwrap_or(0);
                                    pop_stack_idx_pair = Some((stack_name, idx_from_back));
                                }
                            } else {
                                if let Some(stack_name) = stack_name {
                                    *stack_count_map.entry(stack_name).or_insert(0usize) += 1;
                                }
                            }
                        }

                        let len = rule.tokens.len();
                        debug_assert!(len > 0);

                        // let temp = stack.swap_remove( i ); <-- pop_stream
                        // stack.truncate( len-1 ); <-- truncate_stream
                        // stack.push( temp ); <-- push_stream

                        let (stack_pop_stream, stack_push_stream) = if let Some((
                            pop_stack,
                            pop_index_from_back,
                        )) = pop_stack_idx_pair
                        {
                            // if i is the first inserted element on the stack, no need to pop and push
                            if pop_index_from_back
                                == stack_count_map.get(pop_stack).copied().unwrap_or(0)
                            {
                                (quote! {}, quote! {})
                            } else {
                                (
                                    quote! { let __ret = __data_stack.#pop_stack.swap_remove( __data_stack.#pop_stack.len() - 1  - #pop_index_from_back ); },
                                    quote! { __data_stack.#pop_stack.push(__ret); },
                                )
                            }
                        } else {
                            (quote! {}, quote! {})
                        };
                        let mut stack_truncate_stream = TokenStream::new();
                        for (stack_name, count) in stack_count_map {
                            debug_assert!(count > 0);
                            stack_truncate_stream.extend(quote! {
                                __data_stack.#stack_name.truncate(__data_stack.#stack_name.len() - #count);
                            });
                        }

                        let is_0th_token_same_tag_ith_token =
                            token_to_stack_name(rule.tokens[0].token)
                                == token_to_stack_name(rule.tokens[identity_token_idx].token);

                        let (tags_truncate_stream, tag_push_stream) =
                            if is_0th_token_same_tag_ith_token {
                                let truncate_len = len - 1;
                                if truncate_len > 0 {
                                    (
                                        quote! { __data_stack.#tag_stack_name.truncate(__data_stack.#tag_stack_name.len() - #truncate_len); },
                                        quote! {},
                                    )
                                } else {
                                    (quote! {}, quote! {})
                                }
                            } else {
                                let tag_push_stream = if let Some((pop_stack, _)) =
                                    pop_stack_idx_pair
                                {
                                    quote! { __data_stack.#tag_stack_name.push(#tag_enum_name::#pop_stack); }
                                } else {
                                    quote! {__data_stack.#tag_stack_name.push(#tag_enum_name::#empty_tag_name); }
                                };

                                let tags_truncate_stream = quote! { __data_stack.#tag_stack_name.truncate(__data_stack.#tag_stack_name.len() - #len); };

                                (tags_truncate_stream, tag_push_stream)
                            };

                        let location_truncate_stream =
                            quote! { __location_stack.truncate(__location_stack.len() - #len); };

                        fn_reduce_for_each_rule_stream.extend(quote! {
                            #[doc = #rule_debug_str]
                            #[inline(always)]
                            fn #reduce_fn_ident(
                                __data_stack: &mut Self,
                                __location_stack: &mut Vec<#location_typename>,
                            ) {
                                #stack_pop_stream // __ret = stack.swap_remove(i);
                                #stack_truncate_stream // stack.truncate( ... );
                                #stack_push_stream // stack.push( __ret );
                                #location_truncate_stream // location.truncate( ... );
                                #tags_truncate_stream // tags.truncate( ... );
                                #tag_push_stream // tag.push( ... );
                            }
                        });
                    }

                    // E: a b c d ...
                    // 'E' does not have rule type, so no need for a reduce action
                    None => {
                        reduce_action_case_streams.extend(quote! {
                            #rule_index => { Self::#reduce_fn_ident( data_stack, location_stack ); Ok(()) }
                        });

                        let mut stack_count_map = std::collections::BTreeMap::new();
                        for token in rule.tokens.iter() {
                            let stack_name = token_to_stack_name(token.token);
                            if let Some(stack_name) = stack_name {
                                *stack_count_map.entry(stack_name).or_insert(0usize) += 1;
                            }
                        }
                        let mut stack_truncate_stream = TokenStream::new();
                        for (stack_name, count) in stack_count_map {
                            debug_assert!(count > 0);
                            stack_truncate_stream.extend(quote! {
                                __data_stack.#stack_name.truncate(__data_stack.#stack_name.len() - #count);
                            });
                        }
                        let location_truncate_stream = if rule.tokens.len() > 0 {
                            let len = rule.tokens.len();
                            quote! { __location_stack.truncate(__location_stack.len() - #len); }
                        } else {
                            quote! {}
                        };
                        let (tags_truncate_stream, tag_push_stream) = if rule.tokens.len() > 0 {
                            let is_first_token_empty_tag =
                                token_to_stack_name(rule.tokens[0].token).is_none();
                            if is_first_token_empty_tag {
                                // pop n-1 tags and use n-1'th tag as-is
                                let len = rule.tokens.len() - 1;
                                if len > 0 {
                                    (
                                        quote! { __data_stack.#tag_stack_name.truncate(__data_stack.#tag_stack_name.len() - #len); },
                                        quote! {},
                                    )
                                } else {
                                    (quote! {}, quote! {})
                                }
                            } else {
                                // pop all n tags and push empty tag
                                let len = rule.tokens.len();
                                (
                                    quote! { __data_stack.#tag_stack_name.truncate(__data_stack.#tag_stack_name.len() - #len); },
                                    quote! { __data_stack.#tag_stack_name.push(#tag_enum_name::#empty_tag_name); },
                                )
                            }
                        } else {
                            (
                                quote! {},
                                quote! { __data_stack.#tag_stack_name.push(#tag_enum_name::#empty_tag_name); },
                            )
                        };
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            #[doc = #rule_debug_str]
                            #[inline(always)]
                            fn #reduce_fn_ident(
                                __data_stack: &mut Self,
                                __location_stack: &mut Vec<#location_typename>,
                            ) {
                                #stack_truncate_stream
                                #location_truncate_stream
                                #tags_truncate_stream
                                #tag_push_stream
                            }
                        });
                    }
                }
                rule_index += 1;
            }
        }

        let start_idx = *self.nonterminals_index.get(&self.start_rule_name).unwrap();
        let start_stack_name = &stack_names_for_nonterm[start_idx];
        let (start_typename, pop_start) = match start_stack_name {
            Some(stack_name) => {
                let ruletype = self.nonterminals[start_idx]
                    .ruletype
                    .as_ref()
                    .unwrap()
                    .clone();
                (
                    ruletype,
                    quote! {
                        let tag = self.#tag_stack_name.pop();
                        debug_assert!(tag == Some(#tag_enum_name::#stack_name));
                        self.#stack_name.pop()
                    },
                )
            }
            None => (
                quote! {()},
                quote! {
                    let tag = self.#tag_stack_name.pop();
                    debug_assert!(tag == Some(#tag_enum_name::#empty_tag_name));
                    Some(())
                },
            ),
        };

        let mut tag_definition_stream = TokenStream::new();
        for (stack_name, _) in &stack_names_in_order {
            tag_definition_stream.extend(quote! {
                #stack_name,
            });
        }

        let mut stack_definition_stream = TokenStream::new();
        let mut stack_default_stream = TokenStream::new();
        let mut pop_match_stream = TokenStream::new();
        let mut stack_clear_stream = TokenStream::new();
        let mut stack_append_stream = TokenStream::new();
        let stack_len = stack_names_in_order.len();
        let split_off_count_init_stream = quote! {
            let mut __counts: [u8; #stack_len+1] = [0; #stack_len+1];
        };
        let mut split_off_split_stream = TokenStream::new();
        let mut split_off_ctor_stream = TokenStream::new();
        for (stack_idx, (stack_name, typename)) in stack_names_in_order.iter().enumerate() {
            stack_definition_stream.extend(quote! {
                #stack_name: Vec<#typename>,
            });
            stack_default_stream.extend(quote! {
                #stack_name: Vec::new(),
            });
            pop_match_stream.extend(quote! {
                #tag_enum_name::#stack_name => { self.#stack_name.pop(); }
            });
            stack_clear_stream.extend(quote! {
                self.#stack_name.clear();
            });
            stack_append_stream.extend(quote! {
                self.#stack_name.append(&mut other.#stack_name);
            });

            let other_stack_name = format_ident!("__other_{}", stack_name);
            split_off_split_stream.extend(quote! {
                let #other_stack_name = self.#stack_name.split_off( self.#stack_name.len() - (__counts[#stack_idx] as usize) );
            });
            split_off_ctor_stream.extend(quote! {
                #stack_name: #other_stack_name,
            });
        }

        let derive_clone_for_glr = if self.glr {
            quote! {#[derive(Clone)]}
        } else {
            quote! {}
        };

        stream.extend(quote! {
        /// tag for token that represents which stack a token is using
        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub enum #tag_enum_name {
            #tag_definition_stream
            #empty_tag_name,
        }

        /// enum for each non-terminal and terminal symbol, that actually hold data
        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
        #derive_clone_for_glr
        pub struct #data_stack_typename {
            pub #tag_stack_name: Vec<#tag_enum_name>,
            #stack_definition_stream
        }

        impl Default for #data_stack_typename {
            fn default() -> Self {
                Self {
                    #tag_stack_name: Vec::new(),
                    #stack_default_stream
                }
            }
        }

        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #data_stack_typename {
            #fn_reduce_for_each_rule_stream
        }


        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types, unused_variables)]
        impl #module_prefix::parser::data_stack::DataStack for #data_stack_typename {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;
            type ReduceActionError = #reduce_error_typename;
            type UserData = #user_data_typename;
            type StartType = #start_typename;
            type Location = #location_typename;

            fn pop_start(&mut self) -> Option<Self::StartType> {
                #pop_start
            }
            fn pop(&mut self) {
                match self.#tag_stack_name.pop().unwrap() {
                    #pop_match_stream
                    _ => {}
                }
            }
            fn push_terminal(&mut self, term: Self::Term) {
                self.#tag_stack_name.push(#tag_enum_name::#terminal_stack_name);
                self.#terminal_stack_name.push( term );
            }
            fn push_empty(&mut self) {
                self.#tag_stack_name.push(#tag_enum_name::#empty_tag_name);
            }

            fn clear(&mut self) {
                self.#tag_stack_name.clear();
                #stack_clear_stream
            }
            fn reserve(&mut self, additional: usize) {
                self.#tag_stack_name.reserve(additional);
            }

            fn split_off(&mut self, at: usize) -> Self {
                let __other_tag_stack = self.#tag_stack_name.split_off(at);

                #split_off_count_init_stream
                for &tag in &__other_tag_stack {
                    __counts[ tag as usize ] += 1;
                }
                #split_off_split_stream
                Self {
                    #tag_stack_name: __other_tag_stack,
                    #split_off_ctor_stream
                }
            }
            fn append(&mut self, other: &mut Self) {
                self.#tag_stack_name.append(&mut other.#tag_stack_name);
                #stack_append_stream
            }

            fn reduce_action(
                data_stack: &mut Self,
                location_stack: &mut Vec<#location_typename>,
                rule_index: usize,
                shift: &mut bool,
                lookahead: &#module_prefix::TerminalSymbol<Self::Term>,
                user_data: &mut Self::UserData,
                location0: &mut Self::Location,
            ) -> Result<(), Self::ReduceActionError> {
                match rule_index {
                    #reduce_action_case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rule_index );
                    }
                }
            }
        }

        });
    }

    pub fn emit_compiletime(&self) -> TokenStream {
        let mut stream = TokenStream::new();
        self.emit_type_alises(&mut stream);
        self.emit_nonterm_enum(&mut stream);
        self.emit_data_stack(&mut stream);
        self.emit_parser(&mut stream);

        stream
    }
}
