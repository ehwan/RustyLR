use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use rusty_lr_core::Token;

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
        let token_data_typename = format_ident!("{}TokenData", start_rule_name);

        let state_structname = if self.emit_dense {
            format_ident!("DenseState")
        } else {
            format_ident!("SparseState")
        };

        if self.glr {
            // count the number of rules
            // and calculate the integral type for rule index -> u8, u16, u32, usize ...
            let rule_container_type = if self.builder.rules.len() <= u8::MAX as usize {
                quote! { #module_prefix::stackvec::SmallVecU8 }
            } else if self.builder.rules.len() <= u16::MAX as usize {
                quote! { #module_prefix::stackvec::SmallVecU16 }
            } else if self.builder.rules.len() <= u32::MAX as usize {
                quote! { #module_prefix::stackvec::SmallVecU32 }
            } else {
                quote! { #module_prefix::stackvec::SmallVecUsize }
            };

            stream.extend(
        quote! {
                /// type alias for `Context`
                #[allow(non_camel_case_types,dead_code)]
                pub type #context_struct_name = #module_prefix::parser::nondeterministic::Context<#token_data_typename>;
                /// type alias for CFG production rule
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::rule::ProductionRule<&'static str, #enum_name>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::parser::nondeterministic::state::#state_structname<#enum_name, #rule_container_type>;
                /// type alias for `InvalidTerminalError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::parser::nondeterministic::ParseError<#token_data_typename>;
            }
            );
        } else {
            stream.extend(
        quote! {
                /// type alias for `Context`
                #[allow(non_camel_case_types,dead_code)]
                pub type #context_struct_name = #module_prefix::parser::deterministic::Context<#token_data_typename>;
                /// type alias for CFG production rule
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::rule::ProductionRule<&'static str, #enum_name>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::parser::deterministic::state::#state_structname<#enum_name>;
                /// type alias for `ParseError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::parser::deterministic::ParseError<#token_data_typename>;
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
        use rusty_lr_core::builder::Operator;
        use rusty_lr_core::builder::ReduceType;
        use rusty_lr_core::Token;

        let reduce_type_to_stream = |reduce_type: ReduceType| -> TokenStream {
            match reduce_type {
                ReduceType::Left => quote! { #module_prefix::builder::ReduceType::Left },
                ReduceType::Right => quote! { #module_prefix::builder::ReduceType::Right },
            }
        };
        let operator_to_stream = |op: Operator<usize>| -> TokenStream {
            match op {
                Operator::Prec(prec) => quote! { #module_prefix::builder::Operator::Prec(#prec) },
                Operator::Term(term) => quote! { #module_prefix::builder::Operator::Term(#term) },
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
        let token_to_stream = |token: Token<usize, usize>| -> TokenStream {
            match token {
                Token::Term(term) => {
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
            let name = self.class_pretty_name_list(class, 4);
            terminal_class_names_stream.extend(quote! {
                #name,
            });
        }

        let grammar_build_stream = if self.compiled {
            // do not build at runtime
            // write all parser tables and production rules directly here

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

                // lookaheads
                production_rules_body_stream.extend(quote! {
                #module_prefix::rule::ProductionRule{ name: #name, rule: vec![ #tokens_vec_body_stream ] },
            });
            }
            let mut states_body_stream = TokenStream::new();
            for state in &self.states {
                let mut shift_term_body_stream = TokenStream::new();
                for (&term, &next_state) in &state.shift_goto_map_term {
                    shift_term_body_stream.extend(quote! {
                        (#term, #next_state),
                    });
                }

                let mut shift_nonterm_body_stream = TokenStream::new();
                for (&nonterm, &next_state) in &state.shift_goto_map_nonterm {
                    let nonterm_stream = &nonterminals_token[nonterm];
                    shift_nonterm_body_stream.extend(quote! {
                        (#nonterm_stream, #next_state),
                    });
                }

                let mut reduce_body_stream = TokenStream::new();
                for (&term, rules) in &state.reduce_map {
                    let mut rules_body_stream = TokenStream::new();
                    for &rule in rules {
                        rules_body_stream.extend(quote! {
                            #rule,
                        });
                    }
                    reduce_body_stream.extend(quote! {
                        (#term, std::collections::BTreeSet::from([#rules_body_stream])),
                    });
                }

                let mut ruleset_body_stream = TokenStream::new();
                for &rule in &state.ruleset {
                    let shifted = rule.shifted;
                    let rule = rule.rule;
                    ruleset_body_stream.extend(quote! {
                        #module_prefix::rule::ShiftedRuleRef {
                            rule: #rule,
                            shifted: #shifted,
                        },
                    });
                }

                states_body_stream.extend(quote! {
                #module_prefix::builder::State {
                    shift_goto_map_term: std::collections::BTreeMap::from([#shift_term_body_stream]),
                    shift_goto_map_nonterm: std::collections::BTreeMap::from([#shift_nonterm_body_stream]),
                    reduce_map: std::collections::BTreeMap::from([#reduce_body_stream]),
                    ruleset: std::collections::BTreeSet::from([#ruleset_body_stream]),
                },
            });
            }

            quote! {
                let rules = vec![
                    #production_rules_body_stream
                ];
                let terminal_class_names = vec![
                    #terminal_class_names_stream
                ];
                let rules = rules.into_iter().map(
                    move |rule| {
                        rule.map(
                            |term| terminal_class_names[term],
                            |nonterm| nonterm,
                        )
                    }
                ).collect();

                let states = vec![
                    #states_body_stream
                ];
                let states:Vec<#state_typename> = states.into_iter().map(
                    |state| state.into(),
                ).collect();
            }
        } else {
            // build runtime

            // adding reduce types
            let mut add_reduce_type_stream = TokenStream::new();
            for (&op, &reduce_type) in self.builder.reduce_types.iter() {
                let reduce_type_stream = reduce_type_to_stream(reduce_type);
                let op_stream = operator_to_stream(op);
                add_reduce_type_stream.extend(quote! {
                    builder.add_reduce_type( #op_stream, #reduce_type_stream );
                });
            }

            // adding precedence orders
            let mut add_precedence_stream = TokenStream::new();
            for (&op, &level) in self.builder.precedence_map.iter() {
                let op_stream = operator_to_stream(op);
                add_precedence_stream.extend(quote! {
                    builder.add_precedence(#op_stream, #level);
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
                let op_stream = match rule.operator {
                    None => quote! { None },
                    Some(op) => {
                        let op_stream = operator_to_stream(op);
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
                        #op_stream,
                        #dprec_stream
                    );
                });
            }
            // add `error` non-terminal
            add_rules_stream.extend(quote! {
                builder.add_empty_rule(
                    #nonterminals_enum_name::error
                );
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
                let mut builder = #module_prefix::builder::Grammar::new();

                // add reduce types
                #add_reduce_type_stream

                // add precedences
                #add_precedence_stream

                // production rules
                #add_rules_stream

                #build_stream

                let terminal_class_names = vec![
                    #terminal_class_names_stream
                ];

                let rules = builder.rules.into_iter().map(
                    move |rule| {
                        rule.rule.map(
                            |term| terminal_class_names[term],
                            |nonterm| nonterm,
                        )
                    }
                ).collect();

                let states:Vec<#state_typename> = states.into_iter().map(
                    |state| state.into(),
                ).collect();
            }
        };

        let use_range_based_optimization = if self.is_char || self.is_u8 {
            self.calculate_range_terminal_class_map()
        } else {
            false
        };

        let other_class_id = self.other_terminal_class_id;

        let get_error_nonterm_stream = if self.error_used {
            quote! { Some(#nonterminals_enum_name::error) }
        } else {
            quote! { None }
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
                terminal_class_match_body_stream.extend(quote! {
                    #match_case_stream => #class_id,
                });
            }
            terminal_class_match_body_stream.extend(quote! {
                _ => #other_class_id,
            });

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
                fn get_error_nonterm(&self) -> Option<Self::NonTerm> {
                    #get_error_nonterm_stream
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

                terminal_class_match_body_stream.extend(quote! {
                    #match_case_stream => #class_id,
                });
            }
            terminal_class_match_body_stream.extend(quote! {
                _ => #other_class_id,
            });

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
                fn get_error_nonterm(&self) -> Option<Self::NonTerm> {
                    #get_error_nonterm_stream
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

    fn emit_token_data(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let token_data_typename = format_ident!("{}TokenData", self.start_rule_name);
        let token_typename = &self.token_typename;
        let user_data_parameter_name =
            Ident::new(utils::USER_DATA_PARAMETER_NAME, Span::call_site());
        let user_data_typename = &self.userdata_typename;
        let location_typename = self
            .location_typename
            .as_ref()
            .cloned()
            .unwrap_or_else(|| quote! { #module_prefix::DefaultLocation });

        // variant name for terminal symbol
        let terminal_variant_name = Ident::new("Terminals", Span::call_site());
        // enum variant name for each non-terminal
        let mut variant_names_for_nonterm = Vec::with_capacity(self.nonterminals.len());

        // (<RuleType as ToString>, variant_name) map
        let mut ruletype_variant_map: rusty_lr_core::hash::HashMap<String, Ident> =
            Default::default();

        // (variant_name, TokenStream for typename) sorted in insertion order
        // for consistent output
        let mut variant_names_in_order = Vec::new();

        // insert variant for terminal token type
        ruletype_variant_map.insert(
            self.token_typename.to_string(),
            terminal_variant_name.clone(),
        );
        variant_names_in_order.push((terminal_variant_name.clone(), self.token_typename.clone()));

        // insert variant for empty-ruletype
        let empty_ruletype_variant_name = Ident::new("Empty", Span::call_site());
        ruletype_variant_map.insert("".to_string(), empty_ruletype_variant_name.clone());
        // ruletype_variant_map.insert(quote! {()}.to_string(), empty_ruletype_variant_name.clone());
        variant_names_in_order.push((empty_ruletype_variant_name.clone(), quote! {}));

        for nonterm in self.nonterminals.iter() {
            let ruletype_stream = nonterm.ruletype.as_ref().cloned().unwrap_or_default();

            let cur_len = ruletype_variant_map.len();
            let variant_name = ruletype_variant_map
                .entry(ruletype_stream.to_string())
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

        let mut case_streams = quote! {};

        // TokenStream to define reduce function for each production rule
        let mut fn_reduce_for_each_rule_stream = TokenStream::new();

        let identity_reduce_fn_name = format_ident!("reduce_identity");
        let clear_reduce_fn_name = format_ident!("reduce_clear");

        use std::collections::BTreeMap;
        let mut identity_reduce_action_range: BTreeMap<usize, (usize, usize)> = BTreeMap::new();
        let mut clear_reduce_action_range: Option<(usize, usize)> = None;

        for (rule_index, &(nonterm_idx, rule_local_id)) in self.rules_sorted.iter().enumerate() {
            let nonterm = &self.nonterminals[nonterm_idx];
            let rule = &nonterm.rules[rule_local_id];

            use super::nonterminal_info::ReduceAction;
            match &rule.reduce_action {
                Some(ReduceAction::Custom(reduce_action)) => {
                    let mut extract_token_data_from_args = TokenStream::new();
                    for token in rule.tokens.iter() {
                        match &token.token {
                            Token::Term(_) => match &token.mapto {
                                Some(mapto) => {
                                    let location_varname =
                                        format_ident!("__rustylr_location_{}", mapto);
                                    extract_token_data_from_args.extend(quote! {
                                        let (#token_data_typename::#terminal_variant_name(mut #mapto), #location_varname) = __rustylr_args.pop().unwrap() else {
                                            unreachable!()
                                        };
                                    });
                                }
                                None => {
                                    extract_token_data_from_args.extend(quote! {
                                        __rustylr_args.pop();
                                    });
                                }
                            },
                            Token::NonTerm(nonterm_idx) => {
                                match &token.mapto {
                                    Some(mapto) => {
                                        let location_varname =
                                            format_ident!("__rustylr_location_{}", mapto);
                                        let variant_name = &variant_names_for_nonterm[*nonterm_idx];
                                        if variant_name == &empty_ruletype_variant_name {
                                            extract_token_data_from_args.extend(quote! {
                                                let (_, #location_varname) = __rustylr_args.pop().unwrap();
                                            });
                                        } else {
                                            // extract token data from args
                                            extract_token_data_from_args.extend(quote! {
                                                let (#token_data_typename::#variant_name(mut #mapto), #location_varname) = __rustylr_args.pop().unwrap() else {
                                                    unreachable!()
                                                };
                                            });
                                        }
                                    }
                                    None => {
                                        extract_token_data_from_args.extend(quote! {
                                            __rustylr_args.pop();
                                        });
                                    }
                                }
                            }
                        }
                    }
                    let reduce_fn_ident =
                        format_ident!("reduce_{}_{}", nonterm.name, rule_local_id);

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

                    case_streams.extend(quote! {
                        #rule_index => {
                            Self::#reduce_fn_ident( reduce_args, shift, lookahead, user_data, location0 )
                        }
                    });

                    // if typename is defined for this rule, push result of action to stack
                    // else, just execute action
                    if nonterm.ruletype.is_some() {
                        // typename is defined, reduce action must be defined
                        let variant_name = &variant_names_for_nonterm[nonterm_idx];
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            #[doc = #rule_debug_str]
                            #[inline]
                            fn #reduce_fn_ident(
                                __rustylr_args: &mut Vec<(Self, #location_typename)>,
                                shift: &mut bool,
                                lookahead: &#token_typename,
                                #user_data_parameter_name: &mut #user_data_typename,
                                __rustylr_location0: &mut #location_typename,
                            ) -> Result<#token_data_typename, #reduce_error_typename> {
                                #extract_token_data_from_args

                                Ok( #token_data_typename::#variant_name(#reduce_action) )
                            }
                        });
                    } else {
                        // <RuleType> is not defined,
                        // just execute action

                        let variant_name = &variant_names_for_nonterm[nonterm_idx];
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            #[doc = #rule_debug_str]
                            #[inline]
                            fn #reduce_fn_ident(
                                __rustylr_args: &mut Vec<(Self, #location_typename)>,
                                shift: &mut bool,
                                lookahead: &#token_typename,
                                #user_data_parameter_name: &mut #user_data_typename,
                                __rustylr_location0: &mut #location_typename,
                            ) -> Result<#token_data_typename, #reduce_error_typename> {
                                #extract_token_data_from_args
                                #reduce_action

                                Ok( #token_data_typename::#variant_name )
                            }
                        });
                    }
                }
                &Some(ReduceAction::Identity(reduce_action_identity)) => {
                    // reduce_args is in reverse order
                    let reversed_idx = rule.tokens.len() - reduce_action_identity - 1;

                    let (_, max) = identity_reduce_action_range
                        .entry(reversed_idx)
                        .or_insert((rule_index, rule_index));
                    let new_last = (*max).max(rule_index);
                    *max = new_last;
                }
                None => {
                    if let Some((_, last)) = &mut clear_reduce_action_range {
                        let new_last = (*last).max(rule_index);
                        *last = new_last;
                    } else {
                        clear_reduce_action_range = Some((rule_index, rule_index));
                    }
                }
            }
        }

        // add match choices for identity reduce actions.
        // since multiple reduce actions can be identity, merge them into ranges
        for (reversed_idx, (start_rule_index, last_rule_index)) in identity_reduce_action_range {
            if start_rule_index == last_rule_index {
                // identity action
                case_streams.extend(quote! {
                    #start_rule_index => {
                        Ok(Self::#identity_reduce_fn_name( reduce_args, #reversed_idx ))
                    }
                });
            } else {
                // identity action
                case_streams.extend(quote! {
                    #start_rule_index..=#last_rule_index => {
                        Ok(Self::#identity_reduce_fn_name( reduce_args, #reversed_idx ))
                    }
                });
            }
        }
        // add match choices for clear identity reduce actions.
        if let Some((start_rule_index, last_rule_index)) = clear_reduce_action_range {
            if start_rule_index == last_rule_index {
                // no reduce action (without ruletype)
                // clear the arguments stack and return empty token data
                case_streams.extend(quote! {
                    #start_rule_index => {
                        Ok(Self::#clear_reduce_fn_name( reduce_args ))
                    }
                });
            } else {
                // no reduce action (without ruletype)
                // clear the arguments stack and return empty token data
                case_streams.extend(quote! {
                    #start_rule_index..=#last_rule_index => {
                        Ok(Self::#clear_reduce_fn_name( reduce_args ))
                    }
                });
            }
        }

        let start_idx = *self.nonterminals_index.get(&self.start_rule_name).unwrap();
        let start_variant_name = &variant_names_for_nonterm[start_idx];
        let (start_typename, extract_start) = match &self.nonterminals[start_idx].ruletype {
            Some(typename) => (
                typename.clone(),
                quote! { #token_data_typename::#start_variant_name(data) => Ok(data) },
            ),
            None => (
                quote! {()},
                quote! { #token_data_typename::#start_variant_name => Ok(())},
            ),
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

        let derive_clone_for_glr = if self.glr {
            quote! {#[derive(Clone)]}
        } else {
            quote! {}
        };

        stream.extend(quote! {
        /// enum for each non-terminal and terminal symbol, that actually hold data
        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
        #derive_clone_for_glr
        pub enum #token_data_typename {
            #enum_variants_stream
        }

        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #token_data_typename {
            fn #identity_reduce_fn_name(
                args: &mut Vec<(Self, #location_typename)>,
                idx: usize,
            ) -> Self {
                let value = args.swap_remove(idx).0;
                args.clear();
                value
            }
            fn #clear_reduce_fn_name(
                args: &mut Vec<(Self, #location_typename)>,
            ) -> Self {
                args.clear();
                #token_data_typename::#empty_ruletype_variant_name
            }

            #fn_reduce_for_each_rule_stream
        }


        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types, unused_variables)]
        impl #module_prefix::nonterminal::TokenData for #token_data_typename {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;
            type ReduceActionError = #reduce_error_typename;
            type UserData = #user_data_typename;
            type StartType = #start_typename;
            type Location = #location_typename;

            fn reduce_action(
                rule_index: usize,
                reduce_args: &mut Vec<(Self, Self::Location)>,
                shift: &mut bool,
                lookahead: &Self::Term,
                user_data: &mut Self::UserData,
                location0: &mut Self::Location,
            ) -> Result<Self, Self::ReduceActionError> {
                match rule_index {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rule_index );
                    }
                }
            }
            fn new_error_nonterm() -> Self {
                #token_data_typename::#empty_ruletype_variant_name
            }
            fn new_terminal(term: #token_typename) -> Self {
                #token_data_typename::#terminal_variant_name(term)
            }
        }

        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types, unused_variables)]
        impl TryFrom<#token_data_typename> for #start_typename {
            type Error = ();

            fn try_from(token: #token_data_typename) -> Result<Self, Self::Error> {
                match token {
                    #extract_start,
                    _ => Err(()),
                }
            }
        }
        });
    }

    pub fn emit_compiletime(&self) -> TokenStream {
        let mut stream = TokenStream::new();
        self.emit_type_alises(&mut stream);
        self.emit_nonterm_enum(&mut stream);
        self.emit_token_data(&mut stream);
        self.emit_parser(&mut stream);

        stream
    }
}
