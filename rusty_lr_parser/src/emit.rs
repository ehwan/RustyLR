use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use rusty_lr_core::HashMap;
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
        let token_typename = &self.token_typename;
        let enum_name = format_ident!("{}NonTerminals", start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let parse_error_typename = format_ident!("{}ParseError", start_rule_name);
        let invalid_terminal_error = format_ident!("{}InvalidTerminalError", start_rule_name);
        let context_struct_name = format_ident!("{}Context", start_rule_name);

        if self.glr {
            let multiple_path_error = format_ident!("{}MultiplePathError", start_rule_name);
            let node_enum_name = format_ident!("{}NodeEnum", start_rule_name);

            let state_structname = if self.emit_dense {
                format_ident!("DenseState")
            } else {
                format_ident!("SparseState")
            };

            stream.extend(
        quote! {
                /// type alias for `Context`
                #[allow(non_camel_case_types,dead_code)]
                pub type #context_struct_name = #module_prefix::glr::Context<#node_enum_name>;
                /// type alias for CFG production rule
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::ProductionRule<&'static str, #enum_name>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::glr::#state_structname<#enum_name>;
                /// type alias for `InvalidTerminalError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #invalid_terminal_error = #module_prefix::glr::InvalidTerminalError<#token_typename, #enum_name, #reduce_error_typename>;
                /// type alias for `MultiplePathError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #multiple_path_error = #module_prefix::glr::MultiplePathError<#token_typename, #enum_name>;
            }
            );
        } else {
            let stack_struct_name = format_ident!("{}Stack", start_rule_name);
            let state_structname = if self.emit_dense {
                format_ident!("DenseState")
            } else {
                format_ident!("SparseState")
            };

            stream.extend(
        quote! {
                /// type alias for `Context`
                #[allow(non_camel_case_types,dead_code)]
                pub type #context_struct_name = #module_prefix::lr::Context<#stack_struct_name>;
                /// type alias for CFG production rule
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::ProductionRule<&'static str, #enum_name>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::lr::#state_structname<#enum_name>;
                /// type alias for `ParseError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::lr::ParseError<#token_typename, #enum_name, #reduce_error_typename>;
                /// type alias for `InvalidTerminalError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #invalid_terminal_error = #module_prefix::lr::InvalidTerminalError<#token_typename, #enum_name>;
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
        let token_typename = &self.token_typename;

        let mut comma_separated_variants = TokenStream::new();
        let mut case_as_str = TokenStream::new();
        let mut nonterm_trait_is_augmented_case = TokenStream::new();
        let mut nonterm_trait_is_generated_case = TokenStream::new();
        let mut nonterm_trait_is_trace_case = TokenStream::new();
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

            let (is_augmented, is_generated, is_trace) = if name == utils::AUGMENTED_NAME {
                (true, true, false)
            } else {
                // non-term is auto-generated if nonterm.regex_span.is_some()
                (false, nonterm.is_auto_generated(), nonterm.trace)
            };

            nonterm_trait_is_augmented_case.extend(quote! {
                #enum_typename::#name => #is_augmented,
            });
            nonterm_trait_is_generated_case.extend(quote! {
                #enum_typename::#name => #is_generated,
            });
            nonterm_trait_is_trace_case.extend(quote! {
                #enum_typename::#name => #is_trace,
            });
        }

        stream.extend(
    quote! {
            /// An enum that represents non-terminal symbols
            #[allow(non_camel_case_types, dead_code)]
            #[derive(Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
            pub enum #enum_typename {
                #comma_separated_variants
            }

            impl #enum_typename {
                /// convert to string
                pub fn as_str(&self) -> &'static str {
                    match self {
                        #case_as_str
                    }
                }
            }

            impl std::fmt::Display for #enum_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.as_str())
                }
            }
            impl std::fmt::Debug for #enum_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.as_str())
                }
            }

            impl #module_prefix::NonTerminal<#token_typename> for #enum_typename{
                fn is_auto_generated(&self) -> bool {
                    match self {
                        #nonterm_trait_is_generated_case
                    }
                }
                fn is_augmented(&self) -> bool {
                    match self {
                        #nonterm_trait_is_augmented_case
                    }
                }
                fn is_trace(&self) -> bool {
                    match self {
                        #nonterm_trait_is_trace_case
                    }
                }
            }
        }
        );
    }

    fn emit_context_lr(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let stack_struct_name = format_ident!("{}Stack", self.start_rule_name);
        let token_typename = &self.token_typename;
        let terms_stack_name = Ident::new(utils::TERMINAL_STACK_NAME, Span::call_site());
        let user_data_parameter_name =
            Ident::new(utils::USER_DATA_PARAMETER_NAME, Span::call_site());
        let user_data_typename = &self.userdata_typename;

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
                        self.#reduce_fn_ident( lookahead, user_data )
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
                    // this will be ensured by Grammar::from_grammar_args()
                    let action = &rule.reduce_action.as_ref().unwrap().stream;
                    fn_reduce_for_each_rule_stream.extend(quote! {
                        fn #reduce_fn_ident(&mut self, lookahead: &#token_typename, #user_data_parameter_name: &mut #user_data_typename ) -> Result<(), #reduce_error_typename> {
                            #token_pop_stream
                            self.#stack_name.push(#action);
                            Ok(())
                        }
                    });
                } else {
                    // <RuleType> is not defined,
                    // just execute action
                    if let Some(action) = &rule.reduce_action {
                        let action = &action.stream;
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, lookahead: &#token_typename, #user_data_parameter_name:&mut #user_data_typename) -> Result<(), #reduce_error_typename> {
                                #token_pop_stream
                                #action
                                Ok(())
                            }
                        });
                    } else {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(&mut self, lookahead: &#token_typename, #user_data_parameter_name: &mut #user_data_typename) -> Result<(), #reduce_error_typename> {
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

        // pop_term() and pop_nonterm()
        let mut pop_nonterm_cases_stream = TokenStream::new();
        for (nonterm_idx, nonterm) in self.nonterminals.iter().enumerate() {
            let name = &nonterm.name;
            if let Some(stack_name) = &stack_names_for_nonterm[nonterm_idx] {
                pop_nonterm_cases_stream.extend(quote! {
                    #nonterminals_enum_name::#name => {
                        self.#stack_name.pop();
                    }
                });
            }
        }

        stream.extend(quote! {
        /// struct that holds internal parser data,
        /// including data stack for each non-terminal,
        /// and state stack for DFA
        #[allow(unused_braces, unused_parens, unused_variables, unused_mut, non_snake_case, non_camel_case_types)]
        pub struct #stack_struct_name {
            #stack_def_streams
        }
        #[allow(unused_braces, unused_parens, unused_variables, unused_mut, non_snake_case, non_camel_case_types, dead_code)]
        impl #stack_struct_name {
            #fn_reduce_for_each_rule_stream
        }
        #[allow(unused_braces, unused_parens, unused_variables, unused_mut, non_snake_case, non_camel_case_types, dead_code)]
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
                user_data: &mut Self::UserData,
                lookahead: &Self::Term,
            ) -> Result<(), Self::ReduceActionError> {
                match rustylr_macro_generated_ruleid__ {
                    #case_streams
                    _ => {
                        unreachable!( "Invalid Rule: {}", rustylr_macro_generated_ruleid__ );
                    }
                }
            }

            #[allow(clippy::unused_unit)]
            fn pop_start(&mut self) -> Self::StartType {
                #pop_from_start_rule_stack
            }

            fn pop(&mut self, nonterm: Self::NonTerm) {
                match nonterm {
                    #pop_nonterm_cases_stream
                    _ => {} // do nothing
                }
            }
            fn pop_term(&mut self) {
                self.#terms_stack_name.pop();
            }
        }
        });
    }

    fn emit_context_glr(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let reduce_error_typename = &self.error_typename;
        let node_enum_name = format_ident!("{}NodeEnum", self.start_rule_name);
        let token_typename = &self.token_typename;
        let terms_variant_name = Ident::new("Terminals", Span::call_site());

        let user_data_parameter_name =
            Ident::new(utils::USER_DATA_PARAMETER_NAME, Span::call_site());
        let user_data_typename = &self.userdata_typename;

        // enum variant name for each non-terminal
        let mut variant_names_for_nonterm = Vec::with_capacity(self.nonterminals.len());

        // (<RuleType as ToString>, variant_name) map
        let mut ruletype_variant_map: HashMap<String, Ident> = HashMap::default();

        // (variant_name, TokenStream for typename) sorted in insertion order
        // for consistent output
        let mut variant_names_in_order = Vec::new();

        // insert variant for empty-ruletype
        let empty_ruletype_variant_name = Ident::new("EmptyRuleType", Span::call_site());
        ruletype_variant_map.insert("".to_string(), empty_ruletype_variant_name.clone());
        variant_names_in_order.push((empty_ruletype_variant_name.clone(), quote! {}));

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
                                    let #node_enum_name::#terms_variant_name(mut #mapto) = __rustylr_args.pop().unwrap() else {
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
                                            let #node_enum_name::#variant_name(mut #mapto) = __rustylr_args.pop().unwrap() else {
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
                        Self::#reduce_fn_ident( reduce_args, shift, lookahead, user_data )
                    }
                });

                // if typename is defined for this rule, push result of action to stack
                // else, just execute action
                if nonterm.ruletype.is_some() {
                    // typename is defined, reduce action must be defined
                    let variant_name = &variant_names_for_nonterm[nonterm_idx];
                    let action = &rule.reduce_action.as_ref().unwrap().stream;
                    fn_reduce_for_each_rule_stream.extend(quote! {
                        fn #reduce_fn_ident(
                            __rustylr_args: &mut Vec<Self>,
                            shift: &mut bool,
                            lookahead: &#token_typename,
                            #user_data_parameter_name: &mut #user_data_typename
                        ) -> Result<#node_enum_name, #reduce_error_typename> {
                            #extract_data_from_node_enum

                            Ok( #node_enum_name::#variant_name(#action) )
                        }
                    });
                } else {
                    // <RuleType> is not defined,
                    // just execute action

                    let variant_name = &variant_names_for_nonterm[nonterm_idx];
                    if let Some(action) = &rule.reduce_action {
                        let action = &action.stream;
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(
                                __rustylr_args: &mut Vec<Self>,
                                shift: &mut bool,
                                lookahead: &#token_typename,
                                #user_data_parameter_name: &mut #user_data_typename
                            ) -> Result<#node_enum_name, #reduce_error_typename> {
                                #extract_data_from_node_enum
                                #action

                                Ok( #node_enum_name::#variant_name )
                            }
                        });
                    } else {
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            fn #reduce_fn_ident(
                                __rustylr_args: &mut Vec<Self>,
                                shift: &mut bool,
                                lookahead: &#token_typename,
                                #user_data_parameter_name: &mut #user_data_typename
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

        stream.extend(quote! {
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
                reduce_args: &mut Vec<Self>,
                shift: &mut bool,
                lookahead: &Self::Term,
                user_data: &mut Self::UserData,
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

            fn new_error_nonterm() -> Self {
                #node_enum_name::#empty_ruletype_variant_name
            }
        }
        });
    }
    fn emit_parser(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &self.start_rule_name);
        let rule_typename = format_ident!("{}Rule", self.start_rule_name);
        let state_typename = format_ident!("{}State", self.start_rule_name);
        let parser_struct_name = format_ident!("{}Parser", self.start_rule_name);
        let token_typename = &self.token_typename;
        let parser_trait_name = if self.glr {
            quote! {
            #module_prefix::glr::Parser
            }
        } else {
            quote! {
            #module_prefix::lr::Parser
            }
        };

        // ======================
        // building grammar
        // ======================
        use rusty_lr_core::builder::Operator;
        use rusty_lr_core::ReduceType;
        use rusty_lr_core::Token;

        let reduce_type_to_stream = |reduce_type: ReduceType| -> TokenStream {
            match reduce_type {
                ReduceType::Left => quote! { #module_prefix::ReduceType::Left },
                ReduceType::Right => quote! { #module_prefix::ReduceType::Right },
            }
        };
        let operator_to_stream = |op: Operator<usize>| -> TokenStream {
            match op {
                Operator::Prec(prec) => quote! { #module_prefix::builder::Operator::Prec(#prec) },
                Operator::Term(term) => quote! { #module_prefix::builder::Operator::Term(#term) },
            }
        };

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

        let state_convert_stream = if self.glr {
            if self.emit_dense {
                let classes_len = self.terminal_classes.len();
                quote! {
                    let states:Vec<_> = states.into_iter().map(
                        |state| {
                            state.into_glr_dense_state(|x| x, |x| x, #classes_len)
                        },
                    ).collect();
                }
            } else {
                quote! {
                    let states:Vec<_> = states.into_iter().map(
                        |state| {
                            state.into_glr_sparse_state(|x| x, |x| x)
                        },
                    ).collect();
                }
            }
        } else {
            if self.emit_dense {
                let classes_len = self.terminal_classes.len();
                quote! {
                    let states:Vec<_> = states.into_iter().map(
                        |state| {
                            state.into_lr_dense_state(|x| x, |x| x, #classes_len)
                        },
                    ).collect();
                }
            } else {
                quote! {
                    let states:Vec<_> = states.into_iter().map(
                        |state| {
                            state.into_lr_sparse_state(|x| x, |x| x)
                        },
                    ).collect();
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

        let grammar_build_stream = quote! {
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
            #state_convert_stream
        };
        drop(add_reduce_type_stream);
        drop(add_precedence_stream);
        drop(add_rules_stream);
        drop(build_stream);

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
            impl #parser_trait_name for #parser_struct_name {
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
            impl #parser_trait_name for #parser_struct_name {
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

    pub fn emit_compiletime(&self) -> TokenStream {
        let mut stream = TokenStream::new();
        self.emit_type_alises(&mut stream);
        self.emit_nonterm_enum(&mut stream);
        if self.glr {
            self.emit_context_glr(&mut stream);
            self.emit_parser(&mut stream);
        } else {
            self.emit_context_lr(&mut stream);
            self.emit_parser(&mut stream);
        };

        stream
    }
}
