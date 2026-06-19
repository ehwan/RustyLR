use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use crate::grammar::Grammar;
use crate::terminal_info::TerminalName;
use crate::utils;

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

/// emit Rust code for the parser
impl Grammar {
    /// write type alias Context, Rule, State, Error...
    fn emit_type_alises(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let start_rule_span = self
            .span_manager
            .get_span_in_location(&self.start_rule_name.location());
        let rule_typename = Ident::new(&format!("{}Rule", self.start_rule_name), start_rule_span);
        let state_typename = Ident::new(&format!("{}State", self.start_rule_name), start_rule_span);
        let nonterm_typename = Ident::new(
            &format!("{}NonTerminals", self.start_rule_name),
            start_rule_span,
        );
        let parse_error_typename = Ident::new(
            &format!("{}ParseError", self.start_rule_name),
            start_rule_span,
        );
        let context_struct_name =
            Ident::new(&format!("{}Context", self.start_rule_name), start_rule_span);
        let data_stack_typename = Ident::new(
            &format!("{}DataStack", self.start_rule_name),
            start_rule_span,
        );
        let termclass_typename = Ident::new(
            &format!("{}TerminalClasses", self.start_rule_name),
            start_rule_span,
        );
        let location_typename = &self.location_typename;
        let reduce_error_typename = &self.error_typename;

        let state_structname = if self.emit_dense {
            format_ident!("DenseState")
        } else {
            format_ident!("SparseState")
        };
        let token_typename = &self.token_typename;

        let state_index_typename = if self.states.len() <= u8::MAX as usize {
            quote! { u8 }
        } else if self.states.len() <= u16::MAX as usize {
            quote! { u16 }
        } else if self.states.len() <= u32::MAX as usize {
            quote! { u32 }
        } else {
            quote! { usize }
        };
        let rule_index_type = if self.builder.rules.len() <= u8::MAX as usize {
            quote! { u8 }
        } else if self.builder.rules.len() <= u16::MAX as usize {
            quote! { u16 }
        } else if self.builder.rules.len() <= u32::MAX as usize {
            quote! { u32 }
        } else {
            quote! { usize }
        };

        if self.glr {
            // count the max number of multiple reduce rules in a single state
            let max_reduce_rules = self
                .states
                .iter()
                .flat_map(|s| s.reduce_map.iter().map(|(_, rules)| rules.len()))
                .max()
                .unwrap_or(1);

            let rule_container_type = if max_reduce_rules == 1 {
                rule_index_type.clone()
            } else {
                quote! { #module_prefix::parser::state::ArrayVec<#rule_index_type, #max_reduce_rules> }
            };

            stream.extend(
            quote! {
                    /// type alias for `Context`
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #context_struct_name = #module_prefix::parser::nondeterministic::Context<#data_stack_typename, #state_index_typename, #max_reduce_rules>;
                    /// type alias for CFG production rule
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #rule_typename = #module_prefix::rule::ProductionRule<#termclass_typename, #nonterm_typename>;
                    /// type alias for DFA state
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #state_typename = #module_prefix::parser::state::#state_structname<#termclass_typename, #nonterm_typename, #rule_container_type, #state_index_typename>;
                    /// type alias for `InvalidTerminalError`
                    #[allow(non_camel_case_types,dead_code)]
                    pub type #parse_error_typename = #module_prefix::parser::nondeterministic::ParseError<#token_typename, #location_typename, #reduce_error_typename>;
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
                pub type #rule_typename = #module_prefix::rule::ProductionRule<#termclass_typename, #nonterm_typename>;
                /// type alias for DFA state
                #[allow(non_camel_case_types,dead_code)]
                pub type #state_typename = #module_prefix::parser::state::#state_structname<#termclass_typename, #nonterm_typename, #rule_index_type, #state_index_typename>;
                /// type alias for `ParseError`
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::parser::deterministic::ParseError<#token_typename, #location_typename, #reduce_error_typename>;
            }
            );
        }
    }

    fn emit_termclass_enum(&self, stream: &mut TokenStream) {
        let start_rule_span = self
            .span_manager
            .get_span_in_location(&self.start_rule_name.location());
        let termclass_typename = Ident::new(
            &format!("{}TerminalClasses", self.start_rule_name),
            start_rule_span,
        );
        let module_prefix = &self.module_prefix;
        let error_name = format_ident!("{}", utils::ERROR_NAME);
        let eof_name = format_ident!("{}", utils::EOF_NAME);
        let token_typename = &self.token_typename;

        let mut class_variants = Vec::with_capacity(self.terminal_classes.len());
        let mut as_str_match_stream = TokenStream::new();
        for (class_id, class_def) in self.terminal_classes.iter().enumerate() {
            let (variant_name, pretty_name) = if self.is_u8 || self.is_char {
                let name = format_ident!("TermClass{}", class_id);
                let pretty_name =
                    self.class_pretty_name_list(rusty_lr_core::TerminalSymbol::Term(class_id), 4);
                (name, pretty_name)
            } else {
                if class_def.terminals.len() == 1 {
                    let term_idx = class_def.terminals[0];
                    let term_str = self.terminals[term_idx].name.ident_str().unwrap();
                    let term = Ident::new(term_str, Span::call_site());
                    (term.clone(), term.to_string())
                } else {
                    let name = format_ident!("TermClass{}", class_id);
                    let pretty_name = self
                        .class_pretty_name_list(rusty_lr_core::TerminalSymbol::Term(class_id), 4);
                    (name, pretty_name)
                }
            };
            as_str_match_stream.extend(quote! {
                #termclass_typename::#variant_name => #pretty_name,
            });
            class_variants.push(variant_name);
        }
        // generate `Parser::class_precedence()` terminal class -> precedence level match body
        let mut precedence_match_stream = TokenStream::new();
        {
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
                    debug_assert!(level < u8::MAX as usize);
                    let iter = classes.iter().map(|&c| {
                        let var = &class_variants[c];
                        quote! { #termclass_typename::#var }
                    });
                    let level = proc_macro2::Literal::usize_unsuffixed(level);
                    let case_stream = quote! { #(#iter)|* };
                    precedence_match_stream.extend(quote! {
                        #case_stream => #module_prefix::parser::Precedence::new(#level),
                    });
                }
            }
            if let Some(error_prec) = self.error_precedence {
                debug_assert!(error_prec < u8::MAX as usize);
                let error_prec = proc_macro2::Literal::usize_unsuffixed(error_prec);
                precedence_match_stream.extend(quote! {
                #termclass_typename::#error_name => #module_prefix::parser::Precedence::new(#error_prec),
            });
            }
            precedence_match_stream.extend(quote! {
                #termclass_typename::#eof_name => {
                    unreachable!("eof token cannot be used in precedence levels")
                },
                _ => #module_prefix::parser::Precedence::none(),
            });
        }

        let other_class_id = self.other_terminal_class_id;

        let match_terminal_filter_expression = if let Some(filter) = &self.filter {
            quote! { #filter(terminal) }
        } else {
            quote! {terminal}
        };
        let mut from_term_match_stream = TokenStream::new();

        // building terminal-class_id map
        if self.is_char || self.is_u8 {
            // range-compressed Vec based terminal-class_id map

            // for terminal -> terminal_class_id map to_terminal_class()
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
                let class_variant = &class_variants[class_id];
                from_term_match_stream.extend(quote! {
                    #match_case_stream => #termclass_typename::#class_variant,
                });
            }
            let other_variant = &class_variants[other_class_id];
            from_term_match_stream.extend(quote! {
                _ => #termclass_typename::#other_variant,
            });
        } else {
            // match based terminal-class_id map

            // for terminal -> terminal_class_id map to_terminal_class()
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
                let class_variant = &class_variants[class_id];
                from_term_match_stream.extend(quote! {
                    #match_case_stream => #termclass_typename::#class_variant,
                });
            }
            let other_class_variant = &class_variants[other_class_id];
            from_term_match_stream.extend(quote! {
                _ => #termclass_typename::#other_class_variant,
            });
        }

        stream.extend(quote! {
            /// A enum that represents terminal classes
            #[allow(non_camel_case_types, dead_code)]
            #[derive(Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
            pub enum #termclass_typename {
                #(#class_variants),*,
                #error_name,
                #eof_name,
            }

            impl #module_prefix::parser::terminalclass::TerminalClass for #termclass_typename {
                type Term = #token_typename;
                const ERROR:Self = Self::#error_name;
                const EOF:Self = Self::#eof_name;

                fn as_str(&self) -> &'static str {
                    match self {
                        #as_str_match_stream
                        #termclass_typename::#error_name => "error",
                        #termclass_typename::#eof_name => "eof",
                    }
                }
                fn to_usize(&self) -> usize {
                    *self as usize
                }
                fn precedence(&self) -> #module_prefix::parser::Precedence {
                    match self {
                        #precedence_match_stream
                    }
                }
                fn from_term(terminal: &Self::Term) -> Self {
                    #[allow(unreachable_patterns, unused_variables)]
                    match #match_terminal_filter_expression {
                        #from_term_match_stream
                    }
                }
            }

            impl std::fmt::Display for #termclass_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    use #module_prefix::parser::terminalclass::TerminalClass;
                    write!(f, "{}", self.as_str())
                }
            }
            impl std::fmt::Debug for #termclass_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    use #module_prefix::parser::terminalclass::TerminalClass;
                    write!(f, "{}", self.as_str())
                }
            }
        });
    }

    /// write `NonTerminal` enum
    fn emit_nonterm_enum(&self, stream: &mut TokenStream) {
        // =====================================================================
        // =====================Writing NonTerminal Enum========================
        // =====================================================================

        let start_rule_span = self
            .span_manager
            .get_span_in_location(&self.start_rule_name.location());
        let start_rule_ident = Ident::new(&self.start_rule_name, start_rule_span);
        let enum_typename = format_ident!("{}NonTerminals", start_rule_ident);
        let module_prefix = &self.module_prefix;

        let mut comma_separated_variants = TokenStream::new();
        let mut case_as_str = TokenStream::new();
        let mut nonterm_trait_is_trace_case = TokenStream::new();
        let mut nonterm_type_case = TokenStream::new();
        for nonterm in self.nonterminals.iter() {
            let name = utils::ident_from_located(
                nonterm.name.value().as_str(),
                &nonterm.name.location(),
                &self.span_manager,
            );
            // enum variants definition
            comma_separated_variants.extend(quote! {
                #name,
            });

            // impl `Display` and `Debug` for NonTerminal
            let display_str = nonterm.pretty_name.as_str();
            case_as_str.extend(quote! {
                #enum_typename::#name => #display_str,
            });

            let is_trace = if nonterm.name.value().as_str() == utils::AUGMENTED_NAME {
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
                    #enum_typename::#name => Some(#module_prefix::parser::nonterminal::NonTerminalType::#enum_name),
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
                    use #module_prefix::parser::nonterminal::NonTerminal;
                    write!(f, "{}", self.as_str())
                }
            }
            impl std::fmt::Debug for #enum_typename {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    use #module_prefix::parser::nonterminal::NonTerminal;
                    write!(f, "{}", self.as_str())
                }
            }

            impl #module_prefix::parser::nonterminal::NonTerminal for #enum_typename{
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
                fn nonterm_type(&self) -> Option<#module_prefix::parser::nonterminal::NonTerminalType> {
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
        let start_rule_ident = Ident::new(
            &self.start_rule_name,
            self.span_manager
                .get_span_in_location(&self.start_rule_name.location()),
        );
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &start_rule_ident);
        let rule_typename = format_ident!("{}Rule", start_rule_ident);
        let state_typename = format_ident!("{}State", start_rule_ident);
        let parser_struct_name = format_ident!("{}Parser", start_rule_ident);
        let token_typename = &self.token_typename;
        let termclass_typename = format_ident!("{}TerminalClasses", &start_rule_ident);

        let mut class_variants = Vec::with_capacity(self.terminal_classes.len());
        for (class_id, class_def) in self.terminal_classes.iter().enumerate() {
            let variant_name = if self.is_u8 || self.is_char {
                let name = format_ident!("TermClass{}", class_id);
                name
            } else {
                if class_def.terminals.len() == 1 {
                    let term_idx = class_def.terminals[0];
                    let term_str = self.terminals[term_idx].name.ident_str().unwrap();
                    Ident::new(term_str, Span::call_site())
                } else {
                    let name = format_ident!("TermClass{}", class_id);
                    name
                }
            };
            class_variants.push(variant_name);
        }

        // ======================
        // building grammar
        // ======================
        use rusty_lr_core::rule::ReduceType;
        use rusty_lr_core::TerminalSymbol;
        use rusty_lr_core::Token;

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
                let name = utils::ident_from_located(
                    nonterm.name.value().as_str(),
                    &nonterm.name.location(),
                    &self.span_manager,
                );
                quote! {
                    #nonterminals_enum_name::#name
                }
            })
            .collect();
        let token_to_stream = |token: Token<TerminalSymbol<usize>, usize>| -> TokenStream {
            match token {
                Token::Term(term) => {
                    let term = match term {
                        TerminalSymbol::Term(term) => {
                            let var = &class_variants[term];
                            quote! { #termclass_typename::#var }
                        }
                        TerminalSymbol::Error => {
                            let error_name = format_ident!("{}", utils::ERROR_NAME);
                            quote! { #termclass_typename::#error_name }
                        }
                        TerminalSymbol::Eof => {
                            let eof_name = format_ident!("{}", utils::EOF_NAME);
                            quote! { #termclass_typename::#eof_name }
                        }
                    };
                    quote! { #module_prefix::Token::Term(#term) }
                }
                Token::NonTerm(nonterm) => {
                    let nonterm = &nonterminals_token[nonterm];
                    quote! { #module_prefix::Token::NonTerm(#nonterm) }
                }
            }
        };

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
                    #lefts => Some(#module_prefix::rule::ReduceType::Left),
                });
            }
            if !rights.is_empty() {
                stream.extend(quote! {
                    #rights => Some(#module_prefix::rule::ReduceType::Right),
                });
            }
            stream.extend(quote! {
                _ => None,
            });
            stream
        };

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
        for state in &self.states {
            let mut shift_term_body_stream = TokenStream::new();
            for &(term, next_state) in &state.shift_goto_map_term {
                let push = next_state.push;
                let next_state = proc_macro2::Literal::usize_unsuffixed(next_state.state);
                let term = match term {
                    TerminalSymbol::Term(term) => {
                        let var = &class_variants[term];
                        quote! { #termclass_typename::#var }
                    }
                    TerminalSymbol::Error => {
                        let error_name = format_ident!("{}", utils::ERROR_NAME);
                        quote! { #termclass_typename::#error_name }
                    }
                    TerminalSymbol::Eof => {
                        let eof_name = format_ident!("{}", utils::EOF_NAME);
                        quote! { #termclass_typename::#eof_name }
                    }
                };
                shift_term_body_stream.extend(quote! {
                    (#term, #module_prefix::parser::state::ShiftTarget::new(#next_state,#push)),
                });
            }

            let mut shift_nonterm_body_stream = TokenStream::new();
            for &(nonterm, next_state) in &state.shift_goto_map_nonterm {
                let nonterm_stream = &nonterminals_token[nonterm];
                let push = next_state.push;
                let next_state = proc_macro2::Literal::usize_unsuffixed(next_state.state);
                shift_nonterm_body_stream.extend(quote! {
                        (#nonterm_stream, #module_prefix::parser::state::ShiftTarget::new(#next_state,#push)),
                    });
            }

            let mut reduce_map_items = TokenStream::new();
            for (term, rules) in &state.reduce_map {
                let term_stream = match term {
                    TerminalSymbol::Term(term) => {
                        let var = &class_variants[*term];
                        quote! { #termclass_typename::#var }
                    }
                    TerminalSymbol::Error => {
                        let error_name = format_ident!("{}", utils::ERROR_NAME);
                        quote! { #termclass_typename::#error_name }
                    }
                    TerminalSymbol::Eof => {
                        let eof_name = format_ident!("{}", utils::EOF_NAME);
                        quote! { #termclass_typename::#eof_name }
                    }
                };
                let rules_it = rules
                    .iter()
                    .map(|&rule| proc_macro2::Literal::usize_unsuffixed(rule));
                reduce_map_items.extend(quote! {
                    (#term_stream, vec![#(#rules_it),*]),
                });
            }
            let reduce_map_construct_stream = if reduce_map_items.is_empty() {
                quote! { Default::default() }
            } else {
                quote! {
                    vec![#reduce_map_items]
                }
            };

            let mut ruleset_items = TokenStream::new();
            for &rule in &state.ruleset {
                let rule_lit = proc_macro2::Literal::usize_unsuffixed(rule.rule);
                let shifted_lit = proc_macro2::Literal::usize_unsuffixed(rule.shifted);
                ruleset_items.extend(quote! {
                    #module_prefix::rule::ShiftedRuleRef {
                        rule: #rule_lit as usize,
                        shifted: #shifted_lit as usize,
                    },
                });
            }

            let can_accept_error = match state.can_accept_error {
                rusty_lr_core::TriState::False => quote! { #module_prefix::TriState::False },
                rusty_lr_core::TriState::True => quote! { #module_prefix::TriState::True },
                rusty_lr_core::TriState::Maybe => quote! { #module_prefix::TriState::Maybe },
            };

            states_body_stream.extend(quote! {
                #module_prefix::parser::state::IntermediateState {
                    shift_goto_map_term: vec![#shift_term_body_stream],
                    shift_goto_map_nonterm: vec![#shift_nonterm_body_stream],
                    reduce_map: #reduce_map_construct_stream,
                    ruleset: vec![
                        #ruleset_items
                    ],
                    can_accept_error: #can_accept_error,
                },
            });
        }

        let error_used = self.error_used;

        // range-compressed Vec based terminal-class_id map
        stream.extend(quote! {
            /// A lightweight parser struct that references the static parser tables and production rules.
            ///
            /// Since this struct only holds `'static` references to shared, read-only static parser tables,
            /// it is extremely cheap to instantiate, copy, or clone, and takes very little space.
            #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
            #[derive(Clone, Copy)]
            pub struct #parser_struct_name {
                /// production rules
                pub rules: &'static [#rule_typename],
                /// states
                pub states: &'static [#state_typename],
            }

            unsafe impl ::std::marker::Send for #parser_struct_name {}
            unsafe impl ::std::marker::Sync for #parser_struct_name {}
            #[rustfmt::skip]
            impl #module_prefix::parser::Parser for #parser_struct_name {
                type Term = #token_typename;
                type TermClass = #termclass_typename;
                type NonTerm = #nonterminals_enum_name;
                type State = #state_typename;

                const ERROR_USED:bool = #error_used;

                fn precedence_types(&self, level: u8) -> Option<#module_prefix::rule::ReduceType> {
                    #[allow(unreachable_patterns)]
                    match level {
                        #precedence_types_match_body_stream
                    }
                }
                fn get_rules(&self) -> &[#rule_typename] {
                    self.rules
                }
                fn get_states(&self) -> &[#state_typename] {
                    self.states
                }
            }

            /// Calculates the static parser tables from the grammar.
            #[rustfmt::skip]
            #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
            impl #parser_struct_name {
                /// Calculates the states and parser tables from the grammar.
                #[allow(clippy::clone_on_copy)]
                pub fn new() -> Self {
                    static RULES: std::sync::OnceLock<Vec<#rule_typename>> = std::sync::OnceLock::new();
                    let rules = RULES.get_or_init(|| {
                        vec![
                            #production_rules_body_stream
                        ]
                    });
                    static STATES: std::sync::OnceLock<Vec<#state_typename>> = std::sync::OnceLock::new();
                    let states = STATES.get_or_init(|| {
                        let states: Vec<#module_prefix::parser::state::IntermediateState<
                            #termclass_typename, #nonterminals_enum_name, #state_index_typename, #rule_index_typename
                        >> = vec![
                            #states_body_stream
                        ];
                        states.into_iter().map(
                            |state| state.into(),
                        ).collect()
                    });

                    Self {
                        rules: rules.as_slice(),
                        states: states.as_slice(),
                    }
                }
            }
        });
    }

    fn emit_data_stack(&self, stream: &mut TokenStream) {
        use rusty_lr_core::TerminalSymbol;
        use rusty_lr_core::Token;

        let module_prefix = &self.module_prefix;
        let start_rule_ident = Ident::new(
            &self.start_rule_name,
            self.span_manager
                .get_span_in_location(&self.start_rule_name.location()),
        );
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &start_rule_ident);
        let reduce_error_typename = &self.error_typename;
        let data_stack_typename = format_ident!("{}DataStack", start_rule_ident);
        let data_enum_typename = format_ident!("{}Data", &start_rule_ident);
        let token_typename = &self.token_typename;
        let user_data_parameter_name =
            Ident::new(utils::USER_DATA_PARAMETER_NAME, Span::call_site());
        let user_data_typename = &self.userdata_typename;
        let location_typename = &self.location_typename;

        // empty tag name
        let empty_variant_name = format_ident!("Empty");
        // variant name for terminal symbol
        let terminal_variant_name = format_ident!("__terminals");

        // variant name for each non-terminal
        let mut variant_names_for_nonterm = Vec::with_capacity(self.nonterminals.len());

        fn remove_whitespaces(s: String) -> String {
            s.chars().filter(|c| !c.is_whitespace()).collect()
        }

        // Map containing (<RuleType as ToString>, variant_name).
        // This maps each rule type to its enum variant name (e.g. __variant0, __variant1), merging identical types.
        let mut ruletype_variant_map: rusty_lr_core::hash::HashMap<String, Ident> =
            Default::default();

        // (variant_name, TokenStream for typename, is_boxed) sorted in insertion order
        // for consistent output
        let mut variant_names_in_order = Vec::new();

        let mut terminal_data_used = false;

        // insert variant for terminal token type
        if self.terminal_classes.iter().any(|c| c.data_used) {
            terminal_data_used = true;
            let terminal_boxed = self.is_tokentype_boxed;
            let key = format!(
                "{}_boxed:{}",
                remove_whitespaces(self.token_typename.to_string()),
                terminal_boxed
            );
            ruletype_variant_map.insert(key, terminal_variant_name.clone());
            variant_names_in_order.push((
                terminal_variant_name.clone(),
                self.token_typename.clone(),
                terminal_boxed,
            ));
        }

        // iterates through nonterminals
        for nonterm in self.nonterminals.iter() {
            if let Some(ruletype_stream) = nonterm.ruletype.as_ref().cloned() {
                let cur_len = ruletype_variant_map.len();
                let key = format!(
                    "{}_boxed:{}",
                    remove_whitespaces(ruletype_stream.to_string()),
                    nonterm.ruletype_boxed
                );
                let variant_name = ruletype_variant_map
                    .entry(key)
                    .or_insert_with(|| {
                        let new_variant_name = format_ident!("__variant{}", cur_len);
                        variant_names_in_order.push((
                            new_variant_name.clone(),
                            ruletype_stream.clone(),
                            nonterm.ruletype_boxed,
                        ));
                        new_variant_name
                    })
                    .clone();
                variant_names_for_nonterm.push(variant_name);
            } else {
                variant_names_for_nonterm.push(empty_variant_name.clone());
            }
        }

        // Maps tokens to their corresponding variant names. If the token holds no data, it maps to `Empty`.
        let token_to_variant_name = |token: Token<TerminalSymbol<usize>, usize>| match token {
            Token::Term(term) => match term {
                TerminalSymbol::Term(term) => {
                    if self.terminal_classes[term].data_used {
                        &terminal_variant_name
                    } else {
                        &empty_variant_name
                    }
                }
                TerminalSymbol::Error | TerminalSymbol::Eof => &empty_variant_name,
            },
            Token::NonTerm(nonterm_idx) => &variant_names_for_nonterm[nonterm_idx],
        };

        let mut reduce_action_case_streams = quote! {};

        // TokenStream to define reduce function for each production rule
        let mut fn_reduce_for_each_rule_stream = TokenStream::new();

        for (i, action) in self.custom_reduce_actions.iter().enumerate() {
            let fn_name = format_ident!("custom_reduce_action_{}", i);

            let data_arg = action
                .input_type
                .as_ref()
                .map(|(name, ty)| {
                    let name_ident = format_ident!("{}", name);
                    quote! { mut #name_ident: #ty, }
                })
                .unwrap_or_default();

            let location_arg = action
                .input_location
                .as_ref()
                .map(|name| {
                    let name_ident = format_ident!("{}", name);
                    quote! { mut #name_ident: #location_typename, }
                })
                .unwrap_or_default();

            let body = &action.body.body;

            let output_type = if let Some(ty) = action.output_type.as_ref() {
                ty.clone()
            } else {
                quote! { () }
            };

            fn_reduce_for_each_rule_stream.extend(quote! {
                fn #fn_name(
                    #data_arg
                    #location_arg
                    #user_data_parameter_name: &mut #user_data_typename,
                    __rustylr_location0: &mut #location_typename,
                ) -> Result<#output_type, #reduce_error_typename> {
                    Ok(#body)
                }
            });
        }

        let mut rule_index: usize = 0;
        for (nonterm_idx, nonterm) in self.nonterminals.iter().enumerate() {
            for (rule_local_id, rule) in nonterm.rules.iter().enumerate() {
                let reduce_fn_ident =
                    format_ident!("reduce_{}_{}", nonterm.name.value(), rule_local_id);

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

                if rule.is_used {
                    // Generate debug assertions to verify that the variants on the data stack
                    // match the expected token/non-terminal variants in the production rule.
                    let mut debug_tag_check_stream = TokenStream::new();
                    for (token_idx, token) in rule.tokens.iter().enumerate().rev() {
                        let token_index_from_end = rule.tokens.len() - 1 - token_idx;
                        let variant_name = token_to_variant_name(token.token);

                        if variant_name == &empty_variant_name {
                            debug_tag_check_stream.extend(quote! {
                                debug_assert!(
                                    matches!(
                                        __data_stack.__stack.get(
                                            __data_stack.__stack.len()-1-#token_index_from_end
                                        ),
                                        Some(&#data_enum_typename::Empty)
                                    )
                                );
                            });
                        } else {
                            debug_tag_check_stream.extend(quote! {
                                debug_assert!(
                                    matches!(
                                        __data_stack.__stack.get(
                                            __data_stack.__stack.len()-1-#token_index_from_end
                                        ),
                                        Some(&#data_enum_typename::#variant_name(_))
                                    )
                                );
                            });
                        }
                    }

                    // Generate code to extract values and locations from the stacks for the reduce action.
                    // Since the data stack is a single unified vector, we pop from it in reverse chronological order (right to left).
                    let mut extract_data_stream = TokenStream::new();
                    for (token_idx, token) in rule.tokens.iter().enumerate().rev() {
                        let variant_name = token_to_variant_name(token.token);

                        // Determine location_mapto
                        let location_mapto = if token.reduce_action_chains.is_empty() {
                            let location_index_varname_str =
                                format!("__rustylr_location_{}", token_idx);
                            let index_var_used =
                                rule.reduce_action_contains_ident(&location_index_varname_str);

                            if let Some(mapto) = &token.mapto {
                                let location_varname =
                                    format_ident!("__rustylr_location_{}", mapto.value());
                                let location_varname_str =
                                    format!("__rustylr_location_{}", mapto.value());
                                let mapto_used =
                                    rule.reduce_action_contains_ident(&location_varname_str);

                                if index_var_used {
                                    Some(format_ident!("__rustylr_location_{}", token_idx))
                                } else if mapto_used {
                                    Some(location_varname)
                                } else {
                                    None
                                }
                            } else {
                                if index_var_used {
                                    Some(format_ident!("__rustylr_location_{}", token_idx))
                                } else {
                                    None
                                }
                            }
                        } else {
                            if token.reduce_action_chains.iter().any(|&idx| {
                                let action = &self.custom_reduce_actions[idx];
                                action.input_location.is_some()
                            }) {
                                Some(format_ident!("__rustylr_location_{}", token_idx))
                            } else {
                                None
                            }
                        };

                        if let Some(loc_mapto) = location_mapto {
                            extract_data_stream.extend(quote! {
                                let mut #loc_mapto = __location_stack.pop().unwrap();
                            });
                        } else {
                            extract_data_stream.extend(quote! {
                                __location_stack.pop().unwrap();
                            });
                        }

                        // Determine mapto for data
                        let mapto = if variant_name != &empty_variant_name {
                            if let Some(first_chain) = token.reduce_action_chains.first() {
                                let first_chain = &self.custom_reduce_actions[*first_chain];
                                if first_chain.input_type.is_some() {
                                    Some(format_ident!("__rustylr_data_{}", token_idx))
                                } else {
                                    None
                                }
                            } else {
                                // check if index-based variable __rustylr_data_{token_idx} is used
                                let index_var_used = rule.reduce_action_contains_ident(&format!(
                                    "__rustylr_data_{}",
                                    token_idx
                                ));

                                if let Some(mapto) = &token.mapto {
                                    let mapto_used =
                                        rule.reduce_action_contains_ident(mapto.value().as_str());
                                    if index_var_used {
                                        Some(format_ident!("__rustylr_data_{}", token_idx))
                                    } else if mapto_used {
                                        Some(format_ident!("{}", mapto.value()))
                                    } else {
                                        None
                                    }
                                } else {
                                    if index_var_used {
                                        Some(format_ident!("__rustylr_data_{}", token_idx))
                                    } else {
                                        None
                                    }
                                }
                            }
                        } else {
                            None
                        };

                        if variant_name != &empty_variant_name && mapto.is_some() {
                            let data_mapto = mapto.unwrap();
                            let is_boxed = match token.token {
                                Token::Term(_) => self.is_tokentype_boxed,
                                Token::NonTerm(nonterm_idx) => {
                                    self.nonterminals[nonterm_idx].ruletype_boxed
                                }
                            };
                            let val_extracted = if is_boxed {
                                quote! { *val }
                            } else {
                                quote! { val }
                            };
                            extract_data_stream.extend(quote! {
                                let mut #data_mapto = match __data_stack.__stack.pop().unwrap() {
                                    #data_enum_typename::#variant_name(val) => #val_extracted,
                                    _ => unreachable!(),
                                };
                            });
                        } else {
                            extract_data_stream.extend(quote! {
                                __data_stack.__stack.pop().unwrap();
                            });
                        }
                    }

                    let mut alias_stream = TokenStream::new();
                    for (token_idx, token) in rule.tokens.iter().enumerate() {
                        if token.reduce_action_chains.is_empty() {
                            if let Some(mapto) = &token.mapto {
                                let mapto_used =
                                    rule.reduce_action_contains_ident(mapto.value().as_str());
                                let index_var_used = rule.reduce_action_contains_ident(&format!(
                                    "__rustylr_data_{}",
                                    token_idx
                                ));
                                if mapto_used && index_var_used {
                                    let mapto_ident = format_ident!("{}", mapto.value());
                                    let data_varname =
                                        format_ident!("__rustylr_data_{}", token_idx);
                                    alias_stream.extend(quote! {
                                        let mut #mapto_ident = #data_varname;
                                    });
                                }

                                let location_varname_str =
                                    format!("__rustylr_location_{}", mapto.value());
                                let location_mapto_used =
                                    rule.reduce_action_contains_ident(&location_varname_str);
                                let location_index_varname_str =
                                    format!("__rustylr_location_{}", token_idx);
                                let location_index_var_used =
                                    rule.reduce_action_contains_ident(&location_index_varname_str);
                                if location_mapto_used && location_index_var_used {
                                    let mapto_ident = format_ident!("{}", location_varname_str);
                                    let data_varname =
                                        format_ident!("{}", location_index_varname_str);
                                    alias_stream.extend(quote! {
                                        let mut #mapto_ident = #data_varname;
                                    });
                                }
                            }
                        }
                    }

                    let mut custom_reduce_action_stream = TokenStream::new();
                    for (token_idx, token) in rule.tokens.iter().enumerate() {
                        if token.reduce_action_chains.is_empty() {
                            continue;
                        }
                        let data_varname = format_ident!("__rustylr_data_{}", token_idx);
                        let location_varname = format_ident!("__rustylr_location_{}", token_idx);
                        let location_used_in_this_action = if let Some(mapto) = &token.mapto {
                            let location_mapto_str =
                                format!("__rustylr_location_{}", mapto.value());
                            rule.reduce_action_contains_ident(&location_mapto_str)
                        } else {
                            false
                        };

                        for (idx, &chain_idx) in token.reduce_action_chains.iter().enumerate() {
                            let action = &self.custom_reduce_actions[chain_idx];
                            let fn_name = format_ident!("custom_reduce_action_{}", chain_idx);

                            let data_arg = if action.input_type.is_some() {
                                quote! { #data_varname, }
                            } else {
                                quote! {}
                            };

                            // if location is needed for later reduce action, pass by clone
                            // else, pass by move
                            let mut location_used_later = location_used_in_this_action;
                            if !location_used_later {
                                for &chain_idx in token.reduce_action_chains[idx + 1..].iter() {
                                    let action = &self.custom_reduce_actions[chain_idx];
                                    if action.input_location.is_some() {
                                        location_used_later = true;
                                        break;
                                    }
                                }
                            }
                            let location_arg = if action.input_location.is_some() {
                                if location_used_later {
                                    quote! { #location_varname.clone(), }
                                } else {
                                    quote! { #location_varname, }
                                }
                            } else {
                                quote! {}
                            };

                            custom_reduce_action_stream.extend(quote! {
                                let #data_varname = Self::#fn_name(
                                    #data_arg
                                    #location_arg
                                    #user_data_parameter_name,
                                    __rustylr_location0,
                                )?;
                            });
                        }

                        if let Some(mapto) = &token.mapto {
                            let mapto_ident = format_ident!("{}", mapto.value());
                            if rule.reduce_action_contains_ident(mapto.value().as_str()) {
                                custom_reduce_action_stream.extend(quote! {
                                    let mut #mapto_ident = #data_varname;
                                });
                            }
                            let location_mapto_varname =
                                format_ident!("__rustylr_location_{}", mapto.value());
                            let location_mapto_str =
                                format!("__rustylr_location_{}", mapto.value());

                            if rule.reduce_action_contains_ident(&location_mapto_str) {
                                custom_reduce_action_stream.extend(quote! {
                                    let mut #location_mapto_varname = #location_varname;
                                });
                            }
                        }
                    }

                    reduce_action_case_streams.extend(quote! {
                        #rule_index => Self::#reduce_fn_ident( data_stack, location_stack, push_data, shift, lookahead, user_data, location0 ),
                    });

                    let reduce_action_body = match &rule.reduce_action {
                        Some(ReduceAction::Custom(custom)) => {
                            let body = &custom.body;
                            quote! { #body }
                        }
                        Some(ReduceAction::Identity(identity_idx)) => {
                            let ith_ident = format_ident!(
                                "{}",
                                rule.tokens[*identity_idx].mapto.as_ref().unwrap().value()
                            );
                            quote! { #ith_ident }
                        }
                        None => TokenStream::new(),
                    };
                    let variant_name = &variant_names_for_nonterm[nonterm_idx];
                    if variant_name != &empty_variant_name {
                        let res_expr = if reduce_action_body.is_empty() {
                            quote! { () }
                        } else {
                            quote! { #reduce_action_body }
                        };
                        let push_val = if nonterm.ruletype_boxed {
                            quote! { ::std::boxed::Box::new(__res) }
                        } else {
                            quote! { __res }
                        };
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            #[doc = #rule_debug_str]
                            #[inline]
                            fn #reduce_fn_ident(
                                __data_stack: &mut Self,
                                __location_stack: &mut Vec<#location_typename>,
                                __push_data: bool,
                                shift: &mut bool,
                                lookahead: &#module_prefix::TerminalSymbol<#token_typename>,
                                #user_data_parameter_name: &mut #user_data_typename,
                                __rustylr_location0: &mut #location_typename,
                            ) -> Result<(), #reduce_error_typename> {
                                #[cfg(debug_assertions)]
                                {
                                    #debug_tag_check_stream
                                }

                                #extract_data_stream
                                #alias_stream
                                #custom_reduce_action_stream

                                let __res = #res_expr;
                                if __push_data {
                                    __data_stack.__stack.push(#data_enum_typename::#variant_name(#push_val));
                                } else {
                                    __data_stack.__stack.push(#data_enum_typename::Empty);
                                }

                                Ok(())
                            }
                        });
                    } else {
                        let semicolon_or_empty = if reduce_action_body.is_empty() {
                            quote! {}
                        } else {
                            quote! { ; }
                        };
                        fn_reduce_for_each_rule_stream.extend(quote! {
                            #[doc = #rule_debug_str]
                            #[inline]
                            fn #reduce_fn_ident(
                                __data_stack: &mut Self,
                                __location_stack: &mut Vec<#location_typename>,
                                __push_data: bool,
                                shift: &mut bool,
                                lookahead: &#module_prefix::TerminalSymbol<#token_typename>,
                                #user_data_parameter_name: &mut #user_data_typename,
                                __rustylr_location0: &mut #location_typename,
                            ) -> Result<(), #reduce_error_typename> {
                                #[cfg(debug_assertions)]
                                {
                                    #debug_tag_check_stream
                                }

                                #extract_data_stream
                                #alias_stream
                                #custom_reduce_action_stream

                                #reduce_action_body #semicolon_or_empty
                                __data_stack.__stack.push(#data_enum_typename::Empty);

                                Ok(())
                            }
                        });
                    }
                }
                rule_index += 1;
            }
        }

        let start_idx = *self
            .nonterminals_index
            .get(self.start_rule_name.value())
            .unwrap();
        let start_variant_name = &variant_names_for_nonterm[start_idx];
        // Generate the pop_start implementation.
        // At the time of acceptance, the stack contains the EOF token (which has no data, i.e., Empty)
        // on top of the actual start symbol's value. We pop the EOF token first, and then retrieve the start value.
        let (start_typename, pop_start) = if start_variant_name != &empty_variant_name {
            let ruletype = self.nonterminals[start_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .clone();

            let is_start_boxed = self.nonterminals[start_idx].ruletype_boxed;
            let val_expr = if is_start_boxed {
                quote! { *val }
            } else {
                quote! { val }
            };

            (
                ruletype,
                quote! {
                    self.__stack.pop();
                    match self.__stack.pop() {
                        Some(#data_enum_typename::#start_variant_name(val)) => Some(#val_expr),
                        _ => None,
                    }
                },
            )
        } else {
            (
                quote! {()},
                quote! {
                    self.__stack.pop();
                    match self.__stack.pop() {
                        Some(#data_enum_typename::Empty) => Some(()),
                        _ => None,
                    }
                },
            )
        };

        let derive_clone_for_glr = if self.glr {
            quote! {#[derive(Clone)]}
        } else {
            quote! {}
        };

        let push_terminal_body_stream = if terminal_data_used {
            let push_val = if self.is_tokentype_boxed {
                quote! { ::std::boxed::Box::new(term) }
            } else {
                quote! { term }
            };
            quote! {
                self.__stack.push(#data_enum_typename::#terminal_variant_name(#push_val));
            }
        } else {
            quote! {
                self.__stack.push(#data_enum_typename::Empty);
            }
        };
        let push_empty_body_stream = quote! {
            self.__stack.push(#data_enum_typename::Empty);
        };

        let data_enum_definition = {
            let mut variants = TokenStream::new();
            for (variant_name, typename, boxed) in &variant_names_in_order {
                if *boxed {
                    variants.extend(quote! {
                        #variant_name(::std::boxed::Box<#typename>),
                    });
                } else {
                    variants.extend(quote! {
                        #variant_name(#typename),
                    });
                }
            }
            variants.extend(quote! {
                Empty,
            });
            quote! {
                /// enum for each non-terminal and terminal symbol, that actually hold data
                #[rustfmt::skip]
                #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
                #derive_clone_for_glr
                pub enum #data_enum_typename {
                    #variants
                }
            }
        };

        stream.extend(quote! {

        #data_enum_definition

        /// enum for each non-terminal and terminal symbol, that actually hold data
        #[rustfmt::skip]
        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
        #derive_clone_for_glr
        pub struct #data_stack_typename {
            pub __stack: Vec<#data_enum_typename>,
        }

        impl Default for #data_stack_typename {
            fn default() -> Self {
                Self {
                    __stack: Vec::new(),
                }
            }
        }

        #[rustfmt::skip]
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code)]
        impl #data_stack_typename {
            #fn_reduce_for_each_rule_stream
        }


        #[rustfmt::skip]
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
                self.__stack.pop();
            }
            fn push_terminal(&mut self, term: Self::Term) {
                #push_terminal_body_stream
            }
            fn push_empty(&mut self) {
                #push_empty_body_stream
            }

            // Trait operations like clear, split_off, truncate, and append are highly simplified
            // and efficient because they only need to perform a single vector operation on the unified stack.
            fn clear(&mut self) {
                self.__stack.clear();
            }
            fn reserve(&mut self, additional: usize) {
                self.__stack.reserve(additional);
            }

            fn split_off(&mut self, at: usize) -> Self {
                Self {
                    __stack: self.__stack.split_off(at),
                }
            }
            fn truncate(&mut self, at: usize) {
                self.__stack.truncate(at);
            }
            fn append(&mut self, other: &mut Self) {
                self.__stack.append(&mut other.__stack);
            }

            fn reduce_action(
                data_stack: &mut Self,
                location_stack: &mut Vec<#location_typename>,
                push_data: bool,
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
        self.emit_termclass_enum(&mut stream);
        self.emit_nonterm_enum(&mut stream);
        self.emit_data_stack(&mut stream);
        self.emit_parser(&mut stream);

        stream
    }
}
