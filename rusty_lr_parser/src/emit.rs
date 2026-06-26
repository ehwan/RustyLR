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
    /// write type alias Context, Rule, Tables, Error...
    fn emit_type_alises(&self, stream: &mut TokenStream) {
        let module_prefix = &self.module_prefix;
        let start_rule_span = self
            .span_manager
            .get_span_in_location(&self.start_rule_name.location());
        let rule_typename = Ident::new(&format!("{}Rule", self.start_rule_name), start_rule_span);
        let tables_typename =
            Ident::new(&format!("{}Tables", self.start_rule_name), start_rule_span);
        let nonterm_typename = Ident::new(
            &format!("{}NonTerminals", self.start_rule_name),
            start_rule_span,
        );
        let parse_error_typename = Ident::new(
            &format!("{}ParseError", self.start_rule_name),
            start_rule_span,
        );
        let parser_struct_name =
            Ident::new(&format!("{}Parser", self.start_rule_name), start_rule_span);
        let semantic_value_typename =
            Ident::new(&format!("{}Data", self.start_rule_name), start_rule_span);
        let termclass_typename = Ident::new(
            &format!("{}TerminalClasses", self.start_rule_name),
            start_rule_span,
        );
        let location_typename = &self.location_typename;
        let reduce_error_typename = &self.error_typename;

        let table_structname = if self.emit_dense {
            format_ident!("DenseFlatTables")
        } else {
            format_ident!("SparseFlatTables")
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

        let max_reduce_rules = self
            .states
            .iter()
            .flat_map(|s| s.reduce_map.iter().map(|(_, rules)| rules.len()))
            .max()
            .unwrap_or(1);
        let rule_container_type = if self.glr && max_reduce_rules > 1 {
            quote! { #module_prefix::parser::table::ArrayVec<#rule_index_type, #max_reduce_rules> }
        } else {
            rule_index_type.clone()
        };

        let mut context_structs = TokenStream::new();
        for start_rule_name in &self.start_rule_names {
            let ctx_name = format_ident!("{}Context", start_rule_name.value());
            let start_extracter_typename = format_ident!("{}Extracter", start_rule_name.value());

            if self.glr {
                context_structs.extend(quote! {
                    #[allow(non_camel_case_types, dead_code)]
                    pub type #ctx_name = #module_prefix::parser::nondeterministic::Context<#parser_struct_name, #semantic_value_typename, #start_extracter_typename, #state_index_typename, #max_reduce_rules>;
                });
            } else {
                context_structs.extend(quote! {
                    #[allow(non_camel_case_types, dead_code)]
                    pub type #ctx_name = #module_prefix::parser::deterministic::Context<#parser_struct_name, #semantic_value_typename, #start_extracter_typename, #state_index_typename>;
                });
            }
        }
        stream.extend(context_structs);

        if self.glr {
            stream.extend(quote! {
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::production::Production<#termclass_typename, #nonterm_typename>;
                #[allow(non_camel_case_types,dead_code)]
                pub type #tables_typename = #module_prefix::parser::table::#table_structname<#termclass_typename, #nonterm_typename, #rule_container_type, #state_index_typename>;
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::parser::nondeterministic::ParseError<#token_typename, #location_typename, #reduce_error_typename>;
            });
        } else {
            stream.extend(quote! {
                #[allow(non_camel_case_types,dead_code)]
                pub type #rule_typename = #module_prefix::production::Production<#termclass_typename, #nonterm_typename>;
                #[allow(non_camel_case_types,dead_code)]
                pub type #tables_typename = #module_prefix::parser::table::#table_structname<#termclass_typename, #nonterm_typename, #rule_container_type, #state_index_typename>;
                #[allow(non_camel_case_types,dead_code)]
                pub type #parse_error_typename = #module_prefix::parser::deterministic::ParseError<#token_typename, #location_typename, #reduce_error_typename>;
            });
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
                let pretty_name = self
                    .class_pretty_name_list(rusty_lr_core::TerminalSymbol::Terminal(class_id), 4);
                (name, pretty_name)
            } else {
                if class_def.terminals.len() == 1 {
                    let term_idx = class_def.terminals[0];
                    let term_str = self.terminals[term_idx].name.ident_str().unwrap();
                    let term = Ident::new(term_str, Span::call_site());
                    (term.clone(), term.to_string())
                } else {
                    let name = format_ident!("TermClass{}", class_id);
                    let pretty_name = self.class_pretty_name_list(
                        rusty_lr_core::TerminalSymbol::Terminal(class_id),
                        4,
                    );
                    (name, pretty_name)
                }
            };
            as_str_match_stream.extend(quote! {
                #termclass_typename::#variant_name => #pretty_name,
            });
            class_variants.push(variant_name);
        }

        let other_class_id = self.other_terminal_class_id;

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

        let mut virtual_start_variants = Vec::new();
        let mut branch_indices = Vec::new();
        for i in 0..self.start_rule_names.len() {
            virtual_start_variants.push(format_ident!("VirtualStart{}", i));
            branch_indices.push(i as u32);
        }

        let max_variants = self.terminal_classes.len() + 2 + virtual_start_variants.len();

        stream.extend(quote! {
            /// A enum that represents terminal classes
            #[allow(non_camel_case_types, dead_code)]
            #[derive(Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
            // repr(usize) is used to ensure a stable memory layout compatible with integer casting and transmutes
            #[repr(usize)]
            pub enum #termclass_typename {
                #(#class_variants,)*
                #error_name,
                #eof_name,
                #(#virtual_start_variants,)*
            }

            impl #termclass_typename {
                // Decodes a terminal class from its integer index stored in the serialized parser tables.
                // This avoids verbose static enum instantiations and significantly reduces compiled binary size.
                #[inline]
                pub fn from_usize(value: usize) -> Self {
                    debug_assert!(value < #max_variants, "Terminal class index {} is out of bounds (max {})", value, #max_variants);
                    unsafe { ::std::mem::transmute(value) }
                }
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
                        #( #termclass_typename::#virtual_start_variants => "virtual_start", )*
                    }
                }
                fn to_usize(&self) -> usize {
                    *self as usize
                }

                fn from_term(terminal: &Self::Term) -> Self {
                    #[allow(unreachable_patterns, unused_variables)]
                    match terminal {
                        #from_term_match_stream
                    }
                }

                fn from_virtual_start(branch_idx: u32) -> Self {
                    match branch_idx {
                        #( #branch_indices => Self::#virtual_start_variants, )*
                        _ => panic!("Invalid virtual start branch index: {}", branch_idx),
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

        let max_variants = self.nonterminals.len();

        stream.extend(
    quote! {
            /// An enum that represents non-terminal symbols
            #[allow(non_camel_case_types, dead_code)]
            #[derive(Clone, Copy, std::hash::Hash, std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord)]
            // repr(usize) is used to ensure a stable memory layout compatible with integer casting and transmutes
            #[repr(usize)]
            pub enum #enum_typename {
                #comma_separated_variants
            }

            impl #enum_typename {
                // Decodes a non-terminal from its integer index stored in the serialized parser tables.
                // This avoids verbose static enum instantiations and significantly reduces compiled binary size.
                #[inline]
                pub fn from_usize(value: usize) -> Self {
                    debug_assert!(value < #max_variants, "Non-terminal index {} is out of bounds (max {})", value, #max_variants);
                    unsafe { ::std::mem::transmute(value) }
                }
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
        let tables_typename = format_ident!("{}Tables", start_rule_ident);
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
        use rusty_lr_core::TerminalSymbol;

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
        let max_reduce_rules = self
            .states
            .iter()
            .flat_map(|s| s.reduce_map.iter().map(|(_, rules)| rules.len()))
            .max()
            .unwrap_or(1);
        let rule_container_type = if self.glr && max_reduce_rules > 1 {
            quote! { #module_prefix::parser::table::ArrayVec<#rule_index_typename, #max_reduce_rules> }
        } else {
            rule_index_typename.clone()
        };

        // ------------------
        // Rules Serialization
        // ------------------
        let mut rule_names = Vec::new();
        let mut rule_lengths = Vec::new();

        for rule in &self.builder.rules {
            assert!(
                rule.rule.lhs < 32768,
                "Non-terminal index {} exceeds 15-bit serialization limit (32768)",
                rule.rule.lhs
            );
            rule_names.push(rule.rule.lhs as u32);
            rule_lengths.push(rule.rule.rhs.len() as u32);
        }

        // ------------------
        // Table Row Serialization
        // ------------------
        let mut shift_term_data = Vec::new();
        let mut shift_term_offsets = Vec::new();
        let mut shift_nonterm_data = Vec::new();
        let mut shift_nonterm_offsets = Vec::new();
        let mut reduce_data = Vec::new();
        let mut reduce_offsets = Vec::new();
        let mut can_accept_error = Vec::new();

        shift_term_offsets.push(0);
        shift_nonterm_offsets.push(0);
        reduce_offsets.push(0);

        for state in &self.states {
            // 1. shift_goto_map_term
            for &(term, next_state) in &state.shift_goto_map_term {
                let term_idx = match term {
                    TerminalSymbol::Terminal(t) => t,
                    TerminalSymbol::Error => self.terminal_classes.len(),
                    TerminalSymbol::Eof => self.terminal_classes.len() + 1,
                    TerminalSymbol::VirtualStart(i) => self.terminal_classes.len() + 2 + i as usize,
                };
                let state_idx = next_state.state;
                let push = next_state.push;
                assert!(
                    term_idx < 32768,
                    "Terminal class index {} exceeds 15-bit limit (32768)",
                    term_idx
                );
                assert!(
                    state_idx < 65536,
                    "State index {} exceeds 16-bit limit (65536)",
                    state_idx
                );
                let val = (term_idx as u32) | ((state_idx as u32) << 15) | ((push as u32) << 31);
                shift_term_data.push(val);
            }
            shift_term_offsets.push(shift_term_data.len() as u32);

            // 2. shift_goto_map_nonterm
            for &(nonterm, next_state) in &state.shift_goto_map_nonterm {
                let nonterm_idx = nonterm;
                let state_idx = next_state.state;
                let push = next_state.push;
                assert!(
                    nonterm_idx < 32768,
                    "Non-terminal index {} exceeds 15-bit limit (32768)",
                    nonterm_idx
                );
                assert!(
                    state_idx < 65536,
                    "State index {} exceeds 16-bit limit (65536)",
                    state_idx
                );
                let val = (nonterm_idx as u32) | ((state_idx as u32) << 15) | ((push as u32) << 31);
                shift_nonterm_data.push(val);
            }
            shift_nonterm_offsets.push(shift_nonterm_data.len() as u32);

            // 3. reduce_map: Vec<(TermClass, Vec<usize>)>
            for (term, rules) in &state.reduce_map {
                let term_idx = match term {
                    TerminalSymbol::Terminal(t) => *t,
                    TerminalSymbol::Error => self.terminal_classes.len(),
                    TerminalSymbol::Eof => self.terminal_classes.len() + 1,
                    TerminalSymbol::VirtualStart(i) => {
                        self.terminal_classes.len() + 2 + *i as usize
                    }
                };
                assert!(
                    term_idx < 32768,
                    "Terminal class index {} exceeds 15-bit limit (32768)",
                    term_idx
                );
                reduce_data.push(term_idx as u32);
                reduce_data.push(rules.len() as u32);
                for &rule in rules {
                    assert!(
                        rule < 65536,
                        "Rule index {} exceeds 16-bit limit (65536)",
                        rule
                    );
                    reduce_data.push(rule as u32);
                }
            }
            reduce_offsets.push(reduce_data.len() as u32);

            // 5. can_accept_error: TriState
            let tri_val = match state.can_accept_error {
                rusty_lr_core::TriState::False => 0u8,
                rusty_lr_core::TriState::True => 1u8,
                rusty_lr_core::TriState::Maybe => 2u8,
            };
            can_accept_error.push(tri_val);
        }

        let num_rules = self.builder.rules.len();
        let num_states = self.states.len();
        let error_used = self.error_used;

        // to remove suffix from generated data
        let rule_names = rule_names
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);

        let rule_lengths = rule_lengths
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);

        let shift_term_data = shift_term_data
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);
        let shift_term_offsets = shift_term_offsets
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);
        let shift_nonterm_data = shift_nonterm_data
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);
        let shift_nonterm_offsets = shift_nonterm_offsets
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);
        let reduce_data = reduce_data
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);
        let reduce_offsets = reduce_offsets
            .into_iter()
            .map(proc_macro2::Literal::u32_unsuffixed);
        let can_accept_error = can_accept_error
            .into_iter()
            .map(proc_macro2::Literal::u8_unsuffixed);

        // range-compressed Vec based terminal-class_id map
        stream.extend(quote! {
            /// A lightweight parser struct that references the static parser tables and production rules.
            ///
            /// Since this struct only holds `'static` references to shared, read-only static parser tables,
            /// it is extremely cheap to instantiate, copy, or clone, and takes very little space.
            #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
            #[derive(Clone, Copy)]
            pub struct #parser_struct_name;

            unsafe impl ::std::marker::Send for #parser_struct_name {}
            unsafe impl ::std::marker::Sync for #parser_struct_name {}
            #[rustfmt::skip]
            impl #module_prefix::parser::Parser for #parser_struct_name {
                type Term = #token_typename;
                type TermClass = #termclass_typename;
                type NonTerm = #nonterminals_enum_name;
                type StateIndex = #state_index_typename;
                type ReduceRules = #rule_container_type;
                type Tables = #tables_typename;

                const ERROR_USED:bool = #error_used;


                // get_tables returns the decoded flat runtime parser tables.
                // Serialized integer arrays keep the generated source compact while Context keeps the
                // decoded table reference out of the parsing hot path.
                fn get_tables() -> &'static #tables_typename {
                    static TABLES: std::sync::OnceLock<#tables_typename> = std::sync::OnceLock::new();
                    TABLES.get_or_init(|| {
                        // Serialized rule properties:
                        // - RULE_NAMES: NonTerm enum value of the rule LHS name
                        // - RULE_LENGTHS: RHS length of each production rule
                        static RULE_NAMES: &[u32] = &[ #(#rule_names),* ];
                        static RULE_LENGTHS: &[u32] = &[ #(#rule_lengths),* ];

                        // Serialized table row properties:
                        // - SHIFT_TERM_DATA & SHIFT_NONTERM_DATA: Packed transitions (push << 31) | (state_idx << 15) | (symbol_idx)
                        // - SHIFT_TERM_OFFSETS & SHIFT_NONTERM_OFFSETS: Boundaries separating transitions for each state
                        // - REDUCE_DATA: Variable-length reduce map encoding (term_class, len, rules...)
                        // - REDUCE_OFFSETS: Boundaries separating reduce maps for each state
                        // - CAN_ACCEPT_ERROR: TriState (0 = False, 1 = True, 2 = Maybe)
                        static SHIFT_TERM_DATA: &[u32] = &[ #(#shift_term_data),* ];
                        static SHIFT_TERM_OFFSETS: &[u32] = &[ #(#shift_term_offsets),* ];
                        static SHIFT_NONTERM_DATA: &[u32] = &[ #(#shift_nonterm_data),* ];
                        static SHIFT_NONTERM_OFFSETS: &[u32] = &[ #(#shift_nonterm_offsets),* ];
                        static REDUCE_DATA: &[u32] = &[ #(#reduce_data),* ];
                        static REDUCE_OFFSETS: &[u32] = &[ #(#reduce_offsets),* ];
                        static CAN_ACCEPT_ERROR: &[u8] = &[ #(#can_accept_error),* ];

                        let num_rules = #num_rules;
                        let mut rules = Vec::with_capacity(num_rules);
                        for i in 0..num_rules {
                            let lhs = #nonterminals_enum_name::from_usize(RULE_NAMES[i] as usize);

                            rules.push(#module_prefix::parser::table::RuleInfo {
                                lhs,
                                len: RULE_LENGTHS[i] as usize,
                            });
                        }

                        let num_states = #num_states;
                        let mut state_rows = Vec::with_capacity(num_states);
                        for i in 0..num_states {
                            // Decode shift transitions for terminals (terminal class, next state index, push flag)
                            let term_start = SHIFT_TERM_OFFSETS[i] as usize;
                            let term_end = SHIFT_TERM_OFFSETS[i + 1] as usize;
                            let mut shift_goto_map_term = Vec::with_capacity(term_end - term_start);
                            for idx in term_start..term_end {
                                let val = SHIFT_TERM_DATA[idx];
                                let term_class = #termclass_typename::from_usize((val & 0x7fff) as usize);
                                let state = ((val >> 15) & 0xffff) as usize;
                                let push = (val >> 31) != 0;
                                shift_goto_map_term.push((term_class, #module_prefix::parser::table::ShiftTarget::new(state, push)));
                            }

                            // Decode shift transitions for non-terminals (non-terminal index, next state index, push flag)
                            let nonterm_start = SHIFT_NONTERM_OFFSETS[i] as usize;
                            let nonterm_end = SHIFT_NONTERM_OFFSETS[i + 1] as usize;
                            let mut shift_goto_map_nonterm = Vec::with_capacity(nonterm_end - nonterm_start);
                            for idx in nonterm_start..nonterm_end {
                                let val = SHIFT_NONTERM_DATA[idx];
                                let nonterm = #nonterminals_enum_name::from_usize((val & 0x7fff) as usize);
                                let state = ((val >> 15) & 0xffff) as usize;
                                let push = (val >> 31) != 0;
                                shift_goto_map_nonterm.push((nonterm, #module_prefix::parser::table::ShiftTarget::new(state, push)));
                            }

                            // Decode the reduce action map (variable-length encoding)
                            let reduce_start = REDUCE_OFFSETS[i] as usize;
                            let reduce_end = REDUCE_OFFSETS[i + 1] as usize;
                            let mut reduce_map = Vec::new();
                            let mut idx = reduce_start;
                            while idx < reduce_end {
                                let term_val = REDUCE_DATA[idx];
                                let term_class = #termclass_typename::from_usize(term_val as usize);
                                let len = REDUCE_DATA[idx + 1] as usize;
                                let mut rules = Vec::with_capacity(len);
                                for r_idx in 0..len {
                                    rules.push(REDUCE_DATA[idx + 2 + r_idx] as usize);
                                }
                                reduce_map.push((term_class, rules));
                                idx += 2 + len;
                            }

                            let can_accept_error = match CAN_ACCEPT_ERROR[i] {
                                0 => #module_prefix::TriState::False,
                                1 => #module_prefix::TriState::True,
                                2 => #module_prefix::TriState::Maybe,
                                _ => unreachable!(),
                            };

                            let intermediate = #module_prefix::parser::state::IntermediateState {
                                shift_goto_map_term,
                                shift_goto_map_nonterm,
                                reduce_map,
                                ruleset: Vec::new(),
                                can_accept_error,
                            };
                            state_rows.push(intermediate);
                        }

                        #module_prefix::parser::table::IntermediateTables {
                            state_rows,
                            rules,
                        }.into()
                    })
                }
            }
        });
    }

    fn emit_data_stack(&self, stream: &mut TokenStream) {
        use rusty_lr_core::Symbol;
        use rusty_lr_core::TerminalSymbol;

        let module_prefix = &self.module_prefix;
        let start_rule_ident = Ident::new(
            &self.start_rule_name,
            self.span_manager
                .get_span_in_location(&self.start_rule_name.location()),
        );
        let nonterminals_enum_name = format_ident!("{}NonTerminals", &start_rule_ident);
        let reduce_error_typename = &self.error_typename;
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
        let token_to_variant_name = |token: Symbol<TerminalSymbol<usize>, usize>| match token {
            Symbol::Terminal(term) => match term {
                TerminalSymbol::Terminal(term) => {
                    if self.terminal_classes[term].data_used {
                        &terminal_variant_name
                    } else {
                        &empty_variant_name
                    }
                }
                TerminalSymbol::Error | TerminalSymbol::Eof | TerminalSymbol::VirtualStart(_) => {
                    &empty_variant_name
                }
            },
            Symbol::NonTerminal(nonterm_idx) => &variant_names_for_nonterm[nonterm_idx],
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
                            match t.symbol {
                                Symbol::Terminal(term) => self.class_pretty_name_list(term, 5),
                                Symbol::NonTerminal(nonterm) => self.nonterm_pretty_name(nonterm),
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
                        let variant_name = token_to_variant_name(token.symbol);

                        if variant_name == &empty_variant_name {
                            debug_tag_check_stream.extend(quote! {
                                debug_assert!(
                                    matches!(
                                            __data_stack.get(
                                                __data_stack.len()-1-#token_index_from_end
                                        ),
                                        Some(&#data_enum_typename::Empty)
                                    )
                                );
                            });
                        } else {
                            debug_tag_check_stream.extend(quote! {
                                debug_assert!(
                                    matches!(
                                            __data_stack.get(
                                                __data_stack.len()-1-#token_index_from_end
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

                    let mut location_maptos = Vec::with_capacity(rule.tokens.len());
                    let mut data_maptos = Vec::with_capacity(rule.tokens.len());

                    for (token_idx, token) in rule.tokens.iter().enumerate().rev() {
                        let variant_name = token_to_variant_name(token.symbol);

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
                        location_maptos.push(location_mapto);

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
                            let is_boxed = match token.symbol {
                                Symbol::Terminal(_) => self.is_tokentype_boxed,
                                Symbol::NonTerminal(nonterm_idx) => {
                                    self.nonterminals[nonterm_idx].ruletype_boxed
                                }
                            };
                            data_maptos.push(Some((
                                variant_name.clone(),
                                mapto.unwrap(),
                                is_boxed,
                            )));
                        } else {
                            data_maptos.push(None);
                        }
                    }

                    // 1. Generate location pops/truncates
                    {
                        let mut consecutive_unneeded = 0;
                        for loc_mapto in &location_maptos {
                            if let Some(loc_mapto) = loc_mapto {
                                if consecutive_unneeded > 0 {
                                    if consecutive_unneeded == 1 {
                                        extract_data_stream.extend(quote! {
                                            __location_stack.pop();
                                        });
                                    } else {
                                        let consecutive_unneeded =
                                            syn::Index::from(consecutive_unneeded);
                                        extract_data_stream.extend(quote! {
                                            __location_stack.truncate(__location_stack.len() - #consecutive_unneeded);
                                        });
                                    }
                                    consecutive_unneeded = 0;
                                }
                                extract_data_stream.extend(quote! {
                                    let mut #loc_mapto = __location_stack.pop().unwrap();
                                });
                            } else {
                                consecutive_unneeded += 1;
                            }
                        }
                        if consecutive_unneeded > 0 {
                            if consecutive_unneeded == 1 {
                                extract_data_stream.extend(quote! {
                                    __location_stack.pop();
                                });
                            } else {
                                let consecutive_unneeded = syn::Index::from(consecutive_unneeded);
                                extract_data_stream.extend(quote! {
                                    __location_stack.truncate(__location_stack.len() - #consecutive_unneeded);
                                });
                            }
                        }
                    }

                    // 2. Generate data stack pops/truncates
                    {
                        let mut consecutive_unneeded = 0;
                        for data_info in &data_maptos {
                            if let Some((variant_name, data_mapto, is_boxed)) = data_info {
                                if consecutive_unneeded > 0 {
                                    if consecutive_unneeded == 1 {
                                        extract_data_stream.extend(quote! {
                                            __data_stack.pop();
                                        });
                                    } else {
                                        let consecutive_unneeded =
                                            syn::Index::from(consecutive_unneeded);
                                        extract_data_stream.extend(quote! {
                                            __data_stack.truncate(__data_stack.len() - #consecutive_unneeded);
                                        });
                                    }
                                    consecutive_unneeded = 0;
                                }
                                let val_extracted = if *is_boxed {
                                    quote! { *val }
                                } else {
                                    quote! { val }
                                };
                                extract_data_stream.extend(quote! {
                                    let mut #data_mapto = match __data_stack.pop().unwrap() {
                                        #data_enum_typename::#variant_name(val) => #val_extracted,
                                        _ => unreachable!(),
                                    };
                                });
                            } else {
                                consecutive_unneeded += 1;
                            }
                        }
                        if consecutive_unneeded > 0 {
                            if consecutive_unneeded == 1 {
                                extract_data_stream.extend(quote! {
                                    __data_stack.pop();
                                });
                            } else {
                                let consecutive_unneeded = syn::Index::from(consecutive_unneeded);
                                extract_data_stream.extend(quote! {
                                    __data_stack.truncate(__data_stack.len() - #consecutive_unneeded);
                                });
                            }
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
                                __data_stack: &mut Vec<Self>,
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
                                    __data_stack.push(Self::#variant_name(#push_val));
                                } else {
                                    __data_stack.push(Self::Empty);
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
                                __data_stack: &mut Vec<Self>,
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
                                __data_stack.push(Self::Empty);

                                Ok(())
                            }
                        });
                    }
                }
                rule_index += 1;
            }
        }

        let mut start_extracter_impls = TokenStream::new();
        for (branch_idx, start_rule_name) in self.start_rule_names.iter().enumerate() {
            let s_idx = *self
                .nonterminals_index
                .get(start_rule_name.value())
                .unwrap();
            let s_variant_name = &variant_names_for_nonterm[s_idx];
            let s_ruletype = self.nonterminals[s_idx]
                .ruletype
                .as_ref()
                .unwrap_or(&quote! {()})
                .clone();
            let branch_idx_u32 = branch_idx as u32;
            let start_extracter_typename = format_ident!("{}Extracter", start_rule_name.value());

            if s_variant_name != &empty_variant_name {
                let s_val_expr = if self.nonterminals[s_idx].ruletype_boxed {
                    quote! { *val }
                } else {
                    quote! { val }
                };
                start_extracter_impls.extend(quote! {
                    #[doc(hidden)]
                    #[allow(non_camel_case_types, dead_code)]
                    pub struct #start_extracter_typename;

                    impl #module_prefix::parser::semantic_value::StartExtractor<#data_enum_typename> for #start_extracter_typename {
                        type StartType = #s_ruletype;
                        const BRANCH_INDEX: u32 = #branch_idx_u32;

                        fn extract(value: #data_enum_typename) -> Option<Self::StartType> {
                        match value {
                                #data_enum_typename::#s_variant_name(val) => Some(#s_val_expr),
                            _ => None,
                        }
                        }
                    }
                });
            } else {
                start_extracter_impls.extend(quote! {
                    #[doc(hidden)]
                    #[allow(non_camel_case_types, dead_code)]
                    pub struct #start_extracter_typename;

                    impl #module_prefix::parser::semantic_value::StartExtractor<#data_enum_typename> for #start_extracter_typename {
                        type StartType = #s_ruletype;
                        const BRANCH_INDEX: u32 = #branch_idx_u32;

                        fn extract(value: #data_enum_typename) -> Option<Self::StartType> {
                        match value {
                                #data_enum_typename::Empty => Some(()),
                            _ => None,
                        }
                        }
                    }
                });
            }
        }

        let push_terminal_body_stream = if terminal_data_used {
            let push_val = if self.is_tokentype_boxed {
                quote! { ::std::boxed::Box::new(term) }
            } else {
                quote! { term }
            };
            quote! {
                Self::#terminal_variant_name(#push_val)
            }
        } else {
            quote! {
                Self::Empty
            }
        };
        let push_empty_body_stream = quote! {
            Self::Empty
        };

        let data_enum_definition = {
            let mut variants = TokenStream::new();
            let mut debug_arms = TokenStream::new();
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
                debug_arms.extend(quote! {
                    Self::#variant_name(..) => f.write_str(stringify!(#variant_name)),
                });
            }
            variants.extend(quote! {
                Empty,
            });
            debug_arms.extend(quote! {
                Self::Empty => f.write_str("Empty"),
            });
            quote! {
                /// enum for each non-terminal and terminal symbol, that actually hold data
                #[rustfmt::skip]
                #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
                #[derive(Clone)]
                pub enum #data_enum_typename {
                    #variants
                }

                impl ::std::fmt::Debug for #data_enum_typename {
                    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                        match self {
                            #debug_arms
                        }
                    }
                }
            }
        };

        stream.extend(quote! {

        #data_enum_definition

        #start_extracter_impls

        #[rustfmt::skip]
        #[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut, dead_code, unreachable_patterns)]
        impl #data_enum_typename {
            #fn_reduce_for_each_rule_stream
        }


        #[rustfmt::skip]
        #[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types, unused_variables)]
        impl #module_prefix::parser::semantic_value::SemanticValue for #data_enum_typename {
            type Term = #token_typename;
            type NonTerm = #nonterminals_enum_name;
            type ReduceActionError = #reduce_error_typename;
            type UserData = #user_data_typename;
            type Location = #location_typename;

            fn new_empty() -> Self {
                #push_empty_body_stream
            }
            fn new_terminal(term: Self::Term) -> Self {
                #push_terminal_body_stream
            }

            fn reduce_action(
                data_stack: &mut Vec<Self>,
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
