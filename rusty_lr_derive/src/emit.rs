use proc_macro2::TokenStream;

use quote::format_ident;
use quote::quote;

use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::token::Token;

use rusty_lr as rlr;

/// emit Rust code for the parser
impl Grammar {
    pub fn emit_parser(&self) -> Result<TokenStream, ParseError> {
        let terminals = &self.terminals;

        let mut grammar: rlr::Grammar<String, String> = rlr::Grammar::new();

        for (name, _typename, rules) in self.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let tokens: Vec<_> = rule
                    .tokens
                    .iter()
                    .map(|t| match t {
                        Token::Term(term) => rlr::Token::Term(term.to_string()),
                        Token::NonTerm(nonterm) => rlr::Token::NonTerm(nonterm.to_string()),
                    })
                    .collect();

                grammar.add_rule(name.to_string(), tokens, rule.reduce_type);
            }
        }
        let augmented = self.augmented.as_ref().unwrap().to_string();
        let parser = match grammar.build(augmented) {
            Ok(parser) => parser,
            Err(err) => {
                return Err(ParseError::GrammarBuildError(format!("{}", err)));
            }
        };

        let mut ret = quote! {};

        // =====================================================================
        // ==================Writing Production Rules===========================
        // =====================================================================
        ret.extend(
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

            ret.extend(
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
        ret.extend(
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

            ret.extend(
                quote! {
                    {
                        #init_shift_goto_map_term
                        #init_shift_goto_map_nonterm
                        #init_reduce_map
                        let state = ::rusty_lr::State {
                            shift_goto_map_term,
                            shift_goto_map_nonterm,
                            reduce_map,
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
        ret.extend(
            quote! {
                let parser = ::rusty_lr::Parser {
                    rules,
                    states,
                    main_state: #main_state,
                };
            }
            .into_iter(),
        );
        Ok(quote! {
            {
            #ret
            parser
            }
        })
    }

    pub fn emit(&self) -> Result<TokenStream, ParseError> {
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

        for (name, _typename, rules) in self.rules.iter() {
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
                            let #slice_name = &terms[self.range_stack.pop().unwrap()];
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
                            token_pop_stream.extend(quote! {
                                let #var_name = self.#stack_name.pop().unwrap();
                            });
                        }
                    }
                }
                let push_stream = quote! { self.#stack_name.push(#action); };
                let case_straem = quote! {
                    #ruleid => {
                        #token_pop_stream
                        #push_stream
                    }
                };
                case_streams.extend(case_straem.into_iter());

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
        let start_rule_stack_name = Self::stack_name(start_rule_name);
        let start_rule_typename = {
            let mut start_rule_typename = TokenStream::new();
            for (name, typename, _) in self.rules.iter() {
                if name == self.start_rule_name.as_ref().unwrap() {
                    start_rule_typename = typename.clone();
                    break;
                }
            }
            start_rule_typename
        };

        let mut stack_def_streams = quote! {};
        let mut stack_init_streams = quote! {};
        for (name, typename, _rules) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name);

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

        let mut reducer_name = format_ident!("{}Reducer", start_rule_name);
        reducer_name.set_span(start_rule_name.span());
        let mut struct_name = format_ident!("{}Parser", start_rule_name);
        struct_name.set_span(start_rule_name.span());

        let parser_emit = self.emit_parser()?;

        Ok(quote! {
            struct #reducer_name<'a> {
                pub range_stack: Vec<std::ops::Range<usize>>,
                pub #term_stack_name : Vec<&'a #term_typename>,
                #stack_def_streams
            }
            impl<'a> #reducer_name<'a> {
                pub fn new() -> Self {
                    Self {
                        range_stack: Vec::new(),
                        #term_stack_name : Vec::new(),
                        #stack_init_streams
                    }
                }
            fn reduce_impl(
                &mut self,
                terms: &'a [#term_typename],
                tree: &::rusty_lr::Tree,
            )
            {
                // slice of this tree
                let s = &terms[tree.range()];
                match tree {
                    ::rusty_lr::Tree::Terminal(term_idx) => {
                        let term = &terms[*term_idx];
                        self.#term_stack_name.push(term);
                    }
                    ::rusty_lr::Tree::NonTerminal(rustylr_macro_generated_ruleid__, children) => {
                        for child in children.iter() {
                            self.reduce_impl(terms, child);
                        }
                        #match_streams
                    }
                }
                self.range_stack.push( tree.range() );
            }

            pub fn reduce(&mut self, terms: &'a [#term_typename], tree: &::rusty_lr::Tree) -> #start_rule_typename
            {
                self.reduce_impl(terms, tree);
                self.#start_rule_stack_name.pop().unwrap()
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
                    eof: #term_typename
                ) -> Result<#start_rule_typename, ::rusty_lr::ParseError<'a, #term_typename, &'static str>>
                {
                    let tree = self.parser.parse_with_callback( terminals, callback, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree))
                }
                pub fn parse<'a>(&'a self, terminals: &'a [#term_typename], eof: #term_typename) -> Result<#start_rule_typename, ::rusty_lr::ParseError<'a, #term_typename, &'static str>>
                {
                    let tree = self.parser.parse( terminals, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree))
                }
            }
        })
    }
    pub fn emit_str(&self) -> Result<TokenStream, ParseError> {
        // =====================================================================
        // ========================Writing Reducer==============================
        // =====================================================================
        // let mut stack_defs = quote! {};
        let mut ruleid: usize = 0;
        let mut case_streams = quote! {};
        let term_stack_name = Self::stack_name(&format_ident! {"rlterminal"});

        for (name, _typename, rules) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name);

            for rule in rules.rule_lines.iter() {
                let action = if let Some(action) = &rule.reduce_action {
                    quote! { {#action} }
                } else {
                    quote! {
                        ()
                    }
                };
                let mut token_pop_stream = TokenStream::new();
                for (idx, token) in rule.tokens.iter().enumerate().rev() {
                    let var_name = format_ident!("v{}", idx);
                    let slice_name = format_ident!("s{}", idx);
                    token_pop_stream.extend(
                        quote! {
                            let #slice_name = &terms[self.range_stack.pop().unwrap()];
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
                            token_pop_stream.extend(quote! {
                                let #var_name = self.#stack_name.pop().unwrap();
                            });
                        }
                    }
                }
                let push_stream = quote! { self.#stack_name.push(#action); };
                let case_straem = quote! {
                    #ruleid => {
                        #token_pop_stream
                        #push_stream
                    }
                };
                case_streams.extend(case_straem.into_iter());

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
        let start_rule_stack_name = Self::stack_name(start_rule_name);
        let start_rule_typename = {
            let mut start_rule_typename = TokenStream::new();
            for (name, typename, _) in self.rules.iter() {
                if name == self.start_rule_name.as_ref().unwrap() {
                    start_rule_typename = typename.clone();
                    break;
                }
            }
            start_rule_typename
        };

        let mut stack_def_streams = quote! {};
        let mut stack_init_streams = quote! {};
        for (name, typename, _rules) in self.rules.iter() {
            // push result to this stack
            let stack_name = Self::stack_name(name);

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

        let mut reducer_name = format_ident!("{}Reducer", start_rule_name);
        reducer_name.set_span(start_rule_name.span());
        let mut struct_name = format_ident!("{}Parser", start_rule_name);
        struct_name.set_span(start_rule_name.span());

        let parser_emit = self.emit_parser()?;

        Ok(quote! {
            pub struct #reducer_name {
                pub range_stack: Vec<std::ops::Range<usize>>,
                pub #term_stack_name : Vec<char>,
                #stack_def_streams
            }
            impl #reducer_name {
                pub fn new() -> Self {
                    Self {
                        range_stack: Vec::new(),
                        #term_stack_name : Vec::new(),
                        #stack_init_streams
                    }
                }
                fn reduce_impl(
                    &mut self,
                    terms: &str,
                    tree: &::rusty_lr::TreeStr,
                )
                {
                    // slice of this tree
                    let s = &terms[tree.range()];
                    match tree {
                        ::rusty_lr::TreeStr::Terminal(begin, end) => {
                            let term = &terms[*begin..*end];
                            let ch : char = term.chars().next().unwrap();
                            self.#term_stack_name.push(ch);
                        }
                        ::rusty_lr::TreeStr::NonTerminal(rustylr_macro_generated_ruleid__, children) => {
                            for child in children.iter() {
                                self.reduce_impl(terms, child);
                            }
                            #match_streams
                        }
                    }
                    self.range_stack.push( tree.range() );
                }

                pub fn reduce(&mut self, terms: &str, tree: &::rusty_lr::TreeStr) -> #start_rule_typename
                {
                    self.reduce_impl(terms, tree);
                    self.#start_rule_stack_name.pop().unwrap()
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
                ) -> Result<#start_rule_typename, ::rusty_lr::ParseErrorStr<'a, char, &'static str>>
                {
                    let tree = self.parser.parse_str_with_callback( terminals, callback, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree))
                }
                pub fn parse_str<'a>(&'a self, terminals: &'a str, eof: char) -> Result<#start_rule_typename, ::rusty_lr::ParseErrorStr<'a, char, &'static str>>
                {
                    let tree = self.parser.parse_str( terminals, eof )?;
                    let mut reducer = #reducer_name::new();
                    Ok(reducer.reduce(terminals, &tree))
                }
            }
        })
    }
}
