use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use rusty_lr_core::Symbol;
use rusty_lr_core::TerminalSymbol;
use rusty_lr_core::hash::HashMap;
use rusty_lr_core::hash::HashSet;
use rusty_lr_core::production::Precedence;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

use super::Grammar;
use super::ResolvedAllowTarget;
use super::Terminal;
use super::TerminalClassDefinition;
use crate::error::ArgError;
use crate::error::ParseArgError;
use crate::error::ParseError;
use crate::nonterminal_info::CustomReduceAction;
use crate::nonterminal_info::NonTerminalInfo;
use crate::nonterminal_info::ReduceAction;
use crate::nonterminal_info::Rule;
use crate::parser::args::GrammarArgs;
use crate::parser::args::IdentOrLiteral;
use crate::parser::location::Located;
use crate::parser::location::Location;
use crate::parser::location::SpanManager;
use crate::parser::parser_expanded::GrammarContext;
use crate::pattern::Pattern;
use crate::pattern::PatternToToken;
use crate::rangeresolver::RangeResolver;
use crate::symbol::MappedSymbol;
use crate::terminal_info::TerminalInfo;
use crate::terminal_info::TerminalName;
use crate::utils;

impl Grammar {
    pub(crate) fn negate_terminal_set(
        &self,
        terminalset: &BTreeSet<Terminal>,
    ) -> BTreeSet<Terminal> {
        (0..self.terminals.len())
            .filter(|&i| !terminalset.contains(&i))
            .collect()
    }

    pub fn parse_args(input: TokenStream) -> Result<GrammarArgs, (ParseArgError, SpanManager)> {
        let mut context = GrammarContext::new(GrammarArgs::default());

        match crate::parser::lexer::feed_recursive(input, &mut context) {
            Ok(_) => {}
            Err(err) => {
                let message = err.to_string();
                return Err((
                    ParseArgError::MacroLineParse {
                        location: err.location().clone(),
                        message,
                    },
                    context.userdata().span_manager.clone(),
                ));
            }
        }

        let span_manager = context.userdata().span_manager.clone();
        let (_, grammar_args) = match context.accept() {
            Ok(result) => result,
            Err(err) => {
                let message = err.to_string();
                return Err((
                    ParseArgError::MacroLineParse {
                        location: *err.location(),
                        message,
                    },
                    span_manager,
                ));
            }
        };

        Ok(grammar_args)
    }
    pub fn arg_check_error(grammar_args: &GrammarArgs) -> Result<(), ArgError> {
        // %error
        if grammar_args.error_typename.len() > 1 {
            return Err(ArgError::MultipleErrorDefinition(
                grammar_args
                    .error_typename
                    .iter()
                    .map(|(loc, _)| loc)
                    .cloned()
                    .collect(),
            ));
        }

        // %location
        if grammar_args.location_typename.len() > 1 {
            return Err(ArgError::MultipleLocationDefinition(
                grammar_args
                    .location_typename
                    .iter()
                    .map(|(loc, _)| loc)
                    .cloned()
                    .collect(),
            ));
        }

        // %moduleprefix
        if grammar_args.module_prefix.len() > 1 {
            return Err(ArgError::MultipleModulePrefixDefinition(
                grammar_args
                    .module_prefix
                    .iter()
                    .map(|(loc, _)| loc)
                    .cloned()
                    .collect(),
            ));
        }

        // %userdata
        if grammar_args.userdata_typename.len() > 1 {
            return Err(ArgError::MultipleUserDataDefinition(
                grammar_args
                    .userdata_typename
                    .iter()
                    .map(|(loc, _)| loc)
                    .cloned()
                    .collect(),
            ));
        }

        // %tokentype
        if grammar_args.token_typename.is_empty() {
            return Err(ArgError::TokenTypeNotDefined);
        } else if grammar_args.token_typename.len() > 1 {
            return Err(ArgError::MultipleTokenTypeDefinition(
                grammar_args
                    .token_typename
                    .iter()
                    .map(|(loc, _)| loc)
                    .cloned()
                    .collect(),
            ));
        }

        // %start
        if grammar_args.start_rule_name.is_empty() {
            return Err(ArgError::StartNotDefined);
        }
        {
            let mut seen_starts = HashSet::default();
            for start_rule_name in &grammar_args.start_rule_name {
                let name = start_rule_name.value();
                if !seen_starts.insert(name.clone()) {
                    return Err(ArgError::DuplicateStartSymbol {
                        location: start_rule_name.location(),
                        name: name.clone(),
                    });
                }
            }
        }

        // %prec and %dprec in each production rules
        for rules in grammar_args.rules.iter() {
            for rule in rules.rule_lines.iter() {
                let precs: Vec<_> = rule.precs().collect();
                let dprecs: Vec<_> = rule.dprecs().collect();

                if precs.len() > 1 {
                    return Err(ArgError::MultiplePrecDefinition(
                        precs.into_iter().map(|prec| prec.location()).collect(),
                    ));
                }
                if dprecs.len() > 1 {
                    return Err(ArgError::MultipleDPrecDefinition(
                        dprecs.into_iter().map(|dprec| dprec.location()).collect(),
                    ));
                }
            }
        }

        // check duplicated names for terminals and non-terminals
        {
            let mut name_locations = BTreeMap::<String, Vec<Location>>::new();
            // collect locations in terminal definitions
            for (name, _) in grammar_args.terminals.iter() {
                name_locations
                    .entry(name.value().clone())
                    .or_default()
                    .push(name.location());
            }
            // collect locations in non-terminal definitions
            for rules in grammar_args.rules.iter() {
                name_locations
                    .entry(rules.name.value().clone())
                    .or_default()
                    .push(rules.name.location());
            }

            for (name, locations) in name_locations.into_iter() {
                if locations.len() > 1 {
                    return Err(ArgError::MultipleNameDefinition(name, locations));
                }
            }
        }

        // check reserved names for terminals and non-terminals
        {
            /// check if the given identifier name string is reserved
            fn is_reserved_name(name: &str) -> bool {
                if name == utils::AUGMENTED_NAME {
                    return true;
                }
                if name == utils::EOF_NAME {
                    return true;
                }
                if name == utils::ERROR_NAME {
                    return true;
                }
                false
            }

            let mut reserved_names = Vec::new();
            for (name, _) in grammar_args.terminals.iter() {
                if is_reserved_name(name) {
                    reserved_names.push(name.clone());
                }
            }
            for rules in grammar_args.rules.iter() {
                if is_reserved_name(&rules.name) {
                    reserved_names.push(rules.name.clone());
                }
            }

            if !reserved_names.is_empty() {
                return Err(ArgError::ReservedName(reserved_names));
            }
        }

        Ok(())
    }

    pub(crate) fn get_terminal_indices_from_char_range(
        &self,
        start: char,
        last: char,
    ) -> impl Iterator<Item = Terminal> + '_ {
        self.terminals
            .iter()
            .enumerate()
            .filter_map(move |(idx, terminal)| {
                if let TerminalName::CharRange(start_, last_) = &terminal.name {
                    if *last_ < start || *start_ > last {
                        None
                    } else {
                        Some(idx)
                    }
                } else {
                    None
                }
            })
    }
    pub(crate) fn get_terminal_index_from_char(&self, ch: char) -> Terminal {
        let name: TerminalName = (ch, ch).into();
        *self.terminals_index.get(&name).unwrap()
    }

    /// parse the input TokenStream and return a parsed Grammar
    pub fn from_grammar_args(mut grammar_args: GrammarArgs) -> Result<Self, ParseError> {
        #[derive(Clone)]
        enum ResolveState {
            Unresolved,
            Resolving,
            Resolved(TokenStream),
        }

        struct ProviderInfo {
            #[allow(dead_code)]
            name: String,
            location: Location,
            stream: Option<TokenStream>,
            state: ResolveState,
        }

        fn resolve_provider(
            name: &str,
            providers: &mut HashMap<String, ProviderInfo>,
            span_manager: &mut crate::parser::location::SpanManager,
            stack: &mut Vec<String>,
            depth: usize,
            max_depth: usize,
            ref_loc: Location,
        ) -> Result<TokenStream, ParseError> {
            if depth > max_depth {
                return Err(ParseError::MaxSubstitutionDepthExceeded {
                    location: ref_loc,
                    max_depth,
                });
            }

            if stack.contains(&name.to_string()) {
                let mut path = stack.clone();
                path.push(name.to_string());
                return Err(ParseError::CircularDependency {
                    location: ref_loc,
                    path,
                });
            }

            if !providers.contains_key(name) {
                if name.starts_with("nonterm:") {
                    return Err(ParseError::NonTerminalNotDefined(ref_loc));
                } else if name.starts_with("term:") {
                    return Err(ParseError::TerminalNotDefined(ref_loc));
                } else {
                    return Err(ParseError::TerminalNotDefined(ref_loc));
                }
            }

            let state = providers.get(name).unwrap().state.clone();
            match state {
                ResolveState::Resolved(resolved_stream) => {
                    return Ok(resolved_stream);
                }
                ResolveState::Resolving => {
                    let mut path = stack.clone();
                    path.push(name.to_string());
                    return Err(ParseError::CircularDependency {
                        location: ref_loc,
                        path,
                    });
                }
                ResolveState::Unresolved => {}
            }

            providers.get_mut(name).unwrap().state = ResolveState::Resolving;
            stack.push(name.to_string());

            let raw_stream = providers.get(name).unwrap().stream.clone();
            let resolved_stream = match raw_stream {
                Some(stream) => {
                    substitute_stream(stream, providers, span_manager, stack, depth + 1, max_depth)?
                }
                None => {
                    debug_assert!(name.starts_with("nonterm:"));
                    quote! { () }
                }
            };

            stack.pop();
            providers.get_mut(name).unwrap().state =
                ResolveState::Resolved(resolved_stream.clone());

            Ok(resolved_stream)
        }

        fn substitute_stream(
            stream: TokenStream,
            providers: &mut HashMap<String, ProviderInfo>,
            span_manager: &mut crate::parser::location::SpanManager,
            stack: &mut Vec<String>,
            depth: usize,
            max_depth: usize,
        ) -> Result<TokenStream, ParseError> {
            let mut result = TokenStream::new();
            let mut iter = stream.into_iter().peekable();

            while let Some(tt) = iter.next() {
                match tt {
                    proc_macro2::TokenTree::Punct(punct) if punct.as_char() == '$' => {
                        let ref_span = punct.span();
                        // We register this inner token span (e.g., '$') to the SpanManager
                        // so that we can construct a valid Location index. This allows the buildscript
                        // to query the exact byte range of the error for visual diagnostic reporting.
                        // We only need to push the span once, creating a range of length 1.
                        let idx = span_manager.add_span(ref_span);
                        let ref_loc = Location::Range(idx, idx + 1);

                        if let Some(proc_macro2::TokenTree::Ident(ident)) = iter.peek() {
                            let ident_name = ident.to_string();
                            if ident_name == "tokentype" {
                                iter.next();
                                let resolved = resolve_provider(
                                    "tokentype",
                                    providers,
                                    span_manager,
                                    stack,
                                    depth,
                                    max_depth,
                                    ref_loc,
                                )?;
                                result.extend(resolved);
                                continue;
                            } else if ident_name == "location" {
                                iter.next();
                                let resolved = resolve_provider(
                                    "location",
                                    providers,
                                    span_manager,
                                    stack,
                                    depth,
                                    max_depth,
                                    ref_loc,
                                )?;
                                result.extend(resolved);
                                continue;
                            } else if ident_name == "userdata" {
                                iter.next();
                                let resolved = resolve_provider(
                                    "userdata",
                                    providers,
                                    span_manager,
                                    stack,
                                    depth,
                                    max_depth,
                                    ref_loc,
                                )?;
                                result.extend(resolved);
                                continue;
                            } else if ident_name == "error" || ident_name == "errortype" {
                                iter.next();
                                let resolved = resolve_provider(
                                    "errortype",
                                    providers,
                                    span_manager,
                                    stack,
                                    depth,
                                    max_depth,
                                    ref_loc,
                                )?;
                                result.extend(resolved);
                                continue;
                            } else if ident_name == "moduleprefix" {
                                iter.next();
                                let resolved = resolve_provider(
                                    "moduleprefix",
                                    providers,
                                    span_manager,
                                    stack,
                                    depth,
                                    max_depth,
                                    ref_loc,
                                )?;
                                result.extend(resolved);
                                continue;
                            } else if providers.contains_key(&format!("term:{}", ident_name)) {
                                iter.next();
                                let resolved = resolve_provider(
                                    &format!("term:{}", ident_name),
                                    providers,
                                    span_manager,
                                    stack,
                                    depth,
                                    max_depth,
                                    ref_loc,
                                )?;
                                result.extend(resolved);
                                continue;
                            } else if providers.contains_key(&format!("nonterm:{}", ident_name)) {
                                // Direct non-terminal substitution ($NonTerminalName).
                                // Resolves the variable to the rule type of the specified non-terminal.
                                iter.next();
                                let resolved = resolve_provider(
                                    &format!("nonterm:{}", ident_name),
                                    providers,
                                    span_manager,
                                    stack,
                                    depth,
                                    max_depth,
                                    ref_loc,
                                )?;
                                result.extend(resolved);
                                continue;
                            } else {
                                // Fallback: If the variable following '$' is not recognized as any known
                                // configuration, terminal, or non-terminal, we do not throw a compile-time
                                // error. Instead, we skip resolving and leave the token stream unchanged.
                                // TODO: 이거 워닝으로 내면 좋을듯
                            }
                        }

                        result.extend(std::iter::once(proc_macro2::TokenTree::Punct(punct)));
                    }
                    proc_macro2::TokenTree::Group(group) => {
                        let substituted_stream = substitute_stream(
                            group.stream(),
                            providers,
                            span_manager,
                            stack,
                            depth,
                            max_depth,
                        )?;
                        let mut new_group =
                            proc_macro2::Group::new(group.delimiter(), substituted_stream);
                        new_group.set_span(group.span());
                        result.extend(std::iter::once(proc_macro2::TokenTree::Group(new_group)));
                    }
                    other => {
                        result.extend(std::iter::once(other));
                    }
                }
            }

            Ok(result)
        }

        // Initialize providers
        let mut providers: HashMap<String, ProviderInfo> = HashMap::default();

        let moduleprefix_loc = grammar_args
            .module_prefix
            .first()
            .map(|(l, _)| *l)
            .unwrap_or(Location::CallSite);
        let moduleprefix_stream = grammar_args
            .module_prefix
            .first()
            .map(|(_, s)| s.clone())
            .unwrap_or(quote! { ::rusty_lr });
        providers.insert(
            "moduleprefix".to_string(),
            ProviderInfo {
                name: "moduleprefix".to_string(),
                location: moduleprefix_loc,
                stream: Some(moduleprefix_stream),
                state: ResolveState::Unresolved,
            },
        );

        let userdata_loc = grammar_args
            .userdata_typename
            .first()
            .map(|(l, _)| *l)
            .unwrap_or(Location::CallSite);
        let userdata_stream = grammar_args
            .userdata_typename
            .first()
            .map(|(_, s)| s.clone())
            .unwrap_or(quote! { () });
        providers.insert(
            "userdata".to_string(),
            ProviderInfo {
                name: "userdata".to_string(),
                location: userdata_loc,
                stream: Some(userdata_stream),
                state: ResolveState::Unresolved,
            },
        );

        let errortype_loc = grammar_args
            .error_typename
            .first()
            .map(|(l, _)| *l)
            .unwrap_or(Location::CallSite);
        let errortype_stream = grammar_args
            .error_typename
            .first()
            .map(|(_, s)| s.clone())
            .unwrap_or(quote! { $moduleprefix::DefaultReduceActionError });
        providers.insert(
            "errortype".to_string(),
            ProviderInfo {
                name: "errortype".to_string(),
                location: errortype_loc,
                stream: Some(errortype_stream),
                state: ResolveState::Unresolved,
            },
        );

        if let Some((loc, stream)) = grammar_args.token_typename.first() {
            providers.insert(
                "tokentype".to_string(),
                ProviderInfo {
                    name: "tokentype".to_string(),
                    location: *loc,
                    stream: Some(stream.clone()),
                    state: ResolveState::Unresolved,
                },
            );
        }

        let location_loc = grammar_args
            .location_typename
            .first()
            .map(|(l, _)| *l)
            .unwrap_or(Location::CallSite);
        let location_stream = grammar_args
            .location_typename
            .first()
            .map(|(_, s)| s.clone())
            .unwrap_or(quote! { $moduleprefix::DefaultLocation });
        providers.insert(
            "location".to_string(),
            ProviderInfo {
                name: "location".to_string(),
                location: location_loc,
                stream: Some(location_stream),
                state: ResolveState::Unresolved,
            },
        );

        for (ident, stream) in &grammar_args.terminals {
            providers.insert(
                format!("term:{}", ident.value()),
                ProviderInfo {
                    name: format!("term:{}", ident.value()),
                    location: ident.location(),
                    stream: Some(stream.clone()),
                    state: ResolveState::Unresolved,
                },
            );
        }

        for rules_arg in &grammar_args.rules {
            let stream = if is_placeholder_type(&rules_arg.typename) {
                let placeholder_name =
                    format_ident!("__rustylr_placeholder_{}", rules_arg.name.value());
                Some(quote! { #placeholder_name })
            } else {
                rules_arg.typename.clone()
            };

            providers.insert(
                format!("nonterm:{}", rules_arg.name.value()),
                ProviderInfo {
                    name: format!("nonterm:{}", rules_arg.name.value()),
                    location: rules_arg.name.location(),
                    stream,
                    state: ResolveState::Unresolved,
                },
            );
        }

        // Resolve providers
        let provider_keys: Vec<String> = providers.keys().cloned().collect();
        let mut stack = Vec::new();
        for key in provider_keys {
            let loc = providers[&key].location;
            resolve_provider(
                &key,
                &mut providers,
                &mut grammar_args.span_manager,
                &mut stack,
                0,
                100,
                loc,
            )?;
        }

        // Write resolved streams back
        if let Some(entry) = providers.get("moduleprefix") {
            if let ResolveState::Resolved(resolved) = &entry.state {
                if let Some(first) = grammar_args.module_prefix.first_mut() {
                    first.1 = resolved.clone();
                } else {
                    grammar_args
                        .module_prefix
                        .push((Location::CallSite, resolved.clone()));
                }
            }
        }

        if let Some(entry) = providers.get("userdata") {
            if let ResolveState::Resolved(resolved) = &entry.state {
                if let Some(first) = grammar_args.userdata_typename.first_mut() {
                    first.1 = resolved.clone();
                } else {
                    grammar_args
                        .userdata_typename
                        .push((Location::CallSite, resolved.clone()));
                }
            }
        }

        if let Some(entry) = providers.get("errortype") {
            if let ResolveState::Resolved(resolved) = &entry.state {
                if let Some(first) = grammar_args.error_typename.first_mut() {
                    first.1 = resolved.clone();
                } else {
                    grammar_args
                        .error_typename
                        .push((Location::CallSite, resolved.clone()));
                }
            }
        }

        if let Some(entry) = providers.get("tokentype") {
            if let ResolveState::Resolved(resolved) = &entry.state {
                if let Some(first) = grammar_args.token_typename.first_mut() {
                    first.1 = resolved.clone();
                }
            }
        }

        if let Some(entry) = providers.get("location") {
            if let ResolveState::Resolved(resolved) = &entry.state {
                if let Some(first) = grammar_args.location_typename.first_mut() {
                    first.1 = resolved.clone();
                } else {
                    grammar_args
                        .location_typename
                        .push((Location::CallSite, resolved.clone()));
                }
            }
        }

        for (ident, stream) in &mut grammar_args.terminals {
            if let Some(entry) = providers.get(&format!("term:{}", ident.value())) {
                if let ResolveState::Resolved(resolved) = &entry.state {
                    *stream = resolved.clone();
                }
            }
        }

        for rules_arg in &mut grammar_args.rules {
            if rules_arg.typename.is_some() {
                if let Some(entry) = providers.get(&format!("nonterm:{}", rules_arg.name.value())) {
                    if let ResolveState::Resolved(resolved) = &entry.state {
                        if !is_placeholder_type(&rules_arg.typename) {
                            rules_arg.typename = Some(resolved.clone());
                        }
                    }
                }
            }
        }

        for rules_arg in &mut grammar_args.rules {
            for rule_line in &mut rules_arg.rule_lines {
                if let Some(action_stream) = &mut rule_line.reduce_action {
                    *action_stream = substitute_stream(
                        action_stream.clone(),
                        &mut providers,
                        &mut grammar_args.span_manager,
                        &mut stack,
                        0,
                        100,
                    )?;
                }
            }
        }

        let module_prefix = grammar_args.module_prefix.into_iter().next().unwrap().1;
        let error_typename = grammar_args.error_typename.into_iter().next().unwrap().1;
        let location_typename = grammar_args.location_typename.into_iter().next().unwrap().1;

        let (is_tokentype_boxed, token_typename) = {
            let stream = grammar_args.token_typename.into_iter().next().unwrap().1;
            let (boxed, stripped) = check_and_strip_box(stream);
            (boxed, stripped)
        };

        let mut grammar = Grammar {
            module_prefix,
            token_typename,
            is_tokentype_boxed,
            userdata_typename: grammar_args.userdata_typename.into_iter().next().unwrap().1,

            error_typename,
            start_rule_names: grammar_args.start_rule_name.clone(),

            terminals: Default::default(),
            terminals_index: Default::default(),
            precedence_types: Default::default(),
            precedence_levels: Default::default(),

            nonterminals: Default::default(),
            nonterminals_index: Default::default(),

            lalr: grammar_args.lalr,
            glr: grammar_args.glr,

            is_char: false,
            is_u8: false,
            optimize: !grammar_args.no_optim,

            builder: rusty_lr_core::builder::Grammar::new(),
            states: Vec::new(),

            terminal_class_id: Vec::new(),
            terminal_classes: Vec::new(),
            other_terminal_class_id: 0,
            other_used: false,
            error_used: false,

            other_terminal_index: 0,
            range_resolver: RangeResolver::new(),

            emit_dense: false,
            layout: grammar_args.layout,
            dense_limit: grammar_args.dense_limit,

            location_typename,
            error_precedence: None,
            custom_reduce_actions: Vec::new(),

            span_manager: grammar_args.span_manager,

            warnings: Vec::new(),
            infos: Vec::new(),
            allowed_diagnostics: HashMap::default(),
        };
        grammar.is_char = grammar.token_typename.to_string() == "char";
        grammar.is_u8 = grammar.token_typename.to_string() == "u8";

        // add char ranges to resolver
        // iterate over all rules, reduce_type or precedence definitions,
        // checks for what *character ranges* like [a-z] were used.
        // We need to support the whole unicode range (1 ~ 0x10FFFF),
        // but we can't assign every single character to a self.terminals.
        // Rather, we assign a range of characters to a single self.terminals
        if grammar.is_char || grammar.is_u8 {
            // resolve allowed diagnostics ranges first
            for (_, opt_target) in &grammar_args.allowed_diagnostics {
                if let Some(target) = opt_target {
                    target.range_resolve(&mut grammar)?;
                }
            }

            // add terminals from %prec definition in each rule
            for rules_arg in grammar_args.rules.iter() {
                for rule in rules_arg.rule_lines.iter() {
                    if let Some(prec_ident) = rule.precs().next() {
                        prec_ident.range_resolve(&mut grammar)?;
                    }
                }
            }

            // precedence orders
            for (_, _, orders) in grammar_args.precedences.iter() {
                for item in orders {
                    item.range_resolve(&mut grammar)?;
                }
            }

            // production rule definition
            for rules_arg in grammar_args.rules.iter() {
                for rule in &rules_arg.rule_lines {
                    if let Some(prec_ident) = rule.precs().next() {
                        prec_ident.range_resolve(&mut grammar)?;
                    }
                    for (_, pattern) in &rule.tokens {
                        pattern.range_resolve(&mut grammar)?;
                    }
                }
            }

            // add TerminalInfo based on ranges
            for range in grammar.range_resolver.iter() {
                let start = unsafe { char::from_u32_unchecked(range.0) };
                let last = unsafe { char::from_u32_unchecked(range.1) };
                let name = TerminalName::CharRange(start, last);
                let terminal_info = TerminalInfo {
                    name: name.clone(),
                    precedence: None,
                    body: quote! { range_term },
                };
                let index = grammar.terminals.len();
                grammar.terminals.push(terminal_info);
                grammar.terminals_index.insert(name, index);
            }
            if !grammar_args.terminals.is_empty() {
                return Err(ParseError::TokenInLiteralMode(
                    grammar_args
                        .terminals
                        .iter()
                        .map(|(name, _)| name.location())
                        .collect(),
                ));
            }
        } else {
            // add %token terminals
            for (index, (ident, token_expr)) in grammar_args.terminals.into_iter().enumerate() {
                let name = TerminalName::Ident(ident.value().clone());

                let terminal_info = TerminalInfo {
                    name: name.clone(),
                    precedence: None,
                    body: token_expr,
                };
                grammar.terminals.push(terminal_info);
                grammar.terminals_index.insert(name, index);
            }
        }

        // add other_terminals
        {
            let name = TerminalName::Ident(utils::OTHERS_TERMINAL_NAME.to_string());

            let terminal_info = TerminalInfo {
                name: name.clone(),
                precedence: None,
                body: TokenStream::new(),
            };
            let idx = grammar.terminals.len();
            grammar.other_terminal_index = idx;
            grammar.terminals.push(terminal_info);
            grammar.terminals_index.insert(name, idx);
        }

        // insert rule typenames first, since it will be used when inserting rule definitions below
        for (rule_idx, rules_arg) in grammar_args.rules.iter().enumerate() {
            let is_boxed = if let Some(rt) = &rules_arg.typename {
                let (boxed, _) = check_and_strip_box(rt.clone());
                boxed
            } else {
                false
            };

            let ruletype = if rules_arg.typename.is_none() {
                None
            } else if is_placeholder_type(&rules_arg.typename) {
                let placeholder_name =
                    format_ident!("__rustylr_placeholder_{}", rules_arg.name.value());
                Some(quote! { #placeholder_name })
            } else {
                let (_, stripped) = check_and_strip_box(rules_arg.typename.clone().unwrap());
                Some(stripped)
            };

            let nonterminal = NonTerminalInfo {
                name: rules_arg.name.clone(),
                pretty_name: rules_arg.name.value().clone(),
                ruletype,
                ruletype_boxed: is_boxed,
                rules: Vec::new(), // production rules will be added later
                root_location: None,
                protected: false,
                nonterm_type: None,
            };

            grammar.nonterminals.push(nonterminal);

            grammar
                .nonterminals_index
                .insert(rules_arg.name.value().clone(), rule_idx);
        }

        // precedence orders
        let mut reduce_definitions: HashMap<
            IdentOrLiteral,
            Located<rusty_lr_core::production::Associativity>,
        > = HashMap::default();

        for (level, (location, reduce_type, items)) in
            grammar_args.precedences.into_iter().enumerate()
        {
            grammar
                .precedence_types
                .push(Located::new(reduce_type, location)); // set i'th level's precedence type
            for item in items {
                let item_location = item.location();
                let item_prec = Located::new(level, item_location);
                match &item {
                    IdentOrLiteral::Ident(ident) => {
                        if let Some(&term_idx) = grammar
                            .terminals_index
                            .get(&TerminalName::Ident(ident.value().clone()))
                        // TODO do not clone ident.value() here, use &str instead
                        {
                            grammar.terminals[term_idx].precedence = Some(item_prec);
                        } else if ident.value().as_str() == utils::ERROR_NAME {
                            grammar.error_precedence = Some(level);
                        }
                    }
                    IdentOrLiteral::Byte(b) => {
                        let ch = *b.value();
                        if let Some(&term_idx) = grammar.terminals_index.get(&(ch, ch).into()) {
                            grammar.terminals[term_idx].precedence = Some(item_prec);
                        } else {
                            unreachable!("unexpected char type in precedence order");
                        }
                    }
                    IdentOrLiteral::Char(ch) => {
                        let ch = *ch.value();
                        if let Some(&term_idx) = grammar.terminals_index.get(&(ch, ch).into()) {
                            grammar.terminals[term_idx].precedence = Some(item_prec);
                        } else {
                            unreachable!("unexpected char type in precedence order");
                        }
                    }
                }
                if let Some(rt) = reduce_type {
                    if let Some(old) = reduce_definitions.get(&item) {
                        if *old.value() != rt {
                            return Err(ParseError::MultipleReduceDefinition(vec![
                                *old,
                                Located::new(rt, location),
                            ]));
                        }
                    } else {
                        reduce_definitions.insert(item.clone(), Located::new(rt, location));
                    }
                }
                if let Some(old) = grammar.precedence_levels.insert(item, item_prec) {
                    return Err(ParseError::MultiplePrecedenceOrderDefinition(vec![
                        item_location,
                        old.location(),
                    ]));
                }
            }
        }

        // pattern map for auto-generated rules
        let mut pattern_map: HashMap<Pattern, PatternToToken> = HashMap::default();

        // insert production rules & auto-generated rules from regex pattern
        for (rule_idx, rules) in grammar_args.rules.into_iter().enumerate() {
            let mut rule_lines = Vec::new();
            for mut rule in rules.rule_lines.into_iter() {
                let mut tokens = Vec::with_capacity(rule.tokens.len());
                let mut patterns = Vec::with_capacity(rule.tokens.len());
                for (mapto, pattern) in std::mem::take(&mut rule.tokens) {
                    let location = pattern.location();
                    let pattern = pattern.into_pattern(&mut grammar, false)?;
                    let pattern_rule =
                        pattern.to_token(&mut grammar, &mut pattern_map, location.clone())?;

                    tokens.push(MappedSymbol {
                        symbol: pattern_rule.symbol,
                        mapto: mapto.or_else(|| pattern_rule.mapto.clone()),
                        location,
                        reduce_action_chains: Vec::new(),
                    });
                    patterns.push(pattern_rule);
                }

                // parse %prec definition
                let prec = match rule.precs().next() {
                    Some(prec) => {
                        // check if this ident exists in tokens
                        let from_token = match prec {
                            IdentOrLiteral::Ident(ident) => {
                                let mut prec = None;
                                for (idx, token) in tokens.iter().enumerate() {
                                    if token.mapto.as_ref().map(|m| m) == Some(ident) {
                                        prec = Some(idx);
                                        break;
                                    }
                                }
                                prec
                            }
                            _ => None,
                        };
                        if let Some(from_token) = from_token {
                            let from_token_location = tokens[from_token].location;
                            // check if from_token'th token is terminal symbol
                            if let Symbol::Terminal(term) = tokens[from_token].symbol {
                                match term {
                                    TerminalSymbol::Terminal(term_idx) => {
                                        if let Some(level) = grammar.terminals[term_idx].precedence
                                        {
                                            Some(Located::new(
                                                Precedence::Fixed(level.into_value()),
                                                from_token_location,
                                            ))
                                        } else {
                                            return Err(ParseError::PrecedenceNotDefined(
                                                prec.clone(),
                                            ));
                                        }
                                    }
                                    TerminalSymbol::Error => {
                                        if let Some(error_prec) = grammar.error_precedence {
                                            Some(Located::new(
                                                Precedence::Fixed(error_prec),
                                                from_token_location,
                                            ))
                                        } else {
                                            return Err(ParseError::PrecedenceNotDefined(
                                                prec.clone(),
                                            ));
                                        }
                                    }
                                    TerminalSymbol::Eof | TerminalSymbol::VirtualStart(_) => {
                                        unreachable!(
                                            "eof/virtual start token cannot be used in %prec, nor cannot be used in production rules"
                                        )
                                    }
                                }
                            } else {
                                return Err(ParseError::PrecedenceNotDefined(prec.clone()));
                            }
                        } else if let Some(&level) = grammar.precedence_levels.get(&prec) {
                            Some(level.map(Precedence::Fixed))
                        } else {
                            return Err(ParseError::PrecedenceNotDefined(prec.clone()));
                        }
                    }
                    _ => {
                        // not defined,
                        // choose the last terminal symbol that has precedence
                        let mut op = None;
                        for token in tokens.iter().rev() {
                            if let Symbol::Terminal(term) = token.symbol {
                                match term {
                                    TerminalSymbol::Terminal(term_idx) => {
                                        if let Some(level) = grammar.terminals[term_idx].precedence
                                        {
                                            op = Some(Located::new(
                                                Precedence::Fixed(level.into_value()),
                                                token.location,
                                            ));
                                            break;
                                        }
                                    }
                                    TerminalSymbol::Error => {
                                        if let Some(error_prec) = grammar.error_precedence {
                                            op = Some(Located::new(
                                                Precedence::Fixed(error_prec),
                                                token.location,
                                            ));
                                            break;
                                        }
                                    }
                                    TerminalSymbol::Eof | TerminalSymbol::VirtualStart(_) => {
                                        unreachable!(
                                            "eof/virtual start token cannot be used in %prec, nor cannot be used in production rules, this case must be filtered out in parsing stage"
                                        )
                                    }
                                }
                            }
                        }
                        op
                    }
                };

                // parse %dprec literal value
                let dprec = match rule.dprecs().next() {
                    Some(dprec) => {
                        let val = match dprec.base10_parse::<usize>() {
                            Ok(val) => val,
                            Err(_) => {
                                return Err(ParseError::OnlyUsizeLiteral(dprec.location()));
                            }
                        };
                        Some(Located::new(val, dprec.location()))
                    }
                    _ => None,
                };

                // rename all '@var_name' to '__rustylr_location_{var_name}'
                // if reduce_action is not defined, check if it can be auto-generated
                let reduce_action = if let Some(reduce_action) = rule.reduce_action {
                    fn rename_tokenstream_recursive(
                        ts: TokenStream,
                        num_tokens: usize,
                        span_manager: &mut SpanManager,
                    ) -> Result<TokenStream, ParseError> {
                        let mut new_ts = TokenStream::new();
                        let mut it = ts.into_iter().peekable();
                        while let Some(token) = it.next() {
                            match token {
                                proc_macro2::TokenTree::Punct(punct) => {
                                    if punct.as_char() == '@' {
                                        // found '@', check next token
                                        match it.peek() {
                                            Some(proc_macro2::TokenTree::Ident(ident)) => {
                                                // rename to '__rustylr_location_{varname}'
                                                let new_ident =
                                                    format_ident!("__rustylr_location_{}", ident);
                                                new_ts.extend([proc_macro2::TokenTree::Ident(
                                                    new_ident,
                                                )]);
                                                it.next(); // consume the ident
                                            }
                                            Some(proc_macro2::TokenTree::Punct(next_punct)) => {
                                                if next_punct.as_char() == '$' {
                                                    // found '@$', rename to '__rustylr_location0'
                                                    new_ts.extend([proc_macro2::TokenTree::Ident(
                                                        format_ident!("__rustylr_location0"),
                                                    )]);
                                                    it.next(); // consume the next punct
                                                } else {
                                                    // just a punct
                                                    new_ts.extend([proc_macro2::TokenTree::Punct(
                                                        punct,
                                                    )]);
                                                }
                                            }
                                            Some(proc_macro2::TokenTree::Literal(lit)) => {
                                                let lit_str = lit.to_string();
                                                if let Ok(n) = lit_str.parse::<usize>() {
                                                    if n == 0 {
                                                        // '@0' -> rename to '__rustylr_location0' (self location)
                                                        new_ts.extend([
                                                            proc_macro2::TokenTree::Ident(
                                                                format_ident!(
                                                                    "__rustylr_location0"
                                                                ),
                                                            ),
                                                        ]);
                                                        it.next(); // consume the literal
                                                    } else {
                                                        // '@n' where n > 0
                                                        if n <= num_tokens {
                                                            let new_ident = format_ident!(
                                                                "__rustylr_location_{}",
                                                                n - 1
                                                            );
                                                            new_ts.extend([
                                                                proc_macro2::TokenTree::Ident(
                                                                    new_ident,
                                                                ),
                                                            ]);
                                                            it.next(); // consume the literal
                                                        } else {
                                                            let span_idx =
                                                                span_manager.add_span(punct.span());
                                                            span_manager.add_span(lit.span());
                                                            let loc = Location::Range(
                                                                span_idx,
                                                                span_idx + 2,
                                                            );
                                                            return Err(ParseError::BisonVariableOutOfRange {
                                                                location: loc,
                                                                name: format!("@{}", n),
                                                                max: num_tokens,
                                                            });
                                                        }
                                                    }
                                                } else {
                                                    new_ts.extend([proc_macro2::TokenTree::Punct(
                                                        punct,
                                                    )]);
                                                }
                                            }
                                            _ => {
                                                // just a punct, no ident after it
                                                new_ts
                                                    .extend([proc_macro2::TokenTree::Punct(punct)]);
                                            }
                                        }
                                    } else if punct.as_char() == '$' {
                                        // found '$', check next token
                                        match it.peek() {
                                            Some(proc_macro2::TokenTree::Literal(lit)) => {
                                                let lit_str = lit.to_string();
                                                if let Ok(n) = lit_str.parse::<usize>() {
                                                    if n == 0 {
                                                        let span_idx =
                                                            span_manager.add_span(punct.span());
                                                        span_manager.add_span(lit.span());
                                                        let loc =
                                                            Location::Range(span_idx, span_idx + 2);
                                                        return Err(ParseError::BisonVariableZero(
                                                            loc,
                                                        ));
                                                    } else {
                                                        if n <= num_tokens {
                                                            let new_ident = format_ident!(
                                                                "__rustylr_data_{}",
                                                                n - 1
                                                            );
                                                            new_ts.extend([
                                                                proc_macro2::TokenTree::Ident(
                                                                    new_ident,
                                                                ),
                                                            ]);
                                                            it.next(); // consume the literal
                                                        } else {
                                                            let span_idx =
                                                                span_manager.add_span(punct.span());
                                                            span_manager.add_span(lit.span());
                                                            let loc = Location::Range(
                                                                span_idx,
                                                                span_idx + 2,
                                                            );
                                                            return Err(ParseError::BisonVariableOutOfRange {
                                                                location: loc,
                                                                name: format!("${}", n),
                                                                max: num_tokens,
                                                            });
                                                        }
                                                    }
                                                } else {
                                                    new_ts.extend([proc_macro2::TokenTree::Punct(
                                                        punct,
                                                    )]);
                                                }
                                            }
                                            _ => {
                                                new_ts
                                                    .extend([proc_macro2::TokenTree::Punct(punct)]);
                                            }
                                        }
                                    } else {
                                        new_ts.extend([proc_macro2::TokenTree::Punct(punct)]);
                                    }
                                }
                                proc_macro2::TokenTree::Group(group) => {
                                    let new_group = proc_macro2::Group::new(
                                        group.delimiter(),
                                        rename_tokenstream_recursive(
                                            group.stream(),
                                            num_tokens,
                                            span_manager,
                                        )?,
                                    );
                                    let new_group = proc_macro2::TokenTree::Group(new_group);
                                    new_ts.extend([new_group]);
                                }
                                token => {
                                    new_ts.extend([token]);
                                }
                            }
                        }
                        Ok(new_ts)
                    }

                    let new_reduce_action = rename_tokenstream_recursive(
                        reduce_action,
                        tokens.len(),
                        &mut grammar.span_manager,
                    )?;

                    // check if this reduce action can be identity action; i.e., { $1 }
                    fn tokenstream_contains_unique_ident(ts: TokenStream) -> Option<Ident> {
                        let mut it = ts.into_iter();
                        match it.next() {
                            Some(proc_macro2::TokenTree::Ident(ident)) => {
                                if it.next().is_none() {
                                    Some(ident)
                                } else {
                                    None
                                }
                            }
                            Some(proc_macro2::TokenTree::Group(group)) => {
                                if group.delimiter() != proc_macro2::Delimiter::Brace {
                                    return None;
                                }
                                if it.next().is_some() {
                                    return None;
                                }
                                tokenstream_contains_unique_ident(group.stream())
                            }
                            _ => None,
                        }
                    }
                    if let Some(unique_ident) =
                        tokenstream_contains_unique_ident(new_reduce_action.clone())
                    {
                        let unique_idx_opt = if let Some(idx) = {
                            let s = unique_ident.to_string();
                            if s.starts_with("__rustylr_data_") {
                                s["__rustylr_data_".len()..].parse::<usize>().ok()
                            } else {
                                None
                            }
                        } {
                            if idx < tokens.len() { Some(idx) } else { None }
                        } else {
                            tokens
                                .iter()
                                .enumerate()
                                .rev()
                                .find(move |(_, token)| {
                                    token.mapto.as_ref().map(|m| m.value().as_str())
                                        == Some(unique_ident.to_string().as_str())
                                })
                                .map(|(idx, _)| idx)
                        };

                        if let Some(unique_idx) = unique_idx_opt {
                            Some(ReduceAction::Identity(unique_idx))
                        } else {
                            Some(ReduceAction::Custom(CustomReduceAction::new(
                                new_reduce_action,
                            )))
                        }
                    } else {
                        Some(ReduceAction::Custom(CustomReduceAction::new(
                            new_reduce_action,
                        )))
                    }
                } else {
                    // reduce action is not defined,

                    // if ruletype is defined, reduce action must be defined too
                    if rules.typename.is_some() {
                        // check for special case:
                        // only one token in this rule have <RuleType> defined (include terminal)
                        // for example,
                        // rule: A B C D
                        // and only B has <RuleType> defined,
                        // auto-generated reduce action { B } will be used.
                        let mut unique_token_idx = None;
                        for (idx, pattern) in patterns.iter().enumerate() {
                            if pattern.ruletype.is_some() {
                                if unique_token_idx.is_some() {
                                    unique_token_idx = None;
                                    break;
                                } else {
                                    unique_token_idx = Some(idx);
                                }
                            }
                        }
                        if let Some(unique_mapto_idx) = unique_token_idx {
                            Some(ReduceAction::Identity(unique_mapto_idx))
                        } else {
                            let loc = if tokens.is_empty() {
                                rule.separator_location.clone()
                            } else {
                                let first = rule.separator_location.clone();
                                let last = &tokens.last().unwrap().location;
                                first.merge(last)
                            };

                            return Err(ParseError::RuleTypeDefinedButActionNotDefined {
                                nonterm: rules.name.location(),
                                rule: loc,
                            });
                        }
                    } else {
                        None
                    }
                };

                if let Some(ReduceAction::Identity(idx)) = &reduce_action {
                    if tokens[*idx].mapto.is_none() {
                        tokens[*idx].mapto = Some(Located::new(
                            format!("__rustylr_token{}", idx),
                            Location::CallSite,
                        ));
                    }
                }

                // check for duplicated token.mapto
                // if there are variable with same name, the last one will be used (by shadowing)
                // so set the front one to `None`
                for right_token in 0..tokens.len() {
                    if let Some(right_mapto) = tokens[right_token].mapto.as_ref().cloned() {
                        for left_token in 0..right_token {
                            if tokens[left_token].mapto.as_ref().map(|m| m.value())
                                == Some(right_mapto.value())
                            {
                                tokens[left_token].mapto = Some(Located::new(
                                    format!("__rustylr_token{}", left_token),
                                    Location::CallSite,
                                ));
                            }
                        }
                    }
                }

                rule_lines.push(Rule {
                    tokens,
                    reduce_action,
                    separator_location: rule.separator_location,
                    prec,
                    dprec,
                    is_used: true,
                });
            }

            // production rules set here
            grammar.nonterminals[rule_idx].rules = rule_lines;
        }
        drop(pattern_map);

        // Resolve placeholders ('_') using type inference
        {
            let mut resolved: HashMap<String, Option<TokenStream>> = HashMap::default();
            let mut unresolved_placeholders: HashSet<String> = HashSet::default();

            for nonterm in &grammar.nonterminals {
                if let Some(name) = get_placeholder_name(&nonterm.ruletype) {
                    unresolved_placeholders.insert(name);
                }
            }

            let mut changed = true;
            while changed {
                changed = false;
                let mut resolved_this_round = Vec::new();

                for p_name in &unresolved_placeholders {
                    // Find the nonterminal index corresponding to p_name
                    let mut nonterm_idx = None;
                    for (idx, nonterm) in grammar.nonterminals.iter().enumerate() {
                        if let Some(name) = &nonterm.ruletype {
                            if name.to_string() == *p_name {
                                nonterm_idx = Some(idx);
                                break;
                            }
                        }
                    }

                    if let Some(i) = nonterm_idx {
                        let mut inferred_type: Option<Option<TokenStream>> = None;

                        for rule in &grammar.nonterminals[i].rules {
                            if let Some(ReduceAction::Identity(idx)) = &rule.reduce_action {
                                if *idx < rule.tokens.len() {
                                    let token = rule.tokens[*idx].symbol;
                                    match token {
                                        Symbol::Terminal(_) => {
                                            let ty = Some(grammar.token_typename.clone());
                                            inferred_type = Some(ty);
                                            break;
                                        }
                                        Symbol::NonTerminal(to_nonterm_idx) => {
                                            let target_type =
                                                &grammar.nonterminals[to_nonterm_idx].ruletype;
                                            // Substitute any already resolved placeholders
                                            let substituted = match target_type {
                                                Some(ts) => {
                                                    substitute_placeholders(ts.clone(), &resolved)
                                                }
                                                None => None,
                                            };
                                            // Check if the substituted type contains any placeholders
                                            if get_placeholder_name(&substituted).is_none() {
                                                inferred_type = Some(substituted);
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        if let Some(ty) = inferred_type {
                            resolved_this_round.push((p_name.clone(), ty));
                        }
                    }
                }

                for (p_name, ty) in resolved_this_round {
                    resolved.insert(p_name.clone(), ty);
                    unresolved_placeholders.remove(&p_name);
                    changed = true;
                }
            }

            if !unresolved_placeholders.is_empty() {
                let first_p_name = unresolved_placeholders.iter().next().unwrap();
                let mut loc = Location::CallSite;
                for nonterm in &grammar.nonterminals {
                    if let Some(name) = &nonterm.ruletype {
                        if name.to_string() == *first_p_name {
                            loc = nonterm.name.location();
                            break;
                        }
                    }
                }
                return Err(ParseError::TypeInferenceFailed(loc));
            }

            // Substitute resolved types into all nonterminal ruletypes (including helpers)
            for nonterm in &mut grammar.nonterminals {
                nonterm.ruletype = match &nonterm.ruletype {
                    Some(ts) => substitute_placeholders(ts.clone(), &resolved),
                    None => None,
                };
            }
        }

        // check start rules are valid
        for start_rule_name in &grammar.start_rule_names {
            let name = start_rule_name.value();
            if !grammar.nonterminals_index.contains_key(name) {
                return Err(ParseError::StartNonTerminalNotDefined(
                    start_rule_name.location(),
                ));
            }
        }

        // insert augmented rule
        {
            let augmented_name =
                Located::new(utils::AUGMENTED_NAME.to_string(), Location::CallSite);
            let mut augmented_rules = Vec::new();

            for (i, start_rule_name) in grammar.start_rule_names.iter().enumerate() {
                let start_idx = grammar
                    .nonterminals_index
                    .get(start_rule_name.value())
                    .unwrap();
                let rule = Rule {
                    tokens: vec![
                        MappedSymbol {
                            symbol: Symbol::Terminal(TerminalSymbol::VirtualStart(i as u32)),
                            mapto: None,
                            location: Location::CallSite,
                            reduce_action_chains: Vec::new(),
                        },
                        MappedSymbol {
                            symbol: Symbol::NonTerminal(*start_idx),
                            mapto: None,
                            location: Location::CallSite,
                            reduce_action_chains: Vec::new(),
                        },
                        MappedSymbol {
                            symbol: Symbol::Terminal(TerminalSymbol::Eof),
                            mapto: None,
                            location: Location::CallSite,
                            reduce_action_chains: Vec::new(),
                        },
                    ],
                    reduce_action: None,
                    separator_location: Location::CallSite,
                    prec: None,
                    dprec: None,
                    is_used: true,
                };
                augmented_rules.push(rule);
                grammar.nonterminals[*start_idx].protected = true;
            }

            let nonterminal_info = NonTerminalInfo {
                name: augmented_name.clone(),
                pretty_name: utils::AUGMENTED_NAME.to_string(),
                ruletype: None,
                ruletype_boxed: false,
                root_location: None,
                rules: augmented_rules,
                protected: true,
                nonterm_type: Some(rusty_lr_core::parser::nonterminal::NonTerminalType::Augmented),
            };

            let augmented_idx = grammar.nonterminals.len();
            grammar.nonterminals.push(nonterminal_info);
            grammar
                .nonterminals_index
                .insert(augmented_name.into_value(), augmented_idx);
        }

        // initialize terminal classes with one-terminal-one-class
        grammar.terminal_class_id.reserve(grammar.terminals.len());
        grammar.terminal_classes.reserve(grammar.terminals.len());
        let mut multiterm_counter = 0;
        for i in 0..grammar.terminals.len() {
            let len = grammar.terminals[i].name.count();
            if len > 1 {
                multiterm_counter += 1;
            }
            grammar.terminal_class_id.push(i);

            let mut ranges = Vec::new();
            if grammar.is_char || grammar.is_u8 {
                // check if this terminal is a range
                if let TerminalName::CharRange(start, last) = &grammar.terminals[i].name {
                    ranges.push((*start as u32, *last as u32));
                }
            }
            grammar.terminal_classes.push(TerminalClassDefinition {
                terminals: vec![i],
                multiterm_counter,
                ranges,
                data_used: true,
            });
        }
        grammar.other_terminal_class_id = grammar.terminal_class_id[grammar.other_terminal_index];

        // check other, error terminals used
        for nonterm in &grammar.nonterminals {
            for rule in &nonterm.rules {
                for token in &rule.tokens {
                    if token.symbol
                        == Symbol::Terminal(TerminalSymbol::Terminal(grammar.other_terminal_index))
                    {
                        grammar.other_used = true;
                    }
                    if token.symbol == Symbol::Terminal(TerminalSymbol::Error) {
                        grammar.error_used = true;
                    }
                }
            }
        }
        let mut allowed_diagnostics: HashMap<String, Vec<Option<ResolvedAllowTarget>>> =
            HashMap::default();
        let valid_diagnostics: HashSet<&str> = [
            "nonterm_unreachable",
            "unused_nonterm_data",
            "nonterm_unproductive",
            "unused_terminals",
            "terminals_merged",
            "redundant_rule_removed",
            "unit_production_eliminated",
            "glr_optional_expanded",
            "reduce_reduce_conflict_resolved",
            "shift_reduce_conflict_resolved",
            "shift_reduce_conflict_glr",
            "reduce_reduce_conflict_glr",
        ]
        .iter()
        .copied()
        .collect();

        for (allowed_loc, target) in grammar_args.allowed_diagnostics {
            let name = allowed_loc.value();
            if !valid_diagnostics.contains(name.as_str()) {
                return Err(ParseError::InvalidAllowDiagnostic {
                    location: allowed_loc.location(),
                    name: name.clone(),
                });
            }
            let resolved_target = match target {
                None => None,
                Some(t) => Some(t.resolve(&mut grammar)?),
            };
            allowed_diagnostics
                .entry(name.clone())
                .or_default()
                .push(resolved_target);
        }

        grammar.allowed_diagnostics = allowed_diagnostics;

        Ok(grammar)
    }
}

/// Checks if the first token in the token stream is the identifier `box`.
/// If it is, returns `(true, stripped_stream)`. Otherwise, returns `(false, original_stream)`.
pub(crate) fn check_and_strip_box(stream: TokenStream) -> (bool, TokenStream) {
    let mut iter = stream.clone().into_iter();
    if let Some(proc_macro2::TokenTree::Ident(ident)) = iter.next() {
        if ident.to_string() == "box" {
            let rest: TokenStream = iter.collect();
            return (true, rest);
        }
    }
    (false, stream)
}

fn is_placeholder_type(ruletype: &Option<TokenStream>) -> bool {
    if let Some(ts) = ruletype {
        let (_, stripped) = check_and_strip_box(ts.clone());
        let mut it = stripped.into_iter();
        if let Some(proc_macro2::TokenTree::Ident(ident)) = it.next() {
            if ident.to_string() == "_" && it.next().is_none() {
                return true;
            }
        }
    }
    false
}

fn get_placeholder_name(ruletype: &Option<TokenStream>) -> Option<String> {
    if let Some(ts) = ruletype {
        for token in ts.clone() {
            match token {
                proc_macro2::TokenTree::Ident(ident) => {
                    let s = ident.to_string();
                    if s.starts_with("__rustylr_placeholder_") {
                        return Some(s);
                    }
                }
                proc_macro2::TokenTree::Group(group) => {
                    if let Some(name) = get_placeholder_name(&Some(group.stream())) {
                        return Some(name);
                    }
                }
                _ => {}
            }
        }
    }
    None
}

fn substitute_placeholders(
    ts: TokenStream,
    resolved: &HashMap<String, Option<TokenStream>>,
) -> Option<TokenStream> {
    let mut new_ts = TokenStream::new();
    for token in ts {
        match token {
            proc_macro2::TokenTree::Ident(ident) => {
                let s = ident.to_string();
                if s.starts_with("__rustylr_placeholder_") {
                    if let Some(replacement_opt) = resolved.get(&s) {
                        if let Some(replacement) = replacement_opt {
                            new_ts.extend(replacement.clone());
                        }
                    } else {
                        new_ts.extend([proc_macro2::TokenTree::Ident(ident)]);
                    }
                } else {
                    new_ts.extend([proc_macro2::TokenTree::Ident(ident)]);
                }
            }
            proc_macro2::TokenTree::Group(group) => {
                if let Some(sub) = substitute_placeholders(group.stream(), resolved) {
                    let new_group = proc_macro2::Group::new(group.delimiter(), sub);
                    new_ts.extend([proc_macro2::TokenTree::Group(new_group)]);
                }
            }
            other => {
                new_ts.extend([other]);
            }
        }
    }
    if new_ts.is_empty() {
        None
    } else {
        Some(new_ts)
    }
}
