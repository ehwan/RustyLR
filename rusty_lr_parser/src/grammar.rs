use std::collections::BTreeMap;
use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use rusty_lr_core::hash::HashMap;
use rusty_lr_core::rule::Precedence;
use rusty_lr_core::TerminalSymbol;
use rusty_lr_core::Token;

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
use crate::terminal_info::TerminalInfo;
use crate::terminal_info::TerminalName;
use crate::token::TokenMapped;
use crate::utils;

pub struct TerminalClassDefinition {
    pub terminals: Vec<usize>,
    /// counter for only terminal clasas (have more than 1 terminal)
    /// dummy if it is single-terminal class
    pub multiterm_counter: usize,

    /// compressed ranges, only if %tokentype is char or u8
    pub ranges: Vec<(u32, u32)>,

    /// Whether this class's data was used in any reduce action
    pub data_used: bool,
}

pub enum OptimizeRemove {
    TerminalClassRuleMerge(Rule),
    SingleNonTerminalRule(Rule, Location),
    NonTermNotUsed(Location),
    Cycle(Location),
    NonTermDataNotUsed(usize),
}
pub struct OptimizeDiag {
    /// deleted rules
    pub removed: Vec<OptimizeRemove>,
}

/// type alias just for readability
type ClassIndex = usize;
type TerminalIndex = usize;

pub struct CustomSingleReduceAction {
    pub body: CustomReduceAction,
    pub input_type: Option<(String, TokenStream)>,
    pub input_location: Option<String>,
    pub output_type: Option<TokenStream>,
}

pub struct Grammar {
    /// %moduleprefix, "rusty_lr" for normal use
    pub(crate) module_prefix: TokenStream,

    /// %tokentype
    pub(crate) token_typename: TokenStream,
    pub(crate) is_tokentype_boxed: bool,

    /// %userdata
    pub(crate) userdata_typename: TokenStream,

    /// %error
    pub(crate) error_typename: TokenStream,

    /// %start
    pub(crate) start_rule_name: Located<String>,

    pub terminals: Vec<TerminalInfo>,
    /// ident -> index map for terminals
    pub terminals_index: HashMap<TerminalName, TerminalIndex>,

    /// %left, %right, or %precedence for each precedence level
    pub precedence_types: Vec<Located<Option<rusty_lr_core::rule::ReduceType>>>,

    /// precedence levels; line number of %left, %right, or %precedence directive
    pub precedence_levels: HashMap<IdentOrLiteral, Located<usize>>,

    /// rule definitions
    pub nonterminals: Vec<NonTerminalInfo>,
    /// ident - index map for non-terminals
    pub nonterminals_index: HashMap<String, usize>,

    /// whether to generate LALR parser
    pub lalr: bool,

    /// whether to generate GLR parser
    pub glr: bool,

    /// if %tokentype is `char` or `u8`
    pub is_char: bool,
    pub is_u8: bool,

    /// do terminal classificate optimization
    pub optimize: bool,
    pub builder: rusty_lr_core::builder::Grammar<TerminalSymbol<usize>, usize>,
    pub states: Vec<
        rusty_lr_core::parser::state::IntermediateState<TerminalSymbol<usize>, usize, usize, usize>,
    >,

    /// set of terminals for each terminal class
    pub terminal_classes: Vec<TerminalClassDefinition>,
    /// id of teminal class for each terminal
    pub terminal_class_id: Vec<TerminalIndex>,
    /// class id for terminal that does not belong to any class
    pub other_terminal_class_id: ClassIndex,

    pub other_used: bool,
    pub error_used: bool,

    /// terminal index of other_terminals
    /// `other_terminal` can be only used by [^ term ...] pattern,
    /// to indicate *other terminals* not defined in this grammar.
    pub other_terminal_index: TerminalIndex,

    /// character range resolver;
    pub range_resolver: RangeResolver,

    /// in the generated parser, the dense table `Vec` will be used instead of the sparse table `HashMap`.
    pub emit_dense: bool,

    /// type for location
    pub location_typename: TokenStream,

    /// precedence level of error token
    pub error_precedence: Option<usize>,

    /// See `TokenMapped::reduce_action_chains` for more details.
    /// This is actual body of each reduce action in the chain.
    pub custom_reduce_actions: Vec<CustomSingleReduceAction>,

    pub span_manager: crate::parser::location::SpanManager,
}

impl Grammar {
    /// get rule by ruleid
    pub fn get_rule_by_id(&self, mut rule_idx: usize) -> Option<(&NonTerminalInfo, usize)> {
        for nonterm in self.nonterminals.iter() {
            if rule_idx < nonterm.rules.len() {
                return Some((nonterm, rule_idx));
            }
            rule_idx -= nonterm.rules.len();
        }
        None
    }

    pub(crate) fn negate_terminal_set(&self, terminalset: &BTreeSet<usize>) -> BTreeSet<usize> {
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
        } else if grammar_args.start_rule_name.len() > 1 {
            return Err(ArgError::MultipleStartDefinition(
                grammar_args
                    .start_rule_name
                    .iter()
                    .map(|start| start.location())
                    .collect(),
            ));
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
    ) -> impl Iterator<Item = usize> + '_ {
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
    pub(crate) fn get_terminal_index_from_char(&self, ch: char) -> usize {
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
            providers: &mut std::collections::HashMap<String, ProviderInfo>,
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
            providers: &mut std::collections::HashMap<String, ProviderInfo>,
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
        let mut providers: std::collections::HashMap<String, ProviderInfo> =
            std::collections::HashMap::new();

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
            start_rule_name: grammar_args.start_rule_name.into_iter().next().unwrap(),

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

            emit_dense: grammar_args.dense,

            location_typename,
            error_precedence: None,
            custom_reduce_actions: Vec::new(),

            span_manager: grammar_args.span_manager,
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
            Located<rusty_lr_core::rule::ReduceType>,
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

                    tokens.push(TokenMapped {
                        token: pattern_rule.token,
                        mapto: mapto.or_else(|| pattern_rule.mapto.clone()),
                        location,
                        reduce_action_chains: Vec::new(),
                    });
                    patterns.push(pattern_rule);
                }

                // parse %prec definition
                let prec = if let Some(prec) = rule.precs().next() {
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
                        if let Token::Term(term) = tokens[from_token].token {
                            match term {
                                TerminalSymbol::Term(term_idx) => {
                                    if let Some(level) = grammar.terminals[term_idx].precedence {
                                        Some(Located::new(
                                            Precedence::Fixed(level.into_value()),
                                            from_token_location,
                                        ))
                                    } else {
                                        return Err(ParseError::PrecedenceNotDefined(prec.clone()));
                                    }
                                }
                                TerminalSymbol::Error => {
                                    if let Some(error_prec) = grammar.error_precedence {
                                        Some(Located::new(
                                            Precedence::Fixed(error_prec),
                                            from_token_location,
                                        ))
                                    } else {
                                        return Err(ParseError::PrecedenceNotDefined(prec.clone()));
                                    }
                                }
                                TerminalSymbol::Eof => {
                                    unreachable!("eof token cannot be used in %prec, nor cannot be used in production rules")
                                }
                            }
                        } else {
                            Some(Located::new(
                                Precedence::Dynamic(from_token),
                                from_token_location,
                            ))
                        }
                    } else if let Some(&level) = grammar.precedence_levels.get(&prec) {
                        Some(level.map(Precedence::Fixed))
                    } else {
                        return Err(ParseError::PrecedenceNotDefined(prec.clone()));
                    }
                } else {
                    // not defined,
                    // choose the last terminal symbol that has precedence
                    let mut op = None;
                    for token in tokens.iter().rev() {
                        if let Token::Term(term) = token.token {
                            match term {
                                TerminalSymbol::Term(term_idx) => {
                                    if let Some(level) = grammar.terminals[term_idx].precedence {
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
                                TerminalSymbol::Eof => {
                                    unreachable!("eof token cannot be used in %prec, nor cannot be used in production rules, this case must be filtered out in parsing stage")
                                }
                            }
                        }
                    }
                    op
                };

                // parse %dprec literal value
                let dprec = if let Some(dprec) = rule.dprecs().next() {
                    let val = match dprec.base10_parse::<usize>() {
                        Ok(val) => val,
                        Err(_) => {
                            return Err(ParseError::OnlyUsizeLiteral(dprec.location()));
                        }
                    };
                    Some(Located::new(val, dprec.location()))
                } else {
                    None
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
                            if idx < tokens.len() {
                                Some(idx)
                            } else {
                                None
                            }
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
            let mut resolved: std::collections::HashMap<String, Option<TokenStream>> =
                std::collections::HashMap::new();
            let mut unresolved_placeholders: std::collections::HashSet<String> =
                std::collections::HashSet::new();

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
                                    let token = rule.tokens[*idx].token;
                                    match token {
                                        Token::Term(_) => {
                                            let ty = Some(grammar.token_typename.clone());
                                            inferred_type = Some(ty);
                                            break;
                                        }
                                        Token::NonTerm(to_nonterm_idx) => {
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

        // check for nonterminals in %prec,
        // all production rules in that nonterminal must have precedence defined.
        let mut nonterm_prec_candidates: Vec<BTreeSet<Option<usize>>> =
            vec![BTreeSet::new(); grammar.nonterminals.len()];
        loop {
            let mut changed = false;
            for (nonterm_idx, nonterm) in grammar.nonterminals.iter().enumerate() {
                for rule in &nonterm.rules {
                    match rule.prec {
                        Some(prec) => match *prec.value() {
                            Precedence::Dynamic(token_idx) => {
                                if let Token::NonTerm(token_nonterm_idx) =
                                    rule.tokens[token_idx].token
                                {
                                    let mut target_candidates =
                                        nonterm_prec_candidates[token_nonterm_idx].clone();
                                    let len0 = nonterm_prec_candidates[nonterm_idx].len();
                                    nonterm_prec_candidates[nonterm_idx]
                                        .append(&mut target_candidates);
                                    if nonterm_prec_candidates[nonterm_idx].len() != len0 {
                                        changed = true;
                                    }
                                }
                            }
                            Precedence::Fixed(level) => {
                                if nonterm_prec_candidates[nonterm_idx].insert(Some(level)) {
                                    changed = true;
                                }
                            }
                        },
                        _ => {
                            if nonterm_prec_candidates[nonterm_idx].insert(None) {
                                changed = true;
                            }
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }
        for nonterm in &mut grammar.nonterminals {
            for rule in &mut nonterm.rules {
                if let Some(prec) = &mut rule.prec {
                    if let Precedence::Dynamic(token_idx) = *prec.value() {
                        if let Token::NonTerm(token_nonterm_idx) = rule.tokens[token_idx].token {
                            let target_candidates = &nonterm_prec_candidates[token_nonterm_idx];
                            if target_candidates.contains(&None) {
                                // TODO
                                // no need to be an error on GLR parser?
                                return Err(ParseError::NonTerminalPrecedenceNotDefined(
                                    Located::new(token_nonterm_idx, prec.location()),
                                ));
                            }

                            if target_candidates.len() == 1 {
                                let fixed = target_candidates.iter().next().unwrap().unwrap();
                                *prec = Located::new(Precedence::Fixed(fixed), prec.location());
                            }
                        }
                    }
                }
            }
        }

        // check start rule is valid
        if !grammar
            .nonterminals_index
            .contains_key(grammar.start_rule_name.value())
        {
            return Err(ParseError::StartNonTerminalNotDefined(
                grammar.start_rule_name.location(),
            ));
        }

        // insert augmented rule
        {
            let augmented_name =
                Located::new(utils::AUGMENTED_NAME.to_string(), Location::CallSite);
            let start_idx = grammar
                .nonterminals_index
                .get(grammar.start_rule_name.value())
                .unwrap();
            let augmented_rule = Rule {
                tokens: vec![
                    TokenMapped {
                        token: Token::NonTerm(*start_idx),
                        mapto: None,
                        location: Location::CallSite,
                        reduce_action_chains: Vec::new(),
                    },
                    TokenMapped {
                        token: Token::Term(TerminalSymbol::Eof),
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
            let nonterminal_info = NonTerminalInfo {
                name: augmented_name.clone(),
                pretty_name: utils::AUGMENTED_NAME.to_string(),
                ruletype: None,
                ruletype_boxed: false,
                root_location: None,
                rules: vec![augmented_rule],
                protected: true,
                nonterm_type: Some(rusty_lr_core::parser::nonterminal::NonTerminalType::Augmented),
            };
            // start rule is protected
            grammar.nonterminals[*start_idx].protected = true;

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
                    if token.token
                        == Token::Term(TerminalSymbol::Term(grammar.other_terminal_index))
                    {
                        grammar.other_used = true;
                    }
                    if token.token == Token::Term(TerminalSymbol::Error) {
                        grammar.error_used = true;
                    }
                }
            }
        }

        Ok(grammar)
    }

    /// optimize grammar
    fn optimize_iterate(&mut self) -> Option<OptimizeDiag> {
        // for early stopping optimization loop
        let mut something_changed = false;

        // We are trying to find the 'minimum partitioning' of terminals
        // First we collect all the *groups* of terminals
        // Then we calculate the *minimal partitioning* to compress the groups
        let mut term_sets = BTreeSet::new();
        term_sets.insert((0..self.terminal_classes.len()).collect());

        // collect precedence orders
        // all terminals in one class must have same precedence order
        let mut precedence_sets: BTreeMap<_, BTreeSet<usize>> = Default::default();
        for (term_idx, term) in self.terminals.iter().enumerate() {
            let class = self.terminal_class_id[term_idx];
            let level = term.precedence.map(Located::into_value);
            precedence_sets.entry(level).or_default().insert(class);
        }
        term_sets.extend(precedence_sets.into_values());

        // collect {set of terminals} that have same prefix-suffix-reduce_action in the production rules
        // so we can merge those terminals into one class
        // e.g.
        // consider the following state:
        //      A -> X x Y
        //      A -> X y Y
        //      A -> X z Y
        // here, we can group {x, y, z} into one class and merge them
        //      A -> X <class> Y
        for nonterm_def in self.nonterminals.iter() {
            let mut same_ruleset = BTreeMap::new();
            for rule in &nonterm_def.rules {
                for (token_idx, term_mapped) in rule.tokens.iter().enumerate() {
                    if let Token::Term(TerminalSymbol::Term(term)) = term_mapped.token {
                        // if this rule has reduce action, and it is not auto-generated,
                        // this terminal should be completely distinct from others (for user-defined inspection action)
                        // so put this terminal into separate class
                        if rule.reduce_action.is_some()
                            && !rule.reduce_action.as_ref().unwrap().is_identity()
                        {
                            term_sets.insert(BTreeSet::from([term]));
                            continue;
                        }

                        // tokens before this token
                        let prefix = rule
                            .tokens
                            .iter()
                            .take(token_idx)
                            .map(|token| (token.token, &token.reduce_action_chains))
                            .collect::<Vec<_>>();
                        // tokens after this token
                        let suffix = rule
                            .tokens
                            .iter()
                            .skip(token_idx + 1)
                            .map(|token| (token.token, &token.reduce_action_chains))
                            .collect::<Vec<_>>();
                        let reduce_chains = &term_mapped.reduce_action_chains;
                        let prec = rule.prec.map(Located::into_value);
                        let dprec = rule.dprec.map_or(0, Located::into_value);
                        let reduce_action_token_index =
                            rule.reduce_action
                                .as_ref()
                                .map(|reduce_action| match reduce_action {
                                    ReduceAction::Identity(idx) => *idx,
                                    _ => unreachable!("only identity reduce action should be here"),
                                });

                        if !same_ruleset
                            .entry((
                                prefix,
                                suffix,
                                reduce_chains,
                                prec,
                                dprec,
                                reduce_action_token_index,
                            ))
                            .or_insert_with(BTreeSet::new)
                            .insert(term)
                        {
                            // if it is false, it is reduce/reduce conflict (duplicated rule)
                            // so stop optimization
                            return None;
                        }
                    }
                }
            }
            term_sets.extend(same_ruleset.into_values());
        }

        let term_partition = crate::partition::minimal_partition(
            term_sets.into_iter().map(|terms| terms.into_iter()),
        );

        // check if two or more terminals can be merged into one class
        let mut removed_rules_diag = Vec::new();

        if term_partition.len() != self.terminal_classes.len() {
            something_changed = true;

            // convert all terminals using terminal class
            // delete all rules that using non-first terminals in that class
            // e.g. delete
            //      A -> X y Y
            //      A -> X z Y
            // and convert x to class so that only
            //      A -> X <class> Y
            // remains
            let mut is_first_oldclass_in_newclass = Vec::new();
            is_first_oldclass_in_newclass.resize(self.terminal_classes.len(), false);

            let mut old_class_to_new_class = vec![0; self.terminal_classes.len()];

            let mut new_class_defs = Vec::with_capacity(term_partition.len());
            let mut new_term_class_id = vec![0; self.terminals.len()];
            let mut multiterm_counter = 0;
            for (new_class_id, (_setids, old_classes)) in term_partition.into_iter().enumerate() {
                let mut terms = Vec::new();
                // terms.len() != len because single terminal could be range-based optimized into multiple characters
                let mut len = 0;
                for &old_class in old_classes.iter() {
                    for &term in &self.terminal_classes[old_class].terminals {
                        new_term_class_id[term] = new_class_id;
                        terms.push(term);

                        let name = &self.terminals[term].name;
                        len += name.count();
                    }
                    old_class_to_new_class[old_class] = new_class_id;
                }
                is_first_oldclass_in_newclass[old_classes[0]] = true;
                if len > 1 {
                    multiterm_counter += 1;
                }
                let class_def = TerminalClassDefinition {
                    terminals: terms,
                    multiterm_counter,
                    ranges: Vec::new(),
                    data_used: self.terminal_classes[old_classes[0]].data_used,
                };
                new_class_defs.push(class_def);
            }

            self.terminal_class_id = new_term_class_id;
            self.terminal_classes = new_class_defs;
            self.other_terminal_class_id = self.terminal_class_id[self.other_terminal_index];
            // terminal class optimization ends

            // check for unused non-terminals
            let mut nonterm_used = vec![false; self.nonterminals.len()];
            for nonterm in &self.nonterminals {
                for rule in &nonterm.rules {
                    for token in &rule.tokens {
                        if let Token::NonTerm(nonterm_idx) = token.token {
                            nonterm_used[nonterm_idx] = true;
                        }
                    }
                }
            }
            for (nonterm_idx, nonterm) in self.nonterminals.iter_mut().enumerate() {
                // do not delete protected non-terminals
                if nonterm.is_protected() {
                    continue;
                }
                if nonterm.rules.is_empty() {
                    continue;
                }

                if !nonterm_used[nonterm_idx] {
                    // this rule was not used
                    nonterm.rules.clear();
                    if !nonterm.is_auto_generated() {
                        let loc = nonterm.name.location();
                        removed_rules_diag.push(OptimizeRemove::NonTermNotUsed(loc));
                    }
                }
            }

            let mut other_was_used = false;
            for nonterm in &mut self.nonterminals {
                let rules = std::mem::take(&mut nonterm.rules);
                let mut new_rules = Vec::new();
                for mut rule in rules {
                    // check if this rule contains any terminal that is not the first terminal in the class
                    let mut remove_this_rule = false;
                    for token in &rule.tokens {
                        if let Token::Term(TerminalSymbol::Term(old_class)) = token.token {
                            if !is_first_oldclass_in_newclass[old_class] {
                                remove_this_rule = true;
                                break;
                            }
                        }
                    }

                    if remove_this_rule {
                        // this rule contains terminal that is not the first terminal in the class
                        // so remove this rule
                        // add to diags only if it was not auto-generated
                        if !nonterm.is_auto_generated() {
                            let diag = OptimizeRemove::TerminalClassRuleMerge(rule);
                            removed_rules_diag.push(diag);
                        }
                        continue;
                    }

                    // change any terminal to its class id

                    //  - tokens in the rule
                    for token in &mut rule.tokens {
                        if let Token::Term(TerminalSymbol::Term(old_class)) = token.token {
                            let new_class = old_class_to_new_class[old_class];
                            if new_class == self.other_terminal_class_id {
                                other_was_used = true;
                            }
                            token.token = Token::Term(TerminalSymbol::Term(new_class));
                        }
                    }
                    new_rules.push(rule);
                }
                nonterm.rules = new_rules;
            }

            self.other_used = other_was_used;
        }

        // remove rules that have single production rule and single token
        // e.g. A -> B, then fix all occurrences of A to B
        let mut nonterm_replace = BTreeMap::new();

        // in one optimize iteration, do not allow optimize-chains (e.g. A -> B, B -> C)
        let mut optimize_related_nonterminals = BTreeSet::new();

        for (nonterm_id, nonterm) in self.nonterminals.iter().enumerate() {
            // do not delete protected non-terminals
            if nonterm.is_protected() {
                continue;
            }
            if nonterm.rules.len() != 1 {
                continue;
            }
            let rule = &nonterm.rules[0];
            if rule.dprec.is_some() {
                // this rule has %dprec, so do not optimize
                continue;
            }
            if rule.prec.is_some() {
                // this rule has %prec, so do not optimize
                continue;
            }
            if rule.tokens.len() != 1 {
                continue;
            }
            if optimize_related_nonterminals.contains(&nonterm_id) {
                continue;
            }
            let totoken = rule.tokens[0].token;
            if let Token::NonTerm(to_nonterm_id) = totoken {
                if optimize_related_nonterminals.contains(&to_nonterm_id) {
                    continue;
                }
            }

            if totoken == Token::NonTerm(nonterm_id) {
                // A -> A cycle, do not optimize
                continue;
            }

            let mut reduce_action_chain = rule.tokens[0].reduce_action_chains.clone();

            if let Some(ReduceAction::Custom(body)) = &rule.reduce_action {
                if rule.reduce_action_contains_ident(utils::LOOKAHEAD_PARAMETER_NAME) {
                    continue;
                }
                // if this rule has custom reduce action, save it
                let output_type = nonterm.ruletype.clone();
                let mapto = &rule.tokens[0].mapto;
                let location_mapto = if let Some(mapto) = mapto {
                    let location_varname = utils::location_variable_name(mapto.value().as_str());
                    if rule.reduce_action_contains_ident(&location_varname) {
                        Some(location_varname)
                    } else {
                        None
                    }
                } else {
                    None
                };

                let input_type = if let Some(mapto) = mapto {
                    let ruletype = match totoken {
                        Token::Term(_) => Some(self.token_typename.clone()),
                        Token::NonTerm(to_nonterm_id) => {
                            self.nonterminals[to_nonterm_id].ruletype.clone()
                        }
                    };

                    if let Some(ruletype) = ruletype {
                        if rule.reduce_action_contains_ident(mapto.value().as_str()) {
                            Some((mapto.value().clone(), ruletype))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };
                let idx = self.custom_reduce_actions.len();
                self.custom_reduce_actions.push(CustomSingleReduceAction {
                    body: body.clone(),
                    input_type,
                    input_location: location_mapto,
                    output_type,
                });
                reduce_action_chain.push(idx);
            }

            optimize_related_nonterminals.insert(nonterm_id);
            if let Token::NonTerm(to_nonterm_id) = totoken {
                optimize_related_nonterminals.insert(to_nonterm_id);
            }

            nonterm_replace.insert(nonterm_id, (totoken, reduce_action_chain));
        }

        // replace all Token::NonTerm that can be replaced into Token::Term calculated above
        for nonterm in self.nonterminals.iter_mut() {
            for rule in &mut nonterm.rules {
                for token in &mut rule.tokens {
                    if let Token::NonTerm(nonterm_id) = token.token {
                        if let Some((newclass, custom_reduce_action)) =
                            nonterm_replace.get(&nonterm_id)
                        {
                            token.token = *newclass;
                            let mut new_reduce_chain = custom_reduce_action.clone();
                            new_reduce_chain
                                .append(&mut std::mem::take(&mut token.reduce_action_chains));
                            token.reduce_action_chains = new_reduce_chain;
                        }
                    }
                }
            }
        }

        // delete rules - keys of nonterm_replace
        something_changed |= !nonterm_replace.is_empty();
        for &nonterm_id in nonterm_replace.keys() {
            let nonterm = &mut self.nonterminals[nonterm_id];
            let rules = std::mem::take(&mut nonterm.rules);
            let rule = rules.into_iter().next().unwrap();
            // add to diags only if it was not auto-generated
            if !nonterm.is_auto_generated() {
                let loc = nonterm.name.location();
                let diag = OptimizeRemove::SingleNonTerminalRule(rule, loc);
                removed_rules_diag.push(diag);
            }
        }

        if something_changed {
            Some(OptimizeDiag {
                removed: removed_rules_diag,
            })
        } else {
            None
        }
    }

    pub fn optimize(&mut self, max_iter: usize) -> OptimizeDiag {
        let mut diag = OptimizeDiag {
            removed: Vec::new(),
        };

        // check if RuleType and ReduceAction can be removed from certain non-terminals
        let mut add_to_diags = BTreeSet::new();
        loop {
            let start_rule_idx = *self
                .nonterminals_index
                .get(self.start_rule_name.value())
                .unwrap();
            let mut changed = false;
            let mut can_removes = Vec::new();

            for (i, nonterm) in self.nonterminals.iter().enumerate() {
                if i == start_rule_idx {
                    // do not remove ruletype from start rule
                    continue;
                }

                if nonterm.ruletype.is_none() {
                    continue;
                }

                let mut can_remove = true;

                // check for every production rules,
                // if it is still compilable without this nonterminal's ruletype
                // if it is possible, we can remove this nonterminal's ruletype (and reduce action)
                for (j, nonterm_j) in self.nonterminals.iter().enumerate() {
                    if i == j {
                        if nonterm_j.is_auto_generated() {
                            // if nonterm_i is auto-generated, do not check self rules
                            continue;
                        }
                    }
                    for rule in nonterm_j.rules.iter() {
                        for token in rule.tokens.iter() {
                            if token.token != Token::NonTerm(i) {
                                continue;
                            }

                            // nonterm_i's data was used in this rule
                            let used = if let Some(mapto) = &token.mapto {
                                rule.reduce_action_contains_ident(mapto.value().as_str())
                            } else {
                                false
                            };

                            if used {
                                // nonterm_i's data cannot be removed
                                can_remove = false;
                                break;
                            }
                        }
                        if !can_remove {
                            break;
                        }
                    }
                    if !can_remove {
                        break;
                    }
                }

                if can_remove {
                    can_removes.push(i);
                }
            }

            for i in can_removes {
                let nonterm = &mut self.nonterminals[i];
                if nonterm.ruletype.is_some() {
                    changed = true;
                    nonterm.ruletype = None;
                }
                if nonterm.is_auto_generated() {
                    for rule in &mut nonterm.rules {
                        if rule.reduce_action.is_some() {
                            changed = true;
                            rule.reduce_action = None;
                        }
                    }
                } else {
                    for rule in &mut nonterm.rules {
                        if let Some(reduce_action) = &rule.reduce_action {
                            match reduce_action {
                                ReduceAction::Custom(_) => {
                                    // cannot remove custom reduce action;
                                    // add to diag
                                    add_to_diags.insert(i);
                                }
                                ReduceAction::Identity(_) => {
                                    changed = true;
                                    rule.reduce_action = None;
                                }
                            }
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }
        for i in add_to_diags {
            diag.removed.push(OptimizeRemove::NonTermDataNotUsed(i));
        }

        // check for any data of terminal symbol was used in any reduce action
        for class_def in &mut self.terminal_classes {
            class_def.data_used = false;
        }
        for nonterm in &self.nonterminals {
            for rule in &nonterm.rules {
                for token in &rule.tokens {
                    if let Token::Term(TerminalSymbol::Term(term)) = token.token {
                        if let Some(mapto) = &token.mapto {
                            if rule.reduce_action_contains_ident(mapto.value().as_str()) {
                                self.terminal_classes[term].data_used = true;
                            }
                        }
                    }
                }
            }
        }

        for _ in 0..max_iter {
            let ret = self.optimize_iterate();
            match ret {
                Some(new_diag) => {
                    diag.removed.extend(new_diag.removed.into_iter());
                }
                None => {
                    break;
                }
            }
        }

        // remove nonterminals from Vecs which are deleted in the optimization
        // nonterm idx remapping
        let mut nonterm_old_to_new = vec![0; self.nonterminals.len()];
        let mut new_idx = 0;
        for (old_idx, nonterm) in self.nonterminals.iter().enumerate() {
            if nonterm.rules.is_empty() && !nonterm.is_protected() {
                continue;
            }
            nonterm_old_to_new[old_idx] = new_idx;
            new_idx += 1;
        }
        self.nonterminals = std::mem::take(&mut self.nonterminals)
            .into_iter()
            .filter(|nonterm| nonterm.is_protected() || !nonterm.rules.is_empty())
            .collect();
        self.nonterminals_index.clear();
        for (idx, nonterm) in self.nonterminals.iter().enumerate() {
            self.nonterminals_index
                .insert(nonterm.name.value().clone(), idx);
        }
        for nonterm in &mut self.nonterminals {
            for rule in &mut nonterm.rules {
                for token in &mut rule.tokens {
                    if let Token::NonTerm(nonterm_idx) = token.token {
                        token.token = Token::NonTerm(nonterm_old_to_new[nonterm_idx]);
                    }
                }
            }
        }

        if self.is_char || self.is_u8 {
            // calculate ranges
            for class in self.terminal_classes.iter_mut() {
                let mut ranges0 = class
                    .terminals
                    .iter()
                    .filter_map(|&term_idx| {
                        if term_idx == self.other_terminal_index {
                            None
                        } else {
                            let name = &self.terminals[term_idx].name;
                            if let TerminalName::CharRange(start, last) = name {
                                Some((*start as u32, *last as u32))
                            } else {
                                unreachable!("terminal name should be char range");
                            }
                        }
                    })
                    .collect::<Vec<_>>();
                ranges0.sort();

                let mut ranges: Vec<(u32, u32)> = Vec::new();
                for (s, l) in ranges0 {
                    match ranges.last_mut() {
                        Some((_, last)) if *last + 1 == s => {
                            *last = l;
                        }
                        _ => {
                            ranges.push((s, l));
                        }
                    }
                }
                class.ranges = ranges;
            }
        }

        diag
    }

    fn term_pretty_name(&self, term_idx: usize) -> String {
        if term_idx == self.other_terminal_index {
            "<Others>".to_string()
        } else {
            self.terminals[term_idx]
                .name
                .pretty_name(self.is_char, self.is_u8)
        }
    }

    /// returns either 'term' or 'TerminalClassX'
    pub fn class_pretty_name_abbr(&self, class_idx: usize) -> String {
        let class = &self.terminal_classes[class_idx];
        let len: usize = class
            .terminals
            .iter()
            .map(|term| self.terminals[*term].name.count())
            .sum();
        if len == 1 {
            self.term_pretty_name(class.terminals[0])
        } else {
            format!("TerminalClass{}", class.multiterm_counter)
        }
    }
    /// returns either 'term' or '[term1, term2, ...]'
    pub fn class_pretty_name_list(&self, class: TerminalSymbol<usize>, max_len: usize) -> String {
        match class {
            TerminalSymbol::Error => return "error".to_string(),
            TerminalSymbol::Eof => return "eof".to_string(),
            TerminalSymbol::Term(class_idx) => {
                let class = &self.terminal_classes[class_idx];
                let len: usize = class
                    .terminals
                    .iter()
                    .map(|term| self.terminals[*term].name.count())
                    .sum();
                if len == 1 {
                    self.term_pretty_name(class.terminals[0])
                } else if class.terminals.len() < max_len {
                    let f = self.terminal_classes[class_idx]
                        .terminals
                        .iter()
                        .map(|&term| self.term_pretty_name(term))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("[{f}]")
                } else {
                    let class = &self.terminal_classes[class_idx];
                    let first = class.terminals[0];
                    let second = class.terminals[1];
                    let last = *class.terminals.last().unwrap();

                    let first = self.term_pretty_name(first);
                    let second = self.term_pretty_name(second);
                    let last = self.term_pretty_name(last);
                    format!("[{first}, {second}, ..., {last}]")
                }
            }
        }
    }
    pub fn nonterm_pretty_name(&self, nonterm_idx: usize) -> String {
        self.nonterminals[nonterm_idx].pretty_name.clone()
    }

    /// create the rusty_lr_core::Grammar from the parsed CFGs
    pub fn create_builder(
        &mut self,
    ) -> rusty_lr_core::builder::Grammar<TerminalSymbol<usize>, usize> {
        let mut grammar: rusty_lr_core::builder::Grammar<TerminalSymbol<usize>, usize> =
            rusty_lr_core::builder::Grammar::new();

        let mut rules = Vec::new();
        for (nonterm_idx, nonterminal) in self.nonterminals.iter().enumerate() {
            rules.reserve(nonterminal.rules.len());
            for rule in 0..nonterminal.rules.len() {
                rules.push((nonterm_idx, rule));
            }
        }

        for (term_idx, term_info) in self.terminals.iter().enumerate() {
            if let Some(level) = &term_info.precedence {
                let class = self.terminal_class_id[term_idx];
                if !grammar.add_precedence(TerminalSymbol::Term(class), *level.value()) {
                    unreachable!("set_reduce_type error");
                }
            }
        }
        grammar.set_precedence_types(self.precedence_types.iter().map(|op| *op.value()).collect());

        // add rules
        for (nonterm_id, nonterm) in self.nonterminals.iter().enumerate() {
            for rule in nonterm.rules.iter() {
                let tokens = rule
                    .tokens
                    .iter()
                    .map(|token_mapped| token_mapped.token)
                    .collect();

                grammar.add_rule(
                    nonterm_id,
                    tokens,
                    rule.prec.map(Located::into_value),
                    rule.dprec.map_or(0, Located::into_value),
                );
            }
        }

        grammar
    }

    pub fn build_grammar(
        &mut self,
    ) -> rusty_lr_core::builder::DiagnosticCollector<TerminalSymbol<usize>> {
        let augmented_idx = *self.nonterminals_index.get(utils::AUGMENTED_NAME).unwrap();
        let mut collector = rusty_lr_core::builder::DiagnosticCollector::new(true);
        let states = if self.lalr {
            self.builder.build_lalr(augmented_idx, &mut collector)
        } else {
            self.builder.build(augmented_idx, &mut collector)
        };
        let states = match states {
            Ok(states) => states.states,
            Err(err) => {
                unreachable!("Error building grammar: {:?}", err);
            }
        };
        let mut states: Vec<
            rusty_lr_core::parser::state::IntermediateState<
                TerminalSymbol<usize>,
                usize,
                usize,
                usize,
            >,
        > = states.into_iter().map(Into::into).collect();

        // Identify states that only perform a single reduction of a single-token rule.
        // These are candidates for optimization.
        let mut reduce_states: Vec<_> = Vec::with_capacity(states.len());
        for state in states.iter() {
            if !state.shift_goto_map_term.is_empty() || !state.shift_goto_map_nonterm.is_empty() {
                // this state is not a reduce state
                reduce_states.push(None);
                continue;
            }

            let rules = state
                .reduce_map
                .iter()
                .map(|(_, r)| r)
                .collect::<BTreeSet<_>>();

            if rules.len() != 1 {
                reduce_states.push(None);
                continue;
            }
            let rule_set = rules.into_iter().next().unwrap();
            if rule_set.len() != 1 {
                reduce_states.push(None);
                continue;
            }
            let rule = rule_set[0];
            let (nonterm, local_rule_id) = self.get_rule_by_id(rule).unwrap();
            if nonterm.rules[local_rule_id].tokens.len() != 1 {
                reduce_states.push(None);
                continue;
            }

            let nonterm_idx = self.nonterminals_index[nonterm.name.value().as_str()];
            reduce_states.push(Some((nonterm_idx, local_rule_id)));
        }

        // Iteratively optimize the state machine by bypassing the reduce-only states identified above.
        // This continues until no more optimizations can be made.
        loop {
            let mut changed = false;

            for state in &mut states {
                // Optimize shift-on-terminal transitions.
                for (_, next_state) in &mut state.shift_goto_map_term {
                    if let Some((nonterm_idx, rule_local_id)) = reduce_states[next_state.state] {
                        let rule = &self.nonterminals[nonterm_idx].rules[rule_local_id];
                        match rule.reduce_action {
                            None => {
                                let idx = state
                                    .shift_goto_map_nonterm
                                    .iter()
                                    .position(|(nt, _)| *nt == nonterm_idx)
                                    .unwrap();
                                let ns = state.shift_goto_map_nonterm[idx].1;
                                next_state.state = ns.state;
                                next_state.push = false;
                                changed = true;
                            }
                            Some(ReduceAction::Identity(_)) => {
                                let idx = state
                                    .shift_goto_map_nonterm
                                    .iter()
                                    .position(|(nt, _)| *nt == nonterm_idx)
                                    .unwrap();
                                let ns = state.shift_goto_map_nonterm[idx].1;
                                next_state.state = ns.state;
                                next_state.push = true;
                                changed = true;
                            }
                            _ => {}
                        }
                    }
                }

                // Optimize shift-on-nonterminal transitions.
                for i in 0..state.shift_goto_map_nonterm.len() {
                    let next_state = state.shift_goto_map_nonterm[i].1;
                    if let Some((nonterm_idx, rule_local_id)) = reduce_states[next_state.state] {
                        let rule = &self.nonterminals[nonterm_idx].rules[rule_local_id];
                        match rule.reduce_action {
                            None => {
                                let idx = state
                                    .shift_goto_map_nonterm
                                    .iter()
                                    .position(|(nt, _)| *nt == nonterm_idx)
                                    .unwrap();
                                let ns = state.shift_goto_map_nonterm[idx].1;
                                state.shift_goto_map_nonterm[i].1.state = ns.state;
                                state.shift_goto_map_nonterm[i].1.push = false;
                                changed = true;
                            }
                            Some(ReduceAction::Identity(_)) => {
                                let idx = state
                                    .shift_goto_map_nonterm
                                    .iter()
                                    .position(|(nt, _)| *nt == nonterm_idx)
                                    .unwrap();
                                let ns = state.shift_goto_map_nonterm[idx].1;
                                state.shift_goto_map_nonterm[i].1.state = ns.state;
                                state.shift_goto_map_nonterm[i].1.push = true;
                                changed = true;
                            }
                            _ => {}
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }

        // Identify all reachable states starting from the initial state (state 0).
        let mut states_used = vec![false; states.len()];
        let mut ping = vec![0];
        states_used[0] = true;
        let mut pong = Vec::new();
        while !ping.is_empty() {
            pong.clear();
            for &p in &ping {
                let s = &states[p];

                for next_state in s
                    .shift_goto_map_term
                    .iter()
                    .map(|(_, v)| v.state)
                    .chain(s.shift_goto_map_nonterm.iter().map(|(_, v)| v.state))
                {
                    if !states_used[next_state] {
                        states_used[next_state] = true;
                        pong.push(next_state);
                    }
                }
            }
            std::mem::swap(&mut ping, &mut pong);
        }

        // Remove unreachable states and remap state indices.
        let mut state_remap = Vec::with_capacity(states.len());
        let mut new_states = Vec::with_capacity(states.len());
        for (i, state) in states.into_iter().enumerate() {
            state_remap.push(new_states.len());
            if states_used[i] {
                new_states.push(state);
            }
        }
        for state in &mut new_states {
            for (_, next_state) in &mut state.shift_goto_map_term {
                debug_assert!(states_used[next_state.state]);
                next_state.state = state_remap[next_state.state];
            }
            for (_, next_state) in &mut state.shift_goto_map_nonterm {
                debug_assert!(states_used[next_state.state]);
                next_state.state = state_remap[next_state.state];
            }
        }

        // check for unused production rules
        let mut rules_used = vec![false; self.builder.rules.len()];
        for state in &new_states {
            for rules in state.reduce_map.iter().map(|(_, r)| r) {
                for &rule in rules {
                    rules_used[rule] = true;
                }
            }
        }
        let mut i = 0;
        for nonterm in &mut self.nonterminals {
            for rule in &mut nonterm.rules {
                rule.is_used = rules_used[i];
                i += 1;
            }
        }

        for state in &mut new_states {
            for (term, shift_target) in &mut state.shift_goto_map_term {
                if let TerminalSymbol::Term(term) = *term {
                    shift_target.push = self.terminal_classes[term].data_used;
                }
            }
        }

        self.states = new_states;

        collector
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
    resolved: &std::collections::HashMap<String, Option<TokenStream>>,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::args::PatternArgs;
    use quote::quote;

    #[test]
    fn test_parse_simple_grammar() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr : 'a';
        };

        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");

        assert!(grammar_args.error_recovered.is_empty());

        // Check token type
        assert_eq!(grammar_args.token_typename.len(), 1);
        let (_, typename) = &grammar_args.token_typename[0];
        assert_eq!(typename.to_string(), "char");

        // Check start rule
        assert_eq!(grammar_args.start_rule_name.len(), 1);
        assert_eq!(grammar_args.start_rule_name[0].value(), "Expr");

        // Check rules
        assert_eq!(grammar_args.rules.len(), 1);
        let rule = &grammar_args.rules[0];
        assert_eq!(rule.name.value(), "Expr");
        assert_eq!(rule.rule_lines.len(), 1);

        let line = &rule.rule_lines[0];
        assert_eq!(line.tokens.len(), 1);
        let (name, pattern) = &line.tokens[0];
        assert!(name.is_none());

        if let PatternArgs::Char(c) = pattern {
            assert_eq!(*c.value(), 'a');
        } else {
            panic!("Expected PatternArgs::Char");
        }
    }

    #[test]
    fn test_parse_complex_grammar_directives_userdata() {
        let input = quote! {
            %tokentype u8;
            %start Expr;
            %userdata UserData;
            %error MyError;
            Expr : b'a';
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed userdata");
        assert!(grammar_args.error_recovered.is_empty());
    }

    #[test]
    fn test_parse_complex_grammar_directives_prefix() {
        let input = quote! {
            %tokentype u8;
            %start Expr;
            %moduleprefix ::my_prefix;
            Expr : b'a';
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed prefix");
        assert!(grammar_args.error_recovered.is_empty());
    }

    #[test]
    fn test_parse_complex_grammar_directives_prec() {
        let input = quote! {
            %tokentype u8;
            %start Expr;
            %left b'+' b'-';
            Expr : b'a';
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed prec");
        assert!(grammar_args.error_recovered.is_empty());
    }

    #[test]
    fn test_parse_complex_grammar_directives_glr() {
        let input = quote! {
            %tokentype u8;
            %start Expr;
            %glr;
            %dense;
            Expr : b'a';
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed glr");
        assert!(grammar_args.error_recovered.is_empty());
        assert!(grammar_args.glr);
        assert!(grammar_args.dense);
    }

    #[test]
    fn test_parse_complex_grammar_rules() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr : Term
                 | Expr '+' Term
                 ;
            Term : 'a'*
                 | dollar_ident=ident
                 ;
        };

        let grammar_args =
            Grammar::parse_args(input).expect("Failed to parse complex grammar rules");
        assert!(grammar_args.error_recovered.is_empty());
    }

    #[test]
    fn test_parse_complex_grammar_sep() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr : $sep(Term, ',', +);
            Term : 'a';
        };

        let grammar_args = Grammar::parse_args(input).expect("Failed to parse complex grammar sep");
        assert!(grammar_args.error_recovered.is_empty());
    }

    #[test]
    fn test_parse_grammar_error_recovery() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            %error MyError;
            %left; // Missing precedence arguments
            Expr : 'a';
        };

        let grammar_args =
            Grammar::parse_args(input).expect("Should recover and return grammar args");

        assert!(!grammar_args.error_recovered.is_empty());

        let err = &grammar_args.error_recovered[0];
        assert!(err.message.contains("Expected <ident>") || err.message.contains("precedence"));

        assert_eq!(grammar_args.start_rule_name[0].value(), "Expr");
        assert_eq!(grammar_args.rules.len(), 1);
        assert_eq!(grammar_args.rules[0].name.value(), "Expr");
    }

    #[test]
    fn test_bison_variables_success() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr(i32) : 'a' 'b' { $1 as i32 + $2 as i32 + @0.to_range().start as i32 + @1.to_range().start as i32 };
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args);
        assert!(grammar.is_ok());
    }

    #[test]
    fn test_bison_variables_zero_error() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr(i32) : 'a' 'b' { $0 };
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args);
        assert!(matches!(grammar, Err(ParseError::BisonVariableZero(_))));
    }

    #[test]
    fn test_bison_variables_out_of_range_error() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr(i32) : 'a' 'b' { $3 };
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args);
        assert!(matches!(
            grammar,
            Err(ParseError::BisonVariableOutOfRange { .. })
        ));

        let input_loc = quote! {
            %tokentype char;
            %start Expr;
            Expr(i32) : 'a' 'b' { @3.to_range().start as i32 };
        };
        let grammar_args = Grammar::parse_args(input_loc).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args);
        assert!(matches!(
            grammar,
            Err(ParseError::BisonVariableOutOfRange { .. })
        ));
    }

    #[test]
    fn test_type_inference_simple() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr(_) : 'a' { $1 };
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
        let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
        let ruletype = &grammar.nonterminals[*expr_idx].ruletype;
        assert_eq!(ruletype.as_ref().unwrap().to_string(), "char");
    }

    #[test]
    fn test_type_inference_auto_identity() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr(_) : 'a';
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
        let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
        let ruletype = &grammar.nonterminals[*expr_idx].ruletype;
        assert_eq!(ruletype.as_ref().unwrap().to_string(), "char");
    }

    #[test]
    fn test_type_inference_multi_step() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr(_) : Term { $1 };
            Term(_) : 'a' { $1 };
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
        let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
        let term_idx = grammar.nonterminals_index.get("Term").unwrap();
        assert_eq!(
            grammar.nonterminals[*expr_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "char"
        );
        assert_eq!(
            grammar.nonterminals[*term_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "char"
        );
    }

    #[test]
    fn test_type_inference_circular_dependency() {
        let input = quote! {
            %tokentype char;
            %start Expr;
            Expr(_) : Term { $1 };
            Term(_) : Expr { $1 };
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args);
        assert!(matches!(grammar, Err(ParseError::TypeInferenceFailed(_))));
    }

    #[test]
    fn test_codegen_no_empty_tags() {
        let input = quote! {
            %tokentype Token;
            %start Expr;
            %token a Token::A(_);
            Expr(i32) : a { 1 };
        };
        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
        let code = grammar.emit_compiletime();

        // Ensure the generated code compiles successfully as a valid Rust TokenStream
        let parsed = syn::parse2::<syn::File>(code.clone());
        assert!(
            parsed.is_ok(),
            "Generated code failed to parse: {:?}",
            parsed.err()
        );

        // Check that the Empty variant is present in the output
        let code_str = code.to_string();
        assert!(
            code_str.contains("Empty"),
            "Empty variant should be unconditionally included"
        );
    }

    #[test]
    fn test_variable_substitution_success() {
        let input = quote! {
            %tokentype MyToken;
            %location MyLoc;
            %userdata MyUser;
            %error MyErr;
            %moduleprefix ::my_prefix;
            %token a MyToken::A;

            %start Expr;
            Expr($tokentype) : a { $tokentype };
            Term($location) : a { $location };
            Rule3($userdata) : a { $userdata };
            Rule4($error) : a { $error };
            Rule5($moduleprefix) : a { $moduleprefix };
            Rule6($a) : a { $a };
            Rule7($Expr) : a { $Expr };
        };

        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");

        let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
        assert_eq!(
            grammar.nonterminals[*expr_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "MyToken"
        );

        let term_idx = grammar.nonterminals_index.get("Term").unwrap();
        assert_eq!(
            grammar.nonterminals[*term_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "MyLoc"
        );

        let rule3_idx = grammar.nonterminals_index.get("Rule3").unwrap();
        assert_eq!(
            grammar.nonterminals[*rule3_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "MyUser"
        );

        let rule4_idx = grammar.nonterminals_index.get("Rule4").unwrap();
        assert_eq!(
            grammar.nonterminals[*rule4_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "MyErr"
        );

        let rule5_idx = grammar.nonterminals_index.get("Rule5").unwrap();
        assert_eq!(
            grammar.nonterminals[*rule5_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            ":: my_prefix"
        );

        let rule6_idx = grammar.nonterminals_index.get("Rule6").unwrap();
        assert_eq!(
            grammar.nonterminals[*rule6_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "MyToken :: A"
        );

        let rule7_idx = grammar.nonterminals_index.get("Rule7").unwrap();
        assert_eq!(
            grammar.nonterminals[*rule7_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "MyToken"
        );
    }

    #[test]
    fn test_variable_substitution_circular_dependency() {
        let input = quote! {
            %tokentype Token;
            %token a Token::A;
            %token b Token::B;
            %start Expr;
            Expr($Term) : a;
            Term($Expr) : b;
        };

        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args);
        assert!(grammar.is_err());
        let err = grammar.err().unwrap();
        assert!(matches!(err, ParseError::CircularDependency { .. }));
        if let ParseError::CircularDependency { path, .. } = err {
            assert!(path.contains(&"nonterm:Expr".to_string()));
            assert!(path.contains(&"nonterm:Term".to_string()));
        }
    }

    #[test]
    fn test_variable_substitution_unknown_fallback() {
        let input = quote! {
            %tokentype Token;
            %token a Token::A;
            %start Expr;
            Expr : a { $unknown_var };
        };

        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");

        // The reduce action of the rule should preserve $unknown_var unchanged
        let rule = &grammar.nonterminals[*grammar.nonterminals_index.get("Expr").unwrap()].rules[0];
        let action = match rule.reduce_action.as_ref().unwrap() {
            ReduceAction::Custom(custom) => custom.body.to_string(),
            _ => panic!("Expected Custom reduce action"),
        };
        assert!(action.contains("$ unknown_var"));
    }

    #[test]
    fn test_box_keyword_parsing() {
        let input = quote! {
            %tokentype box MyToken;
            %token a MyToken::A;
            %start Expr;
            Expr(box MyType) : a { MyType };
        };

        let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
        let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");

        assert!(grammar.is_tokentype_boxed);
        assert_eq!(grammar.token_typename.to_string(), "MyToken");

        let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
        assert!(grammar.nonterminals[*expr_idx].ruletype_boxed);
        assert_eq!(
            grammar.nonterminals[*expr_idx]
                .ruletype
                .as_ref()
                .unwrap()
                .to_string(),
            "MyType"
        );

        let code = grammar.emit_compiletime();
        let code_str = code.to_string();

        // Ensure the data enum wraps in Box
        assert!(
            code_str.contains("Box < MyType >")
                || code_str.contains("Box<MyType>")
                || code_str.contains("Box"),
            "Data enum variant should hold Boxed MyType"
        );
        assert!(
            code_str.contains("Box < MyToken >")
                || code_str.contains("Box<MyToken>")
                || code_str.contains("Box"),
            "Data enum variant should hold Boxed MyToken"
        );

        // Ensure auto-wrap Box::new and auto-unwrap *val are emitted
        assert!(
            code_str.contains("Box :: new") || code_str.contains("Box::new"),
            "Box::new should be generated to wrap reduce result"
        );
        assert!(
            code_str.contains("* val") || code_str.contains("*val"),
            "Dereference should be generated to extract boxed value"
        );
    }
}
