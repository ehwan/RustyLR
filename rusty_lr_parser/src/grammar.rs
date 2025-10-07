use std::collections::BTreeMap;
use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
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
use crate::parser::args::IdentOrU32;
use crate::parser::parser_expanded::GrammarContext;
use crate::parser::parser_expanded::GrammarParser;
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
    SingleNonTerminalRule(Rule, Span),
    NonTermNotUsed(Span),
    Cycle(Span),
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
    pub input_type: Option<(Ident, TokenStream)>,
    pub input_location: Option<Ident>,
    pub output_type: Option<TokenStream>,
}

pub struct Grammar {
    /// %moduleprefix, "rusty_lr" for normal use
    pub(crate) module_prefix: TokenStream,

    /// %tokentype
    pub(crate) token_typename: TokenStream,

    /// %userdata
    pub(crate) userdata_typename: TokenStream,

    /// %err
    pub(crate) error_typename: TokenStream,

    /// %start
    pub(crate) start_rule_name: Ident,

    pub terminals: Vec<TerminalInfo>,
    /// ident -> index map for terminals
    pub terminals_index: HashMap<TerminalName, TerminalIndex>,

    /// %left, %right, or %precedence for each precedence level
    pub precedence_types: Vec<(Option<rusty_lr_core::rule::ReduceType>, Span)>,

    /// precedence levels; line number of %left, %right, or %precedence directive
    pub precedence_levels: HashMap<IdentOrU32, (usize, Span)>,

    /// rule definitions
    pub nonterminals: Vec<NonTerminalInfo>,
    /// ident - index map for non-terminals
    pub nonterminals_index: HashMap<Ident, usize>,

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

    /// the filter function for the parser feed();
    /// every terminal will be filtered by this function on classification.
    /// the actual code will be:
    /// ```rust
    /// let terminal_class: usize = match filter( terminal ) {
    ///     ...
    /// };
    pub filter: Option<TokenStream>,

    /// type for location
    pub location_typename: Option<TokenStream>,

    /// precedence level of error token
    pub error_precedence: Option<usize>,

    /// See `TokenMapped::reduce_action_chains` for more details.
    /// This is actual body of each reduce action in the chain.
    pub custom_reduce_actions: Vec<CustomSingleReduceAction>,
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

    pub fn parse_args(input: TokenStream) -> Result<GrammarArgs, ParseArgError> {
        let parser = GrammarParser::new();
        let mut context = GrammarContext::new();

        let mut grammar_args = GrammarArgs::default();

        match crate::parser::lexer::feed_recursive(input, &parser, &mut context, &mut grammar_args)
        {
            Ok(_) => {}
            Err(err) => {
                let message = err.to_string();
                let span = err.location().unwrap().pair.unwrap().0;
                return Err(ParseArgError::MacroLineParse { span, message });
            }
        }
        match context.accept(&parser, &mut grammar_args) {
            Ok(_) => {}
            Err(err) => {
                let message = err.to_string();
                let span = Span::call_site();
                return Err(ParseArgError::MacroLineParse { span, message });
            }
        }

        Ok(grammar_args)
    }
    pub fn arg_check_error(grammar_args: &mut GrammarArgs) -> Result<(), ArgError> {
        // %error
        if grammar_args.error_typename.len() > 1 {
            return Err(ArgError::MultipleErrorDefinition(
                grammar_args.error_typename[0].clone(),
                grammar_args.error_typename[1].clone(),
            ));
        }

        // %moduleprefix
        if grammar_args.module_prefix.len() > 1 {
            return Err(ArgError::MultipleUserDataDefinition(
                grammar_args.module_prefix[0].clone(),
                grammar_args.module_prefix[1].clone(),
            ));
        }

        // %userdata
        if grammar_args.userdata_typename.len() > 1 {
            return Err(ArgError::MultipleUserDataDefinition(
                grammar_args.userdata_typename[0].clone(),
                grammar_args.userdata_typename[1].clone(),
            ));
        }

        // %tokentype
        if grammar_args.token_typename.is_empty() {
            return Err(ArgError::TokenTypeNotDefined);
        } else if grammar_args.token_typename.len() > 1 {
            return Err(ArgError::MultipleTokenTypeDefinition(
                grammar_args.token_typename[0].clone(),
                grammar_args.token_typename[1].clone(),
            ));
        }

        // %start
        if grammar_args.start_rule_name.is_empty() {
            return Err(ArgError::StartNotDefined);
        } else if grammar_args.start_rule_name.len() > 1 {
            return Err(ArgError::MultipleStartDefinition(
                grammar_args.start_rule_name[0].clone(),
                grammar_args.start_rule_name[1].clone(),
            ));
        }

        // %prec and %dprec in each production rules
        for rules in grammar_args.rules.iter_mut() {
            use crate::parser::args::PrecDPrecArgs;
            for rule in rules.rule_lines.iter_mut() {
                let mut unique_prec = None;
                let mut unique_dprec = None;
                for prec in std::mem::take(&mut rule.precs) {
                    match prec {
                        PrecDPrecArgs::Prec(p) => {
                            if unique_prec.is_some() {
                                return Err(ArgError::MultiplePrecDefinition(p.span()));
                            }
                            unique_prec = Some(p);
                        }
                        PrecDPrecArgs::DPrec(d) => {
                            if unique_dprec.is_some() {
                                return Err(ArgError::MultipleDPrecDefinition(d.span()));
                            }
                            unique_dprec = Some(d);
                        }
                        _ => {
                            unreachable!("unexpected PrecDPrecArgs variant");
                        }
                    }
                }
                rule.prec = unique_prec;
                rule.dprec = unique_dprec;
            }
        }

        Ok(())
    }

    pub(crate) fn get_char_value(&self, lit: &syn::Lit) -> Result<u32, ParseError> {
        if self.is_char {
            if let syn::Lit::Char(lit) = lit {
                Ok(lit.value() as u32)
            } else {
                Err(ParseError::UnsupportedLiteralType(lit.to_token_stream()))
            }
        } else if self.is_u8 {
            if let syn::Lit::Byte(lit) = lit {
                Ok(lit.value() as u32)
            } else {
                Err(ParseError::UnsupportedLiteralType(lit.to_token_stream()))
            }
        } else {
            Err(ParseError::UnsupportedLiteralType(lit.to_token_stream()))
        }
    }
    pub(crate) fn get_terminal_indices_from_char_range(
        &self,
        start: u32,
        last: u32,
    ) -> impl Iterator<Item = usize> + '_ {
        let start = unsafe { char::from_u32_unchecked(start) };
        let last = unsafe { char::from_u32_unchecked(last) };
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
    pub fn from_grammar_args(grammar_args: GrammarArgs) -> Result<Self, ParseError> {
        let module_prefix =
            if let Some(module_prefix) = grammar_args.module_prefix.into_iter().next() {
                module_prefix.1
            } else {
                quote! { ::rusty_lr }
            };
        let error_typename =
            if let Some(error_typename) = grammar_args.error_typename.into_iter().next() {
                error_typename.1
            } else {
                quote! { #module_prefix::DefaultReduceActionError }
            };
        let mut grammar = Grammar {
            module_prefix,
            token_typename: grammar_args.token_typename.into_iter().next().unwrap().1,
            userdata_typename: grammar_args
                .userdata_typename
                .into_iter()
                .next()
                .map(|(_, stream)| stream)
                .unwrap_or(quote! { () }),

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
            filter: grammar_args.filter,

            location_typename: grammar_args.location_typename,
            error_precedence: None,
            custom_reduce_actions: Vec::new(),
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
                    if let Some(ref prec_ident) = rule.prec {
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
                    if let Some(ref prec_ident) = rule.prec {
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
                    grammar_args.terminals.first().unwrap().0.span(),
                ));
            }
        } else {
            // add %token terminals
            for (index, (ident, token_expr)) in grammar_args.terminals.into_iter().enumerate() {
                // check reserved name
                utils::check_reserved_name(&ident)?;
                let name = ident.into();

                // check duplicate
                if let Some((k, _)) = grammar.terminals_index.get_key_value(&name) {
                    return Err(ParseError::MultipleTokenDefinition(
                        k.ident().unwrap().clone(),
                        name.into_ident().unwrap(),
                    ));
                }

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
            let ident = Ident::new(utils::OTHERS_TERMINAL_NAME, Span::call_site());
            let name: TerminalName = ident.into();

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
            // check reserved name
            utils::check_reserved_name(&rules_arg.name)?;

            let nonterminal = NonTerminalInfo {
                name: rules_arg.name.clone(),
                pretty_name: rules_arg.name.to_string(),
                ruletype: rules_arg.typename.clone(),
                rules: Vec::new(), // production rules will be added later
                regex_span: None,
                trace: false,
                protected: false,
                nonterm_type: None,
            };

            grammar.nonterminals.push(nonterminal);

            // check duplicate
            if let Some(old) = grammar
                .nonterminals_index
                .insert(rules_arg.name.clone(), rule_idx)
            {
                return Err(ParseError::MultipleRuleDefinition(
                    grammar.nonterminals[old].name.clone(),
                    rules_arg.name.clone(),
                ));
            }
        }

        // precedence orders
        for (level, (span, reduce_type, items)) in grammar_args.precedences.into_iter().enumerate()
        {
            grammar.precedence_types.push((reduce_type, span)); // set i'th level's precedence type
            for item in items {
                let span = item.span();
                let itemu = item.clone().into_ident_or_u32(&grammar)?;
                match &itemu {
                    IdentOrU32::Ident(ident) => {
                        if let Some(&term_idx) = grammar.terminals_index.get(&ident.clone().into())
                        {
                            grammar.terminals[term_idx].precedence = Some((level, span));
                        } else if ident == utils::ERROR_NAME {
                            grammar.error_precedence = Some(level);
                        }
                    }
                    &IdentOrU32::U32(ch) => {
                        let ch = unsafe { char::from_u32_unchecked(ch) };
                        if let Some(&term_idx) = grammar
                            .terminals_index
                            .get(&TerminalName::CharRange(ch, ch))
                        {
                            grammar.terminals[term_idx].precedence = Some((level, span));
                        } else {
                            unreachable!("unexpected char type in precedence order");
                        }
                    }
                }
                if let Some(old) = grammar.precedence_levels.insert(itemu, (level, span)) {
                    return Err(ParseError::MultiplePrecedenceOrderDefinition {
                        cur: item,
                        old: old.1,
                    });
                }
            }
        }

        // pattern map for auto-generated rules
        let mut pattern_map: HashMap<Pattern, PatternToToken> = HashMap::default();

        // insert production rules & auto-generated rules from regex pattern
        for (rule_idx, rules) in grammar_args.rules.into_iter().enumerate() {
            let mut rule_lines = Vec::new();
            for rule in rules.rule_lines.into_iter() {
                let mut tokens = Vec::with_capacity(rule.tokens.len());
                let mut patterns = Vec::with_capacity(rule.tokens.len());
                for (mapto, pattern) in rule.tokens.into_iter() {
                    let (begin_span, end_span) = pattern.span_pair();
                    let pattern = pattern.into_pattern(&mut grammar, false)?;
                    let pattern_rule =
                        pattern.to_token(&mut grammar, &mut pattern_map, (begin_span, end_span))?;

                    tokens.push(TokenMapped {
                        token: pattern_rule.token,
                        mapto: mapto.or_else(|| pattern_rule.mapto.clone()),
                        begin_span,
                        end_span,
                        reduce_action_chains: Vec::new(),
                    });
                    patterns.push(pattern_rule);
                }

                // parse %prec definition
                let prec = if let Some(prec) = rule.prec {
                    let span = prec.span();
                    let precu = prec.clone().into_ident_or_u32(&grammar)?;
                    // check if this ident exists in tokens
                    let from_token = match &precu {
                        IdentOrU32::Ident(ident) => {
                            let mut prec = None;
                            for (idx, token) in tokens.iter().enumerate() {
                                if token.mapto.as_ref() == Some(ident) {
                                    prec = Some(idx);
                                    break;
                                }
                            }
                            prec
                        }
                        IdentOrU32::U32(_) => None,
                    };
                    if let Some(from_token) = from_token {
                        // check if from_token'th token is terminal symbol
                        if let Token::Term(term) = tokens[from_token].token {
                            match term {
                                TerminalSymbol::Term(term_idx) => {
                                    if let Some((level, _)) = grammar.terminals[term_idx].precedence
                                    {
                                        let span = tokens[from_token].begin_span;
                                        Some((Precedence::Fixed(level), span))
                                    } else {
                                        return Err(ParseError::PrecedenceNotDefined(prec));
                                    }
                                }
                                TerminalSymbol::Error => {
                                    if let Some(error_prec) = grammar.error_precedence {
                                        Some((Precedence::Fixed(error_prec), span))
                                    } else {
                                        return Err(ParseError::PrecedenceNotDefined(prec));
                                    }
                                }
                                TerminalSymbol::Eof => {
                                    unreachable!("eof token cannot be used in %prec, nor cannot be used in production rules")
                                }
                            }
                        } else {
                            Some((Precedence::Dynamic(from_token), span))
                        }
                    } else if let Some(&(level, _)) = grammar.precedence_levels.get(&precu) {
                        Some((Precedence::Fixed(level), span))
                    } else {
                        return Err(ParseError::PrecedenceNotDefined(prec));
                    }
                } else {
                    // not defined,
                    // choose the last terminal symbol that has precedence
                    let mut op = None;
                    for token in tokens.iter().rev() {
                        if let Token::Term(term) = token.token {
                            match term {
                                TerminalSymbol::Term(term_idx) => {
                                    if let Some((level, _)) = grammar.terminals[term_idx].precedence
                                    {
                                        op = Some((Precedence::Fixed(level), token.end_span));
                                        break;
                                    }
                                }
                                TerminalSymbol::Error => {
                                    if let Some(error_prec) = grammar.error_precedence {
                                        op = Some((Precedence::Fixed(error_prec), token.end_span));
                                        break;
                                    }
                                }
                                TerminalSymbol::Eof => {
                                    unreachable!("eof token cannot be used in %prec, nor cannot be used in production rules")
                                }
                            }
                        }
                    }
                    op
                };

                // parse %dprec literal value
                let dprec = if let Some(dprec) = rule.dprec {
                    let lit = match syn::parse2::<syn::Lit>(dprec.to_token_stream()) {
                        Ok(lit) => lit,
                        Err(_) => {
                            unreachable!("dprec parse error");
                        }
                    };
                    let val = match lit {
                        syn::Lit::Int(lit) => match lit.base10_parse::<usize>() {
                            Ok(val) => val,
                            Err(_) => {
                                return Err(ParseError::OnlyUsizeLiteral(lit.span()));
                            }
                        },
                        _ => {
                            return Err(ParseError::OnlyUsizeLiteral(dprec.span()));
                        }
                    };
                    Some((val, dprec.span()))
                } else {
                    None
                };

                // rename all '@var_name' to '__rustylr_location_{var_name}'
                // if reduce_action is not defined, check if it can be auto-generated
                let reduce_action = if let Some(reduce_action) = rule.reduce_action {
                    fn rename_tokenstream_recursive(ts: TokenStream) -> TokenStream {
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
                                            _ => {
                                                // just a punct, no ident after it
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
                                        rename_tokenstream_recursive(group.stream()),
                                    );
                                    let new_group = proc_macro2::TokenTree::Group(new_group);
                                    new_ts.extend([new_group]);
                                }
                                token => {
                                    new_ts.extend([token]);
                                }
                            }
                        }
                        new_ts
                    }

                    let new_reduce_action = rename_tokenstream_recursive(reduce_action);

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
                        if let Some(unique_idx) = tokens
                            .iter()
                            .enumerate()
                            .rev()
                            .find(move |(_, token)| token.mapto.as_ref() == Some(&unique_ident))
                            .map(|(idx, _)| idx)
                        {
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
                            let span = if tokens.is_empty() {
                                (rule.separator_span, rule.separator_span)
                            } else {
                                let first = rule.separator_span;
                                let last = tokens.last().unwrap().end_span;
                                (first, last)
                            };

                            return Err(ParseError::RuleTypeDefinedButActionNotDefined {
                                name: rules.name.clone(),
                                span,
                            });
                        }
                    } else {
                        None
                    }
                };

                if let Some(ReduceAction::Identity(idx)) = &reduce_action {
                    if tokens[*idx].mapto.is_none() {
                        tokens[*idx].mapto = Some(format_ident!("__rustylr_token{}", idx));
                    }
                }

                // check for duplicated token.mapto
                // if there are variable with same name, the last one will be used (by shadowing)
                // so set the front one to `None`
                for right_token in 0..tokens.len() {
                    if let Some(right_mapto) = tokens[right_token].mapto.as_ref().cloned() {
                        for left_token in 0..right_token {
                            if tokens[left_token].mapto.as_ref() == Some(&right_mapto) {
                                tokens[left_token].mapto =
                                    Some(format_ident!("__rustylr_token{}", left_token));
                            }
                        }
                    }
                }

                rule_lines.push(Rule {
                    tokens,
                    reduce_action,
                    separator_span: rule.separator_span,
                    lookaheads: None,
                    prec,
                    dprec,
                    is_used: true,
                });
            }

            // production rules set here
            grammar.nonterminals[rule_idx].rules = rule_lines;
        }
        drop(pattern_map);

        // check for nonterminals in %prec,
        // all production rules in that nonterminal must have precedence defined.
        let mut nonterm_prec_candidates: Vec<BTreeSet<Option<usize>>> =
            vec![BTreeSet::new(); grammar.nonterminals.len()];
        loop {
            let mut changed = false;
            for (nonterm_idx, nonterm) in grammar.nonterminals.iter().enumerate() {
                for rule in &nonterm.rules {
                    match rule.prec {
                        Some(prec) => match prec.0 {
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
                    if let Precedence::Dynamic(token_idx) = prec.0 {
                        if let Token::NonTerm(token_nonterm_idx) = rule.tokens[token_idx].token {
                            let target_candidates = &nonterm_prec_candidates[token_nonterm_idx];
                            if target_candidates.contains(&None) {
                                // TODO
                                // no need to be an error on GLR parser?
                                return Err(ParseError::NonTerminalPrecedenceNotDefined(
                                    prec.1,
                                    token_nonterm_idx,
                                ));
                            }

                            if target_candidates.len() == 1 {
                                let fixed = target_candidates.iter().next().unwrap().unwrap();
                                prec.0 = Precedence::Fixed(fixed);
                            }
                        }
                    }
                }
            }
        }

        // check start rule is valid
        if !grammar
            .nonterminals_index
            .contains_key(&grammar.start_rule_name)
        {
            return Err(ParseError::StartNonTerminalNotDefined(
                grammar.start_rule_name.clone(),
            ));
        }

        // insert augmented rule
        {
            let augmented_ident = Ident::new(utils::AUGMENTED_NAME, Span::call_site());
            let start_idx = grammar
                .nonterminals_index
                .get(&grammar.start_rule_name)
                .unwrap();
            let augmented_rule = Rule {
                tokens: vec![
                    TokenMapped {
                        token: Token::NonTerm(*start_idx),
                        mapto: None,
                        begin_span: Span::call_site(),
                        end_span: Span::call_site(),
                        reduce_action_chains: Vec::new(),
                    },
                    TokenMapped {
                        token: Token::Term(TerminalSymbol::Eof),
                        mapto: None,
                        begin_span: Span::call_site(),
                        end_span: Span::call_site(),
                        reduce_action_chains: Vec::new(),
                    },
                ],
                reduce_action: None,
                separator_span: Span::call_site(),
                lookaheads: None,
                prec: None,
                dprec: None,
                is_used: true,
            };
            let nonterminal_info = NonTerminalInfo {
                name: augmented_ident.clone(),
                pretty_name: utils::AUGMENTED_NAME.to_string(),
                ruletype: None,
                regex_span: None,
                rules: vec![augmented_rule],
                trace: false,
                protected: true,
                nonterm_type: Some(rusty_lr_core::parser::nonterminal::NonTerminalType::Augmented),
            };
            // start rule is protected
            grammar.nonterminals[*start_idx].protected = true;

            let augmented_idx = grammar.nonterminals.len();
            grammar.nonterminals.push(nonterminal_info);
            grammar
                .nonterminals_index
                .insert(augmented_ident, augmented_idx);
        }

        // set `%trace`
        for trace in grammar_args.traces.into_iter() {
            if let Some(&nonterm_idx) = grammar.nonterminals_index.get(&trace) {
                grammar.nonterminals[nonterm_idx].trace = true;
                grammar.nonterminals[nonterm_idx].protected = true;
            } else {
                return Err(ParseError::NonTerminalNotDefined(trace));
                // no such rule
            }
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
                if let Some(lookaheads) = &rule.lookaheads {
                    if lookaheads.contains(&grammar.other_terminal_index) {
                        grammar.other_used = true;
                    }
                }
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
            let level = term.precedence.map(|(op, _)| op);
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
                        let lookaheads = &rule.lookaheads;
                        let prec = rule.prec.map(|(op, _)| op);
                        let dprec = rule.dprec.map_or(0, |(val, _)| val);
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
                                lookaheads,
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
                        let span = nonterm.name.span();
                        removed_rules_diag.push(OptimizeRemove::NonTermNotUsed(span));
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
                    //  - lookaheads in the rule
                    if let Some(lookaheads) = &mut rule.lookaheads {
                        let new_lookaheads = std::mem::take(lookaheads)
                            .into_iter()
                            .map(|old_class| {
                                let new_class = old_class_to_new_class[old_class];
                                if new_class == self.other_terminal_class_id {
                                    other_was_used = true;
                                }
                                new_class
                            })
                            .collect();
                        *lookaheads = new_lookaheads;
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
                if rule.reduce_action_contains_ident(&format_ident!(
                    "{}",
                    utils::LOOKAHEAD_PARAMETER_NAME
                )) {
                    continue;
                }
                // if this rule has custom reduce action, save it
                let output_type = nonterm.ruletype.clone();
                let mapto = &rule.tokens[0].mapto;
                let location_mapto = if let Some(mapto) = mapto {
                    let location_varname = utils::location_variable_name(mapto);
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
                        if rule.reduce_action_contains_ident(&mapto) {
                            Some((mapto.clone(), ruletype))
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
                let diag = OptimizeRemove::SingleNonTerminalRule(rule, nonterm.name.span());
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
            let start_rule_idx = *self.nonterminals_index.get(&self.start_rule_name).unwrap();
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
                                rule.reduce_action_contains_ident(mapto)
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
                            if rule.reduce_action_contains_ident(mapto) {
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
            self.nonterminals_index.insert(nonterm.name.clone(), idx);
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
            if let Some((level, _)) = &term_info.precedence {
                let class = self.terminal_class_id[term_idx];
                if !grammar.add_precedence(TerminalSymbol::Term(class), *level) {
                    unreachable!("set_reduce_type error");
                }
            }
        }
        grammar.set_precedence_types(self.precedence_types.iter().map(|(op, _)| *op).collect());

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
                    rule.lookaheads.as_ref().map(|lookaheads| {
                        lookaheads
                            .iter()
                            .map(|&t| TerminalSymbol::Term(t))
                            .collect()
                    }),
                    rule.prec.map(|(op, _)| op),
                    rule.dprec.map_or(0, |(p, _)| p),
                );
            }
        }

        grammar
    }

    pub fn build_grammar(
        &mut self,
    ) -> rusty_lr_core::builder::DiagnosticCollector<TerminalSymbol<usize>> {
        let augmented_idx = *self
            .nonterminals_index
            .get(&Ident::new(utils::AUGMENTED_NAME, Span::call_site()))
            .unwrap();
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
                .to_map()
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

            let nonterm_idx = self.nonterminals_index[&nonterm.name];
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
            for rules in state.reduce_map.to_map().iter().map(|(_, r)| r) {
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

        // if this state is convertible to LR(0) table, (e.g. table without lookahead symbols),
        // convert it
        for state in &mut new_states {
            use std::collections::BTreeSet;
            // reduce map must consist with unique ruleset
            let rules: BTreeSet<_> = state
                .reduce_map
                .to_map()
                .iter()
                .map(|(_, rules)| rules)
                .collect();
            if rules.len() != 1 {
                continue;
            }

            let shift_keys: BTreeSet<_> = state
                .shift_goto_map_term
                .iter()
                .map(|(term, _)| term)
                .collect();
            let reduce_keys: BTreeSet<_> = state
                .reduce_map
                .to_map()
                .iter()
                .map(|(term, _)| term)
                .collect();

            // keys of shiftmap and reducemap must not overlap
            if !shift_keys.is_disjoint(&reduce_keys) {
                continue;
            }

            let rules = rules.into_iter().next().unwrap().clone();
            state.reduce_map = rusty_lr_core::parser::state::ReduceMap::Value(rules);
        }

        self.states = new_states;

        collector
    }
}
