use proc_macro2::TokenStream;
use rusty_lr_core::Symbol;
use rusty_lr_core::TerminalSymbol;
use rusty_lr_core::hash::HashMap;
use rusty_lr_core::production::Precedence;
use std::collections::BTreeSet;

use crate::error::Info;
use crate::error::ParseError;
use crate::error::Warning;
use crate::nonterminal_info::CustomReduceAction;
use crate::nonterminal_info::NonTerminalInfo;
use crate::nonterminal_info::ReduceAction;
use crate::nonterminal_info::Rule;
use crate::parser::args::IdentOrLiteral;
use crate::parser::args::TableLayout;
use crate::parser::location::Located;
use crate::parser::location::Location;
use crate::rangeresolver::RangeResolver;
use crate::symbol::MappedSymbol;
use crate::terminal_info::TerminalInfo;
use crate::terminal_info::TerminalName;
use crate::utils;

mod diagnostics;
mod emit;
mod input;
mod optimize;
#[cfg(test)]
mod tests;

pub struct TerminalClassDefinition {
    pub terminals: Vec<Terminal>,
    /// counter for only terminal clasas (have more than 1 terminal)
    /// dummy if it is single-terminal class
    pub multiterm_counter: usize,

    /// compressed ranges, only if %tokentype is char or u8
    pub ranges: Vec<(u32, u32)>,

    /// Whether this class's data was used in any reduce action
    pub data_used: bool,
}

/// type alias just for readability
pub type TerminalClass = usize;
pub type Terminal = usize;

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
    pub(crate) start_rule_names: Vec<Located<String>>,

    pub terminals: Vec<TerminalInfo>,
    /// ident -> index map for terminals
    pub terminals_index: HashMap<TerminalName, Terminal>,

    /// %left, %right, or %precedence for each precedence level
    pub precedence_types: Vec<Located<Option<rusty_lr_core::production::Associativity>>>,

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

    /// Whether grammar and table optimization passes are enabled.
    /// This is false when `%nooptim` is used.
    pub optimize: bool,
    pub builder: rusty_lr_core::builder::Grammar<TerminalSymbol<TerminalClass>, usize>,
    pub states:
        Vec<rusty_lr_core::parser::state::IntermediateState<TerminalSymbol<TerminalClass>, usize>>,

    /// set of terminals for each terminal class
    pub terminal_classes: Vec<TerminalClassDefinition>,
    /// id of teminal class for each terminal
    pub terminal_class_id: Vec<TerminalClass>,
    /// class id for terminal that does not belong to any class
    pub other_terminal_class_id: TerminalClass,

    pub other_used: bool,
    pub error_used: bool,

    /// terminal index of other_terminals
    /// `other_terminal` can be only used by [^ term ...] pattern,
    /// to indicate *other terminals* not defined in this grammar.
    pub other_terminal_index: Terminal,

    /// character range resolver;
    pub range_resolver: RangeResolver,

    /// in the generated parser, the dense table `Vec` will be used instead of the sparse table `HashMap`.
    pub emit_dense: bool,
    pub layout: TableLayout,
    pub dense_limit: usize,

    /// type for location
    pub location_typename: TokenStream,

    /// precedence level of error token
    pub error_precedence: Option<usize>,

    /// See `MappedSymbol::reduce_action_chains` for more details.
    /// This is actual body of each reduce action in the chain.
    pub custom_reduce_actions: Vec<CustomSingleReduceAction>,

    pub span_manager: crate::parser::location::SpanManager,

    pub warnings: Vec<Warning>,
    pub infos: Vec<Info>,
    pub allowed_diagnostics: HashMap<String, Vec<Option<ResolvedAllowTarget>>>,
}

#[derive(Debug, Clone)]
pub enum ResolvedAllowTarget {
    Name(String),
    Terminals(BTreeSet<Terminal>),
}

impl Grammar {
    fn rule_id_to_indices(&self, mut rule_idx: usize) -> Option<(usize, usize)> {
        // Rule ids are assigned by walking non-terminals in storage order; keep the inverse
        // mapping local so table analyses can talk back to the editable grammar.
        for (nonterm_idx, nonterm) in self.nonterminals.iter().enumerate() {
            if rule_idx < nonterm.rules.len() {
                return Some((nonterm_idx, rule_idx));
            }
            rule_idx -= nonterm.rules.len();
        }
        None
    }

    fn create_builder_from_current_rules(
        &self,
    ) -> rusty_lr_core::builder::Grammar<TerminalSymbol<TerminalClass>, usize> {
        let mut grammar: rusty_lr_core::builder::Grammar<TerminalSymbol<TerminalClass>, usize> =
            rusty_lr_core::builder::Grammar::new();

        for (term_idx, term_info) in self.terminals.iter().enumerate() {
            if let Some(level) = &term_info.precedence {
                let class = self.terminal_class_id[term_idx];
                if !grammar.add_precedence(TerminalSymbol::Terminal(class), *level.value()) {
                    unreachable!("set_reduce_type error");
                }
            }
        }
        if let Some(level) = self.error_precedence {
            if !grammar.add_precedence(TerminalSymbol::Error, level) {
                unreachable!("set_reduce_type error");
            }
        }
        grammar.set_precedence_types(self.precedence_types.iter().map(|op| *op.value()).collect());

        for (nonterm_id, nonterm) in self.nonterminals.iter().enumerate() {
            for rule in nonterm.rules.iter() {
                let tokens = rule
                    .tokens
                    .iter()
                    .map(|token_mapped| token_mapped.symbol)
                    .collect();

                grammar.add_rule(
                    nonterm_id,
                    tokens,
                    rule.prec
                        .map(Located::into_value)
                        .unwrap_or(Precedence::None),
                    rule.dprec.map_or(0, Located::into_value),
                );
            }
        }

        grammar
    }

    fn is_optional_epsilon_self_loop_candidate(
        &self,
        nonterm_idx: usize,
        local_rule_idx: usize,
    ) -> bool {
        let nonterm = &self.nonterminals[nonterm_idx];
        let rule = &nonterm.rules[local_rule_idx];

        // Only RustyLR-created optional symbols have known branch semantics; user-written
        // nullable productions may contain intentional actions and must not be rewritten.
        if nonterm.nonterm_type
            != Some(rusty_lr_core::parser::nonterminal::NonTerminalType::Optional)
            || !nonterm.is_auto_generated()
            || nonterm.is_protected()
        {
            return false;
        }

        // The dangerous closure edge must come from the empty branch itself.  Non-empty
        // branches consume input and therefore cannot be the zero-progress cycle we are fixing.
        rule.tokens.is_empty()
    }

    fn rule_uses_positional_bison_variables(rule: &Rule) -> bool {
        // Expanding an optional occurrence changes token positions.  Named bindings remain stable,
        // but already-rewritten `$n` or `@n` identifiers would point at the wrong symbol.
        (0..rule.tokens.len()).any(|idx| {
            rule.reduce_action_contains_ident(&format!("__rustylr_data_{idx}"))
                || rule.reduce_action_contains_ident(&format!("__rustylr_location_{idx}"))
        })
    }

    fn can_expand_optional_occurrence(rule: &Rule, token_idx: usize) -> bool {
        let token = &rule.tokens[token_idx];

        // Existing reduce-action chains mean another optimization has semantic work attached to
        // this exact symbol occurrence, so leave it intact instead of guessing how to distribute it.
        if !token.reduce_action_chains.is_empty() {
            return false;
        }

        // Identity actions are positional by definition; rewriting their RHS shape would require
        // remapping the identity index, so keep this conservative pass focused on custom/empty actions.
        if matches!(rule.reduce_action, Some(ReduceAction::Identity(_))) {
            return false;
        }

        if Self::rule_uses_positional_bison_variables(rule) {
            return false;
        }

        if let Some(mapto) = &token.mapto {
            // If the optional's value or location is observed, replacing it with a present/absent
            // structural branch would change the user's reduce action contract.
            if rule.reduce_action_contains_ident(mapto.value().as_str())
                || rule.reduce_action_contains_ident(&utils::location_variable_name(
                    mapto.value().as_str(),
                ))
            {
                return false;
            }
        }

        true
    }

    fn mapped_symbol_pretty_name(&self, token: &MappedSymbol) -> String {
        match token.symbol {
            Symbol::Terminal(term) => self.class_pretty_name_list(term, 5),
            Symbol::NonTerminal(nonterm) => self.nonterm_pretty_name(nonterm),
        }
    }

    fn production_pretty_name(&self, nonterm_idx: usize, tokens: &[MappedSymbol]) -> String {
        // Keep this close to the generated grammar comment format so the diagnostic explains the
        // exact table-level rewrite without exposing internal rule ids.
        let lhs = self.nonterm_pretty_name(nonterm_idx);
        if tokens.is_empty() {
            format!("{lhs} ->")
        } else {
            let rhs = tokens
                .iter()
                .map(|token| self.mapped_symbol_pretty_name(token))
                .collect::<Vec<_>>()
                .join(" ");
            format!("{lhs} -> {rhs}")
        }
    }

    fn expand_optional_occurrences(&mut self, optional_idx: usize) -> bool {
        let Some(present_token) = self.nonterminals[optional_idx]
            .rules
            .iter()
            .find(|rule| rule.tokens.len() == 1)
            .map(|rule| rule.tokens[0].clone())
        else {
            return false;
        };

        let mut changed = false;
        for nonterm_idx in 0..self.nonterminals.len() {
            if nonterm_idx == optional_idx {
                continue;
            }

            let old_rules = std::mem::take(&mut self.nonterminals[nonterm_idx].rules);
            let mut new_rules = Vec::with_capacity(old_rules.len());

            for rule in old_rules {
                if !rule
                    .tokens
                    .iter()
                    .any(|token| token.symbol == Symbol::NonTerminal(optional_idx))
                {
                    new_rules.push(rule);
                    continue;
                }

                let mut variants = vec![Rule {
                    tokens: Vec::with_capacity(rule.tokens.len()),
                    ..rule.clone()
                }];
                let mut expanded_this_rule = false;
                let mut can_keep_expanding = true;

                for (token_idx, token) in rule.tokens.iter().enumerate() {
                    if token.symbol != Symbol::NonTerminal(optional_idx) {
                        for variant in &mut variants {
                            variant.tokens.push(token.clone());
                        }
                        continue;
                    }

                    if !Self::can_expand_optional_occurrence(&rule, token_idx) {
                        for variant in &mut variants {
                            variant.tokens.push(token.clone());
                        }
                        can_keep_expanding = false;
                        continue;
                    }

                    // Split the occurrence into its absent branch and its present branch.  The
                    // parent action does not observe the optional value, so the present branch
                    // carries only the underlying grammar symbol.
                    let mut present = present_token.clone();
                    present.mapto = None;
                    present.location = token.location;

                    let mut present_variants = variants.clone();
                    for variant in &mut present_variants {
                        variant.tokens.push(present.clone());
                    }
                    variants.extend(present_variants);
                    expanded_this_rule = true;
                }

                if expanded_this_rule && can_keep_expanding {
                    changed = true;
                    self.infos.push(Info::GlrOptionalExpanded {
                        nonterm_name: self.nonterminals[nonterm_idx].name.clone(),
                        rule_location: rule.location(),
                        before: self.production_pretty_name(nonterm_idx, &rule.tokens),
                        after: variants
                            .iter()
                            .map(|variant| {
                                self.production_pretty_name(nonterm_idx, &variant.tokens)
                            })
                            .collect(),
                    });
                    new_rules.extend(variants);
                } else {
                    new_rules.push(rule);
                }
            }

            self.nonterminals[nonterm_idx].rules = new_rules;
        }

        let still_referenced = self.nonterminals.iter().any(|nonterm| {
            nonterm.rules.iter().any(|rule| {
                rule.tokens
                    .iter()
                    .any(|token| token.symbol == Symbol::NonTerminal(optional_idx))
            })
        });
        if changed && !still_referenced {
            // Once every occurrence has been structurally expanded, the generated helper has no
            // remaining parser role and can be left empty for the normal cleanup path.
            self.nonterminals[optional_idx].rules.clear();
        }

        changed
    }

    fn optional_expansion_error_context(
        &self,
        optional_idx: usize,
    ) -> Option<(Location, String, String)> {
        for nonterm in &self.nonterminals {
            for rule in &nonterm.rules {
                for (token_idx, token) in rule.tokens.iter().enumerate() {
                    if token.symbol != Symbol::NonTerminal(optional_idx) {
                        continue;
                    }

                    let production = self.production_pretty_name(
                        *self.nonterminals_index.get(nonterm.name.value()).unwrap(),
                        &rule.tokens,
                    );
                    let help = format!(
                        "Rewrite `{production}` explicitly into one production without the optional branch and one production with the optional branch present."
                    );

                    if !token.reduce_action_chains.is_empty() {
                        return Some((
                            rule.location(),
                            format!(
                                "`{production}` has reduce-action chains attached to the optional occurrence"
                            ),
                            help,
                        ));
                    }
                    if matches!(rule.reduce_action, Some(ReduceAction::Identity(_))) {
                        return Some((
                            rule.location(),
                            format!(
                                "`{production}` uses an identity reduce action whose positional result would change"
                            ),
                            help,
                        ));
                    }
                    if Self::rule_uses_positional_bison_variables(rule) {
                        return Some((
                            rule.location(),
                            format!(
                                "`{production}` uses positional `$n` or `@n` references, so expansion would change token numbering"
                            ),
                            help,
                        ));
                    }
                    if let Some(mapto) = &token.mapto {
                        if rule.reduce_action_contains_ident(mapto.value().as_str()) {
                            return Some((
                                rule.location(),
                                format!(
                                    "`{production}` reads the optional value `{}` in its reduce action",
                                    mapto.value()
                                ),
                                help,
                            ));
                        }
                        if rule.reduce_action_contains_ident(&utils::location_variable_name(
                            mapto.value().as_str(),
                        )) {
                            return Some((
                                rule.location(),
                                format!(
                                    "`{production}` reads the optional location `@{}` in its reduce action",
                                    mapto.value()
                                ),
                                help,
                            ));
                        }
                    }

                    if !Self::can_expand_optional_occurrence(rule, token_idx) {
                        return Some((
                            rule.location(),
                            format!("`{production}` cannot be expanded without changing semantics"),
                            help,
                        ));
                    }
                }
            }
        }
        None
    }

    fn glr_nullable_cycle_error(
        &self,
        state_idx: usize,
        nonterm_idx: usize,
        local_rule_idx: usize,
    ) -> ParseError {
        let nonterm = &self.nonterminals[nonterm_idx];
        let rule = &nonterm.rules[local_rule_idx];
        let nullable_rule = self.production_pretty_name(nonterm_idx, &rule.tokens);

        let default_location = nonterm.root_location.unwrap_or_else(|| rule.location());
        let (location, reason, help) = match nonterm.nonterm_type {
            Some(rusty_lr_core::parser::nonterminal::NonTerminalType::Optional) => {
                self.optional_expansion_error_context(nonterm_idx).unwrap_or_else(|| {
                    (
                        default_location,
                        format!(
                            "`{}` is an optional helper, but no safe occurrence remained for automatic expansion",
                            nonterm.pretty_name
                        ),
                        "Rewrite the optional occurrence explicitly as separate absent and present productions.".to_string(),
                    )
                })
            }
            Some(rusty_lr_core::parser::nonterminal::NonTerminalType::Star) => (
                default_location,
                format!(
                    "`{}` is a zero-or-more repetition helper; `*` denotes an unbounded family of productions and cannot be expanded into a finite replacement here",
                    nonterm.pretty_name
                ),
                "Rewrite the grammar with an explicit non-empty list helper (`P+`) or factor the nullable repetition out of the left-recursive GLR position.".to_string(),
            ),
            _ if nonterm.is_auto_generated() => (
                default_location,
                format!(
                    "`{}` is an auto-generated nullable helper that RustyLR does not know how to expand safely",
                    nonterm.pretty_name
                ),
                "Rewrite the surrounding grammar manually so the nullable helper is not reduced back to the same GLR state without consuming input.".to_string(),
            ),
            _ => (
                rule.location(),
                format!(
                    "`{}` is a user-defined nullable production; RustyLR cannot remove or duplicate its semantic behavior automatically",
                    nullable_rule
                ),
                "Rewrite the grammar manually so this nullable production is not in a GLR reduce cycle, or make the recursive path consume a terminal before returning to the same state.".to_string(),
            ),
        };

        ParseError::GlrNullableReduceCycle {
            location,
            nullable_rule,
            state: state_idx,
            reason,
            help,
        }
    }

    fn find_glr_nullable_reduce_cycle_error(
        &self,
        states: &[rusty_lr_core::builder::State<TerminalSymbol<TerminalClass>, usize>],
    ) -> Option<ParseError> {
        for (state_idx, state) in states.iter().enumerate() {
            for reduce_rules in state.reduce_map.values() {
                for &rule_idx in reduce_rules {
                    let Some((nonterm_idx, local_rule_idx)) = self.rule_id_to_indices(rule_idx)
                    else {
                        continue;
                    };
                    if !self.nonterminals[nonterm_idx].rules[local_rule_idx]
                        .tokens
                        .is_empty()
                    {
                        continue;
                    }
                    if state
                        .shift_goto_map_nonterm
                        .get(&nonterm_idx)
                        .is_some_and(|target| *target == state_idx)
                    {
                        return Some(self.glr_nullable_cycle_error(
                            state_idx,
                            nonterm_idx,
                            local_rule_idx,
                        ));
                    }
                }
            }
        }
        None
    }

    fn expand_glr_epsilon_self_loop_optionals(&mut self) -> Result<(), ParseError> {
        if !self.glr {
            return Ok(());
        }

        // A rewrite can reveal another nullable self-loop through a different optional helper, so
        // iterate to a fixed point while bounding the pass against accidental expansion churn.
        for _ in 0..self.nonterminals.len() {
            let mut builder = self.create_builder_from_current_rules();
            let augmented_idx = *self.nonterminals_index.get(utils::AUGMENTED_NAME).unwrap();
            let mut collector = rusty_lr_core::builder::DiagnosticCollector::new(true);
            let states = if self.lalr {
                builder.build_lalr(augmented_idx, &mut collector)
            } else {
                builder.build(augmented_idx, &mut collector)
            }
            .unwrap_or_else(|err| {
                unreachable!(
                    "Error building grammar during GLR nullable expansion: {:?}",
                    err
                )
            })
            .states;

            let mut rewrite_target = None;
            'states: for (state_idx, state) in states.iter().enumerate() {
                for reduce_rules in state.reduce_map.values() {
                    for &rule_idx in reduce_rules {
                        let Some((nonterm_idx, local_rule_idx)) = self.rule_id_to_indices(rule_idx)
                        else {
                            continue;
                        };
                        if !self
                            .is_optional_epsilon_self_loop_candidate(nonterm_idx, local_rule_idx)
                        {
                            continue;
                        }
                        if state
                            .shift_goto_map_nonterm
                            .iter()
                            .any(|(nonterm, target)| {
                                *nonterm == nonterm_idx && *target == state_idx
                            })
                        {
                            rewrite_target = Some(nonterm_idx);
                            break 'states;
                        }
                    }
                }
            }

            let Some(optional_idx) = rewrite_target else {
                break;
            };
            if !self.expand_optional_occurrences(optional_idx) {
                break;
            }
        }

        let mut builder = self.create_builder_from_current_rules();
        let augmented_idx = *self.nonterminals_index.get(utils::AUGMENTED_NAME).unwrap();
        let mut collector = rusty_lr_core::builder::DiagnosticCollector::new(true);
        let states = if self.lalr {
            builder.build_lalr(augmented_idx, &mut collector)
        } else {
            builder.build(augmented_idx, &mut collector)
        }
        .unwrap_or_else(|err| {
            unreachable!(
                "Error building grammar during GLR nullable cycle validation: {:?}",
                err
            )
        })
        .states;

        if let Some(err) = self.find_glr_nullable_reduce_cycle_error(&states) {
            return Err(err);
        }

        Ok(())
    }

    /// Resolved Rust type for `%tokentype`, after substitutions such as `$tokentype`
    /// and storage modifiers such as `box` have been stripped.
    pub fn token_type(&self) -> &TokenStream {
        &self.token_typename
    }

    /// Whether terminal values are stored as `Box<%tokentype>` in the generated
    /// parser's data enum.
    pub fn token_type_boxed(&self) -> bool {
        self.is_tokentype_boxed
    }

    /// Resolved Rust type for a non-terminal by name, plus whether it is boxed
    /// in the generated parser's data enum.
    pub fn nonterminal_type(&self, name: &str) -> Option<(Option<&TokenStream>, bool)> {
        let index = self.nonterminals_index.get(name)?;
        let nonterminal = &self.nonterminals[*index];
        Some((nonterminal.ruletype.as_ref(), nonterminal.ruletype_boxed))
    }

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

    pub fn term_pretty_name(&self, term_idx: Terminal) -> String {
        if term_idx == self.other_terminal_index {
            "<Others>".to_string()
        } else {
            self.terminals[term_idx]
                .name
                .pretty_name(self.is_char, self.is_u8)
        }
    }

    /// returns either 'term' or 'TerminalClassX'
    pub fn class_pretty_name_abbr(&self, class_idx: TerminalClass) -> String {
        let class = &self.terminal_classes[class_idx];
        if class.terminals.len() == 1 {
            // Exactly one terminal (could be character/byte range or ident) is matched. Print it directly.
            self.term_pretty_name(class.terminals[0])
        } else {
            // Matches multiple terminals.
            // Use abbreviated class name to keep diagnostics concise.
            format!("TerminalClass{}", class.multiterm_counter)
        }
    }
    /// returns either 'term' or '[term1, term2, ...]'
    pub fn class_pretty_name_list(
        &self,
        class: TerminalSymbol<TerminalClass>,
        max_len: usize,
    ) -> String {
        let max_len = max_len.max(3);
        match class {
            TerminalSymbol::Error => return "error".to_string(),
            TerminalSymbol::Eof => return "eof".to_string(),
            TerminalSymbol::VirtualStart(i) => return format!("VirtualStart({})", i),
            TerminalSymbol::Terminal(class_idx) => {
                let class = &self.terminal_classes[class_idx];
                let len: usize = class
                    .terminals
                    .iter()
                    .map(|term| self.terminals[*term].name.count())
                    .sum();
                if len == 1 {
                    // Exactly one single-character/token terminal is matched. Print it directly.
                    self.term_pretty_name(class.terminals[0])
                } else if class.terminals.len() < max_len {
                    // Matches a range or multiple terminals (less than max_len).
                    // Wrap in brackets `[...]` to clearly signify a character class/set in diagnostics.
                    let f = self.terminal_classes[class_idx]
                        .terminals
                        .iter()
                        .map(|&term| self.term_pretty_name(term))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("[{f}]")
                } else {
                    // Matches a large range or set of terminals. Wrap the truncated list in brackets.
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
    ) -> Result<rusty_lr_core::builder::Grammar<TerminalSymbol<TerminalClass>, usize>, ParseError>
    {
        // This normalization is not an optimization knob: it prevents GLR reduce closure from
        // repeatedly applying an auto-generated optional empty branch without consuming input.
        self.expand_glr_epsilon_self_loop_optionals()?;
        Ok(self.create_builder_from_current_rules())
    }

    pub fn build_grammar(
        &mut self,
    ) -> rusty_lr_core::builder::DiagnosticCollector<TerminalSymbol<TerminalClass>> {
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
            rusty_lr_core::parser::state::IntermediateState<TerminalSymbol<TerminalClass>, usize>,
        > = states.into_iter().map(Into::into).collect();

        if self.optimize {
            // Identify states that only perform a single reduction of a single-token rule.
            // These are candidates for optimization.
            let mut reduce_states: Vec<_> = Vec::with_capacity(states.len());
            for state in states.iter() {
                if !state.shift_goto_map_term.is_empty() || !state.shift_goto_map_nonterm.is_empty()
                {
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
                        if let Some((nonterm_idx, rule_local_id)) = reduce_states[next_state.state]
                        {
                            let rule = &self.nonterminals[nonterm_idx].rules[rule_local_id];
                            if let Some(push) =
                                self.reduce_only_state_bypass_push(nonterm_idx, rule)
                            {
                                let idx = state
                                    .shift_goto_map_nonterm
                                    .iter()
                                    .position(|(nt, _)| *nt == nonterm_idx)
                                    .unwrap();
                                let ns = state.shift_goto_map_nonterm[idx].1;
                                next_state.state = ns.state;
                                next_state.push = push;
                                changed = true;
                            }
                        }
                    }

                    // Optimize shift-on-nonterminal transitions.
                    for i in 0..state.shift_goto_map_nonterm.len() {
                        let next_state = state.shift_goto_map_nonterm[i].1;
                        if let Some((nonterm_idx, rule_local_id)) = reduce_states[next_state.state]
                        {
                            let rule = &self.nonterminals[nonterm_idx].rules[rule_local_id];
                            if let Some(push) =
                                self.reduce_only_state_bypass_push(nonterm_idx, rule)
                            {
                                let idx = state
                                    .shift_goto_map_nonterm
                                    .iter()
                                    .position(|(nt, _)| *nt == nonterm_idx)
                                    .unwrap();
                                let ns = state.shift_goto_map_nonterm[idx].1;
                                state.shift_goto_map_nonterm[i].1.state = ns.state;
                                state.shift_goto_map_nonterm[i].1.push = push;
                                changed = true;
                            }
                        }
                    }
                }

                if !changed {
                    break;
                }
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
                if let TerminalSymbol::Terminal(term) = *term {
                    shift_target.push = self.terminal_classes[term].data_used;
                }
            }
        }

        self.states = new_states;

        // Resolve table layout choice (DenseFlatTables vs SparseFlatTables) dynamically.
        // Performance considerations:
        // - DenseFlatTables: Stores each state's merged terminal actions in a row-local dense span
        //   (`min_terminal..=max_terminal`) and each goto row in a row-local dense nonterminal span. This gives
        //   O(1) lookup with better locality, but empty slots inside sparse rows can still add memory pressure.
        // - SparseFlatTables: Stores each state's merged terminal actions as sorted row slices. It has no empty
        //   action slots and is compact, but lookup needs a short linear scan or binary search.
        // - Default/Auto mode: We estimate the decoded runtime table footprint, including the row metadata added by
        //   the flat layouts. If the dense footprint fits within `dense_limit` or the dense/sparse ratio is modest,
        //   we favor the direct-indexing dense table; otherwise we keep the sparse table.
        self.emit_dense = match self.layout {
            TableLayout::Dense => true,
            TableLayout::Sparse => false,
            TableLayout::Auto => {
                let mut dense_action_slots = 0usize;
                let mut dense_goto_slots = 0usize;
                let mut sparse_action_entries = 0usize;
                let mut sparse_goto_entries = 0usize;
                let mut shift_key_entries = 0usize;

                let term_to_usize = |term: TerminalSymbol<TerminalClass>| -> usize {
                    match term {
                        TerminalSymbol::Terminal(t) => t,
                        TerminalSymbol::Error => self.terminal_classes.len(),
                        TerminalSymbol::Eof => self.terminal_classes.len() + 1,
                        TerminalSymbol::VirtualStart(i) => {
                            self.terminal_classes.len() + 2 + i as usize
                        }
                    }
                };

                for state in &self.states {
                    // DenseFlatTables stores one terminal action row built from the union of shift and reduce keys.
                    // Count that union rather than charging shifts and reduces as independent tables.
                    let mut shift_terms = state
                        .shift_goto_map_term
                        .iter()
                        .map(|(term, _)| term_to_usize(*term));
                    let mut reduce_terms = state
                        .reduce_map
                        .iter()
                        .map(|(term, _)| term_to_usize(*term));
                    let first_shift = shift_terms.next();
                    let first_reduce = reduce_terms.next();
                    let term_min = first_shift.into_iter().chain(first_reduce).min();
                    let term_max = state
                        .shift_goto_map_term
                        .last()
                        .map(|(term, _)| term_to_usize(*term))
                        .into_iter()
                        .chain(
                            state
                                .reduce_map
                                .last()
                                .map(|(term, _)| term_to_usize(*term)),
                        )
                        .max();
                    dense_action_slots += match (term_min, term_max) {
                        (Some(min), Some(max)) => max - min + 1,
                        _ => 0,
                    };

                    let mut shift_iter = state
                        .shift_goto_map_term
                        .iter()
                        .map(|(term, _)| term_to_usize(*term));
                    let mut reduce_iter = state
                        .reduce_map
                        .iter()
                        .map(|(term, _)| term_to_usize(*term));
                    let mut shift_next = shift_iter.next();
                    let mut reduce_next = reduce_iter.next();
                    while shift_next.is_some() || reduce_next.is_some() {
                        match (shift_next, reduce_next) {
                            (Some(shift), Some(reduce)) if shift == reduce => {
                                sparse_action_entries += 1;
                                shift_next = shift_iter.next();
                                reduce_next = reduce_iter.next();
                            }
                            (Some(shift), Some(reduce)) if shift < reduce => {
                                sparse_action_entries += 1;
                                shift_next = shift_iter.next();
                            }
                            (Some(_), Some(_)) => {
                                sparse_action_entries += 1;
                                reduce_next = reduce_iter.next();
                            }
                            (Some(_), None) => {
                                sparse_action_entries += 1;
                                shift_next = shift_iter.next();
                            }
                            (None, Some(_)) => {
                                sparse_action_entries += 1;
                                reduce_next = reduce_iter.next();
                            }
                            (None, None) => unreachable!(),
                        }
                    }

                    // DenseFlatTables keeps shift-capable terminal keys for expected-token reporting.
                    shift_key_entries += state.shift_goto_map_term.len();

                    // Non-terminal transitions
                    let nonterm_span = if !state.shift_goto_map_nonterm.is_empty() {
                        let min = state.shift_goto_map_nonterm.first().unwrap().0;
                        let max = state.shift_goto_map_nonterm.last().unwrap().0;
                        max - min + 1
                    } else {
                        0
                    };
                    let nonterm_count = state.shift_goto_map_nonterm.len();
                    dense_goto_slots += nonterm_span;
                    sparse_goto_entries += nonterm_count;
                }

                let state_count = self.states.len();
                let usize_size = std::mem::size_of::<usize>();
                let symbol_size = std::mem::size_of::<usize>();

                // These are intentionally approximate: the concrete enum/Option layout depends on the generated
                // state/rule index widths and niche optimizations. The goal is to choose the right layout class, not
                // to predict the exact allocator footprint byte-for-byte.
                let dense_action_slot_size = 8usize;
                let dense_goto_slot_size = 4usize;
                let sparse_action_entry_size = 12usize;
                let sparse_goto_entry_size = 8usize;

                let dense_row_metadata_size =
                    (state_count + 1) * usize_size * 4 + state_count * usize_size * 2;
                let sparse_row_metadata_size = (state_count + 1) * usize_size * 2;

                let total_dense_size = dense_row_metadata_size
                    + dense_action_slots * dense_action_slot_size
                    + dense_goto_slots * dense_goto_slot_size
                    + (shift_key_entries + sparse_goto_entries) * symbol_size;

                let total_sparse_size = sparse_row_metadata_size
                    + sparse_action_entries * sparse_action_entry_size
                    + sparse_goto_entries * sparse_goto_entry_size;

                // Choose DenseFlatTables if it fits in the cache limit, or if the size overhead is reasonable.
                total_dense_size <= self.dense_limit
                    || (total_sparse_size > 0
                        && (total_dense_size as f64 / total_sparse_size as f64) < 2.5)
            }
        };

        // Collect conflict diagnostics and populate the `self.infos` diagnostics list.

        // 1. Collect resolved reduce-reduce conflicts (resolved via precedence/dprec declarations).
        for (max_priority, reduce_rules, deleted_rules) in &collector.reduce_reduce_resolved {
            self.infos.push(Info::ReduceReduceConflictResolved {
                max_priority: *max_priority,
                reduce_rules: reduce_rules.iter().copied().collect(),
                deleted_rules: deleted_rules.iter().copied().collect(),
            });
        }

        // 2. Collect resolved shift-reduce conflicts where shift was chosen (resolved via precedence/associativity).
        for ((term, shift_rules), (shift_prec, reduce_rules)) in
            &collector.shift_reduce_resolved_shift
        {
            let shift_rule_indices = shift_rules
                .iter()
                .map(|sr| sr.production_idx)
                .collect::<Vec<_>>();
            let reduce_rule_pairs = reduce_rules
                .iter()
                .map(|(&r, &p)| (r, p))
                .collect::<Vec<_>>();
            self.infos.push(Info::ShiftReduceConflictResolvedShift {
                term: *term,
                shift_prec: *shift_prec,
                shift_rules: shift_rule_indices,
                reduce_rules: reduce_rule_pairs,
            });
        }

        // 3. Collect resolved shift-reduce conflicts where reduce was chosen (resolved via precedence/associativity).
        for ((term, shift_rules), (shift_prec, reduce_rules)) in
            &collector.shift_reduce_resolved_reduce
        {
            let shift_rule_indices = shift_rules
                .iter()
                .map(|sr| sr.production_idx)
                .collect::<Vec<_>>();
            let reduce_rule_pairs = reduce_rules
                .iter()
                .map(|(&r, &p)| (r, p))
                .collect::<Vec<_>>();
            self.infos.push(Info::ShiftReduceConflictResolvedReduce {
                term: *term,
                shift_prec: *shift_prec,
                shift_rules: shift_rule_indices,
                reduce_rules: reduce_rule_pairs,
            });
        }

        // 4. Collect unresolved conflicts for GLR warning/help notes.
        // In GLR parsers, unresolved conflicts are resolved at runtime, but we still report them for debugging purposes.
        if self.glr {
            // Collect unresolved shift-reduce conflicts with backtraces.
            for ((term, shift_rules, shift_rules_backtrace), reduce_rules) in
                &collector.shift_reduce_conflicts
            {
                let shift_rule_indices = shift_rules
                    .iter()
                    .map(|sr| sr.production_idx)
                    .collect::<Vec<_>>();

                let mut s_backtrace = Vec::new();
                for shift_rule in shift_rules_backtrace {
                    let rule_str = self.builder.rules[shift_rule.production_idx]
                        .rule
                        .clone()
                        .map(
                            |c| self.class_pretty_name_list(c, 5),
                            |nt| self.nonterm_pretty_name(nt),
                        )
                        .into_shifted(shift_rule.dot);
                    s_backtrace.push(format!("\t>>> {rule_str}"));
                }

                let mut r_rules = Vec::new();
                for (&reduce_rule, reduce_rule_backtrace) in reduce_rules {
                    let mut r_backtrace = Vec::new();
                    let name = self.nonterm_pretty_name(self.builder.rules[reduce_rule].rule.lhs);
                    r_backtrace.push(format!("Backtrace for the reduce rule ({name}):"));
                    for shifted_rule in reduce_rule_backtrace {
                        let rule_str = self.builder.rules[shifted_rule.production_idx]
                            .rule
                            .clone()
                            .map(
                                |c| self.class_pretty_name_list(c, 5),
                                |nt| self.nonterm_pretty_name(nt),
                            )
                            .into_shifted(shifted_rule.dot);
                        r_backtrace.push(format!("\t>>> {rule_str}"));
                    }
                    r_rules.push((reduce_rule, r_backtrace));
                }

                self.infos.push(Info::ShiftReduceConflictGLR {
                    term: *term,
                    shift_rules: shift_rule_indices,
                    shift_rules_backtrace: s_backtrace,
                    reduce_rules: r_rules,
                });
            }

            // Collect unresolved reduce-reduce conflicts with backtraces.
            for (reduce_rules, reduce_terms) in &collector.reduce_reduce_conflicts {
                let mut r_rules = Vec::new();
                for &(reduce_rule, ref reduce_rule_from) in reduce_rules {
                    let mut r_backtrace = Vec::new();
                    let name = self.nonterm_pretty_name(self.builder.rules[reduce_rule].rule.lhs);
                    r_backtrace.push(format!("Backtrace for the reduce rule ({name}):"));
                    for shifted_rule in reduce_rule_from {
                        let rule_str = self.builder.rules[shifted_rule.production_idx]
                            .rule
                            .clone()
                            .map(
                                |c| self.class_pretty_name_list(c, 5),
                                |nt| self.nonterm_pretty_name(nt),
                            )
                            .into_shifted(shifted_rule.dot);
                        r_backtrace.push(format!("\t>>> {rule_str}"));
                    }
                    r_rules.push((reduce_rule, r_backtrace));
                }

                self.infos.push(Info::ReduceReduceConflictGLR {
                    terms: reduce_terms.iter().cloned().collect(),
                    reduce_rules: r_rules,
                });
            }
        }

        collector
    }
}
