use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionResponse, CompletionTextEdit, Documentation,
    MarkupContent, MarkupKind, Position, Range, TextEdit,
};
use proc_macro2::{TokenStream, TokenTree};
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::{GrammarArgs, Location, PatternArgs};
use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use crate::lsp::diagnostics::split_stream;
use crate::lsp::hover;
use crate::lsp::position::{offset_to_position, position_to_offset};

pub(crate) const DIRECTIVES: &[&str] = &[
    "%token",
    "%start",
    "%tokentype",
    "%userdata",
    "%error",
    "%errortype",
    "%location",
    "%left",
    "%right",
    "%precedence",
    "%glr",
    "%lalr",
    "%nooptim",
    "%allow",
    "%moduleprefix",
];

/// Directives only valid inside a production rule line (after the `:` or `|`).
pub(crate) const PRODUCTION_DIRECTIVES: &[&str] = &["%prec", "%dprec"];

pub(crate) const SUBSTITUTION_VARIABLES: &[&str] = &[
    "$tokentype",
    "$location",
    "$userdata",
    "$error",
    "$errortype",
];

pub(crate) const ALLOW_DIAGNOSTICS: &[&str] = &[
    "nonterm_unreachable",
    "nonterm_unproductive",
    "unused_nonterm_data",
    "unused_terminals",
    "terminals_merged",
    "redundant_rule_removed",
    "unit_production_eliminated",
    "reduce_reduce_conflict_resolved",
    "shift_reduce_conflict_resolved",
    "shift_reduce_conflict_glr",
    "reduce_reduce_conflict_glr",
];

pub(crate) const KEYWORDS: &[&str] = &["error", "$sep", "data", "lookahead", "shift", "Err"];

/// Keywords that are only valid inside a ReduceAction block.
const REDUCE_ACTION_KEYWORDS: &[&str] = &["data", "lookahead", "shift", "Err"];

/// Keywords that are only valid outside a ReduceAction (production pattern / symbol list).
const PATTERN_KEYWORDS: &[&str] = &["error", "$sep"];

pub(crate) const SYNTAX_URL: &str = "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md";

#[derive(Clone, Copy, PartialEq, Eq)]
enum CompletionMode {
    Directive,
    Dollar,
    Location,
    AllowDiagnostic,
    Symbol,
}

pub fn completions(content: &str, position: Position) -> CompletionResponse {
    let offset = position_to_offset(content, position);
    let mode = completion_mode(content, offset);
    let replace_range = replacement_range(content, offset, mode);

    let parsed = parse_args(content).ok();
    let names = parsed
        .as_ref()
        .map(|args| CompletionNames::from_args(args, content))
        .unwrap_or_else(|| CompletionNames::from_text(content));
    let line_variables = parsed
        .as_ref()
        .map(|args| variables_for_offset(args, content, offset))
        .unwrap_or_default();

    let mut builder = CompletionBuilder::new(replace_range);

    match mode {
        CompletionMode::Directive => {
            // Directives are only meaningful outside a ReduceAction block.
            // Inside a ReduceAction the user is writing Rust code, not grammar directives.
            if !line_variables.in_reduce_action {
                // inside of a production rule line, only valid directives are %prec and %dprec.
                if line_variables.in_rule_line {
                    for directive in PRODUCTION_DIRECTIVES {
                        builder.keyword(
                            directive,
                            "production rule directive",
                            keyword_documentation(directive),
                        );
                    }
                } else {
                    for directive in DIRECTIVES {
                        builder.keyword(
                            directive,
                            "RustyLR directive",
                            keyword_documentation(directive),
                        );
                    }
                }
            }
        }
        CompletionMode::Dollar => {
            if !line_variables.in_rule_line {
                for variable in SUBSTITUTION_VARIABLES {
                    builder.variable(
                        variable,
                        "built-in RustCode substitution",
                        substitution_documentation(variable),
                    );
                }
                for (name, documentation) in &names.nonterminals {
                    builder.variable(
                    &format!("${name}"),
                    "non-terminal production type",
                    Some(format!(
                        "Substitutes to the production type of non-terminal `{name}`.\n\n{documentation}\n\n[Variable substitution]({SYNTAX_URL}#variable-substitution)"
                        )),
                    );
                }
                for (name, documentation) in &names.terminals {
                    builder.variable(
                    &format!("${name}"),
                    "terminal definition substitution",
                    Some(format!(
                        "Substitutes to the `%token` definition for terminal `{name}`.\n\n{documentation}\n\n[Variable substitution]({SYNTAX_URL}#variable-substitution)"
                        )),
                    );
                }
            } else {
                for keyword in PATTERN_KEYWORDS {
                    let (detail, documentation) = keyword_completion_info(parsed.as_ref(), keyword);
                    builder.keyword(keyword, &detail, documentation);
                }
            }

            // Positional variables ($1, $2, …) are only valid inside a ReduceAction block.
            if line_variables.in_reduce_action {
                for variable in SUBSTITUTION_VARIABLES {
                    builder.variable(
                        variable,
                        "built-in RustCode substitution",
                        substitution_documentation(variable),
                    );
                }
                for (name, documentation) in &names.nonterminals {
                    builder.variable(
                    &format!("${name}"),
                    "non-terminal production type",
                    Some(format!(
                        "Substitutes to the production type of non-terminal `{name}`.\n\n{documentation}\n\n[Variable substitution]({SYNTAX_URL}#variable-substitution)"
                        )),
                    );
                }
                for (name, documentation) in &names.terminals {
                    builder.variable(
                    &format!("${name}"),
                    "terminal definition substitution",
                    Some(format!(
                        "Substitutes to the `%token` definition for terminal `{name}`.\n\n{documentation}\n\n[Variable substitution]({SYNTAX_URL}#variable-substitution)"
                        )),
                    );
                }

                for index in 1..=line_variables.value_count {
                    let (detail, documentation) = if let Some(reference) =
                        line_variables.position_references.get(&index)
                    {
                        (
                            format!("${index}: {}", reference.ty),
                            Some(hover::reduce_action_binding_reference_documentation(
                                &format!("${index}"),
                                reference,
                            )),
                        )
                    } else {
                        (
                            "positional semantic value".to_string(),
                            Some(format!(
                                "Semantic value of RHS symbol #{index} in the current production line.\n\nExample:\n\n```rustylr\nExpr : Expr plus Term {{ $1 }};\n```\n\n[Bison-style positional variables]({SYNTAX_URL}#3-bison-style-positional-variables)"
                            )),
                        )
                    };
                    builder.variable(&format!("${index}"), &detail, documentation);
                }
            }
        }
        CompletionMode::Location => {
            // Location tracking (@name, @N, @$) is only valid inside a ReduceAction block.
            if line_variables.in_reduce_action {
                let (detail, documentation) = location_completion_info(parsed.as_ref(), "@$");
                builder.variable("@$", &detail, documentation);
                let (detail, documentation) = location_completion_info(parsed.as_ref(), "@0");
                builder.variable("@0", &detail, documentation);
                for variable in &line_variables.value_names {
                    let label = format!("@{variable}");
                    let (detail, documentation) =
                        if let Some(reference) = line_variables.value_references.get(variable) {
                            (
                                hover::reduce_action_location_reference_detail(
                                    parsed.as_ref().unwrap(),
                                    &label,
                                ),
                                Some(hover::reduce_action_location_reference_documentation(
                                    parsed.as_ref().unwrap(),
                                    &label,
                                    Some(reference),
                                )),
                            )
                        } else {
                            (
                                "current production binding location".to_string(),
                                location_documentation(&label),
                            )
                        };
                    builder.variable(&label, &detail, documentation);
                }
                for index in 1..=line_variables.value_count {
                    let label = format!("@{index}");
                    let (detail, documentation) =
                        if let Some(reference) = line_variables.position_references.get(&index) {
                            (
                                hover::reduce_action_location_reference_detail(
                                    parsed.as_ref().unwrap(),
                                    &label,
                                ),
                                Some(hover::reduce_action_location_reference_documentation(
                                    parsed.as_ref().unwrap(),
                                    &label,
                                    Some(reference),
                                )),
                            )
                        } else {
                            (
                                "positional location".to_string(),
                                location_documentation(&label),
                            )
                        };
                    builder.variable(&label, &detail, documentation);
                }
            }
        }
        CompletionMode::AllowDiagnostic => {
            // Only diagnostic names are valid after %allow.
            for diagnostic in ALLOW_DIAGNOSTICS {
                builder.keyword(
                    diagnostic,
                    "diagnostic suppression name",
                    allow_diagnostic_documentation(diagnostic),
                );
            }
        }
        CompletionMode::Symbol => {
            if line_variables.in_reduce_action {
                // Inside a ReduceAction: suggest named bindings and ReduceAction-specific keywords.
                for (name, ty) in &line_variables.value_types {
                    if let Some(reference) = line_variables.value_references.get(name) {
                        builder.variable(
                            name,
                            &hover::reduce_action_binding_detail(name, ty),
                            Some(hover::reduce_action_binding_reference_documentation(
                                name, reference,
                            )),
                        );
                    } else {
                        builder.variable(
                            name,
                            &hover::reduce_action_binding_detail(name, ty),
                            Some(hover::reduce_action_binding_documentation(name, ty)),
                        );
                    }
                }
                for keyword in REDUCE_ACTION_KEYWORDS {
                    let (detail, documentation) = keyword_completion_info(parsed.as_ref(), keyword);
                    builder.keyword(keyword, &detail, documentation);
                }
            } else {
                // Outside a ReduceAction: suggest grammar symbols, pattern keywords, and directives.

                if line_variables.in_rule_line && !line_variables.in_reduce_action {
                    add_symbol_items(&mut builder, &names);
                    for keyword in PATTERN_KEYWORDS {
                        let (detail, documentation) =
                            keyword_completion_info(parsed.as_ref(), keyword);
                        builder.keyword(keyword, &detail, documentation);
                    }
                } else {
                    for directive in DIRECTIVES {
                        builder.keyword(
                            directive,
                            "RustyLR directive",
                            keyword_documentation(directive),
                        );
                    }
                }
            }
        }
    }

    CompletionResponse::Array(builder.finish())
}

fn keyword_completion_info(args: Option<&GrammarArgs>, keyword: &str) -> (String, Option<String>) {
    if let Some(args) = args {
        if let Some(documentation) = hover::reduce_action_variable_documentation(args, keyword) {
            let detail = hover::reduce_action_variable_detail(args, keyword)
                .unwrap_or_else(|| "RustyLR keyword".to_string());
            return (detail, Some(documentation));
        }
    }

    (
        "RustyLR keyword".to_string(),
        keyword_documentation(keyword),
    )
}

fn location_completion_info(args: Option<&GrammarArgs>, label: &str) -> (String, Option<String>) {
    if let Some(args) = args {
        if let Some(documentation) = hover::reduce_action_variable_documentation(args, label) {
            let detail = hover::reduce_action_variable_detail(args, label)
                .unwrap_or_else(|| "current production location".to_string());
            return (detail, Some(documentation));
        }
    }

    (
        "current production location".to_string(),
        location_documentation(label),
    )
}

fn add_symbol_items(builder: &mut CompletionBuilder, names: &CompletionNames) {
    for (name, documentation) in &names.nonterminals {
        builder.nonterminal(name, documentation.clone());
    }
    for (name, documentation) in &names.terminals {
        builder.terminal(name, documentation.clone());
    }
}

pub(crate) fn parse_args(content: &str) -> Result<GrammarArgs, ()> {
    let token_stream = TokenStream::from_str(content).map_err(|_| ())?;
    let (_, macro_stream) = split_stream(token_stream).map_err(|_| ())?;
    Grammar::parse_args(macro_stream).map_err(|_| ())
}

fn completion_mode(content: &str, offset: usize) -> CompletionMode {
    let prefix_start = current_prefix_start(content, offset, true);
    if prefix_start < offset {
        match content.as_bytes()[prefix_start] {
            b'%' => return CompletionMode::Directive,
            b'$' => return CompletionMode::Dollar,
            b'@' => return CompletionMode::Location,
            _ => {}
        }
    }

    let line_prefix = line_prefix(content, offset);
    let trimmed = line_prefix.trim_start();
    if trimmed.starts_with("%allow") {
        return CompletionMode::AllowDiagnostic;
    }
    if trimmed.ends_with('%') {
        return CompletionMode::Directive;
    }
    if trimmed.ends_with('$') {
        return CompletionMode::Dollar;
    }
    if trimmed.ends_with('@') {
        return CompletionMode::Location;
    }

    CompletionMode::Symbol
}

fn replacement_range(content: &str, offset: usize, mode: CompletionMode) -> Range {
    let include_sigils = matches!(
        mode,
        CompletionMode::Directive | CompletionMode::Dollar | CompletionMode::Location
    );
    let start = current_prefix_start(content, offset, include_sigils);
    Range::new(
        offset_to_position(content, start),
        offset_to_position(content, offset),
    )
}

pub(crate) fn current_prefix_start(content: &str, offset: usize, include_sigils: bool) -> usize {
    let mut start = offset.min(content.len());
    while start > 0 {
        let Some(ch) = content[..start].chars().next_back() else {
            break;
        };
        if is_ident_continue(ch)
            || (include_sigils && matches!(ch, '$' | '@' | '%'))
            || (include_sigils && ch.is_ascii_digit())
        {
            start -= ch.len_utf8();
        } else {
            break;
        }
    }
    start
}

fn line_prefix(content: &str, offset: usize) -> &str {
    let offset = offset.min(content.len());
    let line_start = content[..offset].rfind('\n').map_or(0, |idx| idx + 1);
    &content[line_start..offset]
}

pub(crate) fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

#[derive(Default)]
struct CompletionNames {
    terminals: BTreeMap<String, String>,
    nonterminals: BTreeMap<String, String>,
}

impl CompletionNames {
    fn from_args(args: &GrammarArgs, content: &str) -> Self {
        let mut names = CompletionNames::default();
        let types = ResolvedTypes::from_args(args);
        for (terminal, _) in &args.terminals {
            let line = line_text_for_location(args, content, &terminal.location());
            names.terminals.insert(
                terminal.value().clone(),
                terminal_documentation(terminal.value(), &line, types.token_type.as_ref()),
            );
        }
        for rule in &args.rules {
            let snippet = rule_definition_text(args, content, rule);
            let documentation = nonterminal_documentation(
                rule.name.value(),
                &snippet,
                types.nonterminals.get(rule.name.value()),
            );
            names
                .nonterminals
                .entry(rule.name.value().clone())
                .and_modify(|existing| {
                    existing.push_str("\n\n---\n\n");
                    existing.push_str(&documentation);
                })
                .or_insert(documentation);
        }
        names
    }

    fn from_text(content: &str) -> Self {
        let mut names = CompletionNames::default();
        let grammar = content
            .split_once("%%")
            .map_or(content, |(_, grammar)| grammar);

        for raw_line in grammar.lines() {
            let line = raw_line.trim_start();
            if let Some(rest) = line.strip_prefix("%token") {
                if let Some(name) = first_ident(rest) {
                    names
                        .terminals
                        .insert(name.to_string(), terminal_documentation(name, line, None));
                }
                continue;
            }

            if line.starts_with('%') {
                continue;
            }

            if let Some(colon_idx) = line.find(':') {
                let head = &line[..colon_idx];
                if let Some(name) = first_ident(head) {
                    names.nonterminals.insert(
                        name.to_string(),
                        nonterminal_documentation(name, line.trim(), None),
                    );
                }
            }
        }

        names
    }
}

#[derive(Default)]
struct ResolvedTypes {
    token_type: Option<ResolvedRustType>,
    nonterminals: BTreeMap<String, ResolvedRustType>,
}

struct ResolvedRustType {
    name: String,
    boxed: bool,
}

impl ResolvedTypes {
    fn from_args(args: &GrammarArgs) -> Self {
        let Ok(grammar) = Grammar::from_grammar_args(args.clone()) else {
            return ResolvedTypes::default();
        };

        let mut types = ResolvedTypes {
            token_type: Some(resolved_rust_type(
                Some(grammar.token_type()),
                grammar.token_type_boxed(),
            )),
            nonterminals: BTreeMap::new(),
        };
        for rule in &args.rules {
            if let Some((rule_type, boxed)) = grammar.nonterminal_type(rule.name.value()) {
                types.nonterminals.insert(
                    rule.name.value().clone(),
                    resolved_rust_type(rule_type, boxed),
                );
            }
        }

        types
    }
}

fn resolved_rust_type(ty: Option<&TokenStream>, boxed: bool) -> ResolvedRustType {
    let name = ty
        .map(TokenStream::to_string)
        .filter(|ty| !ty.is_empty())
        .unwrap_or_else(|| "()".to_string());
    ResolvedRustType { name, boxed }
}

pub(crate) fn line_text_for_location(
    args: &GrammarArgs,
    content: &str,
    location: &Location,
) -> String {
    let offset = args
        .span_manager
        .get_byterange(location)
        .map_or(0, |range| range.start);
    let start = content[..offset.min(content.len())]
        .rfind('\n')
        .map_or(0, |idx| idx + 1);
    let end = content[offset.min(content.len())..]
        .find('\n')
        .map_or(content.len(), |idx| offset + idx);
    content[start..end].trim().to_string()
}

pub(crate) fn rule_definition_text(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
) -> String {
    let rule_start = args
        .span_manager
        .get_byterange(&rule.name.location())
        .map_or(0, |range| range.start);
    let start = content[..rule_start.min(content.len())]
        .rfind('\n')
        .map_or(0, |idx| idx + 1);
    let first_separator = rule.rule_lines.first().and_then(|line| {
        args.span_manager
            .get_byterange(&line.separator_location)
            .map(|range| range.start)
    });
    let header_end = first_separator.unwrap_or(rule_start).min(content.len());
    let header = content[start..header_end].trim();
    let mut definition = String::new();
    definition.push_str(header);
    for (line_idx, line) in rule.rule_lines.iter().enumerate() {
        let tokens = rule_line_tokens_text(args, content, line);
        definition.push('\n');
        definition.push(' ');
        definition.push(if line_idx == 0 { ':' } else { '|' });
        if !tokens.is_empty() {
            definition.push(' ');
            definition.push_str(&tokens);
        }
    }
    definition.push_str("\n ;");
    definition
}

fn first_ident(text: &str) -> Option<&str> {
    let start = text.find(|ch: char| ch == '_' || ch.is_ascii_alphabetic())?;
    let rest = &text[start..];
    let end = rest
        .find(|ch: char| !(ch == '_' || ch.is_ascii_alphanumeric()))
        .unwrap_or(rest.len());
    Some(&rest[..end])
}

fn rule_line_tokens_text(
    args: &GrammarArgs,
    content: &str,
    line: &rusty_lr_parser::RuleLineArgs,
) -> String {
    line.tokens
        .iter()
        .map(|(_, pattern)| {
            let start = pattern_start(args, pattern);
            let end = pattern_end(args, pattern);
            content[start.min(content.len())..end.min(content.len())]
                .trim()
                .to_string()
        })
        .collect::<Vec<_>>()
        .join(" ")
}

#[derive(Default)]
struct LineVariables {
    value_names: BTreeSet<String>,
    value_types: BTreeMap<String, String>,
    value_references: BTreeMap<String, ReduceActionReference>,
    position_references: BTreeMap<usize, ReduceActionReference>,
    value_count: usize,
    /// True when the cursor is anywhere inside a production rule line
    /// (either in the pattern/symbol list or inside the ReduceAction block).
    in_rule_line: bool,
    /// True when the cursor is inside the `{ … }` ReduceAction block of a production rule line.
    in_reduce_action: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct ReduceActionReference {
    pub(crate) ty: String,
    pub(crate) production: String,
    pub(crate) marker: String,
    pub(crate) index: usize,
}

fn variables_for_offset(args: &GrammarArgs, content: &str, offset: usize) -> LineVariables {
    let grammar = Grammar::from_grammar_args(args.clone()).ok();
    for rule in &args.rules {
        for (line_idx, line) in rule.rule_lines.iter().enumerate() {
            let start = args
                .span_manager
                .get_byterange(&line.separator_location)
                .map_or(0, |range| range.start);
            let end = rule_line_end(args, content, rule, line_idx);
            if start <= offset && offset <= end {
                let mut variables = LineVariables::default();
                variables.in_rule_line = true;
                variables.in_reduce_action = reduce_action_contains_offset(line, offset);
                let token_references = grammar
                    .as_ref()
                    .map(|grammar| {
                        production_token_references(args, content, rule, line_idx, grammar)
                    })
                    .unwrap_or_default();
                for (mapped_name, pattern) in &line.tokens {
                    variables.value_count += 1;
                    if let Some(reference) = token_references.get(variables.value_count - 1) {
                        variables
                            .position_references
                            .insert(variables.value_count, reference.clone());
                    }
                    if let Some(name) = mapped_name {
                        variables.value_names.insert(name.value().clone());
                        if let Some(grammar) = &grammar {
                            variables.value_types.insert(
                                name.value().clone(),
                                hover::pattern_final_type(args, grammar, pattern),
                            );
                        }
                        if let Some(reference) = token_references.get(variables.value_count - 1) {
                            variables
                                .value_references
                                .insert(name.value().clone(), reference.clone());
                        }
                    } else {
                        collect_default_bindings(pattern, &mut variables.value_names);
                        if let (Some(grammar), Some(reference)) =
                            (&grammar, token_references.get(variables.value_count - 1))
                        {
                            collect_default_binding_types(
                                args,
                                grammar,
                                pattern,
                                &mut variables.value_types,
                            );
                            collect_default_binding_references(
                                pattern,
                                reference,
                                &mut variables.value_references,
                            );
                        }
                    }
                }
                return variables;
            }
        }
    }

    LineVariables::default()
}

pub(crate) fn reduce_action_binding_reference_for_offset(
    args: &GrammarArgs,
    content: &str,
    offset: usize,
    name: &str,
) -> Option<ReduceActionReference> {
    let variables = variables_for_offset(args, content, offset);
    if !variables.in_reduce_action {
        return None;
    }
    variables.value_references.get(name).cloned()
}

pub(crate) fn reduce_action_positional_reference_for_offset(
    args: &GrammarArgs,
    content: &str,
    offset: usize,
    index: usize,
) -> Option<ReduceActionReference> {
    let variables = variables_for_offset(args, content, offset);
    if !variables.in_reduce_action {
        return None;
    }
    variables.position_references.get(&index).cloned()
}

fn reduce_action_contains_offset(line: &rusty_lr_parser::RuleLineArgs, offset: usize) -> bool {
    line.reduce_action
        .as_ref()
        .and_then(token_stream_range)
        .is_some_and(|range| range.contains(&offset))
}

fn production_token_references(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
    line_idx: usize,
    grammar: &Grammar,
) -> Vec<ReduceActionReference> {
    let line = &rule.rule_lines[line_idx];
    let separator = if line_idx == 0 { ':' } else { '|' };
    let mut production = format!("{} {separator}", rule.name.value());
    let mut token_spans = Vec::new();
    for (mapped_name, pattern) in &line.tokens {
        let token_text = mapped_pattern_text(args, content, mapped_name.as_ref(), pattern);
        if token_text.is_empty() {
            continue;
        }
        production.push(' ');
        let start = production.chars().count();
        production.push_str(&token_text);
        let width = token_text.chars().count().max(1);
        token_spans.push((
            start,
            width,
            hover::pattern_final_type(args, grammar, pattern),
        ));
    }

    token_spans
        .into_iter()
        .enumerate()
        .map(|(idx, (start, width, ty))| ReduceActionReference {
            ty,
            production: production.clone(),
            marker: format!("{}{}", " ".repeat(start), "^".repeat(width)),
            index: idx + 1,
        })
        .collect()
}

fn mapped_pattern_text(
    args: &GrammarArgs,
    content: &str,
    mapped_name: Option<&rusty_lr_parser::Located<String>>,
    pattern: &PatternArgs,
) -> String {
    let start = mapped_name
        .and_then(|name| args.span_manager.get_byterange(&name.location()))
        .map_or_else(|| pattern_start(args, pattern), |range| range.start);
    let end = pattern_end(args, pattern);
    content[start.min(content.len())..end.min(content.len())]
        .trim()
        .to_string()
}

fn rule_line_end(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
    line_idx: usize,
) -> usize {
    if let Some(next_line) = rule.rule_lines.get(line_idx + 1) {
        return args
            .span_manager
            .get_byterange(&next_line.separator_location)
            .map_or(content.len(), |range| range.start);
    }

    let mut end = args
        .span_manager
        .get_byterange(&rule.name.location())
        .map_or(0, |range| range.end);
    for (_, pattern) in &rule.rule_lines[line_idx].tokens {
        end = end.max(pattern_end(args, pattern));
    }
    if let Some(action) = &rule.rule_lines[line_idx].reduce_action {
        end = end.max(token_stream_end(action));
    }

    content[end.min(content.len())..]
        .find(';')
        .map_or(content.len(), |semi| end + semi)
}

fn pattern_end(args: &GrammarArgs, pattern: &PatternArgs) -> usize {
    match pattern {
        PatternArgs::Ident(ident) => args
            .span_manager
            .get_byterange(&ident.location())
            .map_or(0, |range| range.end),
        PatternArgs::Plus { base, op_location }
        | PatternArgs::Star { base, op_location }
        | PatternArgs::Question { base, op_location }
        | PatternArgs::Exclamation { base, op_location } => pattern_end(args, base).max(
            args.span_manager
                .get_byterange(op_location)
                .map_or(0, |range| range.end),
        ),
        PatternArgs::TerminalSet(set) => args
            .span_manager
            .get_byterange(&set.location())
            .map_or(0, |range| range.end),
        PatternArgs::Group {
            alternatives,
            close_location,
            ..
        } => alternatives
            .iter()
            .flatten()
            .map(|pattern| pattern_end(args, pattern))
            .max()
            .unwrap_or(0)
            .max(
                args.span_manager
                    .get_byterange(close_location)
                    .map_or(0, |range| range.end),
            ),
        PatternArgs::Byte(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::ByteString(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::Char(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::String(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::Minus { base, exclude } => {
            pattern_end(args, base).max(pattern_end(args, exclude))
        }
        PatternArgs::Sep {
            base,
            delimiter,
            location,
            ..
        } => pattern_end(args, base)
            .max(pattern_end(args, delimiter))
            .max(
                args.span_manager
                    .get_byterange(location)
                    .map_or(0, |range| range.end),
            ),
    }
}

fn pattern_start(args: &GrammarArgs, pattern: &PatternArgs) -> usize {
    match pattern {
        PatternArgs::Ident(ident) => args
            .span_manager
            .get_byterange(&ident.location())
            .map_or(0, |range| range.start),
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. }
        | PatternArgs::Exclamation { base, .. } => pattern_start(args, base),
        PatternArgs::TerminalSet(set) => args
            .span_manager
            .get_byterange(&set.location())
            .map_or(0, |range| range.start),
        PatternArgs::Group { open_location, .. } => args
            .span_manager
            .get_byterange(open_location)
            .map_or(0, |range| range.start),
        PatternArgs::Byte(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::ByteString(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::Char(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::String(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::Minus { base, .. } => pattern_start(args, base),
        PatternArgs::Sep { location, .. } => args
            .span_manager
            .get_byterange(location)
            .map_or(0, |range| range.start),
    }
}

fn token_stream_end(stream: &TokenStream) -> usize {
    stream
        .clone()
        .into_iter()
        .map(token_tree_end)
        .max()
        .unwrap_or(0)
}

fn token_stream_range(stream: &TokenStream) -> Option<std::ops::Range<usize>> {
    let mut ranges = stream.clone().into_iter().filter_map(token_tree_range);
    let first = ranges.next()?;
    let range = ranges.fold(first, |acc, range| {
        acc.start.min(range.start)..acc.end.max(range.end)
    });
    Some(range)
}

fn token_tree_range(token: TokenTree) -> Option<std::ops::Range<usize>> {
    match token {
        TokenTree::Group(group) => {
            let stream_range = token_stream_range(&group.stream());
            let open = group.span_open().byte_range();
            let close = group.span_close().byte_range();
            let delimiter_range = open.start.min(close.start)..open.end.max(close.end);
            Some(if let Some(stream_range) = stream_range {
                delimiter_range.start.min(stream_range.start)
                    ..delimiter_range.end.max(stream_range.end)
            } else {
                delimiter_range
            })
        }
        TokenTree::Ident(ident) => Some(ident.span().byte_range()),
        TokenTree::Punct(punct) => Some(punct.span().byte_range()),
        TokenTree::Literal(lit) => Some(lit.span().byte_range()),
    }
}

fn token_tree_end(token: TokenTree) -> usize {
    match token {
        TokenTree::Group(group) => token_stream_end(&group.stream())
            .max(group.span_close().byte_range().end)
            .max(group.span_open().byte_range().end),
        TokenTree::Ident(ident) => ident.span().byte_range().end,
        TokenTree::Punct(punct) => punct.span().byte_range().end,
        TokenTree::Literal(lit) => lit.span().byte_range().end,
    }
}

fn collect_default_binding_types(
    args: &GrammarArgs,
    grammar: &Grammar,
    pattern: &PatternArgs,
    bindings: &mut BTreeMap<String, String>,
) {
    match pattern {
        PatternArgs::Ident(ident) => {
            bindings.insert(
                ident.value().clone(),
                hover::pattern_final_type(args, grammar, pattern),
            );
        }
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. } => {
            collect_default_binding_types(args, grammar, base, bindings);
        }
        PatternArgs::Minus { base, exclude } => {
            collect_default_binding_types(args, grammar, base, bindings);
            collect_default_binding_types(args, grammar, exclude, bindings);
        }
        PatternArgs::Sep { base, .. } => {
            collect_default_binding_types(args, grammar, base, bindings);
        }
        PatternArgs::Exclamation { .. }
        | PatternArgs::Group { .. }
        | PatternArgs::TerminalSet(_)
        | PatternArgs::Byte(_)
        | PatternArgs::ByteString(_)
        | PatternArgs::Char(_)
        | PatternArgs::String(_) => {}
    }
}

fn collect_default_binding_references(
    pattern: &PatternArgs,
    reference: &ReduceActionReference,
    bindings: &mut BTreeMap<String, ReduceActionReference>,
) {
    match pattern {
        PatternArgs::Ident(ident) => {
            bindings.insert(ident.value().clone(), reference.clone());
        }
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. } => {
            collect_default_binding_references(base, reference, bindings);
        }
        PatternArgs::Minus { base, exclude } => {
            collect_default_binding_references(base, reference, bindings);
            collect_default_binding_references(exclude, reference, bindings);
        }
        PatternArgs::Sep { base, .. } => {
            collect_default_binding_references(base, reference, bindings);
        }
        PatternArgs::Exclamation { .. }
        | PatternArgs::Group { .. }
        | PatternArgs::TerminalSet(_)
        | PatternArgs::Byte(_)
        | PatternArgs::ByteString(_)
        | PatternArgs::Char(_)
        | PatternArgs::String(_) => {}
    }
}

fn collect_default_bindings(pattern: &PatternArgs, names: &mut BTreeSet<String>) {
    match pattern {
        PatternArgs::Ident(ident) => {
            names.insert(ident.value().clone());
        }
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. }
        | PatternArgs::Exclamation { base, .. } => {
            collect_default_bindings(base, names);
        }
        PatternArgs::Minus { base, exclude } => {
            collect_default_bindings(base, names);
            collect_default_bindings(exclude, names);
        }
        PatternArgs::Sep { base, .. } => {
            collect_default_bindings(base, names);
        }
        PatternArgs::Group { .. }
        | PatternArgs::TerminalSet(_)
        | PatternArgs::Byte(_)
        | PatternArgs::ByteString(_)
        | PatternArgs::Char(_)
        | PatternArgs::String(_) => {}
    }
}

fn terminal_documentation(
    name: &str,
    definition: &str,
    rust_type: Option<&ResolvedRustType>,
) -> String {
    let type_line = type_line(rust_type);
    format!(
        "Terminal symbol `{name}`.\n\n{type_line}\n\nDefinition:\n\n```rustylr\n{definition}\n```\n\n[Token definition]({SYNTAX_URL}#token-definition-must-defined)"
    )
}

fn nonterminal_documentation(
    name: &str,
    definition: &str,
    rust_type: Option<&ResolvedRustType>,
) -> String {
    let type_line = type_line(rust_type);
    format!(
        "Non-terminal symbol `{name}`.\n\n{type_line}\n\nDefinition:\n\n```rustylr\n{definition}\n```\n\n[Production rules]({SYNTAX_URL}#production-rules)"
    )
}

fn type_line(rust_type: Option<&ResolvedRustType>) -> String {
    match rust_type {
        Some(rust_type) if rust_type.boxed => format!("Rust type: `{}` (boxed)", rust_type.name),
        Some(rust_type) => format!("Rust type: `{}`", rust_type.name),
        None => "Rust type: unavailable until the grammar parses successfully.".to_string(),
    }
}

pub(crate) fn keyword_documentation(label: &str) -> Option<String> {
    let documentation = match label {
        "%token" => format!(
            "Defines a terminal symbol and the Rust pattern that recognizes it.\n\nExample:\n\n```rustylr\n%token num Token::Num(_);\n```\n\n[Token definition]({SYNTAX_URL}#token-definition-must-defined)"
        ),
        "%start" => format!(
            "Declares a start non-terminal for parser generation.\n\nExample:\n\n```rustylr\n%start Expr;\n```\n\n[Start symbol]({SYNTAX_URL}#start-symbol-must-defined)"
        ),
        "%tokentype" => format!(
            "Sets the Rust type used as the parser's input terminal token type.\n\nExample:\n\n```rustylr\n%tokentype Token;\n```\n\n[Token type]({SYNTAX_URL}#token-type-must-defined)"
        ),
        "%userdata" => format!(
            "Sets the mutable user-data type threaded through parser contexts and reduce actions.\n\nExample:\n\n```rustylr\n%userdata ParserState;\n```\n\n[Userdata type]({SYNTAX_URL}#userdata-type-optional)"
        ),
        "%error" | "%errortype" => format!(
            "Sets the custom error type returned by reduce actions.\n\nExample:\n\n```rustylr\n%error String;\n```\n\n[Error type]({SYNTAX_URL}#error-type-optional)"
        ),
        "%location" => format!(
            "Sets the source-location type used by `@...` location bindings.\n\nExample:\n\n```rustylr\n%location Span;\n```\n\n[Location tracking]({SYNTAX_URL}#location-tracking)"
        ),
        "%left" => format!(
            "Declares left-associative operator precedence for one or more terminals.\n\nExample:\n\n```rustylr\n%left plus minus;\n```\n\n[Operator precedence]({SYNTAX_URL}#operator-precedence)"
        ),
        "%right" => format!(
            "Declares right-associative operator precedence for one or more terminals.\n\nExample:\n\n```rustylr\n%right caret;\n```\n\n[Operator precedence]({SYNTAX_URL}#operator-precedence)"
        ),
        "%precedence" => format!(
            "Declares precedence without associativity.\n\nExample:\n\n```rustylr\n%precedence unary_minus;\n```\n\n[Operator precedence]({SYNTAX_URL}#operator-precedence)"
        ),
        "%prec" => format!(
            "Overrides the precedence of a specific production line.\n\nExample:\n\n```rustylr\nExpr : minus Expr %prec unary_minus {{ Expr }};\n```\n\n[Explicit precedence]({SYNTAX_URL}#explicit-precedence-prec)"
        ),
        "%dprec" => format!(
            "Assigns a dynamic precedence priority to a production, mainly for GLR reduce/reduce control.\n\nExample:\n\n```rustylr\nExpr : Expr star Expr %dprec 2 {{ ... }};\n```\n\n[Rule priority]({SYNTAX_URL}#rule-priority)"
        ),
        "%glr" => format!(
            "Enables Generalized LR parser generation for ambiguous grammars.\n\nExample:\n\n```rustylr\n%glr;\n```\n\n[GLR parser generation]({SYNTAX_URL}#glr-parser-generation)"
        ),
        "%lalr" => format!(
            "Generates LALR(1) parsing tables instead of the default LR construction.\n\nExample:\n\n```rustylr\n%lalr;\n```\n\n[LALR parser generation]({SYNTAX_URL}#lalr-parser-generation)"
        ),
        "%nooptim" => format!(
            "Disables parser table optimization.\n\nExample:\n\n```rustylr\n%nooptim;\n```\n\n[No optimization]({SYNTAX_URL}#no-optimization)"
        ),
        "%allow" => format!(
            "Suppresses a RustyLR diagnostic globally or for a specific target.\n\nExample:\n\n```rustylr\n%allow unused_terminals(plus);\n```\n\n[Diagnostic suppression]({SYNTAX_URL}#diagnostic-suppression)"
        ),
        "%moduleprefix" => {
            "Internal directive used by RustyLR's own generated parser code. Most grammars should not use this directly.".to_string()
        }
        "error" => format!(
            "Reserved terminal used for panic-mode error recovery.\n\nExample:\n\n```rustylr\nBlock : lbrace error rbrace {{ recover() }};\n```\n\n[Panic-mode error recovery]({SYNTAX_URL}#panic-mode-error-recovery)"
        ),
        "$sep" => format!(
            "Pattern helper for separated repetition.\n\nExample:\n\n```rustylr\nList : $sep(Item, comma, +) {{ Item }};\n```\n\n[Patterns]({SYNTAX_URL}#patterns)"
        ),
        "data" => format!(
            "Mutable user-data binding available inside reduce actions.\n\nExample:\n\n```rustylr\nExpr : num {{ data.count += 1; num }};\n```\n\n[User data]({SYNTAX_URL}#4-user-data-data)"
        ),
        "lookahead" => format!(
            "GLR reduce-action control binding for inspecting the next terminal.\n\nExample:\n\n```rustylr\nif let Some(term) = lookahead.to_term() {{ /* ... */ }}\n```\n\n[Advanced GLR reduce controls]({SYNTAX_URL}#advanced-glr-reduce-controls)"
        ),
        "shift" => format!(
            "GLR reduce-action control binding used to allow or prune a shift branch.\n\nExample:\n\n```rustylr\n*shift = false;\n```\n\n[Advanced GLR reduce controls]({SYNTAX_URL}#advanced-glr-reduce-controls)"
        ),
        // `Err` is a Rust stdlib variant, but the LSP provides grammar-aware context by
        // reflecting the `%error` type as `Result::Err(%error type)` in the detail string.
        "Err" => format!(
            "`Err` constructs the error variant returned from a reduce action's `Result<_, %error>`.\n\nExample:\n\n```rustylr\nExpr : num {{ Err(MyError::InvalidNumber)? }};\n```\n\n[Error type]({SYNTAX_URL}#error-type-optional)"
        ),
        _ => return None,
    };
    Some(documentation)
}

pub(crate) fn substitution_documentation(label: &str) -> Option<String> {
    let documentation = match label {
        "$tokentype" => "`$tokentype` substitutes to the type defined by `%tokentype`.",
        "$location" => "`$location` substitutes to the type defined by `%location`.",
        "$userdata" => "`$userdata` substitutes to the type defined by `%userdata`.",
        "$error" => "`$error` substitutes to the configured reduce-action error type.",
        "$errortype" => "`$errortype` is an alias for the configured reduce-action error type.",
        _ => return None,
    };
    Some(format!(
        "{documentation}\n\nExample:\n\n```rustylr\nRule($tokentype) : token {{ $tokentype }};\n```\n\n[Variable substitution]({SYNTAX_URL}#variable-substitution)"
    ))
}

pub(crate) fn location_documentation(label: &str) -> Option<String> {
    Some(format!(
        "`{label}` refers to a source-location value in the current reduce action.\n\nExamples:\n\n```rustylr\nExpr : left=Expr plus right=Term {{ println!(\"{{:?}}\", @left); }};\nExpr : Expr plus Term {{ println!(\"{{:?}}\", @1); }};\nExpr : Term {{ println!(\"{{:?}}\", @$); }};\n```\n\n[Location tracking]({SYNTAX_URL}#location-tracking)"
    ))
}

pub(crate) fn allow_diagnostic_documentation(name: &str) -> Option<String> {
    Some(format!(
        "Diagnostic suppression name `{name}`.\n\nExample:\n\n```rustylr\n%allow {name};\n%allow {name}(SomeTarget);\n```\n\n[Diagnostic suppression]({SYNTAX_URL}#diagnostic-suppression)"
    ))
}

pub(crate) fn markdown_documentation(value: String) -> Documentation {
    Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value,
    })
}

struct CompletionBuilder {
    range: Range,
    seen: BTreeSet<String>,
    items: Vec<CompletionItem>,
}

impl CompletionBuilder {
    fn new(range: Range) -> Self {
        CompletionBuilder {
            range,
            seen: BTreeSet::new(),
            items: Vec::new(),
        }
    }

    fn terminal(&mut self, label: &str, documentation: String) {
        self.push(
            label,
            CompletionItemKind::ENUM_MEMBER,
            "terminal symbol",
            Some(documentation),
        );
    }

    fn nonterminal(&mut self, label: &str, documentation: String) {
        self.push(
            label,
            CompletionItemKind::CLASS,
            "non-terminal symbol",
            Some(documentation),
        );
    }

    fn keyword(&mut self, label: &str, detail: &str, documentation: Option<String>) {
        self.push(label, CompletionItemKind::KEYWORD, detail, documentation);
    }

    fn variable(&mut self, label: &str, detail: &str, documentation: Option<String>) {
        self.push(label, CompletionItemKind::VARIABLE, detail, documentation);
    }

    fn push(
        &mut self,
        label: &str,
        kind: CompletionItemKind,
        detail: &str,
        documentation: Option<String>,
    ) {
        if !self.seen.insert(label.to_string()) {
            return;
        }

        self.items.push(CompletionItem {
            label: label.to_string(),
            kind: Some(kind),
            detail: Some(detail.to_string()),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: self.range,
                new_text: label.to_string(),
            })),
            documentation: documentation.map(markdown_documentation),
            ..Default::default()
        });
    }

    fn finish(self) -> Vec<CompletionItem> {
        self.items
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MOCK_GRAMMAR: &str = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
    Plus,
}

%%

%tokentype Token;
%start E;

%token num Token::Num(_);
%token plus Token::Plus;

E(i32) : left=E plus num { $ }
       | num { num }
       ;

Boxed(box $tokentype) : num { num };
"#;

    fn labels(response: CompletionResponse) -> BTreeSet<String> {
        match response {
            CompletionResponse::Array(items) => items
                .into_iter()
                .map(|item| item.label)
                .collect::<BTreeSet<_>>(),
            _ => BTreeSet::new(),
        }
    }

    fn items(response: CompletionResponse) -> Vec<CompletionItem> {
        match response {
            CompletionResponse::Array(items) => items,
            _ => Vec::new(),
        }
    }

    #[test]
    fn completes_symbols() {
        let pos = offset_to_position(MOCK_GRAMMAR, MOCK_GRAMMAR.find("plus num").unwrap());
        let labels = labels(completions(MOCK_GRAMMAR, pos));
        assert!(labels.contains("E"));
        assert!(labels.contains("num"));
        assert!(labels.contains("plus"));
        assert!(labels.contains("error"));
    }

    #[test]
    fn completes_dollar_variables() {
        let offset = MOCK_GRAMMAR.find("$ }").unwrap() + 1;
        let labels = labels(completions(
            MOCK_GRAMMAR,
            offset_to_position(MOCK_GRAMMAR, offset),
        ));
        // Built-in substitution variables.
        assert!(labels.contains("$tokentype"));
        // Non-terminal production type substitution.
        assert!(labels.contains("$E"));
        // Terminal pattern substitution (`%token num Token::Num(_)`).
        assert!(labels.contains("$num"));
        assert!(labels.contains("$plus"));
        // Positional semantic value.
        assert!(labels.contains("$1"));
        // `$left` is only a local named binding, NOT a %token or non-terminal,
        // so it must NOT be offered as a substitution target.
        assert!(!labels.contains("$left"));
    }

    #[test]
    fn completes_directives() {
        let content = "%%\n%";
        let labels = labels(completions(content, Position::new(1, 1)));
        assert!(labels.contains("%token"));
        assert!(labels.contains("%start"));
    }

    #[test]
    fn completion_items_include_markdown_documentation() {
        let pos = offset_to_position(MOCK_GRAMMAR, MOCK_GRAMMAR.find("plus num").unwrap());
        let items = items(completions(MOCK_GRAMMAR, pos));

        let terminal = items.iter().find(|item| item.label == "plus").unwrap();
        let markup = markdown_value(terminal);
        assert!(markup.contains("Rust type: `Token`"));
        assert!(markup.contains("%token plus Token::Plus;"));

        let nonterminal = items.iter().find(|item| item.label == "E").unwrap();
        let markup = markdown_value(nonterminal);
        assert!(markup.contains("Rust type: `i32`"));
        assert!(markup.contains("E(i32)"));
        assert!(markup.contains("E(i32)\n : E plus num"));
        assert!(markup.contains("E plus num"));
        assert!(!markup.contains("left=E"));
        assert!(markup.contains("\n | num"));
        assert!(!markup.contains("{ $ }"));
        assert!(!markup.contains("{ num }"));

        let boxed = items.iter().find(|item| item.label == "Boxed").unwrap();
        let markup = markdown_value(boxed);
        assert!(markup.contains("Rust type: `Token` (boxed)"));
        assert!(markup.contains("Boxed(box $tokentype)"));
        assert!(!markup.contains("{ num }"));
    }

    #[test]
    fn completion_items_use_hover_reduce_action_documentation() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32) }
%%
%moduleprefix ::my_prefix;
%userdata $moduleprefix::UserData;
%location $moduleprefix::Span;
%error $userdata;
%tokentype $moduleprefix::Token;
%start Expr;
%token num Token::Num(_);
Expr : num { let _ = ; let _ = @0; 0 };
"#;
        let action_offset = grammar.find("let _ = ;").unwrap() + "let _ = ".len();
        let action_items = items(completions(
            grammar,
            offset_to_position(grammar, action_offset),
        ));

        let data = action_items
            .iter()
            .find(|item| item.label == "data")
            .unwrap();
        assert_eq!(
            data.detail.as_deref(),
            Some("data: &mut ::my_prefix::UserData")
        );
        assert!(markdown_value(data).contains("data: &mut ::my_prefix::UserData"));

        let lookahead = action_items
            .iter()
            .find(|item| item.label == "lookahead")
            .unwrap();
        assert_eq!(
            lookahead.detail.as_deref(),
            Some("lookahead: &::my_prefix::Token")
        );
        assert!(markdown_value(lookahead).contains("lookahead: &::my_prefix::Token"));

        let shift = action_items
            .iter()
            .find(|item| item.label == "shift")
            .unwrap();
        assert_eq!(shift.detail.as_deref(), Some("shift: &mut bool"));
        assert!(markdown_value(shift).contains("shift: &mut bool"));

        let err = action_items
            .iter()
            .find(|item| item.label == "Err")
            .unwrap();
        assert_eq!(
            err.detail.as_deref(),
            Some("Result::Err(::my_prefix::UserData)")
        );
        assert!(markdown_value(err).contains("Result::Err(::my_prefix::UserData)"));

        let location_offset = grammar.find("@0").unwrap() + 1;
        let location_items = items(completions(
            grammar,
            offset_to_position(grammar, location_offset),
        ));
        let at0 = location_items
            .iter()
            .find(|item| item.label == "@0")
            .unwrap();
        assert_eq!(at0.detail.as_deref(), Some("@0: &mut ::my_prefix::Span"));
        assert!(markdown_value(at0).contains("@0: &mut ::my_prefix::Span"));
    }

    #[test]
    fn completes_mapped_symbols_inside_reduce_action() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32), Plus }
%%
%tokentype Token;
%start Expr;
%token num Token::Num(_);
%token plus Token::Plus;
Expr(i32) : left=Expr plus right=Expr {  };
"#;
        let offset = grammar.find("{  }").unwrap() + 2;
        let completion_items = items(completions(grammar, offset_to_position(grammar, offset)));

        let left = completion_items
            .iter()
            .find(|item| item.label == "left")
            .unwrap();
        assert_eq!(left.detail.as_deref(), Some("left: i32"));
        let left_markup = markdown_value(left);
        assert!(left_markup.contains("left: i32"));
        assert!(left_markup.contains("Expr : left=Expr plus right=Expr"));
        assert!(left_markup.contains("       ^^^^^^^^^"));

        let right = completion_items
            .iter()
            .find(|item| item.label == "right")
            .unwrap();
        assert_eq!(right.detail.as_deref(), Some("right: i32"));
        let right_markup = markdown_value(right);
        assert!(right_markup.contains("right: i32"));
        assert!(right_markup.contains("Expr : left=Expr plus right=Expr"));
        assert!(right_markup.contains("                      ^^^^^^^^^"));

        let dollar_grammar = grammar.replacen("{  }", "{ $ }", 1);
        let dollar_offset = dollar_grammar.find("{ $").unwrap() + 3;
        let dollar_items = items(completions(
            &dollar_grammar,
            offset_to_position(&dollar_grammar, dollar_offset),
        ));
        let first = dollar_items.iter().find(|item| item.label == "$1").unwrap();
        assert_eq!(first.detail.as_deref(), Some("$1: i32"));
        assert!(markdown_value(first).contains("       ^^^^^^^^^"));

        let location_grammar = grammar.replacen("{  }", "{ @ }", 1);
        let location_offset = location_grammar.find("{ @").unwrap() + 3;
        let location_items = items(completions(
            &location_grammar,
            offset_to_position(&location_grammar, location_offset),
        ));
        let first_location = location_items
            .iter()
            .find(|item| item.label == "@1")
            .unwrap();
        assert!(markdown_value(first_location).contains("       ^^^^^^^^^"));
    }

    fn markdown_value(item: &CompletionItem) -> &str {
        let documentation = item.documentation.as_ref().unwrap();
        let Documentation::MarkupContent(markup) = documentation else {
            panic!("expected markdown documentation");
        };
        &markup.value
    }
}
