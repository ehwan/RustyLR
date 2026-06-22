use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionResponse, CompletionTextEdit, Position, Range,
    TextEdit,
};
use proc_macro2::{TokenStream, TokenTree};
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::{GrammarArgs, PatternArgs};
use std::collections::BTreeSet;
use std::str::FromStr;

use crate::diagnostics::split_stream;
use crate::position::{offset_to_position, position_to_offset};

const DIRECTIVES: &[&str] = &[
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
    "%prec",
    "%dprec",
    "%glr",
    "%lalr",
    "%nooptim",
    "%allow",
    "%moduleprefix",
];

const SUBSTITUTION_VARIABLES: &[&str] = &[
    "$tokentype",
    "$location",
    "$userdata",
    "$error",
    "$errortype",
];

const ALLOW_DIAGNOSTICS: &[&str] = &[
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

const KEYWORDS: &[&str] = &[
    "error",
    "auto",
    "dense",
    "sparse",
    "$sep",
    "data",
    "lookahead",
    "shift",
];

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
        .map(CompletionNames::from_args)
        .unwrap_or_else(|| CompletionNames::from_text(content));
    let line_variables = parsed
        .as_ref()
        .map(|args| variables_for_offset(args, content, offset))
        .unwrap_or_default();

    let mut builder = CompletionBuilder::new(replace_range);

    match mode {
        CompletionMode::Directive => {
            for directive in DIRECTIVES {
                builder.keyword(directive, "RustyLR directive");
            }
        }
        CompletionMode::Dollar => {
            for variable in SUBSTITUTION_VARIABLES {
                builder.variable(variable, "built-in RustCode substitution");
            }
            for name in &names.nonterminals {
                builder.variable(&format!("${name}"), "non-terminal production type");
            }
            for name in &names.terminals {
                builder.variable(&format!("${name}"), "terminal definition substitution");
            }
            for variable in &line_variables.value_names {
                builder.variable(&format!("${variable}"), "current production binding");
            }
            for index in 1..=line_variables.value_count {
                builder.variable(&format!("${index}"), "positional semantic value");
            }
        }
        CompletionMode::Location => {
            builder.variable("@$", "current production location");
            builder.variable("@0", "current production location");
            for variable in &line_variables.value_names {
                builder.variable(
                    &format!("@{variable}"),
                    "current production binding location",
                );
            }
            for index in 1..=line_variables.value_count {
                builder.variable(&format!("@{index}"), "positional location");
            }
        }
        CompletionMode::AllowDiagnostic => {
            for diagnostic in ALLOW_DIAGNOSTICS {
                builder.keyword(diagnostic, "diagnostic suppression name");
            }
            add_symbol_items(&mut builder, &names);
        }
        CompletionMode::Symbol => {
            add_symbol_items(&mut builder, &names);
            for keyword in KEYWORDS {
                builder.keyword(keyword, "RustyLR keyword");
            }
            for directive in DIRECTIVES {
                builder.keyword(directive, "RustyLR directive");
            }
        }
    }

    CompletionResponse::Array(builder.finish())
}

fn add_symbol_items(builder: &mut CompletionBuilder, names: &CompletionNames) {
    for name in &names.nonterminals {
        builder.nonterminal(name);
    }
    for name in &names.terminals {
        builder.terminal(name);
    }
}

fn parse_args(content: &str) -> Result<GrammarArgs, ()> {
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

fn current_prefix_start(content: &str, offset: usize, include_sigils: bool) -> usize {
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

fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

#[derive(Default)]
struct CompletionNames {
    terminals: BTreeSet<String>,
    nonterminals: BTreeSet<String>,
}

impl CompletionNames {
    fn from_args(args: &GrammarArgs) -> Self {
        let mut names = CompletionNames::default();
        for (terminal, _) in &args.terminals {
            names.terminals.insert(terminal.value().clone());
        }
        for rule in &args.rules {
            names.nonterminals.insert(rule.name.value().clone());
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
                    names.terminals.insert(name.to_string());
                }
                continue;
            }

            if line.starts_with('%') {
                continue;
            }

            if let Some(colon_idx) = line.find(':') {
                let head = &line[..colon_idx];
                if let Some(name) = first_ident(head) {
                    names.nonterminals.insert(name.to_string());
                }
            }
        }

        names
    }
}

fn first_ident(text: &str) -> Option<&str> {
    let start = text.find(|ch: char| ch == '_' || ch.is_ascii_alphabetic())?;
    let rest = &text[start..];
    let end = rest
        .find(|ch: char| !(ch == '_' || ch.is_ascii_alphanumeric()))
        .unwrap_or(rest.len());
    Some(&rest[..end])
}

#[derive(Default)]
struct LineVariables {
    value_names: BTreeSet<String>,
    value_count: usize,
}

fn variables_for_offset(args: &GrammarArgs, content: &str, offset: usize) -> LineVariables {
    for rule in &args.rules {
        for (line_idx, line) in rule.rule_lines.iter().enumerate() {
            let start = args
                .span_manager
                .get_byterange(&line.separator_location)
                .map_or(0, |range| range.start);
            let end = rule_line_end(args, content, rule, line_idx);
            if start <= offset && offset <= end {
                let mut variables = LineVariables::default();
                for (mapped_name, pattern) in &line.tokens {
                    variables.value_count += 1;
                    if let Some(name) = mapped_name {
                        variables.value_names.insert(name.value().clone());
                    } else {
                        collect_default_bindings(pattern, &mut variables.value_names);
                    }
                }
                return variables;
            }
        }
    }

    LineVariables::default()
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

fn token_stream_end(stream: &TokenStream) -> usize {
    stream
        .clone()
        .into_iter()
        .map(token_tree_end)
        .max()
        .unwrap_or(0)
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

    fn terminal(&mut self, label: &str) {
        self.push(label, CompletionItemKind::ENUM_MEMBER, "terminal symbol");
    }

    fn nonterminal(&mut self, label: &str) {
        self.push(label, CompletionItemKind::CLASS, "non-terminal symbol");
    }

    fn keyword(&mut self, label: &str, detail: &str) {
        self.push(label, CompletionItemKind::KEYWORD, detail);
    }

    fn variable(&mut self, label: &str, detail: &str) {
        self.push(label, CompletionItemKind::VARIABLE, detail);
    }

    fn push(&mut self, label: &str, kind: CompletionItemKind, detail: &str) {
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
        assert!(labels.contains("$tokentype"));
        assert!(labels.contains("$E"));
        assert!(labels.contains("$num"));
        assert!(labels.contains("$left"));
        assert!(labels.contains("$1"));
    }

    #[test]
    fn completes_directives() {
        let content = "%%\n%";
        let labels = labels(completions(content, Position::new(1, 1)));
        assert!(labels.contains("%token"));
        assert!(labels.contains("%start"));
    }
}
