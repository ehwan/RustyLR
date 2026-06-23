use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use proc_macro2::TokenStream;
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::terminal_info::TerminalName;
use rusty_lr_parser::{GrammarArgs, Location, PatternArgs, TerminalSetItem};
use std::collections::BTreeSet;
use std::ops::Range as ByteRange;

use crate::lsp::completion::{
    self, ALLOW_DIAGNOSTICS, DIRECTIVES, KEYWORDS, SUBSTITUTION_VARIABLES, SYNTAX_URL,
};
use crate::lsp::position::position_to_offset;

pub fn hover(content: &str, position: Position) -> Option<Hover> {
    let offset = position_to_offset(content, position);
    let parsed = completion::parse_args(content).ok();

    if let Some(args) = &parsed {
        if let Some((brace_range, doc)) = reduce_action_brace_at_offset(args, content, offset) {
            return Some(markdown_hover(content, doc, Some(brace_range)));
        }

        if let Some((pattern, range)) = pattern_at_offset(args, offset) {
            return Some(markdown_hover(
                content,
                pattern_documentation(args, pattern, content),
                Some(range),
            ));
        }
    }

    let word = hover_word(content, offset)?;
    let mut documentation = None;

    if let Some(args) = &parsed {
        let mut assoc_type = "";
        let mut declaration_items = Vec::new();
        let mut found_prec = false;
        for (_, assoc, items) in &args.precedences {
            if items.iter().any(|item| item.to_string() == word) {
                assoc_type = match assoc {
                    Some(rusty_lr_core::production::Associativity::Left) => "%left",
                    Some(rusty_lr_core::production::Associativity::Right) => "%right",
                    None => "%precedence",
                };
                declaration_items = items.iter().map(|i| i.to_string()).collect();
                found_prec = true;
                break;
            }
        }

        if found_prec {
            documentation = Some(format!(
                "### Precedence Symbol `{}`\n\nDeclared via:\n```rustylr\n{} {};\n```",
                word,
                assoc_type,
                declaration_items.join(" ")
            ));
        }
    }

    if documentation.is_none() {
        if let Some(args) = &parsed {
            documentation = reduce_action_variable_documentation(args, &word);
        }
    }

    if documentation.is_none() {
        documentation = hover_word_documentation(&word);
    }

    let documentation = documentation?;
    Some(markdown_hover(content, documentation, None))
}

fn markdown_hover(content: &str, value: String, range: Option<ByteRange<usize>>) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: range.map(|range| crate::lsp::position::range_to_lsp_range(content, range)),
    }
}

fn reduce_action_brace_at_offset(
    args: &GrammarArgs,
    content: &str,
    offset: usize,
) -> Option<(ByteRange<usize>, String)> {
    for rule in &args.rules {
        for line in &rule.rule_lines {
            if let Some(reduce_action) = &line.reduce_action {
                if let Some(proc_macro2::TokenTree::Group(group)) =
                    reduce_action.clone().into_iter().next()
                {
                    if group.delimiter() == proc_macro2::Delimiter::Brace {
                        let action_range = group.span().byte_range();

                        // Check start brace(s)
                        if action_range.start < content.len()
                            && content.as_bytes()[action_range.start] == b'{'
                        {
                            let start_brace_end = if action_range.start + 1 < action_range.end
                                && content.as_bytes()[action_range.start + 1] == b'{'
                            {
                                action_range.start + 2
                            } else {
                                action_range.start + 1
                            };
                            let start_brace_range = action_range.start..start_brace_end;
                            if start_brace_range.contains(&offset) {
                                return Some((start_brace_range, reduce_action_documentation()));
                            }
                        }

                        // Check end brace(s)
                        if action_range.end > action_range.start
                            && action_range.end <= content.len()
                        {
                            if content.as_bytes()[action_range.end - 1] == b'}' {
                                let end_brace_start = if action_range.end - 2 >= action_range.start
                                    && content.as_bytes()[action_range.end - 2] == b'}'
                                {
                                    action_range.end - 2
                                } else {
                                    action_range.end - 1
                                };
                                let end_brace_range = end_brace_start..action_range.end;
                                if end_brace_range.contains(&offset) {
                                    return Some((end_brace_range, reduce_action_documentation()));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn reduce_action_documentation() -> String {
    format!(
        "### Reduce Action\n\nA block of Rust code executed when this production rule is reduced.\n\n[Reduce Actions]({}#reduceaction-optional)",
        SYNTAX_URL
    )
}

fn reduce_action_variable_documentation(args: &GrammarArgs, word: &str) -> Option<String> {
    match word {
        "data" => Some(data_documentation(args)),
        "lookahead" => Some(lookahead_documentation(args)),
        "shift" => Some(shift_documentation()),
        "Err" => Some(err_variant_documentation(args)),
        "@0" | "@$" => Some(current_location_documentation(args, word)),
        _ => None,
    }
}

fn data_documentation(args: &GrammarArgs) -> String {
    let userdata_type = grammar_type_name(&args.userdata_typename, "()");
    let definition_info = type_definition("userdata", &args.userdata_typename);

    format!(
        "### `data: &mut {userdata_type}`{definition_info}\n\nMutable user-data binding available inside reduce actions.\n\nExample:\n\n```rustylr\nExpr : num {{ data.count += 1; num }};\n```\n\n[User data]({SYNTAX_URL}#4-user-data-data)"
    )
}

fn lookahead_documentation(args: &GrammarArgs) -> String {
    let token_type = token_type_name(args);
    let definition_info = type_definition("tokentype", &args.token_typename);

    format!(
        "### `lookahead: &{token_type}`{definition_info}\n\nGLR reduce-action control binding for inspecting the next terminal.\n\nExample:\n\n```rustylr\nif let Some(term) = lookahead.to_term() {{ /* ... */ }}\n```\n\n[Advanced GLR reduce controls]({SYNTAX_URL}#advanced-glr-reduce-controls)"
    )
}

fn shift_documentation() -> String {
    format!(
        "### `shift: &mut bool`\n\nGLR reduce-action control binding used to allow or prune a shift branch.\n\nExample:\n\n```rustylr\n*shift = false;\n```\n\n[Advanced GLR reduce controls]({SYNTAX_URL}#advanced-glr-reduce-controls)"
    )
}

fn err_variant_documentation(args: &GrammarArgs) -> String {
    let error_type = grammar_type_name(
        &args.error_typename,
        "$moduleprefix::DefaultReduceActionError",
    );
    let definition_info = type_definition("error", &args.error_typename);

    format!(
        "### `Result::Err({error_type})`{definition_info}\n\n`Err` constructs the error variant returned from a reduce action's `Result<_, {error_type}>`.\n\nExample:\n\n```rustylr\nExpr : num {{ Err(MyError::InvalidNumber)? }};\n```\n\n[Error type]({SYNTAX_URL}#error-type-optional)"
    )
}

fn current_location_documentation(args: &GrammarArgs, label: &str) -> String {
    let location_type =
        grammar_type_name(&args.location_typename, "$moduleprefix::DefaultLocation");
    let definition_info = type_definition("location", &args.location_typename);

    format!(
        "### `{label}: &mut {location_type}`{definition_info}\n\nCurrent production source-location binding available inside reduce actions.\n\nExamples:\n\n```rustylr\nExpr : Term {{ println!(\"{{:?}}\", @$); }};\nExpr : {{ println!(\"{{:?}}\", @0); }};\n```\n\n[Location tracking]({SYNTAX_URL}#location-tracking)"
    )
}

fn grammar_type_name(items: &[(Location, TokenStream)], default: &str) -> String {
    items
        .first()
        .map(|(_, ty)| ty.to_string())
        .filter(|ty| !ty.is_empty())
        .unwrap_or_else(|| default.to_string())
}

fn token_type_name(args: &GrammarArgs) -> String {
    Grammar::from_grammar_args(args.clone())
        .ok()
        .map(|grammar| grammar.token_type().to_string())
        .unwrap_or_else(|| grammar_type_name(&args.token_typename, "()"))
}

fn type_definition(directive: &str, items: &[(Location, TokenStream)]) -> String {
    let Some((location, ty)) = items.first() else {
        return String::new();
    };
    if matches!(location, Location::CallSite) {
        return String::new();
    }

    format!("\n\nDefinition:\n```rustylr\n%{directive} {};\n```", ty)
}

fn hover_word(content: &str, offset: usize) -> Option<String> {
    let mut offset = offset.min(content.len());
    if offset < content.len() {
        let ch = content[offset..].chars().next()?;
        if ch == '@' || ch == '$' || ch == '%' {
            offset += ch.len_utf8();
        }
    }

    let start = completion::current_prefix_start(content, offset, true);
    let mut end = offset;
    while end < content.len() {
        let ch = content[end..].chars().next()?;
        if completion::is_ident_continue(ch) {
            end += ch.len_utf8();
        } else if ch == '$' && &content[start..end] == "@" {
            end += ch.len_utf8();
            break;
        } else if ch == '$' && &content[start..end] == "$" {
            end += ch.len_utf8();
            break;
        } else {
            break;
        }
    }
    if start == end {
        return None;
    }
    Some(content[start..end].to_string())
}

fn hover_word_documentation(word: &str) -> Option<String> {
    if DIRECTIVES.contains(&word) || KEYWORDS.contains(&word) {
        return completion::keyword_documentation(word);
    }
    if SUBSTITUTION_VARIABLES.contains(&word) {
        return completion::substitution_documentation(word);
    }
    if word.starts_with('@') {
        return completion::location_documentation(word);
    }
    if ALLOW_DIAGNOSTICS.contains(&word) {
        return completion::allow_diagnostic_documentation(word);
    }
    None
}

fn pattern_at_offset(
    args: &GrammarArgs,
    offset: usize,
) -> Option<(&PatternArgs, ByteRange<usize>)> {
    for rule in &args.rules {
        for line in &rule.rule_lines {
            for (_, pattern) in &line.tokens {
                if let Some(range) = args.span_manager.get_byterange(&pattern.location()) {
                    if range.contains(&offset) {
                        return Some((pattern, range));
                    }
                }
            }
        }
    }
    None
}

fn pattern_documentation(args: &GrammarArgs, pattern: &PatternArgs, content: &str) -> String {
    if let Some(documentation) = identifier_pattern_documentation(args, pattern, content) {
        return documentation;
    }

    let pattern_text = pattern_text(args, pattern, content);
    let grammar = Grammar::from_grammar_args(args.clone()).ok();
    let pattern_type = grammar
        .as_ref()
        .and_then(|grammar| pattern_type(args, grammar, pattern));
    let type_line = hover_type_line(pattern_type.as_ref());
    let subterms = grammar
        .as_ref()
        .map(|grammar| subterm_documentation(args, grammar, pattern, content))
        .unwrap_or_default();
    let keyword = pattern_keyword_documentation(pattern);

    let mut documentation = format!("Pattern `{pattern_text}`.\n\n{type_line}");
    if !subterms.is_empty() {
        documentation.push_str("\n\n");
        documentation.push_str(&subterms);
    }
    if let Some(keyword) = keyword {
        documentation.push_str("\n\n---\n\n");
        documentation.push_str(&keyword);
    }
    documentation.push_str(&format!("\n\n[Patterns]({SYNTAX_URL}#patterns)"));
    documentation
}

fn identifier_pattern_documentation(
    args: &GrammarArgs,
    pattern: &PatternArgs,
    content: &str,
) -> Option<String> {
    let PatternArgs::Ident(ident) = pattern else {
        return None;
    };

    let grammar = Grammar::from_grammar_args(args.clone()).ok()?;
    nonterminal_symbol_documentation(args, &grammar, content, ident.value())
        .or_else(|| terminal_symbol_documentation(args, &grammar, content, ident.value()))
        .or_else(|| pattern_keyword_documentation(pattern))
}

fn pattern_text(args: &GrammarArgs, pattern: &PatternArgs, content: &str) -> String {
    args.span_manager
        .get_byterange(&pattern.location())
        .and_then(|range| content.get(range))
        .map(str::trim)
        .filter(|text| !text.is_empty())
        .map(str::to_string)
        .unwrap_or_else(|| pattern.to_string())
}

fn subterm_documentation(
    args: &GrammarArgs,
    grammar: &Grammar,
    pattern: &PatternArgs,
    content: &str,
) -> String {
    let mut seen_nonterminals = BTreeSet::new();
    let mut seen_terminals = BTreeSet::new();
    let mut symbols = Vec::new();
    collect_symbol_documentation(
        args,
        grammar,
        pattern,
        content,
        &mut seen_nonterminals,
        &mut seen_terminals,
        &mut symbols,
    );

    let mut seen_syntax = BTreeSet::new();
    let mut syntax = Vec::new();
    collect_pattern_syntax(pattern, &mut seen_syntax, &mut syntax);

    let mut sections = Vec::new();
    if !symbols.is_empty() {
        sections.push(format!("Identifiers:\n\n{}", symbols.join("\n\n")));
    }
    if !syntax.is_empty() {
        sections.push(format!("Pattern syntax:\n\n{}", syntax.join("\n")));
    }
    sections.join("\n\n")
}

fn collect_symbol_documentation(
    args: &GrammarArgs,
    grammar: &Grammar,
    pattern: &PatternArgs,
    content: &str,
    seen_nonterminals: &mut BTreeSet<String>,
    seen_terminals: &mut BTreeSet<String>,
    symbols: &mut Vec<String>,
) {
    match pattern {
        PatternArgs::Ident(ident) => {
            let name = ident.value();
            if let Some(symbol) = nonterminal_symbol_documentation(args, grammar, content, name)
                .filter(|_| seen_nonterminals.insert(name.clone()))
            {
                symbols.push(symbol);
            } else if let Some(symbol) = terminal_symbol_documentation(args, grammar, content, name)
                .filter(|_| seen_terminals.insert(name.clone()))
            {
                symbols.push(symbol);
            }
        }
        PatternArgs::TerminalSet(terminal_set) => {
            for item in &terminal_set.items {
                match item {
                    TerminalSetItem::Terminal(ident) => {
                        let name = ident.value();
                        if let Some(symbol) =
                            terminal_symbol_documentation(args, grammar, content, name)
                                .filter(|_| seen_terminals.insert(name.clone()))
                        {
                            symbols.push(symbol);
                        }
                    }
                    TerminalSetItem::Range(first, last) => {
                        for ident in [first, last] {
                            let name = ident.value();
                            if let Some(symbol) =
                                terminal_symbol_documentation(args, grammar, content, name)
                                    .filter(|_| seen_terminals.insert(name.clone()))
                            {
                                symbols.push(symbol);
                            }
                        }
                    }
                    TerminalSetItem::Byte(_)
                    | TerminalSetItem::ByteRange(_, _)
                    | TerminalSetItem::Char(_)
                    | TerminalSetItem::CharRange(_, _) => {}
                }
            }
        }
        _ => {}
    }

    for child in pattern_children(pattern) {
        collect_symbol_documentation(
            args,
            grammar,
            child,
            content,
            seen_nonterminals,
            seen_terminals,
            symbols,
        );
    }
}

fn nonterminal_symbol_documentation(
    args: &GrammarArgs,
    grammar: &Grammar,
    content: &str,
    name: &str,
) -> Option<String> {
    let rule = args.rules.iter().find(|rule| rule.name.value() == name)?;
    let (ty, boxed) = grammar.nonterminal_type(name)?;
    let type_line = hover_type_line(rust_type(ty, boxed).as_ref());
    let definition = completion::rule_definition_text(args, content, rule);
    Some(format!(
        "**Non-terminal `{name}`**\n\n{type_line}\n\nDefinition:\n\n{}",
        definition_code_block(&definition)
    ))
}

fn terminal_symbol_documentation(
    args: &GrammarArgs,
    grammar: &Grammar,
    content: &str,
    name: &str,
) -> Option<String> {
    let (terminal, _) = args
        .terminals
        .iter()
        .find(|(terminal, _)| terminal.value() == name)?;
    let type_line = hover_type_line(Some(&token_type(grammar)));
    let definition = completion::line_text_for_location(args, content, &terminal.location());
    Some(format!(
        "**Terminal `{name}`**\n\n{type_line}\n\nDefinition:\n\n{}",
        definition_code_block(&definition)
    ))
}

fn definition_code_block(definition: &str) -> String {
    format!("```rustylr\n{definition}\n```")
}

fn collect_pattern_syntax(
    pattern: &PatternArgs,
    seen: &mut BTreeSet<&'static str>,
    syntax: &mut Vec<String>,
) {
    if let Some(label) = pattern_syntax_label(pattern) {
        if seen.insert(label) {
            syntax.push(format!(
                "- `{label}`: {}",
                pattern_syntax_documentation(pattern)
            ));
        }
    }

    for child in pattern_children(pattern) {
        collect_pattern_syntax(child, seen, syntax);
    }
}

fn pattern_syntax_label(pattern: &PatternArgs) -> Option<&'static str> {
    match pattern {
        PatternArgs::Plus { .. } => Some("A+"),
        PatternArgs::Star { .. } => Some("A*"),
        PatternArgs::Question { .. } => Some("A?"),
        PatternArgs::Exclamation { .. } => Some("A!"),
        PatternArgs::TerminalSet(_) => Some("[...]"),
        PatternArgs::Group { .. } => Some("(...)"),
        PatternArgs::Minus { .. } => Some("A - B"),
        PatternArgs::Sep { .. } => Some("$sep(A, Sep, ...)"),
        PatternArgs::Ident(_)
        | PatternArgs::Byte(_)
        | PatternArgs::ByteString(_)
        | PatternArgs::Char(_)
        | PatternArgs::String(_) => None,
    }
}

fn pattern_children(pattern: &PatternArgs) -> Vec<&PatternArgs> {
    match pattern {
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. }
        | PatternArgs::Exclamation { base, .. } => vec![base.as_ref()],
        PatternArgs::Group { alternatives, .. } => alternatives.iter().flatten().collect(),
        PatternArgs::Minus { base, exclude } => vec![base.as_ref(), exclude.as_ref()],
        PatternArgs::Sep {
            base, delimiter, ..
        } => vec![base.as_ref(), delimiter.as_ref()],
        PatternArgs::Ident(_)
        | PatternArgs::TerminalSet(_)
        | PatternArgs::Byte(_)
        | PatternArgs::ByteString(_)
        | PatternArgs::Char(_)
        | PatternArgs::String(_) => Vec::new(),
    }
}

fn pattern_syntax_documentation(pattern: &PatternArgs) -> String {
    match pattern {
        PatternArgs::Ident(_) => {
            "Identifier pattern. It references a terminal or non-terminal symbol.".to_string()
        }
        PatternArgs::Plus { .. } => {
            "`A+` matches one or more repetitions of `A` and collects valued matches into a `Vec`."
                .to_string()
        }
        PatternArgs::Star { .. } => {
            "`A*` matches zero or more repetitions of `A` and collects valued matches into a `Vec`."
                .to_string()
        }
        PatternArgs::Question { .. } => {
            "`A?` matches zero or one `A` and maps valued matches to `Option<A>`.".to_string()
        }
        PatternArgs::Exclamation { .. } => {
            "`A!` matches `A` but discards its semantic value from the production.".to_string()
        }
        PatternArgs::TerminalSet(_) => {
            "Terminal set pattern. It matches one terminal from the set.".to_string()
        }
        PatternArgs::Group { .. } => {
            "Grouped pattern. Alternatives are matched as a nested pattern; valued children are returned as a single value or tuple."
                .to_string()
        }
        PatternArgs::Byte(_) => {
            "Byte literal pattern. It is available when `%tokentype` is `u8`.".to_string()
        }
        PatternArgs::ByteString(_) => {
            "Byte string literal pattern. It expands to a sequence of byte terminals.".to_string()
        }
        PatternArgs::Char(_) => {
            "Character literal pattern. It is available when `%tokentype` is `char`.".to_string()
        }
        PatternArgs::String(_) => {
            "String literal pattern. It expands to a sequence of character terminals.".to_string()
        }
        PatternArgs::Minus { .. } => {
            "`A - B` matches terminals in `A` excluding terminals in `B`.".to_string()
        }
        PatternArgs::Sep { at_least_one, .. } => {
            let quantifier = if *at_least_one {
                "one or more"
            } else {
                "zero or more"
            };
            format!(
                "`$sep(A, Sep, ...)` matches {quantifier} `A` patterns separated by `Sep` and collects valued `A` matches into a `Vec`."
            )
        }
    }
}

fn pattern_keyword_documentation(pattern: &PatternArgs) -> Option<String> {
    match pattern {
        PatternArgs::Ident(ident) if ident.value() == "error" => {
            completion::keyword_documentation("error")
        }
        PatternArgs::Sep { .. } => completion::keyword_documentation("$sep"),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct HoverRustType {
    name: String,
    boxed: bool,
}

pub(crate) fn pattern_final_type(
    args: &GrammarArgs,
    grammar: &Grammar,
    pattern: &PatternArgs,
) -> String {
    hover_type_name(pattern_type(args, grammar, pattern).as_ref())
}

fn hover_type_line(ty: Option<&HoverRustType>) -> String {
    match ty {
        Some(ty) if ty.boxed => format!("Final type: `{}` (boxed)", ty.name),
        Some(ty) => format!("Final type: `{}`", ty.name),
        None => "Final type: `()`".to_string(),
    }
}

fn hover_type_name(ty: Option<&HoverRustType>) -> String {
    match ty {
        Some(ty) if ty.boxed => format!("{} (boxed)", ty.name),
        Some(ty) => ty.name.clone(),
        None => "()".to_string(),
    }
}

pub(crate) fn pattern_type(
    args: &GrammarArgs,
    grammar: &Grammar,
    pattern: &PatternArgs,
) -> Option<HoverRustType> {
    match pattern {
        PatternArgs::Ident(ident) => {
            if ident.value() == "error" {
                return None;
            }
            if grammar
                .terminals_index
                .contains_key(&TerminalName::Ident(ident.value().clone()))
            {
                return Some(token_type(grammar));
            }
            let (ty, boxed) = grammar.nonterminal_type(ident.value())?;
            rust_type(ty, boxed)
        }
        PatternArgs::Plus { base, .. } | PatternArgs::Star { base, .. } => {
            let base_type = pattern_type(args, grammar, base)?;
            Some(HoverRustType {
                name: format!("Vec<{}>", base_type.name),
                boxed: false,
            })
        }
        PatternArgs::Question { base, .. } => {
            let base_type = pattern_type(args, grammar, base)?;
            Some(HoverRustType {
                name: format!("Option<{}>", base_type.name),
                boxed: false,
            })
        }
        PatternArgs::Exclamation { .. } => None,
        PatternArgs::TerminalSet(_) | PatternArgs::Byte(_) | PatternArgs::Char(_) => {
            Some(token_type(grammar))
        }
        PatternArgs::ByteString(_) => Some(HoverRustType {
            name: "&'static [u8]".to_string(),
            boxed: false,
        }),
        PatternArgs::String(_) => Some(HoverRustType {
            name: "&'static str".to_string(),
            boxed: false,
        }),
        PatternArgs::Group { alternatives, .. } => group_type(args, grammar, alternatives),
        PatternArgs::Minus { .. } => Some(token_type(grammar)),
        PatternArgs::Sep { base, .. } => {
            let base_type = pattern_type(args, grammar, base)?;
            Some(HoverRustType {
                name: format!("Vec<{}>", base_type.name),
                boxed: false,
            })
        }
    }
}

fn group_type(
    args: &GrammarArgs,
    grammar: &Grammar,
    alternatives: &[Vec<PatternArgs>],
) -> Option<HoverRustType> {
    let mut alternatives = alternatives
        .iter()
        .map(|alternative| alternative_type(args, grammar, alternative));
    let first = alternatives.next()?;
    if alternatives.all(|ty| ty == first) {
        first
    } else {
        None
    }
}

fn alternative_type(
    args: &GrammarArgs,
    grammar: &Grammar,
    alternative: &[PatternArgs],
) -> Option<HoverRustType> {
    let child_types = alternative
        .iter()
        .filter_map(|pattern| pattern_type(args, grammar, pattern))
        .collect::<Vec<_>>();
    match child_types.len() {
        0 => None,
        1 => child_types.into_iter().next(),
        _ => Some(HoverRustType {
            name: format!(
                "({})",
                child_types
                    .iter()
                    .map(|ty| format!("{},", ty.name))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            boxed: false,
        }),
    }
}

fn token_type(grammar: &Grammar) -> HoverRustType {
    rust_type(Some(grammar.token_type()), grammar.token_type_boxed()).unwrap()
}

fn rust_type(ty: Option<&TokenStream>, boxed: bool) -> Option<HoverRustType> {
    let name = ty.map(TokenStream::to_string).filter(|ty| !ty.is_empty())?;
    Some(HoverRustType { name, boxed })
}

#[cfg(test)]
mod tests {
    use super::*;

    const MOCK_GRAMMAR: &str = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
    Plus,
    Comma,
}

%%

%tokentype Token;
%start List;

%token num Token::Num(_);
%token plus Token::Plus;
%token comma Token::Comma;

E(i32) : value=num { 0 };
List(Vec<i32>) : $sep(E, comma, +) { E };
"#;

    #[test]
    fn hovers_keyword() {
        let offset = MOCK_GRAMMAR.find("%token num").unwrap() + 1;
        let hover = hover(
            MOCK_GRAMMAR,
            crate::lsp::position::offset_to_position(MOCK_GRAMMAR, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("Defines a terminal symbol"));
    }

    #[test]
    fn hovers_identifier_pattern_with_type() {
        let offset = MOCK_GRAMMAR.find("E(i32) : value=num").unwrap();
        let offset = MOCK_GRAMMAR[offset..].find("num").unwrap() + offset;
        let hover = hover(
            MOCK_GRAMMAR,
            crate::lsp::position::offset_to_position(MOCK_GRAMMAR, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("Final type: `Token`"));
        assert!(markup.value.contains("**Terminal `num`**"));
        assert!(markup
            .value
            .contains("```rustylr\n%token num Token::Num(_);\n```"));
        assert!(!markup.value.contains("Pattern `num`"));
        assert!(!markup.value.contains("Identifiers:"));
        assert!(!markup.value.contains("Identifier pattern"));
    }

    #[test]
    fn hovers_sep_pattern_with_vec_type_and_keyword_details() {
        let offset = MOCK_GRAMMAR.find("$sep").unwrap() + 1;
        let hover = hover(
            MOCK_GRAMMAR,
            crate::lsp::position::offset_to_position(MOCK_GRAMMAR, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("Pattern `$sep(E, comma, +)`"));
        assert!(markup.value.contains("Final type: `Vec<i32>`"));
        assert!(markup.value.contains("Identifiers:"));
        assert!(markup.value.contains("**Non-terminal `E`**"));
        assert!(markup.value.contains("```rustylr\nE(i32)\n : num\n ;\n```"));
        assert!(!markup.value.contains("value=num"));
        assert!(markup.value.contains("**Terminal `comma`**"));
        assert!(markup
            .value
            .contains("```rustylr\n%token comma Token::Comma;\n```"));
        assert!(markup.value.contains("Pattern syntax:"));
        assert!(markup.value.contains("- `$sep(A, Sep, ...)`:"));
        assert_eq!(markup.value.matches("**Non-terminal `E`**").count(), 1);
        assert_eq!(markup.value.matches("**Terminal `comma`**").count(), 1);
        assert!(markup
            .value
            .contains("Pattern helper for separated repetition"));
    }

    #[test]
    fn hovers_whole_pattern_when_cursor_is_on_inner_symbol() {
        let sep_offset = MOCK_GRAMMAR.find("$sep").unwrap();
        let offset = MOCK_GRAMMAR[sep_offset..].find("comma").unwrap() + sep_offset + 1;
        let hover = hover(
            MOCK_GRAMMAR,
            crate::lsp::position::offset_to_position(MOCK_GRAMMAR, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("Pattern `$sep(E, comma, +)`"));
        assert!(markup.value.contains("Final type: `Vec<i32>`"));
        assert!(markup.value.contains("Identifiers:"));
        assert!(markup.value.contains("**Non-terminal `E`**"));
        assert!(markup.value.contains("**Terminal `comma`**"));
        assert!(markup.value.contains("Pattern syntax:"));
        assert!(markup
            .value
            .contains("Pattern helper for separated repetition"));
    }

    #[test]
    fn hovers_data_with_userdata_type() {
        let grammar_with_userdata = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32) }
%%
%userdata MyCoolData;
%tokentype Token;
%start Expr;
%token num Token::Num(_);
Expr : num { *data += 1; 0 };
"#;
        let offset = grammar_with_userdata.find("*data").unwrap() + 1; // points to 'd' in 'data'
        let hover = hover(
            grammar_with_userdata,
            crate::lsp::position::offset_to_position(grammar_with_userdata, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("data: &mut MyCoolData"));
        assert!(markup.value.contains("%userdata MyCoolData;"));
    }

    #[test]
    fn hovers_sigils() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32) }
%%
%location Span;
%userdata MyCoolData;
%tokentype Token;
%start Expr;
%token num Token::Num(_);
Expr : num { println!("{:?}, {:?}, {:?}", @1, @0, @$); 0 };
"#;
        // Hover on '@' of '@1'
        let offset = grammar.find("@1").unwrap();
        let hover1 = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup1) = hover1.contents else {
            panic!("expected markup hover");
        };
        assert!(markup1.value.contains("`@1` refers to a source-location"));

        // Hover on '@' of '@0'
        let offset = grammar.find("@0").unwrap();
        let hover2 = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup2) = hover2.contents else {
            panic!("expected markup hover");
        };
        assert!(markup2.value.contains("@0: &mut Span"));
        assert!(markup2.value.contains("%location Span;"));

        // Hover on '@' of '@$'
        let offset = grammar.find("@$").unwrap();
        let hover3 = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup3) = hover3.contents else {
            panic!("expected markup hover");
        };
        assert!(markup3.value.contains("@$: &mut Span"));
        assert!(markup3.value.contains("%location Span;"));
    }

    #[test]
    fn hovers_shift_with_mutable_bool_type() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32) }
%%
%glr;
%tokentype Token;
%start Expr;
%token num Token::Num(_);
Expr : num { *shift = false; 0 };
"#;
        let offset = grammar.find("shift").unwrap();
        let hover_res = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover_res.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("shift: &mut bool"));
        assert!(markup.value.contains("GLR reduce-action control binding"));
    }

    #[test]
    fn hovers_lookahead_with_token_type() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32) }
%%
%glr;
%tokentype Token;
%start Expr;
%token num Token::Num(_);
Expr : num { if let Some(_term) = lookahead.to_term() {} 0 };
"#;
        let offset = grammar.find("lookahead").unwrap();
        let hover_res = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover_res.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("lookahead: &Token"));
        assert!(markup.value.contains("%tokentype Token;"));
    }

    #[test]
    fn hovers_err_with_reduce_error_type() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32) }
struct MyError;
%%
%error MyError;
%tokentype Token;
%start Expr;
%token num Token::Num(_);
Expr : num { Err(MyError)?; 0 };
"#;
        let offset = grammar.find("Err(").unwrap();
        let hover_res = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover_res.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("Result::Err(MyError)"));
        assert!(markup.value.contains("%error MyError;"));
    }

    #[test]
    fn hovers_precedence_symbol() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32) }
%%
%userdata MyCoolData;
%tokentype Token;
%start Expr;
%left plus minus;
%token num Token::Num(_);
Expr : Expr plus Expr
     | num %prec minus
     ;
"#;
        let offset = grammar.find("minus").unwrap();
        let hover_res = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup) = hover_res.contents else {
            panic!("expected markup hover");
        };
        assert!(markup.value.contains("Precedence Symbol `minus`"));
        assert!(markup.value.contains("```rustylr\n%left plus minus;\n```"));
    }

    #[test]
    fn hovers_reduce_action_braces() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token { Num(i32), Plus }
%%
%tokentype Token;
%start Expr;
%token num Token::Num(_);
%token plus Token::Plus;
Expr : num { 0 }
     | num plus num {{ 0 }}
     ;
"#;

        // 1. Single brace start hover
        let start_brace_offset = grammar.find("{ 0 }").unwrap();
        let hover_start = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, start_brace_offset),
        )
        .unwrap();
        let HoverContents::Markup(markup_start) = hover_start.contents else {
            panic!("expected markup hover");
        };
        assert!(markup_start.value.contains("### Reduce Action"));
        assert!(markup_start
            .value
            .contains("A block of Rust code executed when this production rule is reduced"));
        assert!(markup_start.value.contains("#reduceaction-optional"));
        assert_eq!(
            hover_start.range.unwrap(),
            crate::lsp::position::range_to_lsp_range(
                grammar,
                start_brace_offset..start_brace_offset + 1
            )
        );

        // 2. Single brace end hover
        let end_brace_offset = start_brace_offset + 4; // points to '}' of '{ 0 }'
        let hover_end = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, end_brace_offset),
        )
        .unwrap();
        let HoverContents::Markup(markup_end) = hover_end.contents else {
            panic!("expected markup hover");
        };
        assert!(markup_end.value.contains("### Reduce Action"));
        assert_eq!(
            hover_end.range.unwrap(),
            crate::lsp::position::range_to_lsp_range(
                grammar,
                end_brace_offset..end_brace_offset + 1
            )
        );

        // 3. Double brace start hover (first brace)
        let dstart_brace_offset = grammar.find("{{ 0 }}").unwrap();
        let hover_dstart1 = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, dstart_brace_offset),
        )
        .unwrap();
        let HoverContents::Markup(markup_dstart1) = hover_dstart1.contents else {
            panic!("expected markup hover");
        };
        assert!(markup_dstart1.value.contains("### Reduce Action"));
        assert_eq!(
            hover_dstart1.range.unwrap(),
            crate::lsp::position::range_to_lsp_range(
                grammar,
                dstart_brace_offset..dstart_brace_offset + 2
            )
        );

        // 4. Double brace start hover (second brace)
        let hover_dstart2 = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, dstart_brace_offset + 1),
        )
        .unwrap();
        let HoverContents::Markup(markup_dstart2) = hover_dstart2.contents else {
            panic!("expected markup hover");
        };
        assert!(markup_dstart2.value.contains("### Reduce Action"));
        assert_eq!(
            hover_dstart2.range.unwrap(),
            crate::lsp::position::range_to_lsp_range(
                grammar,
                dstart_brace_offset..dstart_brace_offset + 2
            )
        );

        // 5. Double brace end hover (first of closing braces)
        let dend_brace_offset = grammar.find("}}").unwrap();
        let hover_dend1 = hover(
            grammar,
            crate::lsp::position::offset_to_position(grammar, dend_brace_offset),
        )
        .unwrap();
        let HoverContents::Markup(markup_dend1) = hover_dend1.contents else {
            panic!("expected markup hover");
        };
        assert!(markup_dend1.value.contains("### Reduce Action"));
        assert_eq!(
            hover_dend1.range.unwrap(),
            crate::lsp::position::range_to_lsp_range(
                grammar,
                dend_brace_offset..dend_brace_offset + 2
            )
        );
    }
}
