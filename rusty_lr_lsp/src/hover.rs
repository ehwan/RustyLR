use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use proc_macro2::TokenStream;
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::terminal_info::TerminalName;
use rusty_lr_parser::{GrammarArgs, PatternArgs, TerminalSetItem};
use std::collections::BTreeSet;
use std::ops::Range as ByteRange;

use crate::completion::{
    self, ALLOW_DIAGNOSTICS, DIRECTIVES, KEYWORDS, SUBSTITUTION_VARIABLES, SYNTAX_URL,
};
use crate::position::position_to_offset;

pub fn hover(content: &str, position: Position) -> Option<Hover> {
    let offset = position_to_offset(content, position);
    let parsed = completion::parse_args(content).ok();

    if let Some(args) = &parsed {
        if let Some((pattern, range)) = pattern_at_offset(args, offset) {
            return Some(markdown_hover(
                content,
                pattern_documentation(args, pattern, content),
                Some(range),
            ));
        }
    }

    let word = hover_word(content, offset)?;
    let documentation = if word == "data" {
        let mut userdata_type = "()".to_string();
        let mut definition_info = "".to_string();

        if let Some(args) = &parsed {
            if let Some((_, ts)) = args.userdata_typename.first() {
                userdata_type = ts.to_string();
                definition_info = format!(
                    "\n\nDefinition:\n```rustylr\n%userdata {};\n```",
                    userdata_type
                );
            }
        }

        Some(format!(
            "### `data: &mut {}`{}\n\nMutable user-data binding available inside reduce actions.\n\nExample:\n\n```rustylr\nExpr : num {{ data.count += 1; num }};\n```\n\n[User data]({}#4-user-data-data)",
            userdata_type,
            definition_info,
            SYNTAX_URL
        ))
    } else {
        hover_word_documentation(&word)
    };

    let documentation = documentation?;
    Some(markdown_hover(content, documentation, None))
}

fn markdown_hover(content: &str, value: String, range: Option<ByteRange<usize>>) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: range.map(|range| crate::position::range_to_lsp_range(content, range)),
    }
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
            crate::position::offset_to_position(MOCK_GRAMMAR, offset),
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
            crate::position::offset_to_position(MOCK_GRAMMAR, offset),
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
            crate::position::offset_to_position(MOCK_GRAMMAR, offset),
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
            crate::position::offset_to_position(MOCK_GRAMMAR, offset),
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
            crate::position::offset_to_position(grammar_with_userdata, offset),
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
%userdata MyCoolData;
%tokentype Token;
%start Expr;
%token num Token::Num(_);
Expr : num { println!("{:?}, {:?}", @1, @$); 0 };
"#;
        // Hover on '@' of '@1'
        let offset = grammar.find("@1").unwrap();
        let hover1 = hover(
            grammar,
            crate::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup1) = hover1.contents else {
            panic!("expected markup hover");
        };
        assert!(markup1.value.contains("`@1` refers to a source-location"));

        // Hover on '@' of '@$'
        let offset = grammar.find("@$").unwrap();
        let hover2 = hover(
            grammar,
            crate::position::offset_to_position(grammar, offset),
        )
        .unwrap();
        let HoverContents::Markup(markup2) = hover2.contents else {
            panic!("expected markup hover");
        };
        assert!(markup2.value.contains("`@$` refers to a source-location"));
    }
}
