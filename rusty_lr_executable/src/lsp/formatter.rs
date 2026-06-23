use lsp_types::TextEdit;
use proc_macro2::{TokenStream, TokenTree};
use rusty_lr_parser::{GrammarArgs, PatternArgs};
use std::ops::Range;

use crate::lsp::completion;
use crate::lsp::position::range_to_lsp_range;

const RULE_INDENT: &str = "    ";
const ACTION_INNER_INDENT: &str = "        ";

pub fn formatting(content: &str) -> Vec<TextEdit> {
    let Ok(args) = completion::parse_args(content) else {
        return Vec::new();
    };

    let mut edits = Vec::new();
    edits.extend(directive_edits(content));
    edits.extend(rule_edits(&args, content));
    edits
}

fn directive_edits(content: &str) -> Vec<TextEdit> {
    let Some(grammar_start) = content.find("%%").map(|idx| idx + 2) else {
        return Vec::new();
    };

    let comments = comment_ranges(content);
    let mut edits = Vec::new();
    let mut offset = line_start(content, grammar_start);
    while offset < content.len() {
        let current_line_end = line_end(content, offset);
        if offset >= grammar_start {
            let line_prefix = &content[offset..current_line_end];
            let leading = line_prefix.len() - line_prefix.trim_start().len();
            let directive_start = offset + leading;
            if content[directive_start..current_line_end].starts_with('%') {
                if let Some((range_end, new_text)) =
                    format_directive_block(content, directive_start, &comments)
                {
                    edits.push(TextEdit {
                        range: range_to_lsp_range(content, offset..range_end),
                        new_text,
                    });
                    offset = content[range_end..]
                        .find('\n')
                        .map_or(content.len(), |idx| range_end + idx + 1);
                    continue;
                }
            }
        }
        offset = content[current_line_end..]
            .find('\n')
            .map_or(content.len(), |idx| current_line_end + idx + 1);
    }
    edits
}

fn format_directive_block(
    content: &str,
    start: usize,
    comments: &[Range<usize>],
) -> Option<(usize, String)> {
    let semicolon = find_directive_semicolon(content, start)?;
    if range_has_comment(comments, start..semicolon) {
        return None;
    }

    let range_end = line_end(content, semicolon + 1);
    let directive = &content[start..semicolon];
    let trailing = content[semicolon + 1..range_end].trim_end();
    let mut formatted = normalize_directive_spacing(directive);
    formatted.push(';');
    if !trailing.is_empty() {
        formatted.push_str(trailing);
    }

    (formatted != content[line_start(content, start)..range_end]).then_some((range_end, formatted))
}

fn normalize_directive_spacing(directive: &str) -> String {
    let mut result = String::new();
    let mut pending_space = false;
    let trimmed = directive.trim();
    let mut chars = trimmed.char_indices();
    let mut quote = None;
    let mut escaped = false;

    while let Some((idx, ch)) = chars.next() {
        if let Some(quote_ch) = quote {
            result.push(ch);
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == quote_ch {
                quote = None;
            }
            continue;
        }

        if ch == '"' || (ch == '\'' && is_single_quote_literal_start(trimmed, idx)) {
            if pending_space && !result.is_empty() {
                result.push(' ');
                pending_space = false;
            }
            result.push(ch);
            quote = Some(ch);
        } else if ch.is_whitespace() {
            pending_space = true;
        } else {
            if pending_space && !result.is_empty() {
                result.push(' ');
                pending_space = false;
            }
            result.push(ch);
        }
    }

    result
}

fn find_directive_semicolon(content: &str, start: usize) -> Option<usize> {
    let mut quote = None;
    let mut escaped = false;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;

    let remaining = &content[start..];
    let mut iter = remaining.char_indices().peekable();
    while let Some((relative_idx, ch)) = iter.next() {
        if let Some(quote_ch) = quote {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == quote_ch {
                quote = None;
            }
            continue;
        }

        match ch {
            '"' => quote = Some(ch),
            '\'' if is_single_quote_literal_start(remaining, relative_idx) => quote = Some(ch),
            '/' => match iter.peek().copied() {
                Some((_, '/')) => {
                    iter.next();
                    while let Some((_, next_ch)) = iter.peek() {
                        if *next_ch == '\n' || *next_ch == '\r' {
                            break;
                        }
                        iter.next();
                    }
                }
                Some((_, '*')) => {
                    iter.next();
                    while let Some((_, next_ch)) = iter.next() {
                        if next_ch == '*' {
                            if let Some((_, '/')) = iter.peek() {
                                iter.next();
                                break;
                            }
                        }
                    }
                }
                _ => {}
            },
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '{' => brace_depth += 1,
            '}' => brace_depth = brace_depth.saturating_sub(1),
            ';' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                return Some(start + relative_idx);
            }
            _ => {}
        }
    }
    None
}

fn is_single_quote_literal_start(text: &str, quote_idx: usize) -> bool {
    let mut escaped = false;
    for (relative_idx, ch) in text[quote_idx + 1..].char_indices() {
        if ch == '\n' || ch == '\r' {
            return false;
        }

        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if ch == '\'' {
            let close_end = quote_idx + 1 + relative_idx + ch.len_utf8();
            return match text[close_end..].chars().next() {
                Some(next) => !matches!(next, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'),
                None => true,
            };
        }
    }

    false
}

fn rule_edits(args: &GrammarArgs, content: &str) -> Vec<TextEdit> {
    let comments = comment_ranges(content);
    args.rules
        .iter()
        .filter_map(|rule| {
            let rule_start = args
                .span_manager
                .get_byterange(&rule.name.location())?
                .start;
            let start = line_start(content, rule_start);
            let first_separator = rule.rule_lines.first().and_then(|line| {
                args.span_manager
                    .get_byterange(&line.separator_location)
                    .map(|range| range.start)
            })?;
            let header = content[start..first_separator.min(content.len())].trim();

            let mut formatted = String::new();
            formatted.push_str(header);
            for (line_idx, line) in rule.rule_lines.iter().enumerate() {
                let tokens = line_tokens_text(args, content, line);
                let modifiers = line_modifiers_text(args, content, rule, line_idx);
                let action = line
                    .reduce_action
                    .as_ref()
                    .and_then(|action| token_stream_text(content, action));

                formatted.push('\n');
                formatted.push_str(RULE_INDENT);
                formatted.push(if line_idx == 0 { ':' } else { '|' });
                if !tokens.is_empty() {
                    formatted.push(' ');
                    formatted.push_str(&tokens);
                }
                if !modifiers.is_empty() {
                    formatted.push(' ');
                    formatted.push_str(&modifiers);
                }
                if let Some(action) = action {
                    formatted.push(' ');
                    formatted.push_str(&format_reduce_action(&action));
                }
            }
            formatted.push('\n');
            formatted.push_str(RULE_INDENT);
            formatted.push(';');

            let end = rule_block_end(args, content, rule)?;
            let action_ranges = rule
                .rule_lines
                .iter()
                .filter_map(|line| line.reduce_action.as_ref().and_then(token_stream_range))
                .collect::<Vec<_>>();
            if has_comment_outside_ranges(&comments, start..end, &action_ranges) {
                return None;
            }

            Some(TextEdit {
                range: range_to_lsp_range(content, start..end),
                new_text: formatted,
            })
        })
        .collect()
}

fn format_reduce_action(action: &str) -> String {
    let trimmed = action.trim();
    if !trimmed.contains('\n') {
        return trimmed.to_string();
    }

    let lines = trimmed.lines().collect::<Vec<_>>();
    let last_non_empty = lines.iter().rposition(|line| !line.trim().is_empty());
    let body_indent = lines
        .iter()
        .enumerate()
        .filter(|(idx, line)| *idx != 0 && Some(*idx) != last_non_empty && !line.trim().is_empty())
        .map(|(_, line)| leading_indent_len(line))
        .min()
        .unwrap_or(0);

    lines
        .iter()
        .enumerate()
        .map(|(idx, line)| {
            if idx == 0 {
                line.trim_end().to_string()
            } else if line.trim().is_empty() {
                String::new()
            } else if Some(idx) == last_non_empty && line.trim_start().starts_with('}') {
                format!("{RULE_INDENT}{}", line.trim_start())
            } else {
                format!("{ACTION_INNER_INDENT}{}", strip_indent(line, body_indent))
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn leading_indent_len(line: &str) -> usize {
    line.char_indices()
        .find_map(|(idx, ch)| (!matches!(ch, ' ' | '\t')).then_some(idx))
        .unwrap_or(line.len())
}

fn strip_indent(line: &str, indent: usize) -> &str {
    if leading_indent_len(line) >= indent {
        &line[indent..]
    } else {
        line.trim_start()
    }
}

fn comment_ranges(content: &str) -> Vec<Range<usize>> {
    let mut ranges = Vec::new();
    let mut iter = content.char_indices().peekable();
    let mut quote = None;
    let mut escaped = false;

    while let Some((idx, ch)) = iter.next() {
        if let Some(quote_ch) = quote {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == quote_ch {
                quote = None;
            }
            continue;
        }

        match ch {
            '"' => quote = Some(ch),
            '\'' if is_single_quote_literal_start(content, idx) => quote = Some(ch),
            '/' => match iter.peek().copied() {
                Some((next_idx, '/')) => {
                    iter.next();
                    let end = content[next_idx + 1..]
                        .find('\n')
                        .map_or(content.len(), |line_end| next_idx + 1 + line_end);
                    ranges.push(idx..end);
                    while let Some((comment_idx, _)) = iter.peek().copied() {
                        if comment_idx >= end {
                            break;
                        }
                        iter.next();
                    }
                }
                Some((_, '*')) => {
                    iter.next();
                    let end = content[idx + 2..]
                        .find("*/")
                        .map_or(content.len(), |comment_end| idx + 2 + comment_end + 2);
                    ranges.push(idx..end);
                    while let Some((comment_idx, _)) = iter.peek().copied() {
                        if comment_idx >= end {
                            break;
                        }
                        iter.next();
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    ranges
}

fn range_has_comment(comments: &[Range<usize>], range: Range<usize>) -> bool {
    comments
        .iter()
        .any(|comment| ranges_overlap(comment, &range))
}

fn has_comment_outside_ranges(
    comments: &[Range<usize>],
    outer: Range<usize>,
    allowed: &[Range<usize>],
) -> bool {
    comments
        .iter()
        .filter(|comment| ranges_overlap(comment, &outer))
        .any(|comment| {
            !allowed
                .iter()
                .any(|allowed_range| range_contains(allowed_range, comment))
        })
}

fn ranges_overlap(left: &Range<usize>, right: &Range<usize>) -> bool {
    left.start < right.end && right.start < left.end
}

fn range_contains(outer: &Range<usize>, inner: &Range<usize>) -> bool {
    outer.start <= inner.start && inner.end <= outer.end
}

fn line_tokens_text(
    args: &GrammarArgs,
    content: &str,
    line: &rusty_lr_parser::RuleLineArgs,
) -> String {
    line.tokens
        .iter()
        .map(|(mapped_name, pattern)| {
            let start = mapped_name
                .as_ref()
                .and_then(|name| {
                    args.span_manager
                        .get_byterange(&name.location())
                        .map(|range| range.start)
                })
                .unwrap_or_else(|| pattern_start(args, pattern));
            let end = pattern_end(args, pattern);
            content[start.min(content.len())..end.min(content.len())]
                .trim()
                .to_string()
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn line_modifiers_text(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
    line_idx: usize,
) -> String {
    let line = &rule.rule_lines[line_idx];
    let start = line
        .tokens
        .iter()
        .map(|(_, pattern)| pattern_end(args, pattern))
        .max()
        .unwrap_or_else(|| {
            args.span_manager
                .get_byterange(&line.separator_location)
                .map_or(0, |range| range.end)
        });
    let end = line
        .reduce_action
        .as_ref()
        .and_then(|action| token_stream_range(action).map(|range| range.start))
        .unwrap_or_else(|| rule_line_end(args, content, rule, line_idx));

    content[start.min(content.len())..end.min(content.len())]
        .trim()
        .to_string()
}

fn rule_block_end(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
) -> Option<usize> {
    let last_line_idx = rule.rule_lines.len().checked_sub(1)?;
    let end_hint = rule_line_end(args, content, rule, last_line_idx);
    let semicolon = content[end_hint.min(content.len())..].find(';')?;
    Some(line_end(content, end_hint + semicolon + 1))
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
        if let Some(range) = token_stream_range(action) {
            end = end.max(range.end);
        }
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

fn token_stream_text(content: &str, stream: &TokenStream) -> Option<String> {
    let range = token_stream_range(stream)?;
    content
        .get(range.start.min(content.len())..range.end.min(content.len()))
        .map(str::trim)
        .filter(|text| !text.is_empty())
        .map(str::to_string)
}

fn token_stream_range(stream: &TokenStream) -> Option<std::ops::Range<usize>> {
    let mut start = usize::MAX;
    let mut end = 0;
    for token in stream.clone() {
        let range = token_tree_range(token);
        start = start.min(range.start);
        end = end.max(range.end);
    }
    if start == usize::MAX {
        None
    } else {
        Some(start..end)
    }
}

fn token_tree_range(token: TokenTree) -> std::ops::Range<usize> {
    match token {
        TokenTree::Group(group) => {
            let open = group.span_open().byte_range();
            let close = group.span_close().byte_range();
            let inner = token_stream_range(&group.stream());
            let start = inner
                .as_ref()
                .map_or(open.start, |range| range.start)
                .min(open.start);
            let end = inner
                .as_ref()
                .map_or(close.end, |range| range.end)
                .max(close.end);
            start..end
        }
        TokenTree::Ident(ident) => ident.span().byte_range(),
        TokenTree::Punct(punct) => punct.span().byte_range(),
        TokenTree::Literal(lit) => lit.span().byte_range(),
    }
}

fn line_start(content: &str, offset: usize) -> usize {
    content[..offset.min(content.len())]
        .rfind('\n')
        .map_or(0, |idx| idx + 1)
}

fn line_end(content: &str, offset: usize) -> usize {
    content[offset.min(content.len())..]
        .find('\n')
        .map_or(content.len(), |idx| offset + idx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::position::position_to_offset;

    const MOCK_GRAMMAR: &str = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
    Plus,
}

%%

%tokentype
    Token;
%start    E;
%userdata
    ParserState;
%allow
    unused_terminals([
        'a'-'z'
        '+'
    ]);
%left    plus   "spaced literal";
%token n Token::Num(_);
%token plus   Token::Plus;

E(i32):left=E plus n { left }
| n {
    n
}
;
"#;

    #[test]
    fn formats_tokens_and_productions() {
        let edits = formatting(MOCK_GRAMMAR);
        let formatted = apply_edits(MOCK_GRAMMAR, edits);

        assert!(formatted.contains("%token n Token::Num(_);"));
        assert!(formatted.contains("%token plus Token::Plus;"));
        assert!(formatted.contains("%tokentype Token;"));
        assert!(formatted.contains("%start E;"));
        assert!(formatted.contains("%userdata ParserState;"));
        assert!(formatted.contains("%allow unused_terminals([ 'a'-'z' '+' ]);"));
        assert!(formatted.contains("%left plus \"spaced literal\";"));
        assert!(formatted
            .contains("E(i32)\n    : left=E plus n { left }\n    | n {\n        n\n    }\n    ;"));
    }

    #[test]
    fn indents_reduce_action_body_one_level_deeper() {
        let edits = formatting(MOCK_GRAMMAR);
        let formatted = apply_edits(MOCK_GRAMMAR, edits);
        assert!(formatted.contains("{\n        n\n    }"));
    }

    #[test]
    fn reindents_reduce_action_as_a_whole_block() {
        let action = r#"{
                if n > 0 {
                    n
                } else {
                    0
                }
            }"#;

        assert_eq!(
            format_reduce_action(action),
            "{\n        if n > 0 {\n            n\n        } else {\n            0\n        }\n    }"
        );
    }

    #[test]
    fn formats_multiline_directive_as_one_line() {
        let content = "%%\n%tokentype\n    [u8; 32];\n%userdata\n    &'a   str;\n%start\n    E;\n";
        let formatted = apply_edits(content, directive_edits(content));

        assert!(formatted.contains("%tokentype [u8; 32];"));
        assert!(formatted.contains("%userdata &'a str;"));
        assert!(formatted.contains("%start E;"));
    }

    #[test]
    fn skips_rule_formatting_when_grammar_comment_would_be_lost() {
        let content = r#"
#[derive(Debug, Clone)]
pub enum Token { A }

%%

%tokentype Token;
%token a Token::A;

Rule(i32): a { 1 }
// | a { 2 }
;
"#;
        let formatted = apply_edits(content, formatting(content));

        assert!(formatted.contains("Rule(i32): a { 1 }\n// | a { 2 }\n;"));
    }

    #[test]
    fn formats_reduce_action_comments_inside_action_range() {
        let content = r#"
#[derive(Debug, Clone)]
pub enum Token { A }

%%

%tokentype Token;
%token a Token::A;

Rule(i32): a {
    // keep this comment
    1
}
;
"#;
        let formatted = apply_edits(content, formatting(content));

        assert!(formatted.contains(
            "Rule(i32)\n    : a {\n        // keep this comment\n        1\n    }\n    ;"
        ));
    }

    #[test]
    fn skips_multiline_directive_with_inline_comment() {
        let content = "%%\n%tokentype\n    // token type comment\n    Token;\n";
        let formatted = apply_edits(content, directive_edits(content));

        assert!(formatted.contains("%tokentype\n    // token type comment\n    Token;"));
    }

    #[test]
    fn formats_directive_with_comments_containing_semicolons() {
        // Single-line comment with semicolon
        let content1 = "%%\n%token num Token::Num(_); // comment; here\n";
        let formatted1 = apply_edits(content1, formatting(content1));
        assert_eq!(
            formatted1,
            "%%\n%token num Token::Num(_); // comment; here\n"
        );

        // Multi-line block comment with semicolon
        let content2 = "%%\n%token num Token::Num(_) /* comment; here */ ;\n";
        let formatted2 = apply_edits(content2, formatting(content2));
        assert_eq!(
            formatted2,
            "%%\n%token num Token::Num(_) /* comment; here */ ;\n"
        );
    }

    #[test]
    fn preserves_comments_in_parser_grammar_fixture() {
        let content = include_str!("../../../rusty_lr_parser/src/parser/parser.rustylr");
        let formatted = apply_edits(content, formatting(content));

        assert!(formatted.contains("// | Pattern error {"));
        assert!(formatted.contains("//     Pattern"));
        assert!(crate::lsp::completion::parse_args(&formatted).is_ok());
    }

    fn apply_edits(content: &str, edits: Vec<TextEdit>) -> String {
        let mut edits = edits
            .into_iter()
            .map(|edit| {
                let start = position_to_offset(content, edit.range.start);
                let end = position_to_offset(content, edit.range.end);
                (start, end, edit.new_text)
            })
            .collect::<Vec<_>>();
        edits.sort_by_key(|(start, _, _)| *start);

        let mut result = String::new();
        let mut cursor = 0;
        for (start, end, new_text) in edits {
            result.push_str(&content[cursor..start]);
            result.push_str(&new_text);
            cursor = end;
        }
        result.push_str(&content[cursor..]);
        result
    }
}
